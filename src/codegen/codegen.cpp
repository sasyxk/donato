#include "codegen.h"
#include "compiler_flags.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Program.h"
#include <filesystem>
#include <stdexcept>
#include <vector>

namespace {
namespace fs = std::filesystem;

void checkPaths(const std::vector<fs::path>& paths) {
    for (size_t i = 0; i < paths.size(); ++i) {
        if (paths[i].empty()) continue;
        for (size_t j = 0; j < i; ++j) {
            if (paths[j].empty()) continue;
            std::error_code error;
            bool sameFile = fs::equivalent(paths[i], paths[j], error);
            if ((!error && sameFile) || fs::weakly_canonical(paths[i]) == fs::weakly_canonical(paths[j])) {
                throw std::runtime_error("Conflicting compiler paths: '" + paths[i].string()
                                         + "' and '" + paths[j].string() + "'");
            }
        }
    }
}

void writeIR(llvm::Module& module) {
    std::error_code error;
    llvm::raw_fd_ostream out("output.ll", error);
    if (error) {
        out.clear_error();
        throw std::runtime_error("Cannot open IR file 'output.ll': " + error.message());
    }
    module.print(out, nullptr);
    out.close();
    if (out.has_error()) {
        error = out.error();
        out.clear_error();
        throw std::runtime_error("Cannot write or close IR file 'output.ll': " + error.message());
    }
}

void runTool(const std::string& name, const std::vector<std::string>& arguments) {
    auto program = llvm::sys::findProgramByName(name);
    if (!program) {
        throw std::runtime_error("Tool '" + name + "' not found on PATH: "
                                 + program.getError().message());
    }
    std::vector<llvm::StringRef> argv = {*program};
    for (const auto& argument : arguments) argv.push_back(argument);
    std::string error;
    bool executionFailed = false;
    int status = llvm::sys::ExecuteAndWait(*program, argv, std::nullopt, {}, 0, 0,
                                          &error, &executionFailed);
    if (executionFailed || status == -1) {
        throw std::runtime_error("Cannot execute '" + name + "': " + error);
    }
    if (status < 0) {
        throw std::runtime_error("Tool '" + name + "' terminated abnormally: " + error);
    }
    if (status != 0) {
        throw std::runtime_error("Tool '" + name + "' failed with exit status "
                                 + std::to_string(status));
    }
}

void checkProduct(const fs::path& path, const std::string& tool, bool executable) {
    std::error_code error;
    auto status = fs::symlink_status(path, error);
    if (error || !fs::is_regular_file(status)) {
        throw std::runtime_error("Tool '" + tool + "' did not produce a regular output file");
    }
    auto size = fs::file_size(path, error);
    if (error || size == 0) {
        throw std::runtime_error("Tool '" + tool + "' did not produce a nonempty output file");
    }
    if (executable && !llvm::sys::fs::can_execute(path.string())) {
        throw std::runtime_error("Tool '" + tool + "' did not produce an executable file");
    }
}
}

void generateExecutable(llvm::Module& module, const std::string& outputName) {
    try {
        if (llvm::verifyModule(module, &llvm::errs())) {
            throw std::runtime_error("LLVM module verification failed");
        }
        if (outputName.empty()) {
            throw std::runtime_error("Output executable path is empty");
        }
        auto& flags = CompilerFlags::instance();
        fs::path target = fs::absolute(outputName).lexically_normal();
        // Writing diagnostic intermediates must not overwrite the input or
        // previous executable, including aliases through symlinks/hard links.
        checkPaths({target, "output.ll", "output.o", flags.filename});
        if (fs::exists(target) && !fs::is_regular_file(target)) {
            throw std::runtime_error("Output executable path is not a regular file: '"
                                     + target.string() + "'");
        }

        llvm::SmallString<128> directory;
        std::error_code error = llvm::sys::fs::createUniqueDirectory(
            (target.parent_path() / ".dtc").string(), directory);
        if (error) {
            throw std::runtime_error("Cannot create temporary output directory: " + error.message());
        }
        fs::path temporaryDirectory(directory.str().str());
        auto cleanup = llvm::make_scope_exit([&] {
            std::error_code cleanupError;
            fs::remove_all(temporaryDirectory, cleanupError);
            if (cleanupError) {
                llvm::errs() << "Cannot remove temporary output directory '"
                             << temporaryDirectory.string() << "': " << cleanupError.message() << "\n";
            }
        });
        fs::path object = temporaryDirectory / "output.o";
        fs::path executable = temporaryDirectory / "program";

        writeIR(module);
        runTool("llc", {"-O" + std::to_string(flags.optimizationLevel),
                        "-filetype=obj", "output.ll", "-o", object.string()});
        checkProduct(object, "llc", false);
        // Retain the diagnostic object used by the existing checks. Linking
        // always uses this invocation's private object, never an older copy.
        fs::copy_file(object, "output.o", fs::copy_options::overwrite_existing, error);
        if (error) {
            throw std::runtime_error("Cannot write object file 'output.o': " + error.message());
        }
        runTool("clang", {"-I", "../src/error_handling", "../src/error_handling/errors.c",
                          object.string(), "-o", executable.string()});
        checkProduct(executable, "clang", true);
        // The temporary executable is on the destination filesystem. Publish
        // only after every compilation step and output check has succeeded.
        fs::rename(executable, target, error);
        if (error) {
            throw std::runtime_error("Cannot replace output executable '" + outputName
                                     + "': " + error.message());
        }
    } catch (const std::exception& error) {
        throw std::runtime_error(std::string(error.what()) + "\nOutput executable '"
                                 + outputName + "' was not updated.");
    }
}
