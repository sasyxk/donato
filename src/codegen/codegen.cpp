#include "codegen.h"
#include "compiler_flags.h"
#include <cstdlib>

void generateExecutable(llvm::Module& module, const std::string& outputName) {
    auto& flags = CompilerFlags::instance();


    // Write IR to file
    std::error_code EC;
    llvm::raw_fd_ostream out("output.ll", EC);
    module.print(out, nullptr);
    out.close();

    std::string optLevel = "-O" + std::to_string(flags.optimizationLevel);

    std::string command = "llc " + optLevel + " -filetype=obj output.ll -o output.o && "
                          "clang -I ../src/error_handling ../src/error_handling/errors.c output.o -o " + flags.outputName;
    system(command.c_str());

    // Compile and link
    //system(("llc -O3 -filetype=obj output.ll -o output.o && clang -I ../src/error_handling ../src/error_handling/errors.c  output.o -o " + outputName).c_str());
}