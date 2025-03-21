#include "codegen.h"
#include <cstdlib>

void generateExecutable(llvm::Module& module, const std::string& outputName) {
    // Write IR to file
    std::error_code EC;
    llvm::raw_fd_ostream out("output.ll", EC);
    module.print(out, nullptr);
    out.close();

    // Compile and link
    system(("llc -filetype=obj output.ll -o output.o && clang ee.c output.o -o " + outputName).c_str());
}