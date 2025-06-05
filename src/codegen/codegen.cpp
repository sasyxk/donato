#include "codegen.h"
#include <cstdlib>

void generateExecutable(llvm::Module& module, const std::string& outputName) {
    // Write IR to file
    std::error_code EC;
    llvm::raw_fd_ostream out("output.ll", EC);
    module.print(out, nullptr);
    out.close();

    // Compile and link
    system(("llc -O3 -filetype=obj output.ll -o output.o && clang -I ../src/error_handling ../src/error_handling/errors.c  output.o -o " + outputName).c_str());
    //system(("llc -filetype=obj output.ll -o output.o && clang  ../Src/output_eval.c ../Src/error.c output.o -o " + outputName).c_str());
}