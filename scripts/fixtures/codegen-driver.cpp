#include "codegen.h"
#include "compiler_flags.h"
#include <iostream>

int main(int argc, char** argv) {
    if (argc != 4) return 2;
    llvm::LLVMContext context;
    llvm::Module input("driver_regression", context);
    llvm::IRBuilder<> builder(context);
    auto* type = llvm::FunctionType::get(builder.getInt32Ty(), false);
    auto* function = llvm::Function::Create(type, llvm::Function::ExternalLinkage, "main", input);
    builder.SetInsertPoint(llvm::BasicBlock::Create(context, "entry", function));
    // An unterminated block exercises the real driver's verifier even after
    // all known Donato sources of invalid IR have been fixed.
    if (std::string(argv[1]) == "valid") builder.CreateRet(builder.getInt32(0));
    CompilerFlags::instance().optimizationLevel = std::stoi(argv[2]);
    try {
        generateExecutable(input, argv[3]);
    } catch (const std::exception& error) {
        std::cerr << "Error in codegen:: " << error.what() << "\n";
        return 1;
    }
    return 0;
}
