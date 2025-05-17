#include "setup_default_functions.h"

void setupFunctions(llvm::IRBuilder<> &builder, llvm::Module* module){
    llvm::LLVMContext &ctx = builder.getContext();

    llvm::FunctionType* errFuncType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(ctx),
        { llvm::Type::getInt32Ty(ctx) },
        false
    );
    llvm::Function::Create(
        errFuncType,
        llvm::Function::ExternalLinkage,
        "llvm_error",
        module
    );
}
