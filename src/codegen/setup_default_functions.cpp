#include "setup_default_functions.h"




void errorsFunction(
    llvm::IRBuilder<> &builder,
    llvm::Module* module
){
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

void mallocFunction(
    llvm::IRBuilder<> &builder,
    llvm::Module* module
){
    llvm::LLVMContext &ctx = builder.getContext();

    llvm::FunctionType* mallocType = llvm::FunctionType::get(
        llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0),
        { llvm::Type::getInt64Ty(ctx) },
        false
    );

    llvm::Function::Create(
        mallocType,
        llvm::Function::ExternalLinkage,
        "d_malloc",
        module
    );
}

void freeFunction(
    llvm::IRBuilder<> &builder,
    llvm::Module* module
){
    llvm::LLVMContext &ctx = builder.getContext();

    // free(i8*)
    llvm::FunctionType* freeType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(ctx),
        { llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0) },
        false
    );

    llvm::Function::Create(
        freeType,
        llvm::Function::ExternalLinkage,
        "d_free",
        module
    );
}

void setupFunctions(
    llvm::IRBuilder<> &builder,
    llvm::Module* module
){
    errorsFunction(builder, module);
    mallocFunction(builder, module);
    freeFunction(builder, module);
}
