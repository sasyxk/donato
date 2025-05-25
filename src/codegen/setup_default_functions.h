#pragma once
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

void errorsFunction(
    llvm::IRBuilder<> &builder,
    llvm::Module* module
);

void mallocFunction(
    llvm::IRBuilder<> &builder,
    llvm::Module* module
);

void freeFunction(
    llvm::IRBuilder<> &builder,
    llvm::Module* module
);

void setupFunctions(
    llvm::IRBuilder<> &builder,
    llvm::Module* module
);