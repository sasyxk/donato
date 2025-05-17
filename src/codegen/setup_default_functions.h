#pragma once
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

void setupFunctions(llvm::IRBuilder<> &builder, llvm::Module* module);