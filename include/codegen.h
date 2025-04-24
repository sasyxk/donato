#pragma once
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

extern llvm::Module* module;

void generateExecutable(llvm::Module& module, const std::string& outputName);