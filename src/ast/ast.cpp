#include "ast.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <iostream>

std::vector<std::map<std::string, SymbolInfo>> symbolTable;

std::vector<std::pair<std::string, Type*>> symbolFunctions; //Name, return
std::vector<StructType*> symbolStructsType;