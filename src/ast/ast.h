#pragma once
#include <vector>
#include <string>
#include <map>
#include <utility>
#include "llvm/IR/Value.h"
#include "llvm/IR/IRBuilder.h"
#include "codegen.h"
#include "type.h"
#include "value.h"
#include "double_value.h"
#include "signed_int_value.h"
#include "bool_value.h"

struct SymbolInfo {
    llvm::AllocaInst* alloca;  
    Type* type;               
};

extern std::vector<std::map<std::string, SymbolInfo>> symbolTable;
extern std::vector<std::pair<std::string, Type*>> symbolFunctions;
extern std::vector<StructType* > symbolStructsType;
extern llvm::Module* module;