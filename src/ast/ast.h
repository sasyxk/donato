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
    llvm::Value* alloca;  
    Type* type;               
};

struct SymbolFunction {
    Type* returnType;
    std::vector<Type* > argType;
    llvm::Function* func;
    bool classFunction = false;
    std::string className = "";
};

extern std::vector<std::map<std::string, SymbolInfo>> symbolTable;
extern std::vector<std::pair<std::string, SymbolFunction>> symbolFunctions;
extern std::vector<StructType* > symbolStructsType;
extern std::vector<ClassType* > symbolClassType;
extern llvm::Module* module;


std::pair<llvm::Value*, Type*> getStructMemberGEP(
    llvm::IRBuilder<>& builder,
    llvm::Value* ptrToStruct,
    StructType* rootType,
    const std::vector<std::string>& memberChain
);

void generateAllocFunction(
    llvm::IRBuilder<> &builder,
    std::string className,
    llvm::StructType* classType
);

void generateFreeFunction(
    llvm::IRBuilder<> &builder,
    std::string className,
    llvm::StructType* classType
);