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
#include "pointer_value.h"

class Expr;

struct SymbolInfo {
    llvm::Value* alloca;  
    Type* type;               
};

struct SymbolFunction {
    TypeInfo returnType;
    std::vector<TypeInfo> argType;
    llvm::Function* func;
    bool classFunction = false;
    std::string className = "";
};

// Structure for generalized function call parameters
struct FunctionCallParams {
    std::string functionName;
    std::string className = ""; 
    bool isClassFunction = false;
    bool isConstructor = false;  
    bool requiresVoidReturn = false;  
    std::vector<llvm::Value*> extraArgs;  // For additional arguments like 'this' pointer
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

Value* invokeMemberFunction(
    std::string nameOfClass,
    ClassType* classType,
    std::string memberName,
    std::string nameCurrVar,
    std::vector<Expr*> args,
    llvm::Value* currentPtr,
    bool wantReturn,
    llvm::IRBuilder<>& builder
);

Value* generateClassFunctionCall(
    llvm::IRBuilder<>& builder,
    const std::string firstVariableName,
    const std::vector<std::string> memberChain,
    const std::string nameOfClass,
    const std::vector<Expr*> args,
    bool returnsValue
);

std::pair<SymbolFunction*, std::vector<llvm::Value*>> 
prepareAndValidateFunctionCall(
    const FunctionCallParams& params,
    const std::vector<Expr*>& args,
    llvm::IRBuilder<>& builder
); 
