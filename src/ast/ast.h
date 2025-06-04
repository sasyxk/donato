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
    Type* returnType;
    std::vector<Type* > argType;
    llvm::Function* func;
    bool classFunction = false;
    std::string className = "";
};

// Struttura per i parametri della funzione generalizzata
struct FunctionCallParams {
    std::string functionName;
    std::string className = "";  // Vuoto per funzioni non-member
    bool isClassFunction = false;
    bool isConstructor = false;   // Per costruttori - logica di ricerca diversa
    bool requiresVoidReturn = false;  // Per CallFuncStatement
    std::vector<llvm::Value*> extraArgs;  // Per argomenti aggiuntivi come 'this' pointer
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

// Funzione generalizzata per la preparazione e validazione delle chiamate a funzione
std::pair<SymbolFunction*, std::vector<llvm::Value*>> 
prepareAndValidateFunctionCall(
    const FunctionCallParams& params,
    const std::vector<Expr*>& args,
    llvm::IRBuilder<>& builder
); 
