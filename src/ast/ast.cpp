#include "ast.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <iostream>

std::vector<std::map<std::string, SymbolInfo>> symbolTable;

std::vector<std::pair<std::string, SymbolFunction>> symbolFunctions;
std::vector<StructType*> symbolStructsType;
std::vector<ClassType* > symbolClassType;


std::pair<llvm::Value*, Type*> getStructMemberGEP(
    llvm::IRBuilder<>& builder,
    llvm::Value* ptrToStruct,
    StructType* rootType,
    const std::vector<std::string>& memberChain
) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* currentPtr = ptrToStruct;
    StructType* currentStruct = rootType;
    Type* currentType = nullptr;

    for (size_t depth = 0; depth < memberChain.size(); ++depth) {
        const std::string& memberName = memberChain[depth];
        auto members = currentStruct->getMembers();

        bool found = false;
        size_t index = 0;
        for (size_t i = 0; i < members.size(); ++i) {
            if (members[i].second == memberName) {
                index = i;
                currentType = members[i].first;
                found = true;
                break;
            }
        }

        if (!found)
            throw std::runtime_error("Member '" + memberName + "' not found in struct.");

        currentPtr = builder.CreateStructGEP(
            currentStruct->getLLVMType(ctx),
            currentPtr,
            index,
            memberName
        );

        if (depth < memberChain.size() - 1) {
            currentStruct = dynamic_cast<StructType*>(currentType);
            if (!currentStruct)
                throw std::runtime_error("Member '" + memberName + "' is not a nested struct.");
        }
    }

    return {currentPtr, currentType};
}
