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


/*
    This function does nothing but generate a
    llvm function that returns a new pointer
    that is in the heap memory of the specific
    class type. Below is an extended example of
    how the llvm code that is written thanks to
    the function would be unoptimized, then the
    compiler will optimize it automatically.


    define %ClassName* @Point3D_alloc() {
    entry:
        %null = inttoptr i64 0 to %ClassName*
        %gep = getelementptr %ClassName, %ClassName* %null, i32 1
        %size = ptrtoint %ClassName* %gep to i64
        %mem = call i8* @d_malloc(i64 %size)
        %pointer= bitcast i8* %mem to %ClassName*
        ret %ClassName* %pointer
    }
*/
void generateAllocFunction(
    llvm::IRBuilder<> &builder,
    std::string className,
    llvm::StructType* classType
){
    // Create the pointer type to the class
    llvm::PointerType* classPtrType = llvm::PointerType::get(classType, 0);
    
    // Defines the type of the function: %ClassName* ()
    llvm::FunctionType* funcType = llvm::FunctionType::get(classPtrType, false);
    
    // Create function name: ClassName_alloc
    std::string funcName = className + "_alloc";
    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Function* allocFunc = llvm::Function::Create(funcType, 
                                                      llvm::Function::ExternalLinkage,
                                                      funcName, module);
    
    llvm::BasicBlock* entryBB = llvm::BasicBlock::Create(ctx, "entry", allocFunc);
    builder.SetInsertPoint(entryBB);
    
    // 1. %null = inttoptr i64 0 to %ClassName*
    llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx), 0);
    llvm::Value* nullPtr = builder.CreateIntToPtr(zero, classPtrType, "null");
    
    // 2. %gep = getelementptr %ClassName, %ClassName* %null, i32 1
    llvm::Value* one = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 1);
    llvm::Value* gep = builder.CreateGEP(classType, nullPtr, one, "gep");
    
    // 3. %size = ptrtoint %ClassName* %gep to i64
    llvm::Value* size = builder.CreatePtrToInt(gep, llvm::Type::getInt64Ty(ctx), "size");
    
    // Get the d_malloc function
    llvm::Function* mallocFunc = module->getFunction("d_malloc");
    if (!mallocFunc) {
        throw std::runtime_error("Function d_malloc not found");
    }
    
    // 4. %mem = call i8* @malloc(i64 %size)
    llvm::Value* mem = builder.CreateCall(mallocFunc, {size}, "mem");
    
    // 5. %pointer = bitcast i8* %mem to %ClassName*
    std::string varName = className;
    std::transform(varName.begin(), varName.end(), varName.begin(), ::tolower);
    llvm::Value* value = builder.CreateBitCast(mem, classPtrType, varName);
    
    // 6. ret %ClassName* %pointer
    builder.CreateRet(value);
}