#include "ast.h"
#include "expr.h"

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

/*
    void @ClassName_free(%ClassName* %pointer) {
    entry:
        %mem = bitcast %ClassName* %pointer to i8*
        call void @d_free(i8* %mem)
        ret void
    }
*/
void generateFreeFunction(
    llvm::IRBuilder<> &builder,
    std::string className,
    llvm::StructType* classType
){
    // Create the pointer type to the class
    llvm::PointerType* classPtrType = llvm::PointerType::get(classType, 0);
    
    // Defines the type of the function: void (%ClassName*)
    llvm::FunctionType* funcType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(builder.getContext()),
        {classPtrType}, false);
    
    // Create function name: ClassName_free
    std::string funcName = className + "_free";
    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Function* freeFunc = llvm::Function::Create(
        funcType, 
        llvm::Function::ExternalLinkage,
        funcName,
        module);
    
    // Get the function parameter (the pointer to free)
    llvm::Argument* pointArg = freeFunc->arg_begin();
    std::string varName = className;
    std::transform(varName.begin(), varName.end(), varName.begin(), ::tolower);
    pointArg->setName(varName);
    
    llvm::BasicBlock* entryBB = llvm::BasicBlock::Create(ctx, "entry", freeFunc);
    builder.SetInsertPoint(entryBB);
    
    // 1. %mem = bitcast %ClassName* %point to i8*
    llvm::Value* mem = builder.CreateBitCast(pointArg, llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0), "mem");
    
    // Get the d_free function
    llvm::Function* d_freeFunc = module->getFunction("d_free");
    if (!d_freeFunc) {
        throw std::runtime_error("Function d_free not found");
    }
    
    // 2. call void @d_free(i8* %mem)
    builder.CreateCall(d_freeFunc, {mem});
    
    // 3. ret void
    builder.CreateRetVoid();
}


// This function prepares and validates a function or constructor call in the code generation phase.
// It searches for the correct function definition based on the name and context, such as whether
// it's a constructor or a class method. It checks that the number of arguments matches the expected
// function signature and ensures the return type is compatible when a void return is required.
// Then, it generates the LLVM IR values for the arguments, handling reference passing and type checking.
// Finally, it returns the matched function along with the list of generated argument values.
std::pair<SymbolFunction*, std::vector<llvm::Value*>> prepareAndValidateFunctionCall(
    const FunctionCallParams& params,
    const std::vector<Expr*>& args,
    llvm::IRBuilder<>& builder
) {
   // Function search
    SymbolFunction* functionStruct = nullptr;
    bool checkFunc = false;
    
    for (auto& function : symbolFunctions) {
        bool nameMatch = (function.first == params.functionName);
        
        // Logic for constructor
        if (params.isConstructor) {
            if (nameMatch) {
                functionStruct = &function.second;
                checkFunc = true;
                break;
            }
        } else {
            // Logic for functions and methods
            bool classMatch = params.isClassFunction ? 
                (function.second.classFunction && function.second.className == params.className) :
                (!function.second.classFunction);
                
            if (nameMatch && classMatch) {
                functionStruct = &function.second;
                checkFunc = true;
                break;
            }
        }
    }
    
    if (!checkFunc) {
        std::string errorMsg;
        if (params.isConstructor) {
            errorMsg = "Constructor '" + params.functionName + "' not found";
        } else if (params.isClassFunction) {
            errorMsg = "Function '" + params.functionName + "' of class '" + params.className + "' not found";
        } else {
            errorMsg = "Function not found: " + params.functionName;
        }
        throw std::runtime_error(errorMsg);
    }
    
    // Validate number of arguments
    size_t expectedArgCount = args.size() + params.extraArgs.size();
    if (functionStruct->argType.size() != expectedArgCount) {
        std::string context;
        if (params.isConstructor) {
            context = "Constructor " + params.functionName;
        } else if (params.isClassFunction) {
            context = params.functionName;
        } else {
            context = params.functionName;
        }
        throw std::runtime_error("Argument count mismatch for " + context);
    }
    
   // Check return type for void (if required)
    if (params.requiresVoidReturn && dynamic_cast<VoidType*>(functionStruct->returnType) == nullptr) {
        throw std::runtime_error(
            "The function '" + params.functionName + 
            "' is not Void, Can't call without store the return value"
        );
    }
    
    // Generate argument values
    std::vector<llvm::Value*> argValues;
    
    // Add extra arguments (like 'this' pointer) at the beginning
    for (auto* extraArg : params.extraArgs) {
        argValues.push_back(extraArg);
    }
    
    // Process normal arguments
    for (auto* arg : args) {
        size_t currentArgIndex = argValues.size();
        bool isVar = false;
        
       // Check whether the argument should be passed by reference
        if (Var* varPtr = dynamic_cast<Var*>(arg)) {
            if (functionStruct->argType.at(currentArgIndex)->isPointer()) {
                isVar = true;
            }
        }
        else if (StructVar* structVarPtr = dynamic_cast<StructVar*>(arg)) {
            if (functionStruct->argType.at(currentArgIndex)->isPointer()) {
                isVar = true;
            }
        }
        
        Value* value = arg->codegen(builder, isVar);
        
        // Reference validation
        if (functionStruct->argType.at(currentArgIndex)->isPointer() && 
            !value->getType()->isPointer()) {
            throw std::runtime_error(
                "Function " + params.functionName + 
                " argument " + std::to_string(currentArgIndex + 1) + 
                " wants a reference pass, insert a variable as an argument"
            );
        }
        
        // Type validation
        if (!(*value->getType() == *functionStruct->argType.at(currentArgIndex))) {
            throw std::runtime_error(
                "Type mismatch in argument " + std::to_string(currentArgIndex + 1)
            );
        }
        
        argValues.push_back(value->getLLVMValue());
        delete value;
    }
    
    return {functionStruct, argValues};
}

/*
    Create at llvm level the actual
    function call of the class member.
*/
Value* invokeMemberFunction(
    std::string nameOfClass,
    ClassType* classType,
    std::string memberName,
    std::string nameCurrVar,
    std::vector<Expr*> args,
    llvm::Value* currentPtr,
    bool wantReturn,
    llvm::IRBuilder<>& builder
){
    if(nameOfClass != "" && classType == nullptr) {
        for(auto& classType1 : symbolClassType){
            if(nameOfClass == classType1->getNameClass()){
                classType = classType1;
                break;
            }
        }
    }
    if(classType == nullptr){ //debug error
        throw std::runtime_error("ClassType is nullpt in ClassCallFunc::codegen");
    }
    
    llvm::LLVMContext& ctx = builder.getContext();
    
    FunctionCallParams params;
    params.functionName = memberName;
    params.className = classType->getNameClass();
    params.isClassFunction = true;
    params.extraArgs.push_back(currentPtr);  // 'this' pointer
    
    auto [functionStruct, argValues] = prepareAndValidateFunctionCall(params, args, builder);
    
    llvm::Function* callee = functionStruct->func;
    
   if (wantReturn) {
        Type* returnType = functionStruct->returnType;
        llvm::Value* llvmValueReturn = builder.CreateCall(callee, argValues, "calltmp");
        return returnType->createValue(llvmValueReturn, ctx);
    } else {
        if (!dynamic_cast<VoidType*>(functionStruct->returnType)) {
            throw std::runtime_error("Expected void return type in " + memberName);
        }
        builder.CreateCall(callee, argValues);
        return nullptr;
    }
}

Value* generateClassFunctionCall(
    llvm::IRBuilder<>& builder,
    const std::string firstVariableName,
    const std::vector<std::string> memberChain,
    const std::string nameOfClass,
    const std::vector<Expr*> args,
    bool returnsValue
){
    llvm::LLVMContext& ctx = builder.getContext();

    /*
        Pointer to the memory area
        where the variable is located
        and Type of that variable
    */
    llvm::Value* ptrToStruct = nullptr;
    Type* type = nullptr;

    // Retrieve the variable from the symbolTable
    bool foundVar = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(firstVariableName);
        if (found != it->end()) {
            ptrToStruct = found->second.alloca;
            type = found->second.type;
            foundVar = true;
            break;
        }
    }
    if (!foundVar)
        throw std::runtime_error("Undeclared variable: " + firstVariableName);

    // This is the value returned by the last GEP performed in the cycle
    llvm::Value* currentPtr = ptrToStruct;

    std::string nameCurrVar = firstVariableName;

    // We loop over the length of all members passed by the parser
    for (size_t depth = 0; depth < memberChain.size(); ++depth) {
        const std::string& memberName = memberChain[depth];

        if (auto classType = dynamic_cast<ClassType*>(type)) {
            /*
                If the current variable is of the type ClassType,
                then the next member in the chain must be
                the last one, since it must be the class
                function that will be called.
            */
            if(depth + 1 < memberChain.size()){
                throw std::runtime_error(
                    "The class variable '" +
                    nameCurrVar +
                    "' cannot have multiple internal calls"
                );
            }

            // Get the return value of the called function
            /*
                If the current member of the chain is not the
                variable with the name 'this', then you need
                to pass what type of class that member has.
            */
            return invokeMemberFunction(
                nameOfClass,
                classType,  // ClassType
                memberName,
                nameCurrVar,
                args,
                currentPtr,
                returnsValue,
                builder
            );
        }

        if (auto structType = dynamic_cast<StructType*>(type)) {
            auto members = structType->getMembers();
            size_t index = 0;
            bool found = false;

            /*
                Check if the selected member is present
                in the current struct
            */
            for (size_t i = 0; i < members.size(); ++i) {
                if (members[i].second == memberName) {
                    index = i;
                    found = true;
                    break;
                }
            }

            /* 
                if we don't find the member in the struct,
                but the struct it's 'this' with just one argument
                we need to discover if is a function
            */
            if(!found && nameOfClass != "" && (depth + 1 == memberChain.size())) {
                return invokeMemberFunction(
                    nameOfClass,
                    nullptr,  // ClassType
                    memberName,
                    nameCurrVar,
                    args,
                    currentPtr,
                    returnsValue,
                    builder
                );
            }

            if (!found)
                throw std::runtime_error("Member '" + memberName + "' not found in struct '"+ nameCurrVar +"'");

            /*
                Get a pointer to the specified member
                (by index) within the struct instance
            */
            currentPtr = builder.CreateStructGEP(structType->getLLVMType(ctx), currentPtr, index, memberName);
            nameCurrVar = memberName;

            type = members[index].first;
        } else {
            throw std::runtime_error("Invalid variable member of chain: '"+ nameCurrVar+"' with type '"+type->toString()+"'");
        }
    }

    throw std::runtime_error("Invalid member chain access.");
}
