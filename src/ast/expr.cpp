#include "expr.h"

StructVar::StructVar(const std::string &vsn, const std::vector<std::string> &mc) : varStructName(vsn) , memberChain(mc) {}

Value* StructVar::codegen(llvm::IRBuilder<> &builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Value* ptrToStruct = nullptr;
    Type* type = nullptr;

    // Retrieve the variable from the symbolTable
    bool foundVar = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(varStructName);
        if (found != it->end()) {
            ptrToStruct = found->second.alloca;
            type = found->second.type;
            foundVar = true;
            break;
        }
    }
    if (!foundVar)
        throw std::runtime_error("Undeclared variable: " + varStructName);

    // Let's check if the variable found is actually a Struct
    StructType* currentStruct = dynamic_cast<StructType*>(type);
    if (!currentStruct)
        throw std::runtime_error("Variable '" + varStructName + "' is not a struct.");

    // This is the value returned by the last GEP performed in the cycle
    llvm::Value* currentPtr = ptrToStruct;

    // We loop over the length of all members passed by the parser
    for (size_t depth = 0; depth < memberChain.size(); ++depth) {
        const std::string& memberName = memberChain[depth];

        auto members = currentStruct->getMembers();
        bool found = false;
        size_t index = 0;

        /*
        Let's check if the member in which we need to read
        the value is present among the Struct members.
        */ 
        for (size_t i = 0; i < members.size(); ++i) {
            if (members[i].second == memberName) {
                index = i;
                found = true;
                break;
            }
        }

        if (!found)
            throw std::runtime_error("Member '" + memberName + "' not found in struct.");

        // GEP for the current field
        
        currentPtr = builder.CreateStructGEP(currentStruct->getLLVMType(ctx), currentPtr, index, memberName);

        // Only if we have reached the last level of depth then
        if (depth == memberChain.size() - 1) {
            Type* finalType = members[index].first;

            if (isPointer) {
                Value * value = finalType->createValue(currentPtr, ctx);
                value->getType()->setPointer(true);
                return value;
            }

            llvm::Value* loadedVal = builder.CreateLoad(
                finalType->getLLVMType(ctx),
                currentPtr,
                memberName + "_val");
            return finalType->createValue(loadedVal, ctx);
        }

        // Otherwise, we continue inside the struct
        Type* nextType = members[index].first;
        currentStruct = dynamic_cast<StructType*>(nextType);
        if (!currentStruct)
            throw std::runtime_error("Member '" + memberName + "' is not a struct.");
    }

    throw std::runtime_error("Invalid member chain access.");
}

CallFunc::CallFunc(const std::string &fn, const std::string noc,  std::vector<Expr *> a) : funcName(fn),nameOfClass(noc),  args(a) {}
//todo nameOfClass here is useless
Value* CallFunc::codegen(llvm::IRBuilder<> &builder, bool isPointer) {
    // Search for the function in the module
    SymbolFunction* functionStruct = nullptr;

    bool checkFunc = false;
    for (auto& function : symbolFunctions) {
        if (function.first == funcName && !function.second.classFunction) {
            functionStruct = &function.second;
            checkFunc = true;
            break;
        }
    }
    if(!checkFunc)
        throw std::runtime_error("Function not found: " + funcName);
        
    if(functionStruct->argType.size() != args.size()){
        throw std::runtime_error("Argument count mismatch for " + funcName);
    }

    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Function* callee = functionStruct->func;

    // Generate argument values
    std::vector<llvm::Value*> argValues;
    for(auto* arg : args) {
        bool isVar = false;
        
        if (Var* varPtr = dynamic_cast<Var*>(arg)) {
            if(callee->getFunctionType()->getParamType(argValues.size())->isPointerTy())
                isVar = true;
        }
        else if (StructVar* structVarPtr = dynamic_cast<StructVar*>(arg)) {
            if(callee->getFunctionType()->getParamType(argValues.size())->isPointerTy())
                isVar = true;
        }
        Value* value  = arg->codegen(builder, isVar);
        llvm::Value* llvmVal = nullptr; 
        
        if (callee->getFunctionType()->getParamType(argValues.size())->isPointerTy() && !value->getLLVMValue()->getType()->isPointerTy()) {
            
            throw std::runtime_error(
                "Function " +
                funcName +
                " argument " +
                std::to_string(argValues.size() + 1) +
                " wants a reference pass, insert a"  +
                " variable as an argument"
            );
        }
        
        if(!(*value->getType() ==  *functionStruct->argType.at(argValues.size())  )){
            throw std::runtime_error("Type mismatch in argument " + std::to_string(argValues.size() + 1));
        }
        argValues.push_back(!llvmVal ?
            value->getLLVMValue() : llvmVal);
        delete value;
    }

    llvm::Value* llvmValueReturn = builder.CreateCall(callee, argValues, "calltmp");
    Type* returnType = functionStruct->returnType;
    
    Value* returnValue = returnType->createValue(llvmValueReturn, ctx);

    return returnValue;
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

    // memberName is a fuction
    SymbolFunction* functionStruct = nullptr;

    bool checkFunc = false;
    for (auto& function : symbolFunctions) {
        if (function.first == memberName && 
            function.second.classFunction &&
            classType->isFuctionOfClass(memberName)
        ){
            functionStruct = &function.second;
            checkFunc = true;
            break;
        }
    }
    if(!checkFunc)
        throw std::runtime_error("Function '" + memberName + "' of class '"+ nameCurrVar + "' not found");

    if(functionStruct->argType.size() != args.size() + 1){
        throw std::runtime_error("Argument count mismatch for " + memberName);
    }

    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Function* callee = functionStruct->func;

    // Generate argument values
    std::vector<llvm::Value*> argValues;
    argValues.push_back(currentPtr); // is a pointer

    for(auto* arg : args) {
        bool isVar = false;
        
        if (Var* varPtr = dynamic_cast<Var*>(arg)) {
            if(callee->getFunctionType()->getParamType(argValues.size())->isPointerTy())
                isVar = true;
        }
        else if (StructVar* structVarPtr = dynamic_cast<StructVar*>(arg)) {
            if(callee->getFunctionType()->getParamType(argValues.size())->isPointerTy())
                isVar = true;
        }
        Value* value  = arg->codegen(builder, isVar);

        llvm::Value* llvmVal = nullptr; 
        if (callee->getFunctionType()->getParamType(argValues.size())->isPointerTy() && !value->getLLVMValue()->getType()->isPointerTy()) {
            throw std::runtime_error(
                "Function " +
                memberName +
                " argument " +
                std::to_string(argValues.size() + 1) +
                " wants a reference pass, insert a"  +
                " variable as an argument"
            );
        }
        
        if(!(*value->getType() ==  *functionStruct->argType.at(argValues.size())  )){
            throw std::runtime_error("Type mismatch in argument " + std::to_string(argValues.size() + 1));
        }
        argValues.push_back(!llvmVal ?
            value->getLLVMValue() : llvmVal);
        delete value;
    }

    llvm::Value* llvmValueReturn = builder.CreateCall(callee, argValues, "calltmp");
    Type* returnType = functionStruct->returnType;
    
    Value* returnValue = returnType->createValue(llvmValueReturn, ctx);
    return returnValue;
}

ClassCallFunc::ClassCallFunc(
    const std::string fvn,
    const std::vector<std::string> mn,
    std::string noc,
    std::vector<Expr *> a
):
    firstVariableName(fvn),
    memberChain(mn),
    nameOfClass(noc),
    args(a)
{}

/*
    It manages how the retrieval of elements of the various
    structs should be handled, which can be chained up to
    the last element which must be a function call.
*/
Value* ClassCallFunc::codegen(llvm::IRBuilder<>& builder, bool isPointer){
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

            //Get the return value of the called function
            /*
                If the current member of the chain is not the
                variable with the name 'this', then you need
                to pass what type of class that member has.
            */
            Value* value = invokeMemberFunction(
                    nameOfClass,
                    classType,  //ClassType
                    memberName,
                    nameCurrVar,
                    args,
                    currentPtr,
                    builder
                );
            return value;
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
                Value* value = invokeMemberFunction(
                    nameOfClass,
                    nullptr,
                    memberName,
                    nameCurrVar,
                    args,
                    currentPtr,
                    builder
                );
                return value;
            
            }
            if (!found)
                throw std::runtime_error("Member '" + memberName + "' not found in struct '"+ nameCurrVar +"'");

            /*
                et a pointer to the specified member
                (by index)  within the struct instance
            */
            currentPtr = builder.CreateStructGEP(structType->getLLVMType(ctx), currentPtr, index, memberName);
            nameCurrVar = memberName;

            type = members[index].first;
        }
        else{
            throw std::runtime_error("Invalid variable member of chain: '"+ nameCurrVar+"' with type '"+type->toString()+"'");
        }
        
    }

    throw std::runtime_error("Invalid member chain access.");
    
}

BinaryCond::BinaryCond(const std::string& o, Expr* l, Expr* r) : op(o), left(l), right(r) {}

Value* BinaryCond::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    Value* l = left->codegen(builder);
    Value* r = right->codegen(builder);
    Value* result;
    
    if      (op == "==") result = l->eq(r,builder);
    else if (op == "!=") result = l->neq(r,builder);
    else if (op == "<")  result = l->lt(r,builder);
    else if (op == "<=") result = l->lte(r,builder);
    else if (op == ">")  result = l->gt(r,builder);
    else if (op == ">=") result = l->gte(r,builder);
    else throw std::runtime_error("Unknown operator in BinaryCond: " + op);
    
    delete l;
    delete r;
    return result;
}

BinaryOp::BinaryOp(const std::string& o, Expr* l, Expr* r) : op(o), left(l), right(r) {}

Value* BinaryOp::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    Value* l = left->codegen(builder);
    Value* r = right->codegen(builder);
    Value* result;

    if       (op == "+") result = l->add(r,builder);
    else if  (op == "-") result = l->sub(r,builder);
    else if  (op == "*") result = l->mul(r,builder);
    else if  (op == "/") result = l->div(r,builder);
    else throw std::runtime_error("Unknown operator in BinaryOp");

    delete l;
    delete r;
    return result;    
}

UnaryOp::UnaryOp(const std::string& o, Expr* x) : op(o), x(x) {}

Value* UnaryOp::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    Value* v = x->codegen(builder);
    Value* result = v->neg(builder);
    
    delete v;
    return result;
}

DoubleNum::DoubleNum(double v, Type* t) : val(v) , type(t) {}

Value* DoubleNum::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* llvmValue = llvm::ConstantFP::get(ctx, llvm::APFloat(val));
    return new DoubleValue(type->clone(), llvmValue, ctx);
}

SignedIntNum::SignedIntNum(std::int64_t v, Type* t) : val(v) , type(t) {}

Value* SignedIntNum::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();

    unsigned bits = static_cast<SignedIntType*>(type)->getBits();

    llvm::Value* llvmValue = llvm::ConstantInt::get(
        llvm::IntegerType::get(ctx, bits),
        val,
        true
    );
    return new SignedIntValue(type->clone(), llvmValue, ctx);
}

BoolNum::BoolNum(bool v, Type* t) : val(v) , type(t) {}

Value* BoolNum::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* llvmValue = llvm::ConstantInt::get(ctx, llvm::APInt(1, val ? 1 : 0));
    return new BoolValue(type->clone(), llvmValue, ctx);
}

Var::Var(const std::string& n) : name(n) {}

Value* Var::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::outs() << "VAR the name of func is : " << func << "\n";
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    llvm::LLVMContext& ctx = builder.getContext();

    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) { 
            if(isPointer){
                llvm::Value* provaVal = found->second.alloca;
                Type* provaType = found->second.type->clone();
                provaType->setPointer(true);
                Value* value = provaType->createValue(provaVal , ctx);
                delete provaType;
                return value;
            }
            llvm::Value* llvmValue = builder.CreateLoad(
                found->second.type->getLLVMType(ctx), 
                found->second.alloca, 
                name + "_val"
            );

            return found->second.type->createValue(llvmValue , ctx);
            //return Value::createValue(found->second.type->clone(),llvmValue,ctx);
        }
    }
    throw std::runtime_error("Undefined variable: " + name);
}

IfOp::IfOp(Expr* c, Expr* t, Expr* e) : cond(c), thenExpr(t), elseExpr(e) {}

Value* IfOp::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    Value* condVal = cond->codegen(builder);

    // Check if the condition is already a boolean value
    if(!dynamic_cast<const BoolType*>(condVal->getType())){
        Value* newCondVal = condVal->getBoolValue(builder);  // Convert to boolean
        delete condVal;                                      // Free old value
        condVal = newCondVal;                                // Update pointer
    }

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(ctx, "then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(ctx, "else",func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx, "ifcont",func);
    
    builder.CreateCondBr(condVal->getLLVMValue(), thenBB, elseBB);
    
    builder.SetInsertPoint(thenBB);
    Value* thenVal = thenExpr->codegen(builder);
    llvm::BasicBlock* thenExitBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);
    
    builder.SetInsertPoint(elseBB);
    Value* elseVal = elseExpr->codegen(builder);
    llvm::BasicBlock* elseExitBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);
    
    builder.SetInsertPoint(mergeBB);
    
    if(!(*thenVal->getType() == *elseVal->getType())){
        throw std::runtime_error("IfOp has branches with results of different types");
    }
    
    llvm::PHINode* phi = builder.CreatePHI(thenVal->getType()->getLLVMType(ctx), 2, "iftmp");
    phi->addIncoming(thenVal->getLLVMValue(), thenExitBB);
    phi->addIncoming(elseVal->getLLVMValue(), elseExitBB);

    Value* phiValue = thenVal->getType()->createValue(phi, ctx);
    
    delete thenVal;
    delete elseVal;
    delete condVal;
    return phiValue;
}

LetOp::LetOp(const std::vector<std::pair<std::string, Expr*>>& b, Expr* bod) : bindings(b), body(bod) {}

Value* LetOp::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    symbolTable.emplace_back();

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    
    for (auto& [name, expr] : bindings) {

        Value* value = expr->codegen(builder);
        llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
        builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
        llvm::AllocaInst* alloca = builder.CreateAlloca(value->getType()->getLLVMType(ctx), nullptr, name);

        builder.SetInsertPoint(currentBlock);
        builder.CreateStore(value->getLLVMValue(), alloca);

        llvm::outs() << "The variable allocated with name: " << alloca->getName() << "\n";

        symbolTable.back()[name] = {alloca, value->getType()->clone()};

        delete value;
    }

    Value* bodyVal = body->codegen(builder);

    for (auto& [name, info] : symbolTable.back()) {
        delete info.type;
    }
    symbolTable.pop_back();

    return bodyVal;
}

