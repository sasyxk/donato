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
    StructType* rootStruct = dynamic_cast<StructType*>(type);
    if (!rootStruct)
        throw std::runtime_error("Variable '" + varStructName + "' is not a struct.");

    auto [finalPtr, finalType] = getStructMemberGEP(builder, ptrToStruct, rootStruct, memberChain);

    if (isPointer) {
        // Finaltype is a type that is taken from the initial list
        // of the class created in the parser, it will then be destroyed
        // automatically at the end of the program
        finalType->setPointer(true);
        Value* value = finalType->createValue(finalPtr, ctx);
        finalType->setPointer(false);
        return value;
    }

    llvm::Value* loadedVal = builder.CreateLoad(
        finalType->getLLVMType(ctx),
        finalPtr,
        memberChain.back() + "_val"
    );
    return finalType->createValue(loadedVal, ctx);
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
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        else if (StructVar* structVarPtr = dynamic_cast<StructVar*>(arg)) {
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        Value* value  = arg->codegen(builder, isVar);
        llvm::Value* llvmVal = nullptr; 
        
        if (functionStruct->argType.at(argValues.size())->isPointer() && !value->getType()->isPointer()) {
            
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

    // memberName is a fuction
    SymbolFunction* functionStruct = nullptr;

    bool checkFunc = false;
    for (auto& function : symbolFunctions) {
        if (function.first == memberName && 
            function.second.classFunction &&
            function.second.className == classType->getNameClass() &&
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
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        else if (StructVar* structVarPtr = dynamic_cast<StructVar*>(arg)) {
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        Value* value  = arg->codegen(builder, isVar);

        llvm::Value* llvmVal = nullptr; 
        if (functionStruct->argType.at(argValues.size())->isPointer() && !value->getType()->isPointer()) {
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

// It manages how the retrieval of elements of the various
// structs should be handled, which can be chained up to
// the last element which must be a function call.
Value* ClassCallFunc::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    return generateClassFunctionCall(
        builder,
        firstVariableName,
        memberChain,
        nameOfClass,
        args,
        true
    );
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

DereferenceOp::DereferenceOp(Expr* x) : x(x) {}


Value* DereferenceOp::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    Value* value = x->codegen(builder);

    auto pointerValue = dynamic_cast<PointerValue* >(value);
    if(pointerValue == nullptr){
        throw std::runtime_error("DereferenceOp not valid with " + value->getType()->toString());
    }

    auto pointerType = static_cast<PointerType* >(pointerValue->getType()); 

    Value* result;
    //if we don't need the pointer, just read the value
    // of the pointer inside the pointer
    if(!isPointer){
        llvm::Value* loadedVal = builder.CreateLoad(
            pointerType->getTypePointed()->getLLVMType(ctx),
            pointerValue->getLLVMValue(),
            "deref_val"
        );
        result =  pointerType->getTypePointed()->createValue(loadedVal, ctx);
    }
    // else, we need to create a new Value with the pointer  
    else{
        auto pointedType = pointerType->getTypePointed();
        pointedType->setPointer(true);
        result =  pointedType->createValue(value->getLLVMValue(), ctx);
    }
    

    return result;
}

Value* newClass(
    llvm::IRBuilder<>& builder,
    std::string &nameClass,
    std::vector<Expr*> args,
    llvm::Value* ptrToStruct,
    Type* type
){
    llvm::LLVMContext& ctx = builder.getContext();
    //Call the constructor;   
    SymbolFunction* functionStruct = nullptr;

    bool checkFunc = false;
    for (auto& function : symbolFunctions) {
        if (function.first == nameClass){
            functionStruct = &function.second;
            checkFunc = true;
            break;
        }
    }
    if(!checkFunc)
        throw std::runtime_error("Constructor '" + nameClass + "' not found");

    if(functionStruct->argType.size() != args.size() + 1){ 
        throw std::runtime_error("Argument count mismatch for Constructor" + nameClass);
    }

    llvm::Function* callee = functionStruct->func;

    // Generate argument values
    std::vector<llvm::Value*> argValues;
    argValues.push_back(ptrToStruct); //is a pointer

    for(auto* arg : args) {
        bool isVar = false;
        
        if (Var* varPtr = dynamic_cast<Var*>(arg)) {
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        else if (StructVar* structVarPtr = dynamic_cast<StructVar*>(arg)) {
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        Value* value  = arg->codegen(builder, isVar);

        llvm::Value* llvmVal = nullptr; 
        if (functionStruct->argType.at(argValues.size())->isPointer() && !value->getType()->isPointer()) {
            throw std::runtime_error(
                "Constructor " +
                nameClass +
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

    //It's Void type
    builder.CreateCall(callee, argValues);

    auto pointerType = new PointerType(type->clone());
    Value* result = pointerType->createValue(ptrToStruct, ctx);

    delete pointerType;
    
    return result;

}

Value* newStruct(
    llvm::IRBuilder<>& builder,
    std::string &nameClass,
    std::vector<Expr*> membersExpr,
    llvm::Value* ptrToStruct,
    StructType* structType
){
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Type* llvmStructType = structType->getLLVMType(ctx);

    const auto& structMembers = structType->getMembers();
    for (size_t i = 0; i < structMembers.size(); ++i) {
        const auto& member = structMembers[i];
        Expr* memberExpr = membersExpr[i];

        llvm::Value* fieldIGEP = builder.CreateStructGEP(llvmStructType, ptrToStruct, i, member.second);
        Value* memberValue = memberExpr->codegen(builder);
        if (memberValue->getType()->isPointer()){
            throw std::runtime_error(
                "You can't assign a ptr to one member of the struct without using ref"
            );
        }
        if(!(*member.first == *memberValue->getType())){
            if(!memberValue->getType()->isCastTo(member.first)){
                throw std::runtime_error(
                    "The type of struct member '" +
                    member.second + "' (expected '" +
                    member.first->toString() +
                    "') is not compatible with the provided value of type '" + 
                    memberValue->getType()->toString() + "'"
                );
            }
        
            Value* newVal = memberValue->castTo(member.first, builder);
            delete memberValue;
            memberValue = newVal;
        }
        builder.CreateStore(memberValue->getLLVMValue(), fieldIGEP);
        delete memberValue;
    }
    auto pointerType = new PointerType(structType->clone());
    Value* result = pointerType->createValue(ptrToStruct, ctx);

    delete pointerType;

    return result;
}

NewOp::NewOp(std::string nc, std::vector<Expr*> a) : nameClass(nc) , args(a){
    Type* type = nullptr;      
    for (auto t : symbolClassType) {
        if (t->getNameClass() == nameClass) {
            type = static_cast<ClassType *>(t->clone()); 
            break;
        }
    }
    if (type == nullptr) {
        for (auto t : symbolStructsType) {
            if (t->getNameStruct() == nameClass) {
                type = static_cast<StructType*>(t->clone());
                break;
            }
        }
    }
    if (!type) {
        throw std::runtime_error("Undefined Type with '" + nameClass + "'");
    }
    this->type = type;
}


Value* NewOp::codegen(llvm::IRBuilder<>& builder, bool isPointer){
    // Allocation 
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    //Malloc-----------------------<>
    std::string class_Alloc = nameClass + "_alloc";
    llvm::Function* d_mallocFuncClass = module->getFunction(class_Alloc);
    if (!d_mallocFuncClass) {
        throw std::runtime_error("Function not found: " + class_Alloc);
    }
    std::string nameMalloc = nameClass;
    nameMalloc[0] = std::tolower(nameMalloc[0]);
    llvm::Value* ptrToStruct = builder.CreateCall(d_mallocFuncClass, {}, nameMalloc);
    //Malloc-----------------------<\>
    
    if(auto t = dynamic_cast<StructType*>(type)){
        return newStruct(builder, nameClass, args, ptrToStruct, t);
    }
    return newClass(builder, nameClass, args, ptrToStruct, type);
}

AddressOp::AddressOp(Expr* v) : value(v) {}

Value* AddressOp::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    
    // If they are of this type, I have to generate the codegen with isPointer set to true
    bool needsPointer = dynamic_cast<Var*>(value) || 
                        dynamic_cast<DereferenceOp*>(value) || 
                        dynamic_cast<StructVar*>(value);
    
    // If they are of this type, I have to generate the codegen with isPointer set to false
    bool isFunction = dynamic_cast<CallFunc*>(value) || 
                      dynamic_cast<ClassCallFunc*>(value);
    
    if (!needsPointer && !isFunction) {
        throw std::runtime_error(
            "You cannot assign a value to a reference that is not a pointer (Var StructVar)."
        );
    }
    
    // Pick the Pointer of the variable with appropriete flag
    Value* resultCodegen = value->codegen(builder, needsPointer);
    
    // For functions, make sure they return a pointer
    if (isFunction && !resultCodegen->getType()->isPointer()) {
        throw std::runtime_error(
            "You cannot assign a value to a reference that is not a pointer because\n"
            "the function you call does not return a pointer but a value."
        );

    }
    
    // Save that pointer
    llvm::Value* alloca = resultCodegen->getLLVMValue();
    // Store the default type inside
    resultCodegen->getType()->setPointer(false);
    
    PointerType* pointerType = new PointerType(resultCodegen->getType()->clone());
    Value* pointerValue = pointerType->createValue(alloca, ctx);
    
    // Cleanup
    delete resultCodegen;
    delete pointerType;
    
    return pointerValue;
}