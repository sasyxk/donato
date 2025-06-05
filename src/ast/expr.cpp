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

    Value* value = finalType->createValue(finalPtr, ctx, true);
    value->loadLLVMValue(memberChain.back(), builder);
    return value;
}

CallFunc::CallFunc(const std::string &fn, const std::string noc,  std::vector<Expr *> a) : funcName(fn),nameOfClass(noc),  args(a) {}
//todo nameOfClass here is useless
Value* CallFunc::codegen(llvm::IRBuilder<> &builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    
    FunctionCallParams params;
    params.functionName = funcName;
    
    auto [functionStruct, argValues] = prepareAndValidateFunctionCall(params, args, builder);
    
    llvm::Function* callee = functionStruct->func;
    llvm::Value* llvmValueReturn = builder.CreateCall(callee, argValues, "calltmp");
    Type* returnType = functionStruct->returnType.type;
    
    return returnType->createValue(llvmValueReturn, ctx, functionStruct->returnType.isReference);
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
    if (l->getLLVMValue() == nullptr){
       l->loadLLVMValue("left_op", builder);
    }
    if (r->getLLVMValue() == nullptr){
       r->loadLLVMValue("right_op", builder);
    }
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
    if (l->getLLVMValue() == nullptr){
       l->loadLLVMValue("left_op", builder);
    }
    if (r->getLLVMValue() == nullptr){
       r->loadLLVMValue("right_op", builder);
    }
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
    if (v->getLLVMValue() == nullptr){
       v->loadLLVMValue("unary_op", builder);
    }
    Value* result = v->neg(builder);
    
    delete v;
    return result;
}

DoubleNum::DoubleNum(double v, Type* t) : val(v) , type(t) {}

Value* DoubleNum::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* llvmValue = llvm::ConstantFP::get(ctx, llvm::APFloat(val));
    return type->createValue(llvmValue, ctx);
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
    return type->createValue(llvmValue, ctx);
}

BoolNum::BoolNum(bool v, Type* t) : val(v) , type(t) {}

Value* BoolNum::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* llvmValue = llvm::ConstantInt::get(ctx, llvm::APInt(1, val ? 1 : 0));
    return type->createValue(llvmValue, ctx);
}

Var::Var(const std::string& n) : name(n) {}

Value* Var::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::outs() << "Retrive Var : " << name << "\n";
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    llvm::LLVMContext& ctx = builder.getContext();

    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) { 
            Value* value = found->second.type->createValue(found->second.alloca , ctx, true);
            value->loadLLVMValue(name, builder);
            /*llvm::Value* llvmValue = builder.CreateLoad(
                found->second.type->getLLVMType(ctx), 
                found->second.alloca, 
                name + "_val"
            );
            value->setLLVMValue(llvmValue, found->second.type, ctx);
            */
            return value;
        }
    }
    throw std::runtime_error("Undefined variable: " + name);
}

IfOp::IfOp(Expr* c, Expr* t, Expr* e) : cond(c), thenExpr(t), elseExpr(e) {}

Value* IfOp::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    Value* condVal = cond->codegen(builder);
    if (condVal->getLLVMValue() == nullptr){
       condVal->loadLLVMValue("cond_op", builder);
    }

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
    if (thenVal->getLLVMValue() == nullptr){
       thenVal->loadLLVMValue("then_op", builder);
    }
    llvm::BasicBlock* thenExitBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);
    
    builder.SetInsertPoint(elseBB);
    Value* elseVal = elseExpr->codegen(builder);
    if (elseVal->getLLVMValue() == nullptr){
       thenVal->loadLLVMValue("else_op", builder);
    }
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
        if (value->getLLVMValue() == nullptr){
            value->loadLLVMValue("let_op", builder);
        }
        llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
        builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
        llvm::AllocaInst* alloca = builder.CreateAlloca(value->getType()->getLLVMType(ctx), nullptr, name);

        builder.SetInsertPoint(currentBlock);
        builder.CreateStore(value->getLLVMValue(), alloca);

        llvm::outs() << "The variable allocated with name: " << alloca->getName() << "\n";

        symbolTable.back()[name] = {alloca, value->getType()};

        delete value;
    }

    Value* bodyVal = body->codegen(builder);
    if (bodyVal->getLLVMValue() == nullptr){
        bodyVal->loadLLVMValue("body_let_op", builder);
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
    
    if(value->getLLVMValue() == nullptr){
        value->loadLLVMValue("load_pointer", builder);
    }
    llvm::Value* ptrAlloca  = pointerValue->getLLVMValue();

    Value* result =  pointerType->getTypePointed()->createValue(ptrAlloca, ctx, true);
    //result->loadLLVMValue("deref_val", builder);

    return result;
}

Value* newClass(
    llvm::IRBuilder<>& builder,
    std::string &nameClass,
    std::vector<Expr*> args,
    llvm::Value* ptrToStruct,
    Type* type
) {
    llvm::LLVMContext& ctx = builder.getContext();
    
    FunctionCallParams params;
    params.functionName = nameClass;
    params.isConstructor = true; 
    params.extraArgs.push_back(ptrToStruct); // Add the pointer to the struct as the first argument
    
    auto [functionStruct, argValues] = prepareAndValidateFunctionCall(params, args, builder);
    
    llvm::Function* callee = functionStruct->func;
    builder.CreateCall(callee, argValues);
    
    auto pointerType = TypeManager::instance().getPointerType(type);
    Value* result = pointerType->createValue(ptrToStruct, ctx);
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

    if(structMembers.size() != membersExpr.size()){
       throw std::runtime_error(
            "Incorrect number of arguments passed to struct '" +
            nameClass +
            "' during 'new' instantiation."
        );
    }

    for (size_t i = 0; i < structMembers.size(); ++i) {
        const auto& member = structMembers[i];
        Expr* memberExpr = membersExpr[i];

        llvm::Value* fieldIGEP = builder.CreateStructGEP(llvmStructType, ptrToStruct, i, member.second);
        Value* memberValue = memberExpr->codegen(builder);
        if (memberValue->getLLVMValue() == nullptr){
            memberValue->loadLLVMValue(member.second, builder);
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
    auto pointerType = TypeManager::instance().getPointerType(structType);
    Value* result = pointerType->createValue(ptrToStruct, ctx);

    return result;
}

NewOp::NewOp(std::string nc, std::vector<Expr*> a) : nameClass(nc) , args(a){
    Type* type = nullptr;      
    for (auto t : symbolClassType) {
        if (t->getNameClass() == nameClass) {
            type = static_cast<ClassType *>(t); 
            break;
        }
    }
    if (type == nullptr) {
        for (auto t : symbolStructsType) {
            if (t->getNameStruct() == nameClass) {
                type = static_cast<StructType*>(t);
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

    Value* resultCodegen = value->codegen(builder);

    llvm::Value* alloca = resultCodegen->getAllocation();
    if(alloca == nullptr){
        throw std::runtime_error(
            "You cannot assign a value to a reference that is not a pointer (Var StructVar)."
        );
    }

    PointerType* pointerType = TypeManager::instance().getPointerType(resultCodegen->getType());
    Value* pointerValue = pointerType->createValue(alloca, ctx);
    delete resultCodegen;
    
    return pointerValue;
}


NullPtr::NullPtr(Type* t) : type(t) {}

Value* NullPtr::codegen(llvm::IRBuilder<>& builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::PointerType* ptrType = llvm::PointerType::getUnqual(type->getLLVMType(ctx));
    llvm::Value* nullPtr = llvm::ConstantPointerNull::get(ptrType);
    type = TypeManager::instance().getPointerType(type);
   
    return type->createValue(nullPtr, ctx);
}