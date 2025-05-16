#include "expr.h"

StructVar::StructVar(const std::string &vsn, const std::string &mn): varStructName(vsn) , memberName(mn) {}

Value* StructVar::codegen(llvm::IRBuilder<> &builder, bool isPointer) {
    llvm::LLVMContext& ctx = builder.getContext();

    bool checkVariable = false;
    llvm::Value* ptrToStruct;
    Type* type;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(varStructName);
        if (found != it->end()) {
            ptrToStruct = found->second.alloca;
            type = found->second.type;
            checkVariable = true;
            break;
        }
    }
    if(!checkVariable) 
        throw std::runtime_error("Undeclared variable: " + varStructName);

    StructType* structType = dynamic_cast<StructType*>(type);
    if (!structType)
        throw std::runtime_error("The variable '" + varStructName + "' is not a struct but a '" + type->toString() + "'");
 
    size_t i = 0;
    for(auto member : structType->getMembers()){
        if(member.second == memberName){

            llvm::Value* fieldIGEP = builder.CreateStructGEP(structType->getLLVMType(ctx), ptrToStruct, i, memberName);

            if(isPointer){
                Type* provaType = member.first;
                provaType->setPointer(true);
                return provaType->createValue(fieldIGEP , ctx);
            }

            llvm::Value* llvmValue = builder.CreateLoad(
                member.first->getLLVMType(ctx), 
                fieldIGEP, 
                memberName + "_val"
            );
            return member.first->createValue(llvmValue, ctx);
        }
        i++;
    }
    throw std::runtime_error("The member '" + memberName + "' is not recognized in the struct.");
}

CallFunc::CallFunc(const std::string &fn, std::vector<Expr *> a) : funcName(fn), args(a) {}

Value* CallFunc::codegen(llvm::IRBuilder<> &builder, bool isPointer) {
    // Search for the function in the module
    SymbolFunction* functionStruct = nullptr;

    bool checkFunc = false;
    for (auto& function : symbolFunctions) {
        if (function.first == funcName) {
            functionStruct = &function.second;
            checkFunc = true;
            break;
        }
    }
    if(!checkFunc)
        throw std::runtime_error("Function not found: " + funcName);
    
    /*llvm::Function* callee = module->getFunction(funcName);
    if(!callee) {
        throw std::runtime_error("Function not found: " + funcName);
    }
    */
    if(functionStruct->argType.size() != args.size()){
        throw std::runtime_error("Argument count mismatch for " + funcName);
    }

    // Check the correctness of the arguments
    /*if(callee->arg_size() != args.size()) {
        throw std::runtime_error("Argument count mismatch for " + funcName);
    }*/

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
        /*
        if(isVar){
           value->getType()->setPointer(true);
            llvm::outs() << "AURAAAAA\n";

        }
        */
        llvm::Value* llvmVal = nullptr; 
        //functionStruct->argType.at(argValues.size())->isPointer()  // not usefull here
        if (callee->getFunctionType()->getParamType(argValues.size())->isPointerTy() && !value->getLLVMValue()->getType()->isPointerTy()) {
            // The variable passed is not a pointer -> it is created
            llvm::outs() << "DENTRO\n";
            // Temporary space allocation
            llvm::AllocaInst* temp = builder.CreateAlloca(value->getLLVMValue()->getType(), nullptr, "argtmp");
            builder.CreateStore(value->getLLVMValue(), temp);
            llvmVal = temp;
        }
        
        if(!(*value->getType() ==  *functionStruct->argType.at(argValues.size())  )){//callee->getFunctionType()->getParamType(argValues.size()))) {
            throw std::runtime_error("Type mismatch in argument " + std::to_string(argValues.size() + 1));
        }
        argValues.push_back(!llvmVal ?
            value->getLLVMValue() : llvmVal);
        delete value;
    }

    llvm::Value* llvmValueReturn = builder.CreateCall(callee, argValues, "calltmp");
    Type* returnType = functionStruct->returnType;
    /*for (const auto& func : symbolFunctions) {
        if (func.first == funcName) {  
            returnType = func.second.returnType->clone();
            break;
        }
    }*/

    Value* returnValue = returnType->createValue(llvmValueReturn, ctx);

    return returnValue;
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
                Type* provaType = found->second.type;
                provaType->setPointer(true);
                return provaType->createValue(provaVal , ctx);
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

