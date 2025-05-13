#include "ast.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <iostream>

std::vector<std::map<std::string, SymbolInfo>> symbolTable;

std::vector<std::pair<std::string, Type*>> symbolFunctions; //Name, return
std::vector<StructType*> symbolStructsType;

// Statements --------------

DefineStruct::DefineStruct(std::string ns, std::vector<std::pair<Type*, std::string>> m) : nameStruct(ns), members(m) {
    StructType* structType = new StructType(nameStruct, members);
    for(auto st : symbolStructsType){
        if(st == structType || structType->equalName(*st)){
            delete structType;
            throw std::runtime_error("The Struct has already been defined: \n" + st->toString());
        }
    }
    symbolStructsType.push_back(structType);
}

void DefineStruct::codegen(llvm::IRBuilder<> &builder) {

    llvm::LLVMContext& ctx = builder.getContext();

    StructType* structType;
    for(auto typeValue : symbolStructsType){
        if(typeValue->getNameStruct() == nameStruct){
            structType = typeValue;
            break;
        }
    }

    llvm::StructType* pointType = llvm::StructType::create(ctx, nameStruct);

    std::vector<llvm::Type*> members;
    for(auto member : this->members){
        members.push_back(member.first->getLLVMType(ctx));
    }
    pointType->setBody(members);

    structType->setLLVMType(pointType);

    /*
    StructType* structType;
    for(auto typeValue : symbolStructsType){
        if(typeValue->getNameStruct() == "Point"){
            structType = typeValue;
            break;
        }
    }
    llvm::Type* llvmStructType = structType->getLLVMType(ctx);

    llvm::AllocaInst* ptrToStruct = builder.CreateAlloca(llvmStructType, nullptr, "point");

    llvm::Value* field0GEP = builder.CreateStructGEP(llvmStructType, ptrToStruct, 0, "field0");
    llvm::Value* field0GEP222 = builder.CreateStructGEP(llvmStructType, ptrToStruct, 1, "field1");
    builder.CreateStore(llvm::ConstantInt::get(
        llvm::IntegerType::get(ctx, 32),
        32,
        true
    ), field0GEP);
    field0GEP = builder.CreateStructGEP(llvmStructType, ptrToStruct, 0, "field0");
    builder.CreateStore(llvm::ConstantInt::get(
        llvm::IntegerType::get(ctx, 32),
        50,
        true
    ), field0GEP);
    llvm::Value* loadVal = builder.CreateLoad(
                llvm::IntegerType::get(ctx, 32), 
                field0GEP, 
                "LOADTRY"
    );
    builder.CreateStore(loadVal, field0GEP222);
    */
}

StructDecl::StructDecl(std::string ns, std::string vrs, std::vector<Expr*> me){
    
    bool check = false;
    for(auto st : symbolStructsType){
        if(st->getNameStruct() == ns){
            if(st->getMembers().size() == me.size()){
                check = true;
            }
            else{
                throw std::runtime_error(
                    "Struct '" + ns + "' expects " + 
                    std::to_string(st->getMembers().size()) + 
                    " members, but " + 
                    std::to_string(me.size()) + 
                    " were provided."
                );
            }
            break;
        }
    }
    if (!check)
        throw std::runtime_error("Undefined struct with name '"+ns+"'");
    
    this->nameStruct = ns;
    this->varStructName = vrs;
    this->membersExpr = me;
}

void StructDecl::codegen(llvm::IRBuilder<>& builder){
    std::cout<<"AURA"<<std::endl;
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    //structType = dynamic_cast<StructType*>(type->clone());
    StructType* structType;
    for (auto type : symbolStructsType) {
        if (type->getNameStruct() == nameStruct) {
            structType = type;
            break;
        }
    }

    // Allocation Struct
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    llvm::Type* llvmStructType = structType->getLLVMType(ctx);
    llvm::AllocaInst* ptrToStruct = builder.CreateAlloca(llvmStructType, nullptr, varStructName);

    builder.SetInsertPoint(currentBlock);

    const auto& structMembers = structType->getMembers();
    for (size_t i = 0; i < structMembers.size(); ++i) {
        const auto& member = structMembers[i];
        Expr* memberExpr = membersExpr[i];

        llvm::Value* fieldIGEP = builder.CreateStructGEP(llvmStructType, ptrToStruct, i, member.second);
        Value* memberValue = memberExpr->codegen(builder);
        if(!(*member.first == *memberValue->getType())){
            throw std::runtime_error(
                "The type of struct member '" +
                member.second + "' (expected '" +
                member.first->toString() +
                 "') is not compatible with the provided value of type '" + 
                memberValue->getType()->toString() + "'"
            );
        }
        builder.CreateStore(memberValue->getLLVMValue(), fieldIGEP);
        delete memberValue;
    }
}

Function::Function(Type* tf,const std::string nf, const std::vector<std::pair<Type*, std::string>> p,
    std::vector<Statement*> b) : typeFunc(tf), nameFunc(nf), parameters(p) ,body(b) {}

void Function::codegen(llvm::IRBuilder<> &builder) {

    symbolFunctions.emplace_back(
        nameFunc,
        typeFunc->clone()
    );

    std::vector<llvm::Type*> paramTypes;
    llvm::LLVMContext& ctx = builder.getContext();
    for (const auto& param : parameters) {
        paramTypes.push_back(param.first->getLLVMType(ctx));
    }
    llvm::Type* returnType = typeFunc->getLLVMType(ctx);

    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = module->getFunction(nameFunc);

    if (function) throw std::runtime_error("Redefinition of function: " + nameFunc); //&& function->getFunctionType() == funcType

    function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, nameFunc, module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(builder.getContext(), "entry", function);
    builder.SetInsertPoint(entry);
    symbolTable.emplace_back();

    llvm::Function::arg_iterator argIt = function->arg_begin();
    for (const auto& param : parameters) {
        llvm::Argument* arg = &*argIt++;
        arg->setName(param.second);
        llvm::AllocaInst* alloca = builder.CreateAlloca(arg->getType(), nullptr, param.second);
        builder.CreateStore(arg, alloca);
        symbolTable.back()[param.second] = {alloca, param.first};
    }

    for (Statement* stm : body) {
        stm->codegen(builder);
    }

    for (auto& [name, info] : symbolTable.back()) {
        delete info.type;
    }
    symbolTable.pop_back();
}

Return::Return(Expr* e, std::string fn) : expr(e), funcName(fn) {}

void Return::codegen(llvm::IRBuilder<> &builder)
{
    Value* retVal = expr->codegen(builder);
    Type* returnType;
    for (const auto& func : symbolFunctions) {
        if (func.first == funcName) {  
            returnType = func.second->clone();
            break;
        }
    }
    if(!(*retVal->getType() == *returnType))
        throw std::runtime_error(
            "The returned type '" +
            retVal->getType()->toString() + 
            "' is not compatible with the type that the function '" +
            funcName +
            "', is supposed to return '"+returnType->toString()+"'"
        );
    builder.CreateRet(retVal->getLLVMValue());
}

WhileStm::WhileStm(Expr* c, std::vector<Statement*> w) : cond(c), whileExpr(w) {}

void WhileStm::codegen(llvm::IRBuilder<>& builder) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* condWhileBB = llvm::BasicBlock::Create(builder.getContext(), "condWhile", func);
    llvm::BasicBlock* bodyWhileBB = llvm::BasicBlock::Create(builder.getContext(), "bodyWhile", func);
    llvm::BasicBlock* nextBB = llvm::BasicBlock::Create(builder.getContext(), "mergeWhile", func);

    builder.CreateBr(condWhileBB);

    builder.SetInsertPoint(condWhileBB);
    Value* condVal = cond->codegen(builder);
    // Check if the condition is already a boolean value
    if(!dynamic_cast<const BoolType*>(condVal->getType())){
        Value* newCondVal = condVal->getBoolValue(builder);  // Convert to boolean
        delete condVal;                                      // Free old value
        condVal = newCondVal;                                // Update pointer
    }
    builder.CreateCondBr(condVal->getLLVMValue(), bodyWhileBB, nextBB);
    delete condVal;

    symbolTable.emplace_back();

    builder.SetInsertPoint(bodyWhileBB);
    for (Statement* stm : whileExpr) {
        stm->codegen(builder);
    }
    builder.CreateBr(condWhileBB);

    for (auto& [name, info] : symbolTable.back()) {
        delete info.type;
    }
    symbolTable.pop_back();

    builder.SetInsertPoint(nextBB);
}

IfStm::IfStm(Expr* c, std::vector<Statement*> t, std::vector<Statement*> e) : cond(c), thenExpr(t), elseExpr(e) {}

void IfStm::codegen(llvm::IRBuilder<>& builder) {
    Value* condVal = cond->codegen(builder);
    // Check if the condition is already a boolean value
    if(!dynamic_cast<const BoolType*>(condVal->getType())){
        Value* newCondVal = condVal->getBoolValue(builder);  // Convert to boolean
        delete condVal;                                      // Free old value
        condVal = newCondVal;                                // Update pointer
    }

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(ctx, "then", func);
    llvm::BasicBlock* elseBB = !elseExpr.empty() ? llvm::BasicBlock::Create(ctx, "else", func) : nullptr;
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx, "merge",func);

    if(!elseExpr.empty()){
        builder.CreateCondBr(condVal->getLLVMValue(), thenBB, elseBB);
    }
    else{
        builder.CreateCondBr(condVal->getLLVMValue(), thenBB, mergeBB);
    }

    delete condVal;

    symbolTable.emplace_back();
    
    builder.SetInsertPoint(thenBB);
    for (Statement* stm : thenExpr) {
        stm->codegen(builder);
    }

    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(mergeBB);
    }

    for (auto& [name, info] : symbolTable.back()) {
        delete info.type;
    }
    symbolTable.pop_back();
    symbolTable.emplace_back();

    if (!elseExpr.empty()) {
        builder.SetInsertPoint(elseBB);
        for (Statement* stm : elseExpr) {
            stm->codegen(builder);
        }
        if (!builder.GetInsertBlock()->getTerminator()) {
            builder.CreateBr(mergeBB);
        }
    }

    for (auto& [name, info] : symbolTable.back()) {
        delete info.type;
    }
    symbolTable.pop_back();

    builder.SetInsertPoint(mergeBB);
}

VarUpdt::VarUpdt(const std::string n, Expr* v) : nameVar(n), value(v) {}

void VarUpdt::codegen(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();
    bool checkVariable = false;
    llvm::AllocaInst* alloca;
    Type* type;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            alloca = found->second.alloca;
            type = found->second.type;
            checkVariable = true;
            break;
        }
    }

    if(!checkVariable) throw std::runtime_error("Undeclared variable: " + nameVar);

    Value* val = value->codegen(builder);
    /*if(!(*val->getType() == *type)){
        throw std::runtime_error(
            "Updated value of variable '"+
            nameVar+
            "' not compatible with the type of the variable itself"
        );
    }*/
    if(!(*val->getType() == *type)){
            if(!val->getType()->isCastTo(type)){
                throw std::runtime_error(
                "Type mismatch for variable '" + 
                nameVar + 
                "': expected " + 
                type->toString() + 
                ", got " + 
                val->getType()->toString()
                );
            }
            Value* newVal = val->castTo(type, builder);
            delete val;
            val = newVal;
    }
    builder.CreateStore(val->getLLVMValue(), alloca);
    delete val;
}

VarDecl::VarDecl(const std::string n, Type* t, Expr* v) : nameVar(n), type(t), value(v) {}

void VarDecl::codegen(llvm::IRBuilder<> &builder)
{

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();

    bool checkVariable = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            checkVariable = true;
            break;
        }
    }
    if(checkVariable) throw std::runtime_error("Variable already declared: " + nameVar);
 
    Value* val = value->codegen(builder);
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
    llvm::Type* typeVar;
    if(!type){
        typeVar = val->getType()->getLLVMType(ctx);
        type = val->getType()->clone();
    }
    else{
        if(!(*val->getType() == *type)){
            if(!val->getType()->isCastTo(type)){
                throw std::runtime_error(
                "Type mismatch for variable '" + 
                nameVar + 
                "': expected " + 
                type->toString() + 
                ", got " + 
                val->getType()->toString()
                );
            }
            Value* newVal = val->castTo(type, builder);
            delete val;
            val = newVal;
        }
        typeVar = type->getLLVMType(ctx);
    }
    
    builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    llvm::AllocaInst* alloca = builder.CreateAlloca(typeVar, nullptr, nameVar);

    builder.SetInsertPoint(currentBlock);
    builder.CreateStore(val->getLLVMValue(), alloca);

    llvm::outs() << "The variable allocated with name: " << alloca->getName() << "\n";

    symbolTable.back()[nameVar] = {alloca, val->getType()->clone()};

    delete val;
}

// Expressions --------------

CallFunc::CallFunc(const std::string &fn, std::vector<Expr *> a) : funcName(fn), args(a) {}

Value* CallFunc::codegen(llvm::IRBuilder<>& builder) {
    // Search for the function in the module
    llvm::Function* callee = module->getFunction(funcName);
    if(!callee) {
        throw std::runtime_error("Function not found: " + funcName);
    }

    // Check the correctness of the arguments
    if(callee->arg_size() != args.size()) {
        throw std::runtime_error("Argument count mismatch for " + funcName);
    }

    llvm::LLVMContext& ctx = builder.getContext();

    // Generate argument values
    std::vector<llvm::Value*> argValues;
    for(auto* arg : args) {
        Value* value = arg->codegen(builder);
        
        if(value->getType()->getLLVMType(ctx) != callee->getFunctionType()->getParamType(argValues.size())) {
            throw std::runtime_error("Type mismatch in argument " + std::to_string(argValues.size() + 1));
        }
        argValues.push_back(value->getLLVMValue());
        delete value;
    }

    llvm::Value* llvmValueReturn = builder.CreateCall(callee, argValues, "calltmp");
    Type* returnType;
    for (const auto& func : symbolFunctions) {
        if (func.first == funcName) {  
            returnType = func.second->clone();
            break;
        }
    }
    Value* returnValue = returnType->createValue(llvmValueReturn, ctx);
    
    return returnValue;
}

BinaryCond::BinaryCond(const std::string& o, Expr* l, Expr* r) : op(o), left(l), right(r) {}

Value* BinaryCond::codegen(llvm::IRBuilder<>& builder) {
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

Value* BinaryOp::codegen(llvm::IRBuilder<>& builder) {
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

Value* UnaryOp::codegen(llvm::IRBuilder<>& builder) {
    Value* v = x->codegen(builder);
    Value* result = v->neg(builder);
    
    delete v;
    return result;
}

DoubleNum::DoubleNum(double v, Type* t) : val(v) , type(t) {}

Value* DoubleNum::codegen(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* llvmValue = llvm::ConstantFP::get(ctx, llvm::APFloat(val));
    return new DoubleValue(type->clone(), llvmValue, ctx);
}

SignedIntNum::SignedIntNum(std::int64_t v, Type* t) : val(v) , type(t) {}

Value* SignedIntNum::codegen(llvm::IRBuilder<>& builder) {
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

Value* BoolNum::codegen(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* llvmValue = llvm::ConstantInt::get(ctx, llvm::APInt(1, val ? 1 : 0));
    return new BoolValue(type->clone(), llvmValue, ctx);
}

Var::Var(const std::string& n) : name(n) {}

Value* Var::codegen(llvm::IRBuilder<>& builder) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::outs() << "VAR the name of func is : " << func << "\n";
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    llvm::LLVMContext& ctx = builder.getContext();

    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            llvm::Value* llvmValue = builder.CreateLoad(
                found->second.type->getLLVMType(ctx), 
                found->second.alloca, 
                name + "_val"
            );

            return found->second.type->createValue(llvmValue,ctx);
            //return Value::createValue(found->second.type->clone(),llvmValue,ctx);
        }
    }
    throw std::runtime_error("Undefined variable: " + name);
}

IfOp::IfOp(Expr* c, Expr* t, Expr* e) : cond(c), thenExpr(t), elseExpr(e) {}

Value* IfOp::codegen(llvm::IRBuilder<>& builder) {
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

Value* LetOp::codegen(llvm::IRBuilder<>& builder) {
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

