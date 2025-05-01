#include "ast.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <iostream>



std::vector<std::map<std::string, SymbolInfo>> symbolTable;

// Statements --------------

Function::Function(const std::string tf,const std::string nf, const std::vector<std::pair<std::string, std::string>> p,
    std::vector<Statement*> b) : typeFunc(tf), nameFunc(nf), parameters(p) ,body(b) {}

void Function::codegen(llvm::IRBuilder<> &builder) {
    std::vector<llvm::Type*> paramTypes;
    for (const auto& param : parameters) {
        llvm::Type* paramType;
        if (param.first == "double") {
            paramType = builder.getDoubleTy();
        } else if (param.first == "int") {
            paramType = builder.getInt32Ty();
        } else {
            throw std::runtime_error("Unknown parameter type: " + param.first);
        }
        paramTypes.push_back(paramType);
    }

    llvm::Type* returnType;
    if (typeFunc == "double") {
        returnType = builder.getDoubleTy();
    } else if (typeFunc == "int") {
        returnType = builder.getInt32Ty();
    } else {
        throw std::runtime_error("Invalid function type: " + typeFunc);
    }

    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = module->getFunction(nameFunc);

    if (function && function->getFunctionType() == funcType) {
        throw std::runtime_error("Redefinition of function: " + nameFunc);
    }

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

        //To updated
        Type* paramType;
        if (param.first == "double") {
            paramType = new DoubleType(); 
        } else if (param.first == "int") {
            paramType = new IntType(32, true); 
        } else {
            throw std::runtime_error("Unknown parameter type: " + param.first);
        }

        symbolTable.back()[param.second] = {alloca, paramType};
    }

    for (Statement* stm : body) {
        stm->codegen(builder);
    }
    symbolTable.pop_back();
}

Return::Return(Expr* e) : expr(e) {}

void Return::codegen(llvm::IRBuilder<>& builder) {
    Type* retVal = expr->codegen(builder);
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
    Type* condVal = cond->codegen(builder);
    //check if the condition is already a boolean value
   if (!condVal->getLLVMValue()->getType()->isIntegerTy(1)) { //TO change into !dynamic_cast<const BoolType*>(condVal) != nullptr
        Type* zero = Type::fromString("double");
        zero->setLLVMValue(llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(0.0)));
        condVal = Type::operateWith(builder, condVal, zero, "==");
    }
    builder.CreateCondBr(condVal->getLLVMValue(), bodyWhileBB, nextBB);

    symbolTable.emplace_back();

    builder.SetInsertPoint(bodyWhileBB);
    for (Statement* stm : whileExpr) {
        stm->codegen(builder);
    }
    builder.CreateBr(condWhileBB);

    symbolTable.pop_back();

    builder.SetInsertPoint(nextBB);
}

IfStm::IfStm(Expr* c, std::vector<Statement*> t, std::vector<Statement*> e) : cond(c), thenExpr(t), elseExpr(e) {}

void IfStm::codegen(llvm::IRBuilder<>& builder) {
    Type* condVal = cond->codegen(builder);
    //check if the condition is already a boolean value
    /*if (!condVal->getType()->isIntegerTy(1)) {
        condVal = builder.CreateFCmpONE(condVal, llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(0.0)), "ifconf");
    }*/
    if (!condVal->getLLVMValue()->getType()->isIntegerTy(1)) { //TO change into !dynamic_cast<const BoolType*>(condVal) != nullptr
        Type* zero = Type::fromString("double");
        zero->setLLVMValue(llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(0.0)));
        condVal = Type::operateWith(builder, condVal, zero, "==");
        /*llvm::outs() << "Valore condVal il ==: ";
        condVal1->getLLVMValue()->getType()->print(llvm::outs());
        llvm::outs() << "\n";*/
    }
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(builder.getContext(), "then", func);
    llvm::BasicBlock* elseBB = !elseExpr.empty() ? llvm::BasicBlock::Create(ctx, "else", func) : nullptr;
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(builder.getContext(), "merge",func);

    if(!elseExpr.empty()){
        builder.CreateCondBr(condVal->getLLVMValue(), thenBB, elseBB);
    }
    else{
        builder.CreateCondBr(condVal->getLLVMValue(), thenBB, mergeBB);
    }

    symbolTable.emplace_back();
    
    builder.SetInsertPoint(thenBB);
    for (Statement* stm : thenExpr) {
        stm->codegen(builder);
    }

    if (!thenBB->getTerminator()) {
        builder.CreateBr(mergeBB);
    }

    symbolTable.pop_back();
    symbolTable.emplace_back();

    if (!elseExpr.empty()) {
        builder.SetInsertPoint(elseBB);
        for (Statement* stm : elseExpr) {
            stm->codegen(builder);
        }
        if (!elseBB->getTerminator()) {
            builder.CreateBr(mergeBB);
        }
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

    Type* val = value->codegen(builder);
    llvm::Value* llvmVal = val->getLLVMValue();
    if(!(val == type)){
        
        if(!val->canCastTo(*type))
            throw std::runtime_error("Updating variable '"+nameVar+"' with different type not allowed");
        
        llvmVal = val->generateCast(builder, val->getLLVMValue(),*type);
    }
    builder.CreateStore(llvmVal, alloca);
}

VarDecl::VarDecl(const std::string n, Expr* v) : nameVar(n), value(v) {}

void VarDecl::codegen(llvm::IRBuilder<>& builder) {
    
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    //llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    bool checkVariable = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            checkVariable = true;
            break;
        }
    }
    if(checkVariable) throw std::runtime_error("Variable already declared: " + nameVar);

    Type* val = value->codegen(builder);
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
    builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    llvm::AllocaInst* alloca = builder.CreateAlloca(val->toLLVMType(ctx), nullptr, nameVar);

    builder.SetInsertPoint(currentBlock);
    builder.CreateStore(val->getLLVMValue(), alloca);

    llvm::outs() << "The variable allocated with name: " << alloca->getName() << "\n";

    symbolTable.back()[nameVar] = {alloca, val};

}

// Expressions --------------

CallFunc::CallFunc(const std::string &fn, std::vector<Expr *> a) : funcName(fn), args(a) {}

Type* CallFunc::codegen(llvm::IRBuilder<>& builder) {
    // Search for the function in the module
    llvm::Function* callee = module->getFunction(funcName);
    if(!callee) {
        throw std::runtime_error("Function not found: " + funcName);
    }

    // Check the correctness of the arguments
    if(callee->arg_size() != args.size()) {
        throw std::runtime_error("Argument count mismatch for " + funcName);
    }

    // Generate argument values
    std::vector<llvm::Value*> argValues;
    for(auto* arg : args) {
        Type* val = arg->codegen(builder);
        
        if(val->getLLVMValue()->getType() != callee->getFunctionType()->getParamType(argValues.size())) {
            throw std::runtime_error("Type mismatch in argument " + std::to_string(argValues.size() + 1));
        }
        argValues.push_back(val->getLLVMValue());
    }

    llvm::Value* returnValue = builder.CreateCall(callee, argValues, "calltmp");
    Type* returnTypeObj = new DoubleType(); //Let's assume for simplicity that it always returns double -------!!!
    returnTypeObj->setLLVMValue(returnValue);
    return returnTypeObj;
}


BinaryCond::BinaryCond(const std::string& o, Expr* l, Expr* r) : op(o), left(l), right(r) {}

Type* BinaryCond::codegen(llvm::IRBuilder<>& builder) {
    Type* L = left->codegen(builder);
    Type* R = right->codegen(builder);

    return Type::operateWith(builder,L,R,op);
}

BinaryOp::BinaryOp(const std::string& o, Expr* l, Expr* r) : op(o), left(l), right(r) {}

Type* BinaryOp::codegen(llvm::IRBuilder<>& builder) {
    Type* L = left->codegen(builder);
    Type* R = right->codegen(builder);

    return Type::operateWith(builder,L,R,op);
}

UnaryOp::UnaryOp(const std::string& o, Expr* x) : op(o), x(x) {}

Type* UnaryOp::codegen(llvm::IRBuilder<>& builder) {
    Type* V = x->codegen(builder);
    //return V->unaryOp();    /// To implement -----------------------------------------------!!!
    return nullptr;
    //builder.CreateFNeg(V, "negtmp");
}

Num::Num(const std::string& v, Type* t) : val(v), type(t) {}

Type* Num::codegen(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();
    if (auto intType = dynamic_cast<IntType*>(type)) {
        if (intType->isSigned()) {
            intType->setLLVMValue(llvm::ConstantInt::get(
                intType->toLLVMType(ctx), 
                llvm::APInt(intType->getSize() * 8, std::stoll(val))
            ));
        } else {
            intType->setLLVMValue(llvm::ConstantInt::get(
                intType->toLLVMType(ctx), 
                llvm::APInt(intType->getSize() * 8, std::stoull(val), false)
            ));
        }
        return intType;
    } 
    else if (auto doubleType = dynamic_cast<DoubleType*>(type)) {
        doubleType->setLLVMValue(llvm::ConstantFP::get(ctx, llvm::APFloat(std::stod(val))));
        return doubleType;
    }
    
    throw std::runtime_error("Unsupported type '" + type->toString()+"' in Num::codegen");
    
    //return llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(val));
}

Var::Var(const std::string& n) : name(n) {}

Type* Var::codegen(llvm::IRBuilder<>& builder) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::outs() << "VAR the name of func is : " << func << "\n";
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            llvm::Value* loadedValue = builder.CreateLoad(
                found->second.type->toLLVMType(builder.getContext()), 
                found->second.alloca, 
                name + "_val"
            );
            Type* resultType = found->second.type->clone();
            resultType->setLLVMValue(loadedValue);
            return resultType;
        }
    }
    throw std::runtime_error("Undefined variable: " + name);
}

IfOp::IfOp(Expr* c, Expr* t, Expr* e) : cond(c), thenExpr(t), elseExpr(e) {}

Type* IfOp::codegen(llvm::IRBuilder<>& builder) {

    Type* condVal = cond->codegen(builder);
    //check if the condition is already a boolean value
    if (!condVal->getLLVMValue()->getType()->isIntegerTy(1)) { //TO change into !dynamic_cast<const BoolType*>(condVal) != nullptr
        //condVal->setLLVMValue(builder.CreateFCmpONE(condVal->getLLVMValue(), llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(0.0)), "ifconf"));
        Type* zero = Type::fromString("double");
        zero->setLLVMValue(llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(0.0)));
        condVal = Type::operateWith(builder, condVal, zero, "==");
    }
    
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(builder.getContext(), "then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(builder.getContext(), "else",func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(builder.getContext(), "ifcont",func);
    
    builder.CreateCondBr(condVal->getLLVMValue(), thenBB, elseBB);
    
    builder.SetInsertPoint(thenBB);
    Type* thenVal = thenExpr->codegen(builder);
    llvm::BasicBlock* thenExitBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);
    
    builder.SetInsertPoint(elseBB);
    Type* elseVal = elseExpr->codegen(builder);
    llvm::BasicBlock* elseExitBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);
    
    builder.SetInsertPoint(mergeBB);

    if(!(*thenVal == *elseVal)){
        throw std::runtime_error("IfOp has branches with results of different types");
    }
    
    llvm::PHINode* phi = builder.CreatePHI(llvm::Type::getDoubleTy(builder.getContext()), 2, "iftmp");
    phi->addIncoming(thenVal->getLLVMValue(), thenExitBB);
    phi->addIncoming(elseVal->getLLVMValue(), elseExitBB);

    Type* phiType = thenVal;

    phiType->setLLVMValue(phi);
    return phiType;
}

LetOp::LetOp(const std::vector<std::pair<std::string, Expr*>>& b, Expr* bod) : bindings(b), body(bod) {}

Type* LetOp::codegen(llvm::IRBuilder<>& builder) {
    symbolTable.emplace_back();

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    
    for (auto& [name, expr] : bindings) {
        //llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
        //builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
        //llvm::AllocaInst* alloca = builder.CreateAlloca(llvm::Type::getDoubleTy(ctx), nullptr, name);

        //builder.SetInsertPoint(currentBlock);
        Type* val = expr->codegen(builder);
        llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
        builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
        llvm::AllocaInst* alloca = builder.CreateAlloca(val->toLLVMType(ctx), nullptr, name);

        builder.SetInsertPoint(currentBlock);
        builder.CreateStore(val->getLLVMValue(), alloca);

        llvm::outs() << "The variable allocated with name: " << alloca->getName() << "\n";

        symbolTable.back()[name] = {alloca, val};
    }

    Type* bodyVal = body->codegen(builder);

    symbolTable.pop_back();

    return bodyVal;
}

