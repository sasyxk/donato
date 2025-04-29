#include "ast.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <iostream>


std::vector<std::map<std::string, llvm::AllocaInst*>> symbolTable;
std::stack<llvm::BasicBlock*> mergeBlockStack;

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
        symbolTable.back()[param.second] = alloca;
    }

    for (Statement* stm : body) {
        stm->codegen(builder);
    }
    symbolTable.pop_back();
}

Return::Return(Expr* e) : expr(e) {}

void Return::codegen(llvm::IRBuilder<>& builder) {
    llvm::Value* retVal = expr->codegen(builder);
    builder.CreateRet(retVal); 
}

WhileStm::WhileStm(Expr* c, std::vector<Statement*> w) : cond(c), whileExpr(w) {}

void WhileStm::codegen(llvm::IRBuilder<>& builder) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* condWhileBB = llvm::BasicBlock::Create(builder.getContext(), "condWhile", func);
    llvm::BasicBlock* bodyWhileBB = llvm::BasicBlock::Create(builder.getContext(), "bodyWhile", func);
    llvm::BasicBlock* nextBB = llvm::BasicBlock::Create(builder.getContext(), "mergeWhile", func);

    builder.CreateBr(condWhileBB);

    builder.SetInsertPoint(condWhileBB);
    llvm::Value* condVal = cond->codegen(builder);
    //check if the condition is already a boolean value
    if (!condVal->getType()->isIntegerTy(1)) {
        condVal = builder.CreateFCmpONE(condVal, llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(0.0)), "ifconf");
    }
    builder.CreateCondBr(condVal, bodyWhileBB, nextBB);

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
    llvm::Value* condVal = cond->codegen(builder);
    //check if the condition is already a boolean value
    if (!condVal->getType()->isIntegerTy(1)) {
        condVal = builder.CreateFCmpONE(condVal, llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(0.0)), "ifconf");
    }
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(builder.getContext(), "then", func);
    llvm::BasicBlock* elseBB = !elseExpr.empty() ? llvm::BasicBlock::Create(ctx, "else", func) : nullptr;
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(builder.getContext(), "merge",func);

    if(!elseExpr.empty()){
        builder.CreateCondBr(condVal, thenBB, elseBB);
    }
    else{
        builder.CreateCondBr(condVal, thenBB, mergeBB);
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
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            alloca = found->second;
            checkVariable = true;
            break;
        }
    }

    if(!checkVariable) throw std::runtime_error("Undeclared variable: " + nameVar);

    llvm::Value* val = value->codegen(builder);
    builder.CreateStore(val, alloca);
}

VarDecl::VarDecl(const std::string n, Expr* v) : nameVar(n), value(v) {}

void VarDecl::codegen(llvm::IRBuilder<>& builder) {
    
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    bool checkVariable = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            checkVariable = true;
            break;
        }
    }
    if(checkVariable) throw std::runtime_error("Variable already declared: " + nameVar);

    builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    llvm::AllocaInst* alloca = builder.CreateAlloca(llvm::Type::getDoubleTy(ctx), nullptr, nameVar);
    builder.SetInsertPoint(currentBlock);  

    llvm::Value* val = value->codegen(builder);
    builder.CreateStore(val, alloca);
    symbolTable.back()[nameVar] = alloca;
}

// Expressions --------------

CallFunc::CallFunc(const std::string &fn, std::vector<Expr *> a) : funcName(fn), args(a) {}

llvm::Value* CallFunc::codegen(llvm::IRBuilder<>& builder) {
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
        llvm::Value* val = arg->codegen(builder);
        
        if(val->getType() != callee->getFunctionType()->getParamType(argValues.size())) {
            throw std::runtime_error("Type mismatch in argument " + std::to_string(argValues.size() + 1));
        }
        argValues.push_back(val);
    }

    return builder.CreateCall(callee, argValues, "calltmp");
}


BinaryCond::BinaryCond(const std::string& o, Expr* l, Expr* r) : op(o), left(l), right(r) {}

llvm::Value* BinaryCond::codegen(llvm::IRBuilder<>& builder) {
    llvm::Value* L = left->codegen(builder);
    llvm::Value* R = right->codegen(builder);
    //llvm::outs() << "the Rigth of BinaryCond  is : " << R->getName() << "\n";
    if (op == "==") return builder.CreateFCmpOEQ(L, R, "eqtmp");
    if (op == "<=") return builder.CreateFCmpOLE(L, R, "leqtmp");
    if (op == ">=")  return builder.CreateFCmpOGE(L, R, "geqtmp");
    if (op == ">") return builder.CreateFCmpOGT(L, R, "gtmp");
    if (op == "<") return builder.CreateFCmpOLT(L, R, "ltmp");
    if (op == "!=") return builder.CreateFCmpONE(L, R, "netmp");
    throw std::runtime_error("Unknown operator");
}

BinaryOp::BinaryOp(const std::string& o, Expr* l, Expr* r) : op(o), left(l), right(r) {}

llvm::Value* BinaryOp::codegen(llvm::IRBuilder<>& builder) {
    llvm::Value* L = left->codegen(builder);
    llvm::Value* R = right->codegen(builder);
    //llvm::outs() << "the Rigth of Binary  is : " << R->getName() << "\n";
    if (op == "+") return builder.CreateFAdd(L, R, "addtmp");
    if (op == "-") return builder.CreateFSub(L, R, "subtmp");
    if (op == "*") return builder.CreateFMul(L, R, "multmp");
    if (op == "/") return builder.CreateFDiv(L, R, "divtmp");
    throw std::runtime_error("Unknown operator");
}

UnaryOp::UnaryOp(const std::string& o, Expr* x) : op(o), x(x) {}

llvm::Value* UnaryOp::codegen(llvm::IRBuilder<>& builder) {
    llvm::Value* V = x->codegen(builder);
    return builder.CreateFNeg(V, "negtmp");
}

Num::Num(double v) : val(v) {}

llvm::Value* Num::codegen(llvm::IRBuilder<>& builder) {
    return llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(val));
}

Var::Var(const std::string& n) : name(n) {}

llvm::Value* Var::codegen(llvm::IRBuilder<>& builder) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::outs() << "VAR the name of func is : " << func << "\n";
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return builder.CreateLoad(
                llvm::Type::getDoubleTy(builder.getContext()), 
                found->second, 
                name + "_val"
            );
        }
    }
    throw std::runtime_error("Undefined variable: " + name);
}

IfOp::IfOp(Expr* c, Expr* t, Expr* e) : cond(c), thenExpr(t), elseExpr(e) {}

llvm::Value* IfOp::codegen(llvm::IRBuilder<>& builder) {

    llvm::Value* condVal = cond->codegen(builder);
    //check if the condition is already a boolean value
    if (!condVal->getType()->isIntegerTy(1)) {
        condVal = builder.CreateFCmpONE(condVal, llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(0.0)), "ifconf");
    }
    
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(builder.getContext(), "then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(builder.getContext(), "else",func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(builder.getContext(), "ifcont",func);
    
    builder.CreateCondBr(condVal, thenBB, elseBB);
    
    builder.SetInsertPoint(thenBB);
    llvm::Value* thenVal = thenExpr->codegen(builder);
    llvm::BasicBlock* thenExitBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);
    
    builder.SetInsertPoint(elseBB);
    llvm::Value* elseVal = elseExpr->codegen(builder);
    llvm::BasicBlock* elseExitBB = builder.GetInsertBlock();
    builder.CreateBr(mergeBB);
    
    builder.SetInsertPoint(mergeBB);
    
    llvm::PHINode* phi = builder.CreatePHI(llvm::Type::getDoubleTy(builder.getContext()), 2, "iftmp");
    phi->addIncoming(thenVal, thenExitBB);
    phi->addIncoming(elseVal, elseExitBB);
    return phi;
}

LetOp::LetOp(const std::vector<std::pair<std::string, Expr*>>& b, Expr* bod) : bindings(b), body(bod) {}

llvm::Value* LetOp::codegen(llvm::IRBuilder<>& builder) {
    symbolTable.emplace_back();

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    
    for (auto& [name, expr] : bindings) {
        llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
        builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
        llvm::AllocaInst* alloca = builder.CreateAlloca(llvm::Type::getDoubleTy(ctx), nullptr, name);

        builder.SetInsertPoint(currentBlock);
        llvm::Value* val = expr->codegen(builder);
        builder.CreateStore(val, alloca);

        llvm::outs() << "The variable allocated with name: " << alloca->getName() << "\n";

        symbolTable.back()[name] = alloca;
    }

    llvm::Value* bodyVal = body->codegen(builder);

    symbolTable.pop_back();

    return bodyVal;
}

