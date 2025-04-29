#include "ast.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <iostream>


std::vector<std::map<std::string, llvm::AllocaInst*>> symbolTable;
std::stack<llvm::BasicBlock*> mergeBlockStack;
//extern llvm::Module* module;

Function::Function(const std::string tf,const std::string nf, const std::vector<std::pair<std::string, std::string>> p,
    std::vector<Statement*> b, Statement *nxt) : typeFunc(tf), nameFunc(nf), parameters(p) ,body(b),next(nxt) {}

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

    //body->codegen(builder);
    for (Statement* stm : body) {
        stm->codegen(builder);
    }
    symbolTable.pop_back();

    if (next) next->codegen(builder);
}

Return::Return(Expr* e) : expr(e) {}

void Return::codegen(llvm::IRBuilder<>& builder) {
    llvm::Value* retVal = expr->codegen(builder);
    builder.CreateRet(retVal); 
}

WhileStm::WhileStm(Expr* c, std::vector<Statement*> w, Statement* nxt) : cond(c), whileExpr(w), next(nxt) {}

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
    //whileExpr->codegen(builder);
    for (Statement* stm : whileExpr) {
        stm->codegen(builder);
    }
    builder.CreateBr(condWhileBB);

    symbolTable.pop_back();

    builder.SetInsertPoint(nextBB);
    /*if (next) {
        next->codegen(builder);
    }*/
    /*else{
        nextBB->eraseFromParent();
    }*/
    //
}

IfStm::IfStm(Expr* c, std::vector<Statement*> t, std::vector<Statement*> e, Statement* nxt) : cond(c), thenExpr(t), elseExpr(e), next(nxt) {}

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

    //llvm::BasicBlock* parentMergeBB = mergeBlockStack.empty() ? nullptr : mergeBlockStack.top();
    //mergeBlockStack.push(mergeBB);

    if(!elseExpr.empty()){
        builder.CreateCondBr(condVal, thenBB, elseBB);
    }
    else{
        builder.CreateCondBr(condVal, thenBB, mergeBB);
    }

    symbolTable.emplace_back();

    //llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
    
    builder.SetInsertPoint(thenBB);
    //thenExpr->codegen(builder);
    for (Statement* stm : thenExpr) {
        stm->codegen(builder);
    }

    /*if (thenBB->getTerminator() && !elseExpr) {
        mergeBB = llvm::BasicBlock::Create(builder.getContext(), "merge",func);
        builder.CreateBr(mergeBB);
    }*/
    if (!thenBB->getTerminator()) {
        //mergeBB = llvm::BasicBlock::Create(builder.getContext(), "merge",func);
        builder.CreateBr(mergeBB);
    }
    //builder.CreateBr(mergeBB);

    symbolTable.pop_back();
    symbolTable.emplace_back();

    if (!elseExpr.empty()) {
        builder.SetInsertPoint(elseBB);
        //elseExpr->codegen(builder); 
        for (Statement* stm : elseExpr) {
            stm->codegen(builder);
        }
        if (!elseBB->getTerminator()) {
            //if(!mergeBB) { mergeBB = llvm::BasicBlock::Create(builder.getContext(), "merge",func);};
            builder.CreateBr(mergeBB);
        }
        //builder.CreateBr(mergeBB);
    }

    //mergeBlockStack.pop();
    symbolTable.pop_back();

    //builder.SetInsertPoint(currentBlock);

    builder.SetInsertPoint(mergeBB);
    /*if (next) {
        next->codegen(builder);
    }
    if(parentMergeBB){
        //jump into the merge of the previous IF if it doesn't have a terminator
        if (!mergeBB->getTerminator()) {
            builder.CreateBr(parentMergeBB);
        }
        
    }
    //no block jumps to the merge block
    if (mergeBB->use_empty()) {
        //removes the block from the function
        llvm::outs() << "Remove block : " << mergeBB->getName() << "\n";
        mergeBB->eraseFromParent();  
    }*/
    
    //return llvm::ConstantFP::get(ctx, llvm::APFloat(0.0));
}

VarUpdt::VarUpdt(const std::string n, Expr* v, Statement* nxt) : nameVar(n), value(v), next(nxt) {}

void VarUpdt::codegen(llvm::IRBuilder<>& builder) {
    
    //llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    //llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

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

    //if (next) next->codegen(builder);


    //return llvm::ConstantFP::get(ctx, llvm::APFloat(0.0));// return 0;
}

VarDecl::VarDecl(const std::string n, Expr* v, Statement* nxt) : nameVar(n), value(v), next(nxt) {}

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

    //if (next) next->codegen(builder);


    //return llvm::ConstantFP::get(ctx, llvm::APFloat(0.0));// return 0;
}


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
    //llvm::AllocaInst* alloca = nullptr;
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    //for (auto& inst : *currentBlock) {
    /*for (auto& inst : func->getEntryBlock()) {
        if (auto* a = llvm::dyn_cast<llvm::AllocaInst>(&inst)) {
            //llvm::outs() << "the name of var is : " << a->getName() << "\n";
            if (a->getName() == name) {
                alloca = a;
                break;
            }
        }
    }*/
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

    //if (!alloca) throw std::runtime_error("Undefined variable: " + name);
    //return builder.CreateLoad(alloca->getAllocatedType(), alloca, name + "_val");
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

    //llvm::BasicBlock* StartBB = builder.GetInsertBlock();
    
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
    //llvm::outs() << "LETOP the name of func is : " << func << "\n";
    llvm::LLVMContext& ctx = builder.getContext();

    //llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
    //builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    
    for (auto& [name, expr] : bindings) {
        llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
        builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
        //llvm::outs() << "the value of "<< name <<" is : " << val << "\n";
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

