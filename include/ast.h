#pragma once
#include <vector>
#include <string>
#include <map>
#include <stack>
#include <utility>
#include "llvm/IR/Value.h"
#include "llvm/IR/IRBuilder.h"
#include "codegen.h"

extern std::vector<std::map<std::string, llvm::AllocaInst*>> symbolTable;
extern std::stack<llvm::BasicBlock*> mergeBlockStack;
extern llvm::Module* module;


class Expr {
public:
    virtual ~Expr() = default;
    virtual llvm::Value* codegen(llvm::IRBuilder<>& builder) = 0;
};

class Statement {
public:
    virtual ~Statement() = default;
    virtual void codegen(llvm::IRBuilder<>& builder) = 0;
};

class Function : public Statement{
    std::string typeFunc;
    std::string nameFunc;
    std::vector<std::pair<std::string, std::string>> parameters;
    std::vector<Statement*> body;
public:
    Function(const std::string tf,const std::string nf,const std::vector<std::pair<std::string, std::string>> p,
        std::vector<Statement*> b);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class VarDecl : public Statement {
    std::string nameVar;
    Expr* value;
public:
    VarDecl(const std::string n, Expr* v);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class VarUpdt : public Statement {
    std::string nameVar;
    Expr* value;
public:
    VarUpdt(const std::string n, Expr* v);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class WhileStm : public Statement {
    Expr* cond;
    std::vector<Statement*> whileExpr;
public:
    WhileStm(Expr* c, std::vector<Statement*> w);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class IfStm : public Statement {
    Expr* cond;
    std::vector<Statement*> thenExpr;
    std::vector<Statement*> elseExpr;
public:
    IfStm(Expr* c, std::vector<Statement*> t, std::vector<Statement*> e);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class Return : public Statement {
    Expr* expr;
public:
    Return(Expr* e);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class CallFunc : public Expr {
    std::string funcName;
    std::vector<Expr*> args;
public:
    CallFunc(const std::string& fn, std::vector<Expr*> a);    
    llvm::Value* codegen(llvm::IRBuilder<>& builder) override;
};

class BinaryCond : public Expr {
    std::string op;
    Expr* left;
    Expr* right;
public:
    BinaryCond(const std::string& o, Expr* l, Expr* r);
    llvm::Value* codegen(llvm::IRBuilder<>& builder) override;
};

class BinaryOp : public Expr {
    std::string op;
    Expr* left;
    Expr* right;
public:
    BinaryOp(const std::string& o, Expr* l, Expr* r);
    llvm::Value* codegen(llvm::IRBuilder<>& builder) override;
};

class UnaryOp : public Expr {
    std::string op;
    Expr* x;
public:
    UnaryOp(const std::string& o, Expr* x);
    llvm::Value* codegen(llvm::IRBuilder<>& builder) override;
};

class Num : public Expr {
    double val;
public:
    Num(double v);
    llvm::Value* codegen(llvm::IRBuilder<>& builder) override;
};

class Var : public Expr {
    std::string name;
public:
    Var(const std::string& n);
    llvm::Value* codegen(llvm::IRBuilder<>& builder) override;
};

class IfOp : public Expr {
    Expr* cond;
    Expr* thenExpr;
    Expr* elseExpr;
public:
    IfOp(Expr* c, Expr* t, Expr* e);
    llvm::Value* codegen(llvm::IRBuilder<>& builder) override;
};

class LetOp : public Expr {
    std::vector<std::pair<std::string, Expr*>> bindings;
    Expr* body;
public:
    LetOp(const std::vector<std::pair<std::string, Expr*>>& b, Expr* bod);
    llvm::Value* codegen(llvm::IRBuilder<>& builder) override;
};