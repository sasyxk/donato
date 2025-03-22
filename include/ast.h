#pragma once
#include <vector>
#include <string>
#include <map>
#include <stack>
#include <utility>
#include "llvm/IR/Value.h"
#include "llvm/IR/IRBuilder.h"

extern std::vector<std::map<std::string, llvm::AllocaInst*>> symbolTable;
extern std::stack<llvm::BasicBlock*> mergeBlockStack;


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

class VarDecl : public Statement {
    std::string nameVar;
    Expr* value;
    Statement* next;
public:
    VarDecl(const std::string n, Expr* v, Statement* nxt = nullptr);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class VarUpdt : public Statement {
    std::string nameVar;
    Expr* value;
    Statement* next;
public:
    VarUpdt(const std::string n, Expr* v, Statement* nxt = nullptr);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class WhileStm : public Statement {
    Expr* cond;
    Statement* whileExpr;
    Statement* next;
public:
    WhileStm(Expr* c, Statement* w, Statement* nxt = nullptr);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class IfStm : public Statement {
    Expr* cond;
    Statement* thenExpr;
    Statement* elseExpr;
    Statement* next;
public:
    IfStm(Expr* c, Statement* t, Statement* e, Statement* nxt = nullptr);
    void codegen(llvm::IRBuilder<>& builder) override;
};

class Return : public Statement {
    Expr* expr;
public:
    Return(Expr* e);
    void codegen(llvm::IRBuilder<>& builder) override;
};

/*
class Block : public Statement {
    Statement* first;
    Statement* rest;
public:
    Block(Statement* f, Statement* r = nullptr);
    llvm::Value* codegen(llvm::IRBuilder<>& builder) override;
};
*/


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