#pragma once
#include "ast.h"

class Expr {
public:
    virtual ~Expr() = default;
    virtual Value* codegen(llvm::IRBuilder<>& builder) = 0;
};

class StructVar : public Expr {
    std::string varStructName;
    std::string memberName;
public:
    StructVar(const std::string& vsn, const std::string& mn);
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class CallFunc : public Expr {
    std::string funcName;
    std::vector<Expr*> args;
public:
    CallFunc(const std::string& fn, std::vector<Expr*> a);  
    ~CallFunc() {
        for (Expr* arg : args) {
            delete arg;
        }
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class BinaryCond : public Expr {
    std::string op;
    Expr* left;
    Expr* right;
public:
    BinaryCond(const std::string& o, Expr* l, Expr* r);
    ~BinaryCond() {
        delete left; 
        delete right; 
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class BinaryOp : public Expr {
    std::string op;
    Expr* left;
    Expr* right;
public:
    BinaryOp(const std::string& o, Expr* l, Expr* r);
    ~BinaryOp() {
        delete left; 
        delete right; 
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class UnaryOp : public Expr {
    std::string op;
    Expr* x;
public:
    UnaryOp(const std::string& o, Expr* x);
    ~UnaryOp() {
        delete x;  
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class DoubleNum : public Expr {
    double val;
    Type* type;
public:
    DoubleNum(double v, Type* t);
    ~DoubleNum() {
        delete type;  
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class SignedIntNum : public Expr {
    std::int64_t val;
    Type* type;
public:
    SignedIntNum(std::int64_t v, Type* t);
    ~SignedIntNum() {
        delete type;  
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class BoolNum : public Expr {
    bool val;
    Type* type;
public:
    BoolNum(bool v, Type* t);
    ~BoolNum() {
        delete type;  
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class Var : public Expr {
    std::string name;
public:
    Var(const std::string& n);
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class IfOp : public Expr {
    Expr* cond;
    Expr* thenExpr;
    Expr* elseExpr;
public:
    IfOp(Expr* c, Expr* t, Expr* e);
    ~IfOp() {
        delete cond;  
        delete thenExpr; 
        delete elseExpr; 
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class LetOp : public Expr {
    std::vector<std::pair<std::string, Expr*>> bindings;
    Expr* body;
public:
    LetOp(const std::vector<std::pair<std::string, Expr*>>& b, Expr* bod);
    ~LetOp() {
        delete body;  
        for (auto& binding : bindings) {
            delete binding.second;
        }
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};