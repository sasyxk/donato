#pragma once
#include "ast.h"

class Expr {
public:
    virtual ~Expr() = default;
    virtual Value* codegen(llvm::IRBuilder<>& builder) = 0;
};

class StructVar : public Expr {
    std::string varStructName;
    std::vector<std::string> memberChain;
public:
    StructVar(const std::string& vsn, const std::vector<std::string> &mn);
    ~StructVar()  = default;
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class CallFunc : public Expr {
    std::string funcName;
    std::vector<Expr*> args;
    std::string nameOfClass;
public:
    CallFunc(const std::string& fn, const std::string nameOfClass, std::vector<Expr*> a);  
    ~CallFunc() {
        for (Expr* arg : args) {
            delete arg;
        }
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

 class  ClassCallFunc : public Expr {
    std::string firstVariableName;
    std::vector<std::string> memberChain;
    std::string nameOfClass;
    std::vector<Expr*> args;
public:
    ClassCallFunc(
        const std::string fvn,
        const std::vector<std::string> mn,
        std::string noc,
        std::vector<Expr*> a);
    ~ClassCallFunc() {
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
    ~DoubleNum()  = default;
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class SignedIntNum : public Expr {
    std::int64_t val;
    Type* type;
public:
    SignedIntNum(std::int64_t v, Type* t);
    ~SignedIntNum()  = default;
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class BoolNum : public Expr {
    bool val;
    Type* type;
public:
    BoolNum(bool v, Type* t);
    ~BoolNum() = default;
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class Var : public Expr {
    std::string name;
public:
    Var(const std::string& n);
    ~Var() = default;
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

class DereferenceOp : public Expr {
    Expr* x;
public:
    DereferenceOp(Expr* x);
    ~DereferenceOp() {
        delete x;
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class NewOp : public Expr {
    std::vector<Expr*> args;
    std::string nameClass;
    Type* type;
public:
    NewOp(std::string nc, std::vector<Expr*> a);
    ~NewOp(){
        for(auto arg : args ){
            delete arg;
        }
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class AddressOp : public Expr {
    Expr* value;
public:
    AddressOp(Expr* v);
    ~AddressOp(){
        delete value;
    }
    Value* codegen(llvm::IRBuilder<>& builder) override;
};

class NullPtr : public Expr {
    Type* type;
public:
    NullPtr(Type* t);
    ~NullPtr() = default;
    Value* codegen(llvm::IRBuilder<>& builder) override;
};