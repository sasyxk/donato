#pragma once
#include "ast.h"

class Expr {
public:
    virtual ~Expr() = default;
    virtual Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) = 0;
};


Value* invokeMemberFunction(
    std::string nameOfClass,
    ClassType* classType,
    std::string memberName,
    std::string nameCurrVar,
    std::vector<Expr*> args,
    llvm::Value* currentPtr,
    bool wantReturn,
    llvm::IRBuilder<>& builder
);

Value* generateClassFunctionCall(
    llvm::IRBuilder<>& builder,
    const std::string firstVariableName,
    const std::vector<std::string> memberChain,
    const std::string nameOfClass,
    const std::vector<Expr*> args,
    bool returnsValue
);

class StructVar : public Expr {
    std::string varStructName;
    std::vector<std::string> memberChain;
public:
    StructVar(const std::string& vsn, const std::vector<std::string> &mn);
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
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
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
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
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
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
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
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
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
};

class UnaryOp : public Expr {
    std::string op;
    Expr* x;
public:
    UnaryOp(const std::string& o, Expr* x);
    ~UnaryOp() {
        delete x;  
    }
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
};

class DoubleNum : public Expr {
    double val;
    Type* type;
public:
    DoubleNum(double v, Type* t);
    ~DoubleNum() {
        delete type;  
    }
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
};

class SignedIntNum : public Expr {
    std::int64_t val;
    Type* type;
public:
    SignedIntNum(std::int64_t v, Type* t);
    ~SignedIntNum() {
        delete type;  
    }
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
};

class BoolNum : public Expr {
    bool val;
    Type* type;
public:
    BoolNum(bool v, Type* t);
    ~BoolNum() {
        delete type;  
    }
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
};

class Var : public Expr {
    std::string name;
public:
    Var(const std::string& n);
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
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
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
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
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
};


class DereferenceOp : public Expr {
    Expr* x;
public:
    DereferenceOp(Expr* x);
    ~DereferenceOp() {
        delete x;
    }
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
};

class NewOp : public Expr {
    std::vector<Expr*> args;
    std::string nameClass;
    ClassType* classType;
public:
    NewOp(std::string nc, std::vector<Expr*> a);
    ~NewOp(){
        for(auto arg : args ){
            delete arg;
        }
        delete classType;
    }
    Value* codegen(llvm::IRBuilder<>& builder, bool isPointer = false) override;
};