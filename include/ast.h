#pragma once
#include <vector>
#include <string>
#include <map>
#include <utility>
#include "llvm/IR/Value.h"
#include "llvm/IR/IRBuilder.h"
#include "codegen.h"
#include "type.h"
#include "value.h"
#include "double_value.h"
#include "signed_int_value.h"
#include "bool_value.h"

struct SymbolInfo {
    llvm::AllocaInst* alloca;  
    Type* type;               
};

extern std::vector<std::map<std::string, SymbolInfo>> symbolTable;
extern std::vector<std::pair<std::string, Type*>> symbolFunctions;
extern std::vector<StructType* > symbolStructsType;
extern llvm::Module* module;

class Expr {
public:
    virtual ~Expr() = default;
    virtual Value* codegen(llvm::IRBuilder<>& builder) = 0;
};

class Statement {
public:
    virtual ~Statement() = default;
    virtual void codegen(llvm::IRBuilder<>& builder) = 0;
};

class Function : public Statement{
    Type* typeFunc;
    std::string nameFunc;
    std::vector<std::pair<Type*, std::string>> parameters;
    std::vector<Statement*> body;
public:
    Function(Type* tf,const std::string nf,const std::vector<std::pair<Type*, std::string>> p,
        std::vector<Statement*> b);
    ~Function() {
        for (Statement* stmt : body) {
            delete stmt;
        }
    }

    void codegen(llvm::IRBuilder<>& builder) override;
};

class VarDecl : public Statement {
    std::string nameVar;
    Expr* value;
    Type* type;
public:
    VarDecl(const std::string n, Type* t, Expr* v);
    ~VarDecl() {
        delete value;
    }

    void codegen(llvm::IRBuilder<>& builder) override;
};

class VarUpdt : public Statement {
    std::string nameVar;
    Expr* value;
public:
    VarUpdt(const std::string n, Expr* v);
    ~VarUpdt() {
        delete value;
    }

    void codegen(llvm::IRBuilder<>& builder) override;
};

class WhileStm : public Statement {
    Expr* cond;
    std::vector<Statement*> whileExpr;
public:
    WhileStm(Expr* c, std::vector<Statement*> w);
    ~WhileStm() {
        delete cond;
        for (Statement* stmt : whileExpr) {
            delete stmt;
        }
    }

    void codegen(llvm::IRBuilder<>& builder) override;
};

class IfStm : public Statement {
    Expr* cond;
    std::vector<Statement*> thenExpr;
    std::vector<Statement*> elseExpr;
public:
    IfStm(Expr* c, std::vector<Statement*> t, std::vector<Statement*> e);
    ~IfStm() {
        delete cond;
        for (Statement* stmt : thenExpr) {
            delete stmt;
        }
        for (Statement* stmt : elseExpr) {
            delete stmt;
        }
    }

    void codegen(llvm::IRBuilder<>& builder) override;
};

class Return : public Statement {
    Expr* expr;
    std::string funcName;
public:
    Return(Expr* e, std::string fn);
    ~Return() {
        delete expr;
    }

    void codegen(llvm::IRBuilder<>& builder) override;
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