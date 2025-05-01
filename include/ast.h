#pragma once
#include <vector>
#include <string>
#include <map>
#include <stack>
#include <utility>
#include "llvm/IR/Value.h"
#include "llvm/IR/IRBuilder.h"
#include "codegen.h"
#include "type.h"

struct SymbolInfo {
    llvm::AllocaInst* alloca;  
    Type* type;          
};

extern std::vector<std::map<std::string, SymbolInfo>> symbolTable;
extern std::stack<llvm::BasicBlock*> mergeBlockStack;
extern llvm::Module* module;



class Expr {
public:
    virtual ~Expr() = default;
    //virtual llvm::Value* codegen(llvm::IRBuilder<>& builder) = 0;
    virtual Type* codegen(llvm::IRBuilder<>& builder) = 0;
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
public:
    VarDecl(const std::string n, Expr* v);
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
public:
    Return(Expr* e);
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

    Type* codegen(llvm::IRBuilder<>& builder) override;
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

    Type* codegen(llvm::IRBuilder<>& builder) override;
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
    Type* codegen(llvm::IRBuilder<>& builder) override;
};

class UnaryOp : public Expr {
    std::string op;
    Expr* x;
public:
    UnaryOp(const std::string& o, Expr* x);
    ~UnaryOp() {
        delete x;  
    }
    Type* codegen(llvm::IRBuilder<>& builder) override;
};

class Num : public Expr {
    std::string val;
    Type* type;
public:
    Num(const std::string& v, Type* t);
    Type* codegen(llvm::IRBuilder<>& builder) override;
};

class Var : public Expr {
    std::string name;
public:
    Var(const std::string& n);
    Type* codegen(llvm::IRBuilder<>& builder) override;
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
    Type* codegen(llvm::IRBuilder<>& builder) override;
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
    Type* codegen(llvm::IRBuilder<>& builder) override;
};