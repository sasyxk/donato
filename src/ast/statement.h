#pragma once
#include "ast.h"
#include "expr.h"

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
    void setClassArg(StructType* arg);
    std::string getName() const {return nameFunc;}
};



class DefineClass : public Statement{
    ClassType* classType;
    std::vector<Function*> functions;
public:
    DefineClass(
        std::string nc,
        std::vector<std::pair<Type *, std::string>> pm,
        std::vector<std::pair<Type *, std::string>> ca,
        std::vector<Statement *> cbs,
        std::vector<Function*> pf 
    );
    ~DefineClass() {
        delete classType;
        for (auto function : functions){
            delete function;
        }
    }
    void codegen(llvm::IRBuilder<>& builder) override;
};

class DefineStruct : public Statement{
    std::string nameStruct;
    std::vector<std::pair<Type*, std::string>> members;
public:
    DefineStruct(std::string ns, std::vector<std::pair<Type*, std::string>> m);
    ~DefineStruct() {
        for(auto member : members){
            delete member.first;
        }
    }   
    void codegen(llvm::IRBuilder<>& builder) override;
};

class StructDecl : public Statement{
    std::string nameStruct;
    std::string varStructName;
    std::vector<Expr*>  membersExpr;
public:
    StructDecl(std::string ns, std::string vrs, std::vector<Expr*> me);
    ~StructDecl(){
        for(auto memberExpr : membersExpr){
            delete memberExpr;
        }
    }
    void codegen(llvm::IRBuilder<>& builder) override;
};

class VarStructUpdt : public Statement{
    std::string nameVar;
    std::string NameMember;
    Expr* value;
public:
    VarStructUpdt(std::string nv, std::string nm, Expr* v);
    ~VarStructUpdt(){};
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
