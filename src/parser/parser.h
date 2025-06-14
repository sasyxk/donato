#pragma once
#include <string>
#include <vector>
#include "tokenizer.h"
#include "statement.h"
#include "type.h"
#include "type_manager.h"

extern std::vector<StructType* > symbolStructsType;

class Parser {
    Token currentToken;
    std::string lastFuncion;
    bool isInsideClass = false;
    std::string nameOfClass = "";
    Tokenizer& tokenizer;
public:
    Parser(Tokenizer& t, std::string lastFuncion = "");
    Statement* parseCode();
    bool hasMoreTokens();
private:
    Statement* parseStm();
    Expr* parse();
    Expr* parseExpr();
    Expr* parseTerm();
    Expr* parseFactor();
    Expr* parseNum(std::string val);
    void eat(TokenType expected);
    std::string parseVar(std::string valueVar);
    TypeInfo parseType(std::string stringType, bool isReference = false);
    Type* parseBaseType(std::string stringType);
    Type* wrapWithPointers(Type* baseType);
    void errorFunction();
    std::vector<std::string> parseClassFunctionNames();
};