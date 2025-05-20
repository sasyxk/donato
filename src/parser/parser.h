#pragma once
#include <string>
#include <vector>
#include "tokenizer.h"
#include "statement.h"
#include "type.h"

extern std::vector<StructType* > symbolStructsType;

class Parser {
public:
    Parser(Tokenizer& t, std::string lastFuncion = "");
    Statement* parseCode();
    bool hasMoreTokens();
private:
    Token currentToken;
    std::string lastFuncion;
    bool isInsideClass = false;
    Tokenizer& tokenizer;

    Statement* parseStm();
    Expr* parse();
    Expr* parseExpr();
    Expr* parseTerm();
    Expr* parseFactor();
    Expr* parseNum(std::string val);
    void eat(TokenType expected);
    std::string parseVar(std::string valueVar);
    Type* parseType(std::string stringType, bool isReference = false);
};