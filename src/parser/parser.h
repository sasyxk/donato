#pragma once
#include <string>
#include <vector>
#include "tokenizer.h"
#include "statement.h"
#include "type.h"

extern std::vector<StructType* > symbolStructsType;

class Parser {
    Tokenizer& tokenizer;
    Token currentToken;
    std::string lastFuncion;
public:
    Parser(Tokenizer& t, std::string lastFuncion = "");
    Statement* parseCode();
    Statement* parseStm();
    Expr* parse();
    Expr* parseExpr();
    Expr* parseTerm();
    Expr* parseFactor();
    Expr* parseNum(std::string val);
    Type* parseType(std::string stringType, bool isReference = false);
    void eat(TokenType expected);
    bool hasMoreTokens();
};