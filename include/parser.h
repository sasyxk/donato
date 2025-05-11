#pragma once
#include <string>
#include <vector>
#include "ast.h"
#include "type.h"

extern std::vector<StructType* > symbolStructsType;

enum TokenType { DVAR, NUM, VAR, OP, IF, THEN, ELSE, LET, IN, COMMA,
                 LPAREN, RPAREN, EQ, END, ENDEXPR, RETURN,LBRACE ,RBRACE,
                 CONDOP, WHILE, COMMENT, FUNCTION, TYPE, AUTO, STRUCT};

struct Token {
    TokenType type;
    std::string value;
    Token(TokenType t, const std::string& v = "") : type(t), value(v) {}
};

class Tokenizer {
    std::string input;
    size_t pos = 0;
public:
    Tokenizer(const std::string& s);
    Token nextToken();
};

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
    Type* parseType(std::string stringType);
    void eat(TokenType expected);
    bool hasMoreTokens();
};