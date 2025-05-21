#pragma once
#include <string>
#include <vector>

enum TokenType { DVAR, NUM, VAR, OP, IF, THEN, ELSE, LET, IN, COMMA,
                 LPAREN, RPAREN, EQ, END, ENDEXPR, RETURN,LBRACE ,RBRACE,
                 CONDOP, WHILE, COMMENT, FUNCTION, TYPE, AUTO, STRUCT,
                 UPPERNAME, POINT, REF, CLASS, PUBLIC, PRIVATE, COLON, 
                 THIS, VOID};

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
    std::string getPos();
};