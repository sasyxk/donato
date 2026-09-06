#pragma once
#include <stdexcept>
#include <string>
#include <vector>

enum TokenType { DVAR, NUM, VAR, OP, IF, THEN, ELSE, LET, IN, COMMA,
                 LPAREN, RPAREN, EQ, END, ENDEXPR, RETURN,LBRACE ,RBRACE,
                 CONDOP, WHILE, COMMENT, FUNCTION, TYPE, AUTO, STRUCT,
                 UPPERNAME, POINT, REF, CLASS, PUBLIC, PRIVATE, COLON, 
                 THIS, VOID, DELETE, NEW, MEM, PRINT, NULLPTR};

struct Token {
    TokenType type;
    std::string value;
    Token(TokenType t, const std::string& v = "") : type(t), value(v) {}
};

class LexicalError : public std::runtime_error {
public:
    LexicalError(const std::string& message, const std::string& position)
        : std::runtime_error(message + " " + position) {}
};

class Tokenizer {
    std::string input;
    size_t pos = 0;
public:
    Tokenizer(const std::string& s);
    Token nextToken();
    // Explicit positions are zero-based byte offsets; diagnostics are one-based.
    std::string getPos(size_t position = std::string::npos) const;
    void setPosition(size_t pos) {
        this->pos = pos;
    }
    size_t getCurrentPos(){
        return this->pos;
    }
};