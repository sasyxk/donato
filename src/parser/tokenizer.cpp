#include "tokenizer.h"
#include <stdexcept>
#include <cctype>

Tokenizer::Tokenizer(const std::string& s) : input(s), pos(0) {}

Token Tokenizer::nextToken() {
    while (pos < input.size() && isspace(input[pos])) pos++;
    if (pos >= input.size()) return Token(END);

    char c = input[pos];
    if (isdigit(c) || (c == '.' && isdigit(input[pos + 1])) || (c == '-' && (isdigit(input[pos + 1]) || input[pos + 1] == '.'))) {
        size_t start = pos;
        if(input[pos] == '-') pos++;
        while (pos < input.size() && (isdigit(input[pos]) || input[pos] == '.')) pos++;
        
        // Check scientific notation
        if (pos < input.size() && (input[pos] == 'e' || input[pos] == 'E')) {
            pos++;
            if (pos < input.size() && (input[pos] == '+' || input[pos] == '-')) pos++;

            while (pos < input.size() && isdigit(input[pos])) pos++;
        }
        return Token(NUM, input.substr(start, pos - start));
    }
    if (isalpha(c)) {
        size_t start = pos;
        while (pos < input.size() && isalnum(input[pos])) pos++;
        std::string word = input.substr(start, pos - start);
        if (isupper(word[0])) return Token(UPPERNAME, word);
        if (word == "if") return Token(IF, word);
        if (word == "then") return Token(THEN, word);
        if (word == "else") return Token(ELSE, word);
        if (word == "let") return Token(LET, word);
        if (word == "in") return Token(IN, word);
        if (word == "while") return Token(WHILE, word);
        if (word == "this") return Token(THIS, word);
        if (word == "return") return Token(RETURN, word);
        if (word == "struct") return Token(STRUCT, word);
        if (word == "class") return Token(CLASS, word); 
        if (word == "private") return Token(PRIVATE, word);
        if (word == "public") return Token(PUBLIC, word);
        if (word == "function") return Token(FUNCTION, word);
        if (word == "void") return Token(VOID, word);
        if (word == "ref") return Token(REF, word);
        if (word == "delete") return Token(DELETE, word);
        if (word == "new") return Token(NEW, word);
        if (word == "true" || word == "false") return Token(NUM, word);
        if (word == "double" ||
            word == "bool"   ||
            word == "int"    || 
            word == "int8"   ||
            word == "int16"  ||
            word == "int32"  ||
            word == "int64"    
        ) return Token(TYPE, word);
        if (word == "auto") return Token(AUTO, word);
        return Token(VAR, word);
    }
    //if (c == '&') { pos++; return Token(REF, std::string(1, c)); }
    if (c == '.') { pos++; return Token(POINT, std::string(1, c)); }
    if (c == ':') { pos++; return Token(COLON, std::string(1, c)); }
    if (c == '(') { pos++; return Token(LPAREN, std::string(1, c)); }
    if (c == ')') { pos++; return Token(RPAREN, std::string(1, c)); }
    if (c == '{') { pos++; return Token(LBRACE, std::string(1, c)); }
    if (c == '}') { pos++; return Token(RBRACE, std::string(1, c)); }
    if (c == ',') { pos++; return Token(COMMA, std::string(1, c)); }
    if (c == '=' || c == '!' || c == '<' || c == '>') {
        if (pos + 1 < input.size()) {
            char next_c = input[pos + 1];
            // Check compound operators
            if ((c == '=' && next_c == '=') || 
                (c == '!' && next_c == '=') || 
                (c == '<' && next_c == '=') || 
                (c == '>' && next_c == '=')) {
                std::string op = std::string(1, c) + std::string(1, next_c);
                pos += 2;
                return Token(CONDOP, op);
            }
        }
        // Single operator management
        if (c == '=') { pos++; return Token(EQ); }
        if (c == '<' || c == '>') {
            pos++;
            return Token(CONDOP, std::string(1, c));
        }
    }

    // Comment operator management /* ... */
    if (c == '/') {
        if (pos + 1 < input.size() && input[pos + 1] == '*') {
            pos += 2;
            size_t start = pos;
            while (pos + 1 < input.size() && !(input[pos] == '*' && input[pos + 1] == '/')) {
                pos++;
            }
            std::string commentContent = input.substr(start, pos - start);
            if (pos + 1 < input.size()) {
                pos += 2;
            }
            return Token(COMMENT, commentContent);
        }
        if (pos + 1 < input.size() && input[pos + 1] == '/') {
            pos += 2;
            size_t start = pos;
            while (pos < input.size() && input[pos] != '\n') {
                pos++;
            }
            std::string commentContent = input.substr(start, pos - start);
            return Token(COMMENT, commentContent);
        }
    }

    // Mathematical operator management
    if (c == '+' || c == '-' || c == '*' || c == '/') {
        pos++;
        return Token(OP, std::string(1, c));
    }

    if (c == ';') { pos++; return Token(ENDEXPR, std::string(1, c)); }

    throw std::runtime_error("Unknown token: " + std::string(1, c));
}

std::string Tokenizer::getPos() {
    size_t row = 1;
    size_t col = 1;
    size_t posStart = 0;
    std::string position = "";
    for (const auto& c : input) {
        if(++posStart == pos){
            position += "[" + std::to_string(row) + ", " + std::to_string(col) +  "]" ;
            return position;
        }
        col++;
        if(c == '\n'){
            col = 1;
            row++;
        }
    }
    return "[Precise location in file not identified]";
}
