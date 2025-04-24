#include "parser.h"
#include <stdexcept>
#include <iostream>

Tokenizer::Tokenizer(const std::string& s) : input(s), pos(0) {}

Token Tokenizer::nextToken() {
    while (pos < input.size() && isspace(input[pos])) pos++;
    if (pos >= input.size()) return Token(END);

    char c = input[pos];
    if (isdigit(c) || c == '.') {
        size_t start = pos;
        while (pos < input.size() && (isdigit(input[pos]) || input[pos] == '.')) pos++;
        return Token(NUM, input.substr(start, pos - start));
    }
    if (isalpha(c)) {
        size_t start = pos;
        while (pos < input.size() && isalnum(input[pos])) pos++;
        std::string word = input.substr(start, pos - start);
        if (word == "if") return Token(IF, word);
        if (word == "then") return Token(THEN, word);
        if (word == "else") return Token(ELSE, word);
        if (word == "let") return Token(LET, word);
        if (word == "in") return Token(IN, word);
        if (word == "var") return Token(DVAR, word);
        if (word == "while") return Token(WHILE, word);
        if (word == "return") return Token(RETURN, word);
        if (word == "function") return Token(FUNCTION, word);
        if (word == "call") return Token(CALL, word);
        return Token(VAR, word);
    }
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
    }

    // Mathematical operator management
    if (c == '+' || c == '-' || c == '*' || c == '/') {
        pos++;
        return Token(OP, std::string(1, c));
    }

    if (c == ';') { pos++; return Token(ENDEXPR, std::string(1, c)); } //to be replaced with a carriage return

    throw std::runtime_error("Unknown token: " + std::string(1, c));
}

Parser::Parser(Tokenizer& t) : tokenizer(t), currentToken(t.nextToken()) {}

void Parser::eat(TokenType expected) {
    if (currentToken.type != expected)
        throw std::runtime_error("Unexpected Token: '" + currentToken.value + "'");
    currentToken = tokenizer.nextToken();
}

Statement* Parser::parseCode(){
    Statement* stm = parseStm();
    if (currentToken.type != END) {
        throw std::runtime_error("Unexpected tokens after Statement");
    };
    return stm;
}

Statement* Parser::parseStm(){
    if (currentToken.type == DVAR) {  // var try = Expr
        eat(DVAR);
        std::string var = currentToken.value;
        eat(VAR);
        eat(EQ);
        Expr* value = parse();
        Statement* next = parseStm();
        return new VarDecl(var, value, next);
    }
    if(currentToken.type == RETURN) {
        eat(RETURN);
        Expr* value = parse();
        return new Return(value);
    }
    if(currentToken.type == FUNCTION) {
        eat(FUNCTION);
        std::string typeFunc = currentToken.value;
        eat(VAR);
        std::string nameFunc = currentToken.value;
        eat(VAR);
        eat(LPAREN);
        std::vector<std::pair<std::string, std::string>> parameters; // (type name),
        do {
            if(currentToken.type == RPAREN){break;}
            std::string type = currentToken.value;
            eat(VAR);
            std::string param = currentToken.value;
            eat(VAR);
            parameters.emplace_back(type, param);
        } while (currentToken.type == COMMA && (eat(COMMA), true));
        eat(RPAREN);
        eat(LBRACE);
        Statement* functionBodyStatemets = parseStm();
        eat(RBRACE);
        Statement* next = parseStm();
        return new Function(typeFunc,nameFunc,parameters,functionBodyStatemets,next);
    }
    if(currentToken.type == IF) {
        eat(IF);
        eat(LPAREN);
        Expr* condLeft = parseExpr();
        if(currentToken.type == CONDOP){
            std::string op = currentToken.value;
            eat(CONDOP);
            Expr* condRight = parseExpr();
            //optional, check 'and' 'or' with while
            condLeft = new BinaryCond(op, condLeft,condRight);
        }
        eat(RPAREN);
        eat(LBRACE);
        Statement* thenStd = parseStm();
        Statement* elseStd = nullptr;
        eat(RBRACE);
        if(currentToken.type == ELSE){
            eat(ELSE);
            eat(LBRACE);
            elseStd = parseStm();
            eat(RBRACE);
        }
        Statement* next = parseStm();
        return new IfStm(condLeft,thenStd,elseStd, next);
    }
    if (currentToken.type == WHILE){
        eat(WHILE);
        eat(LPAREN);
        Expr* condLeft = parseExpr();
        if(currentToken.type == CONDOP){
            std::string op = currentToken.value;
            eat(CONDOP);
            Expr* condRight = parseExpr();
            condLeft = new BinaryCond(op, condLeft,condRight);
        }
        eat(RPAREN);
        eat(LBRACE);
        Statement* whileStd = parseStm();
        eat(RBRACE);
        Statement* next = parseStm();
        return new WhileStm(condLeft,whileStd,next);
    }
    if(currentToken.type == COMMENT){
        eat(COMMENT);
        Statement* next = parseStm();
        return next;
    }
    if (currentToken.type == VAR) {
        std::string var = currentToken.value;
        eat(VAR);
        eat(EQ);
        Expr* value = parse();
        Statement* next = parseStm();
        return new VarUpdt(var, value, next);
    }
    return nullptr;
}

Expr* Parser::parse() {
    Expr* expr = parseExpr();
    if (currentToken.type != ENDEXPR) {
        throw std::runtime_error("Unexpected tokens after expression");
    }
    eat(ENDEXPR);
    return expr;
}

Expr* Parser::parseExpr() {
    Expr* left = parseTerm();
    while (currentToken.type == OP && (currentToken.value == "+" || currentToken.value == "-")) {
        std::string op = currentToken.value;
        eat(OP);
        Expr* right = parseTerm();
        left = new BinaryOp(op, left, right);
    }
    return left;
}

Expr* Parser::parseTerm() {
    Expr* left = parseFactor();
    while (currentToken.type == OP && (currentToken.value == "*" || currentToken.value == "/")) {
        std::string op = currentToken.value;
        eat(OP);
        Expr* right = parseFactor();
        left = new BinaryOp(op, left, right);
    }
    return left;
}

Expr* Parser::parseFactor() {
    if (currentToken.type == OP && currentToken.value == "-") {
        eat(OP);
        Expr* x = parseFactor();
        return new UnaryOp("-", x);
    }
    if (currentToken.type == LPAREN) {
        eat(LPAREN);
        Expr* x = parseExpr();
        eat(RPAREN);
        return x;
    }
    if (currentToken.type == IF) {
        eat(IF);
        //Expr* cond = parseExpr();
        bool isParent = false;
        if(currentToken.type == LPAREN) {
            eat(LPAREN);
            isParent = true;
        }
        Expr* condLeft = parseExpr();
        if(currentToken.type == CONDOP){
            std::string op = currentToken.value;
            eat(CONDOP);
            Expr* condRight = parseExpr();
            //optional, check 'and' 'or' with while
            condLeft = new BinaryCond(op, condLeft,condRight);
        }
        if(isParent) eat(RPAREN);
        eat(THEN);
        Expr* thenExpr = parseExpr();
        eat(ELSE);
        Expr* elseExpr = parseExpr();
        return new IfOp(condLeft, thenExpr, elseExpr);
    }
    if (currentToken.type == CALL) {
        eat(CALL);
        std::string funcName = currentToken.value;
        eat(VAR);
        eat(LPAREN);
        std::vector<Expr*> args;
        do {
            if(currentToken.type == RPAREN){break;}
            Expr* arg = parseExpr();
            args.push_back(arg);
        } while (currentToken.type == COMMA && (eat(COMMA), true));
        eat(RPAREN);
        return new CallFunc(funcName, args); 
    }
    if (currentToken.type == LET) {
        eat(LET);
        std::vector<std::pair<std::string, Expr*>> bindings;
        do {
            std::string var = currentToken.value;
            eat(VAR);
            eat(EQ);
            Expr* val = parseExpr();
            bindings.emplace_back(var, val);
        } while (currentToken.type == COMMA && (eat(COMMA), true));
        eat(IN);
        Expr* body = parseExpr();
        return new LetOp(bindings, body);
    }
    if (currentToken.type == NUM) {
        double val = std::stod(currentToken.value);
        eat(NUM);
        return new Num(val);
    }
    if (currentToken.type == VAR) {
        std::string name = currentToken.value;
        eat(VAR);
        return new Var(name);
    }
    throw std::runtime_error("Unexpected factor");
}