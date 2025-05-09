#include "parser.h"
#include <stdexcept>
#include <iostream>

Tokenizer::Tokenizer(const std::string& s) : input(s), pos(0) {}

Token Tokenizer::nextToken() {
    while (pos < input.size() && isspace(input[pos])) pos++;
    if (pos >= input.size()) return Token(END);

    char c = input[pos];
    if (isdigit(c) || c == '.' || (c == '-' && (isdigit(input[pos + 1]) || input[pos + 1] == '.'))) {
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
        if (word == "if") return Token(IF, word);
        if (word == "then") return Token(THEN, word);
        if (word == "else") return Token(ELSE, word);
        if (word == "let") return Token(LET, word);
        if (word == "in") return Token(IN, word);
        if (word == "var") return Token(DVAR, word);
        if (word == "while") return Token(WHILE, word);
        if (word == "return") return Token(RETURN, word);
        if (word == "function") return Token(FUNCTION, word);
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

    if (c == ';') { pos++; return Token(ENDEXPR, std::string(1, c)); }

    throw std::runtime_error("Unknown token: " + std::string(1, c));
}

Parser::Parser(Tokenizer& t, std::string lf) : tokenizer(t), currentToken(t.nextToken()) {}

void Parser::eat(TokenType expected) {
    if (currentToken.type != expected)
        throw std::runtime_error("Unexpected Token: '" + currentToken.value + "'");
    currentToken = tokenizer.nextToken();
}

bool Parser::hasMoreTokens(){
    if (currentToken.type != END) {
        return true;
    };
    return false;
}

Statement* Parser::parseCode(){
    Statement* stm = parseStm();
    return stm;
}

Statement* Parser::parseStm(){
    if (currentToken.type == TYPE || currentToken.type == AUTO) { 
        Type* typeVar = parseType(currentToken.value);
        typeVar == nullptr ? eat(AUTO) : eat(TYPE);
        std::string var = currentToken.value;
        eat(VAR);
        eat(EQ);
        Expr* value = parse();
        return new VarDecl(var, typeVar, value);
    }
    if(currentToken.type == RETURN) {
        eat(RETURN);
        Expr* value = parse();
        return new Return(value, lastFuncion);
    }
    if(currentToken.type == FUNCTION) {
        eat(FUNCTION);
        Type* typeFunc = parseType(currentToken.value);
        eat(TYPE);
        std::string nameFunc = currentToken.value;
        lastFuncion = nameFunc;
        eat(VAR);
        eat(LPAREN);
        std::vector<std::pair<Type*, std::string>> parameters; // (type name),
        do {
            if(currentToken.type == RPAREN){break;}
            Type* type = parseType(currentToken.value);
            eat(TYPE);
            std::string param = currentToken.value;
            eat(VAR);
            parameters.emplace_back(type, param);
        } while (currentToken.type == COMMA && (eat(COMMA), true));
        eat(RPAREN);
        eat(LBRACE);
        std::vector<Statement*> functionBodyStatemets;
        do {
            if(currentToken.type == FUNCTION){
                throw std::runtime_error("Unexpected Statement token Function inside a Function");
            }
            functionBodyStatemets.push_back(parseStm());
        } while (currentToken.type != RBRACE); 
        eat(RBRACE);
        lastFuncion = "";
        return new Function(typeFunc, nameFunc, parameters, functionBodyStatemets);
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
        std::vector<Statement*> elseStd;
        std::vector<Statement*> thenStd;
        do {
            if(currentToken.type == FUNCTION){
                throw std::runtime_error("Unexpected Statement token Function inside a IF");
            }
            thenStd.push_back(parseStm());
        } while (currentToken.type != RBRACE);
        eat(RBRACE);
        if(currentToken.type == ELSE){
            eat(ELSE);
            eat(LBRACE);
            do {
                if(currentToken.type == FUNCTION){
                    throw std::runtime_error("Unexpected Statement token Function inside a ELSE");
                }
                elseStd.push_back(parseStm());
            } while (currentToken.type != RBRACE);
            eat(RBRACE);
        }
        return new IfStm(condLeft,thenStd,elseStd);
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
        std::vector<Statement*> whileStd;
        do {
            if(currentToken.type == FUNCTION){
                throw std::runtime_error("Unexpected Statement token Function inside a ELSE");
            }
            whileStd.push_back(parseStm());
        } while (currentToken.type != RBRACE);
        eat(RBRACE);
        return new WhileStm(condLeft,whileStd);
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
        return new VarUpdt(var, value);
    }
    throw std::runtime_error("Unexpected Statement Token: '" + currentToken.value + "'");
}

Expr* Parser::parse() {
    Expr* expr = parseExpr();
    if (currentToken.type != ENDEXPR) {
        throw std::runtime_error("Unexpected tokens '"+currentToken.value+"' after expression");
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
        std::string val = currentToken.value;
        eat(NUM);
        return parseNum(val);
    }
    if (currentToken.type == VAR) {
        std::string name = currentToken.value;
        eat(VAR);
        if(currentToken.type == LPAREN){
            eat(LPAREN);
            std::vector<Expr*> args;
            do {
                if(currentToken.type == RPAREN){break;}
                Expr* arg = parseExpr();
                args.push_back(arg);
            } while (currentToken.type == COMMA && (eat(COMMA), true));
            eat(RPAREN);
            return new CallFunc(name, args);
        }
        return new Var(name);
    }
    throw std::runtime_error("Unexpected factor");
}

//-----------------------------------------------------------------------------------
Expr* Parser::parseNum(std::string val){
    Type* type;
    if (val.find('.') != std::string::npos) {
        try {
            size_t pos;
            double num = std::stod(val, &pos);
            if (pos != val.size()) throw std::runtime_error("Invalid characters");
            type = new DoubleType();
            return new DoubleNum(num, type);
        } catch (const std::exception& e) {
            throw std::runtime_error("Invalid floating-point string value: " + val + " (" + e.what() + ")");
        }
    }

    if (val == "true" || val == "false") {
        bool boolVal = (val == "true");
        type = new BoolType();
        return new BoolNum(boolVal, type);
    }

    // INTEGER (32-bit)
    try {
        std::int64_t num = std::stoll(val); 
        if(num > 20){type = new SignedIntType(16);}
        else{
            type = new SignedIntType(8);
        }
        return new SignedIntNum(num, type);
    } catch (...) {}

    throw std::runtime_error("No valid type associated with value: '" + val + "'");
}

Type* Parser::parseType(std::string stringType){
    if (stringType == "double") {
        return new DoubleType();
    } 
    else if (stringType == "bool") {
        return new BoolType();
    }
    else if(stringType == "auto") {
        return nullptr;
    }
    else if(stringType == "int8"){
        return new SignedIntType(8);
    }
    else if(stringType == "int16"){
        return new SignedIntType(16);
    }
    else if(stringType == "int32" || stringType == "int"){
        return new SignedIntType(32);
    }
    else if(stringType == "int64"){
        return new SignedIntType(64);
    }
    throw std::runtime_error("Type '" + stringType + "' does not exist.");
}
