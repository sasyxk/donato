#include "parser.h"
#include <stdexcept>
#include <iostream>

Parser::Parser(Tokenizer& t, std::string lf) : tokenizer(t), currentToken(t.nextToken()) {}

void Parser::eat(TokenType expected) {
    if (currentToken.type != expected)
        throw std::runtime_error("Unexpected Token: '" + currentToken.value + "' with type '"+ std::to_string(currentToken.type)+ "'");
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
    if(currentToken.type == CLASS){
        eat(CLASS);
        isInsideClass = true;
        std::string nameClass = currentToken.value;
        eat(UPPERNAME);
        eat(LBRACE);
        bool havePrivateMembers = false;
        if (currentToken.type == PRIVATE ){
            eat(PRIVATE);
            havePrivateMembers = true;
            eat(COLON);
        }
        std::vector<std::pair<Type*, std::string>> privateMembers;
        if(currentToken.type != PUBLIC || havePrivateMembers){
            havePrivateMembers = true;
            do {
                Type* type = parseType(currentToken.value);
                std::string privateMember = currentToken.value;
                eat(VAR);
                privateMembers.emplace_back(type, privateMember);
                eat(ENDEXPR);
            } while (currentToken.type == TYPE);
        }
        eat(PUBLIC);
        eat(COLON);
        if(currentToken.value != nameClass){
            throw std::runtime_error(
                "Define the Constructor of class '"+
                nameClass +
                "' with the same name not '"+
                currentToken.value +
                "'."
            );
        }
        eat(UPPERNAME);
        eat(LPAREN);
        std::vector<std::pair<Type*, std::string>> constructorArgs; // (type name),
        do {
            if(currentToken.type == RPAREN){break;}
            bool isReference = false;
            if(currentToken.type == REF) {
                eat(REF);
                isReference = true;
            }
            Type* type = parseType(currentToken.value, isReference);
            //eat(TYPE);
            std::string arg = currentToken.value;
            eat(VAR);
            constructorArgs.emplace_back(type, arg);
        } while (currentToken.type == COMMA && (eat(COMMA), true));
        eat(RPAREN);
        eat(LBRACE);
        std::vector<Statement*> ConstructorBodyStatemets;
        lastFuncion = nameClass;
        do { //todo generalize this error with a errorFunction
            if(currentToken.type == FUNCTION){
                throw std::runtime_error("Unexpected Statement token 'FUNCTION' inside a Function");
            }
            if(currentToken.type == STRUCT){
                throw std::runtime_error("Unexpected Statement token 'STRUCT' inside a Function");
            }
            ConstructorBodyStatemets.push_back(parseStm());
        } while (currentToken.type != RBRACE); 
        eat(RBRACE);
        std::vector<Function*> publicFunctions;
        do{
            if(currentToken.type == RBRACE){break;} // End of the class
            Function* function = dynamic_cast<Function*>(parseStm());
            if (!function) {
                throw std::runtime_error("Expected a function declaration inside the class body.");
            }
            publicFunctions.push_back(function);
        } while(currentToken.type == FUNCTION);

        eat(RBRACE);
        isInsideClass = false;
        return new DefineClass(nameClass,
            privateMembers,
            constructorArgs,
            ConstructorBodyStatemets,
            publicFunctions);
    }
    if(currentToken.type == STRUCT){
        eat(STRUCT);
        std::string nameStruct = currentToken.value;
        eat(UPPERNAME);
        eat(LBRACE);
        std::vector<std::pair<Type*, std::string>> members;
        do {
            Type* type = parseType(currentToken.value);
            //eat(TYPE);
            std::string member = currentToken.value;
            eat(VAR);
            members.emplace_back(type, member);
            eat(ENDEXPR);
        } while (currentToken.type == TYPE);
        eat(RBRACE);
        return new DefineStruct(nameStruct, members);
        return nullptr;
    }
    if (currentToken.type == TYPE || currentToken.type == AUTO) { 
        Type* typeVar = parseType(currentToken.value);
        //typeVar == nullptr ? eat(AUTO) : eat(TYPE);
        std::string var = parseVar(currentToken.value);
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
        //eat(TYPE);
        std::string nameFunc = currentToken.value;
        lastFuncion = nameFunc;
        eat(VAR);
        eat(LPAREN);
        std::vector<std::pair<Type*, std::string>> parameters; // (type name),
        do {
            if(currentToken.type == RPAREN){break;}
            bool isReference = false;
            if(currentToken.type == REF) {
                eat(REF);
                isReference = true;
            }
            Type* type = parseType(currentToken.value, isReference);
            //eat(TYPE);
            std::string param = currentToken.value;
            eat(VAR);
            parameters.emplace_back(type, param);
        } while (currentToken.type == COMMA && (eat(COMMA), true));
        eat(RPAREN);
        eat(LBRACE);
        std::vector<Statement*> functionBodyStatemets;
        do { //todo generalize this error with a errorFunction
            if(currentToken.type == FUNCTION){
                throw std::runtime_error("Unexpected Statement token 'FUNCTION' inside a Function");
            }
            if(currentToken.type == STRUCT){
                throw std::runtime_error("Unexpected Statement token 'STRUCT' inside a Function");
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
    if (currentToken.type == VAR || currentToken.type == THIS) { //Ok not change for class
        std::string var = parseVar(currentToken.value);
        if(currentToken.type == POINT) {
            eat(POINT);
            std::string memberName = currentToken.value;
            eat(VAR);
            eat(EQ);
            Expr* value = parse();
            return new VarStructUpdt(var, memberName, value);
        }
        eat(EQ);
        Expr* value = parse();
        return new VarUpdt(var, value);
    }
    if (currentToken.type == UPPERNAME) {
        std::string nameStruct = currentToken.value;
        eat(UPPERNAME);

        std::string varStructName = currentToken.value;
        eat(VAR);
        eat(EQ);

        eat(LBRACE);
        std::vector<Expr*> membersExpr;
        do{
            Expr* member = parseExpr();
            membersExpr.push_back(member);
        }while(currentToken.type == COMMA && (eat(COMMA), true));
        eat(RBRACE);
        eat(ENDEXPR);
        return new StructDecl(nameStruct, varStructName, membersExpr);
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
    if (currentToken.type == OP &&
        currentToken.value == "-") {
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
    if (currentToken.type == VAR ||
        currentToken.type == THIS) {
        std::string name = parseVar(currentToken.value);
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
        if(currentToken.type == POINT){
            std::vector<std::string> memberChain;
            do {
                eat(POINT);
                std::string memberName = currentToken.value;
                eat(VAR);
                memberChain.push_back(memberName);

            } while(currentToken.type == POINT);
            
            return new StructVar(name, memberChain);
        }
        return new Var(name);
    }
    throw std::runtime_error("Unexpected factor: " + currentToken.value);
}
//-----------------------------------------------------------------------------------
Expr* Parser::parseNum(std::string val){
    Type* type;

    if (val == "true" || val == "false") {
        bool boolVal = (val == "true");
        type = new BoolType();
        return new BoolNum(boolVal, type);
    }

    if (val.find('.') != std::string::npos ||
        val.find('e') != std::string::npos ||
        val.find('E') != std::string::npos) {
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

    try {
        std::int64_t num = std::stoll(val); 
        SignedIntType* type = nullptr;
        type = new SignedIntType(64);
        /*if (num >= INT8_MIN && num <= INT8_MAX) {
            type = new SignedIntType(8);
        } else if (num >= INT16_MIN && num <= INT16_MAX) {
            type = new SignedIntType(16);
        } else if (num >= INT32_MIN && num <= INT32_MAX) {
            type = new SignedIntType(32);
        } else {
            type = new SignedIntType(64);
        }*/
        return new SignedIntNum(num, type);
    } catch (...) {
        throw std::runtime_error("Invalid integer string value: " + val);
    }

    throw std::runtime_error("No valid type associated with value: '" + val + "'");
}

Type* Parser::parseType(std::string stringType, bool isReference){
    if (stringType == "double") {
        eat(TYPE);
        return new DoubleType(isReference);
    } 
    else if (stringType == "bool") {
        eat(TYPE);
        return new BoolType(isReference);
    }
    else if(stringType == "auto") {
        eat(AUTO);
        return nullptr;
    }
    else if(stringType == "int8"){
        eat(TYPE);
        return new SignedIntType(8, isReference);
    }
    else if(stringType == "int16"){
        eat(TYPE);
        return new SignedIntType(16, isReference);
    }
    else if(stringType == "int32"){
        eat(TYPE);
        return new SignedIntType(32, isReference);
    }
    else if(stringType == "int64" || stringType == "int"){
        eat(TYPE);
        return new SignedIntType(64, isReference);
    }
    else if(isupper(stringType[0])){
        for(auto structType : symbolStructsType){
            if(structType->getNameStruct() == stringType){
                eat(UPPERNAME);
                auto* st = structType->clone();
                st->setPointer(isReference);
                return st;
            }
        }
    }
    throw std::runtime_error("Type '" + stringType + "' does not exist.");
}


std::string Parser::parseVar(std::string valueVar){
    if(valueVar == "this" &&  !isInsideClass)
        throw std::runtime_error("Cannot use 'this' outside class");
    valueVar == "this" ? eat(THIS) : eat(VAR);
    return valueVar;
}