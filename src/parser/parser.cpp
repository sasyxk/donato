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

void Parser::errorFunction(){
    if(currentToken.type == FUNCTION){
        throw std::runtime_error("Unexpected Statement token 'FUNCTION'");
    }
    if(currentToken.type == STRUCT){
        throw std::runtime_error("Unexpected Statement token 'STRUCT'");
    }
    if(currentToken.type == CLASS){
        throw std::runtime_error("Unexpected Statement token 'CLASS'");
    }
}

Statement* Parser::parseCode(){
    Statement* stm = parseStm();
    return stm;
}

std::vector<std::string> Parser::parseClassFunctionNames(){
    std::vector<std::string> nameFunctions;
    eat(UPPERNAME);
    eat(LPAREN);
    while(currentToken.type != RPAREN){
        eat(currentToken.type);
    }
    eat(RPAREN);
    int braceCount = 1;
    eat(LBRACE);
    while (braceCount > 0) {
        if (currentToken.type == LBRACE) {
            braceCount++;
        } else if (currentToken.type == RBRACE) {
            braceCount--;
        }
        eat(currentToken.type);
    }
    while(currentToken.type == FUNCTION){
        eat(FUNCTION);
        eat(currentToken.type);
        while(currentToken.value == "*"){
            eat(currentToken.type);
        }
        nameFunctions.push_back(currentToken.value);
        eat(VAR);
        eat(LPAREN);
        while(currentToken.type != RPAREN){
            eat(currentToken.type);
        }
        eat(RPAREN);
        eat(LBRACE);
        int braceCount = 1;
        while (braceCount > 0) {
            if (currentToken.type == LBRACE) {
                braceCount++;
            } else if (currentToken.type == RBRACE) {
                braceCount--;
            }
            eat(currentToken.type);
        }
    }
    return nameFunctions;
}

Statement* Parser::parseStm(){
    if(currentToken.type == CLASS){
        eat(CLASS);
        isInsideClass = true;
        std::string nameClass = currentToken.value;
        nameOfClass = nameClass;
        eat(UPPERNAME);
        eat(LBRACE);
        bool havePrivateMembers = false;
        if (currentToken.type == PRIVATE ){
            eat(PRIVATE);
            havePrivateMembers = true;
            eat(COLON);
        }
        std::vector<std::pair<Type*, std::string>> privateMembers;
        ClassType* classType = TypeManager::instance().createClassType(nameClass);
        symbolClassType.push_back(classType);

        if(currentToken.type != PUBLIC || havePrivateMembers){
            havePrivateMembers = true;
            do {
                Type* type;
                TypeInfo typeInfo = parseType(currentToken.value);
                type = typeInfo.type;
                if(dynamic_cast<ClassType*>(type) != nullptr){
                    throw std::runtime_error("You cannot declare other classes as members of the class without a pointer");
                }
                if(dynamic_cast<StructType*>(type) != nullptr){
                    throw std::runtime_error("You cannot declare other structs without pointers as class members");
                }
                std::string privateMember = currentToken.value;
                eat(VAR);
                privateMembers.emplace_back(type, privateMember);
                eat(ENDEXPR);
            } while (currentToken.type == TYPE || currentToken.type == UPPERNAME);
        }
        eat(PUBLIC);
        eat(COLON);
        classType->setMembers(privateMembers);

        // Load current position
        size_t currPos = tokenizer.getCurrentPos();
        Token currentToken_ = currentToken;

        //parse names
        auto nameFunctions = parseClassFunctionNames();
        classType->setNameFunctions(nameFunctions);

        // Restore position
        tokenizer.setPosition(currPos);
        currentToken = currentToken_;

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
        std::vector<std::pair<TypeInfo, std::string>> constructorArgs; // (type name),
        do {
            if(currentToken.type == RPAREN){break;}
            bool isReference = false;
            if(currentToken.type == REF) {
                eat(REF);
                isReference = true;
            }
            TypeInfo typeInfo = parseType(currentToken.value, isReference);
            //eat(TYPE);
            std::string arg = currentToken.value;
            eat(VAR);
            constructorArgs.emplace_back(typeInfo, arg);
        } while (currentToken.type == COMMA && (eat(COMMA), true));
        eat(RPAREN);
        eat(LBRACE);
        std::vector<Statement*> ConstructorBodyStatemets;
        lastFuncion = nameClass;
        do { 
            errorFunction();
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
        nameOfClass = "";

        return new DefineClass(nameClass,
            privateMembers,
            constructorArgs,
            ConstructorBodyStatemets,
            publicFunctions,
            classType
        );
    }
    if(currentToken.type == STRUCT){
        eat(STRUCT);
        std::string nameStruct = currentToken.value;
        StructType* structType = TypeManager::instance().createStructType(nameStruct);
        symbolStructsType.push_back(structType);
        eat(UPPERNAME);
        eat(LBRACE);
        std::vector<std::pair<Type*, std::string>> members;
        do {
            Type* type;
            TypeInfo typeInfo = parseType(currentToken.value);
            type = typeInfo.type;
            if(dynamic_cast<ClassType*>(type) != nullptr){
                throw std::runtime_error("You cannot declare other classes as members of the class without a pointer");
            }
            if(dynamic_cast<StructType*>(type) != nullptr){
                throw std::runtime_error("You cannot declare other structs without pointers as class members");
            }
            std::string member = currentToken.value;
            eat(VAR);
            members.emplace_back(type, member);
            eat(ENDEXPR);
        } while (currentToken.type == TYPE || currentToken.type  == UPPERNAME);
        eat(RBRACE);
        structType->setMembers(members);

        return new DefineStruct(structType);
    }
    if (currentToken.type == TYPE  || currentToken.type == UPPERNAME) { 
        TypeInfo typeVar = parseType(currentToken.value);
        std::string var = parseVar(currentToken.value);
        eat(EQ);
        Expr* value = parse();
        return new VarDecl(var, typeVar.type, value);
    }
    if(currentToken.type == AUTO){
        eat(AUTO);
        std::string var = parseVar(currentToken.value);
        eat(EQ);
        Expr* value = parse();
        return new VarDecl(var, nullptr, value);
    }
    if(currentToken.type == RETURN) {
        eat(RETURN);
        if(currentToken.type == ENDEXPR){
            eat(ENDEXPR);
            return new ReturnVoid(lastFuncion, nameOfClass);
        }
        Expr* value = parse();
        return new Return(value, lastFuncion, nameOfClass);
    }
    if(currentToken.type == FUNCTION) {
        eat(FUNCTION);
        bool isReference = false;
        if(currentToken.type == REF) {
            eat(REF);
            isReference = true;
        }
        TypeInfo typeFunc = parseType(currentToken.value, isReference);
        std::string nameFunc = currentToken.value;
        lastFuncion = nameFunc;
        eat(VAR);
        eat(LPAREN);
        std::vector<std::pair<TypeInfo, std::string>> parameters; // (type name),
        do {
            if(currentToken.type == RPAREN){break;}
            bool isReference = false;
            if(currentToken.type == REF) {
                eat(REF);
                isReference = true;
            }
            TypeInfo type = parseType(currentToken.value, isReference);
            //eat(TYPE);
            std::string param = currentToken.value;
            eat(VAR);
            parameters.emplace_back(type, param);
        } while (currentToken.type == COMMA && (eat(COMMA), true));
        eat(RPAREN);
        eat(LBRACE);
        std::vector<Statement*> functionBodyStatemets;
        do {
            errorFunction();
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
            errorFunction();
            thenStd.push_back(parseStm());
        } while (currentToken.type != RBRACE);
        eat(RBRACE);
        if(currentToken.type == ELSE){
            eat(ELSE);
            eat(LBRACE);
            do {
                errorFunction();
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
            errorFunction();
            whileStd.push_back(parseStm());
        } while (currentToken.type != RBRACE);
        eat(RBRACE);
        return new WhileStm(condLeft,whileStd);
    }
    if (currentToken.type == VAR ||
        currentToken.type == THIS) { // To the left of the class called with this it behaves exactly like a struct, we can leave it like that
        std::string var = parseVar(currentToken.value);
        if(currentToken.type == LPAREN){ //VAR is function name CallFuncStatement
            eat(LPAREN);
            std::vector<Expr*> args;
            do {
                if(currentToken.type == RPAREN){break;}
                Expr* arg = parseExpr();
                args.push_back(arg);
            } while (currentToken.type == COMMA && (eat(COMMA), true));
            eat(RPAREN);
            eat(ENDEXPR);
            return new CallFuncStatement(var, args);
        }
        if(currentToken.type == POINT) { //todo generalise the number of point
            std::vector<std::string> memberChain;
            do {
                eat(POINT);
                std::string memberName = currentToken.value;
                eat(VAR);
                memberChain.push_back(memberName);

            } while(currentToken.type == POINT);
            std::vector<Expr*> args;
            if(currentToken.type == LPAREN){ //Function Class call VOID
                eat(LPAREN);
                do{
                    if(currentToken.type == RPAREN){break;}
                    Expr* arg = parseExpr();
                    args.push_back(arg);
                } while (currentToken.type == COMMA && (eat(COMMA), true));
                eat(RPAREN);
                eat(ENDEXPR);
                //throw std::runtime_error("Not yet implemented");
                return new ClassCallVoidFunc(var, memberChain, var == "this" ? nameOfClass : "", args);
            }
            eat(EQ);
            Expr* value = parse();
            return new VarStructUpdt(var, memberChain, value);
        }
        eat(EQ);
        Expr* value = parse();
        return new VarUpdt(var, value);
    }
    if(currentToken.type == REF){
        eat(REF);
        TypeInfo typeVar = parseType(currentToken.value, true);
        //typeVar == nullptr ? eat(AUTO) : eat(TYPE);
        std::string var = parseVar(currentToken.value);
        eat(EQ);
        Expr* value = parse();
        return new RefDecl(var, typeVar.type, value);
    }
    if(currentToken.type == DELETE){
        eat(DELETE);
        Expr* value = parse();
        return new DeleteVar(value);
    }
    if(currentToken.type == PRINT){
        eat(PRINT);
        eat(LPAREN);
        Expr* x = parseExpr(); 
        eat(RPAREN);
        eat(ENDEXPR);
        return new PrintVar(x);
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
    if (currentToken.type == OP) {
        if(currentToken.value == "-"){
            eat(OP);
            Expr* x = parseFactor();
            return new UnaryOp("-", x);
        }
        if(currentToken.value == "*"){
            eat(OP);
            Expr* x = parseFactor();
            return new DereferenceOp(x);
        }
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
        if(currentToken.type == LPAREN){ //VAR is function name CallFunc
            eat(LPAREN);
            std::vector<Expr*> args;
            do {
                if(currentToken.type == RPAREN){break;}
                Expr* arg = parseExpr();
                args.push_back(arg);
            } while (currentToken.type == COMMA && (eat(COMMA), true));
            eat(RPAREN);
            return new CallFunc(name, nameOfClass, args);
        }
        if(currentToken.type == POINT){ //Var is struct
            std::vector<std::string> memberChain;
            do {
                eat(POINT);
                std::string memberName = currentToken.value;
                eat(VAR);
                memberChain.push_back(memberName);

            } while(currentToken.type == POINT);
            std::vector<Expr*> args;
            if(currentToken.type == LPAREN){ //Function Class call
                eat(LPAREN);
                do{
                    if(currentToken.type == RPAREN){break;}
                    Expr* arg = parseExpr();
                    args.push_back(arg);
                } while (currentToken.type == COMMA && (eat(COMMA), true));
                eat(RPAREN);
                return new ClassCallFunc(name, memberChain, name == "this" ? nameOfClass : "", args);
            }
            return new StructVar(name, memberChain);
        }
        return new Var(name); // Just Var
    }
    if(currentToken.type == NEW){
        eat(NEW);
        std::string className = currentToken.value;
        eat(UPPERNAME);
        eat(LPAREN);
        std::vector<Expr*> args;
        do {
            if(currentToken.type == RPAREN){break;}
            Expr* arg = parseExpr();
            args.push_back(arg);
        } while (currentToken.type == COMMA && (eat(COMMA), true));
        eat(RPAREN);
        return new NewOp(className, args);
    }
    if(currentToken.type == MEM){
        eat(MEM);
        Expr* x = parseExpr();
        return new AddressOp(x);
    }
    if(currentToken.type == NULLPTR){
        eat(NULLPTR);
        if(currentToken.value != "<"){
            throw std::runtime_error("Unexpected factor: " + currentToken.value);
        }
        eat(CONDOP);
        TypeInfo typeInfo = parseType(currentToken.value);
        if(currentToken.value != ">"){
            throw std::runtime_error("Unexpected factor: " + currentToken.value);
        }
        eat(CONDOP);
        return new NullPtr(typeInfo.type);

    }
    throw std::runtime_error("Unexpected factor: " + currentToken.value);
}
//-----------------------------------------------------------------------------------
Expr* Parser::parseNum(std::string val){
    Type* type;

    if (val == "true" || val == "false") {
        bool boolVal = (val == "true");
        type = TypeManager::instance().getBoolType();
        return new BoolNum(boolVal, type);
    }

    if (val.find('.') != std::string::npos ||
        val.find('e') != std::string::npos ||
        val.find('E') != std::string::npos) {
        try {
            size_t pos;
            double num = std::stod(val, &pos);
            if (pos != val.size()) throw std::runtime_error("Invalid characters");
            type = TypeManager::instance().getDoubleType();
            return new DoubleNum(num, type);
        } catch (const std::exception& e) {
            throw std::runtime_error("Invalid floating-point string value: " + val + " (" + e.what() + ")");
        }
    }

    try {
        std::int64_t num = std::stoll(val); 
        SignedIntType* type = nullptr;
        type = TypeManager::instance().getSignedIntType(64);
        return new SignedIntNum(num, type);
    } catch (...) {
        throw std::runtime_error("Invalid integer string value: " + val);
    }

    throw std::runtime_error("No valid type associated with value: '" + val + "'");
}

Type* Parser::parseBaseType(std::string stringType){
    if (stringType == "double") {
        eat(TYPE);
        return TypeManager::instance().getDoubleType();
    } 
    else if (stringType == "bool") {
        eat(TYPE);
        return TypeManager::instance().getBoolType();
    }
    else if(stringType == "int8"){
        eat(TYPE);
        return TypeManager::instance().getSignedIntType(8);
    }
    else if(stringType == "int16"){
        eat(TYPE);
        return TypeManager::instance().getSignedIntType(16);
    }
    else if(stringType == "int32"){
        eat(TYPE);
        return TypeManager::instance().getSignedIntType(32);
    }
    else if(stringType == "int64" || stringType == "int"){
        eat(TYPE);
        return TypeManager::instance().getSignedIntType(64);
    }
    if(stringType == "void"){
        eat(VOID);
        return TypeManager::instance().getVoidType();
    }
    else if(isupper(stringType[0])){
        for(auto structType : symbolStructsType){
            if(structType->getNameStruct() == stringType){
                eat(UPPERNAME);
                auto* st = structType;
                return st;
            }
        }
    }
    if(isupper(stringType[0])){
       for(auto classType : symbolClassType){
            if(classType->getNameClass() == stringType){
                eat(UPPERNAME);
                auto* st = classType;
                return st;
            }
        } 
    }
    throw std::runtime_error("Type '" + stringType + "' does not exist.");
}

Type* Parser::wrapWithPointers(Type* baseType) {
    Type* currentType = baseType;

    while (currentToken.value == "*") {
        eat(OP);
        currentType = TypeManager::instance().getPointerType(currentType);
    }
    return currentType;
}


TypeInfo Parser::parseType(std::string stringType, bool isReference){   

    Type* baseType = parseBaseType(stringType);

    Type* finalType = wrapWithPointers(baseType);

    return TypeInfo{finalType, isReference};
    
}

std::string Parser::parseVar(std::string valueVar){
    if(valueVar == "this" &&  !isInsideClass)
        throw std::runtime_error("Cannot use 'this' outside class");
    valueVar == "this" ? eat(THIS) : eat(VAR);
    return valueVar;
}