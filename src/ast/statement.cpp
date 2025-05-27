#include "statement.h"

DefineClass::DefineClass(
    std::string nameClass,
    std::vector<std::pair<Type *, std::string>> privateMembers,
    std::vector<std::pair<Type *, std::string>> constructorArgs,
    std::vector<Statement *> ConstructorBodyStatemets,
    std::vector<Function *> publicFunctions
){
    for(auto st : symbolClassType){  
        if(st->getNameClass() == nameClass) {
            throw std::runtime_error(
                "The Class has already been defined: \n" 
                + nameClass
            );
        }
    }
    for(auto st : symbolStructsType){
        if(st->getNameStruct() == nameClass){
            throw std::runtime_error(
                "You cannot declare a class with the same name as a struct: " +
                nameClass
            );
        }
    }
    StructType* structType = new StructType(nameClass, privateMembers);
    structType->setPointer(true); 
    constructorArgs.insert(constructorArgs.begin(), {structType->clone(), "this"});
    Function* constructor = new Function(
        new VoidType(),
        nameClass,  
        constructorArgs,
        ConstructorBodyStatemets
    );
    constructor->setClassFunction(true);
    constructor->setClassName(nameClass);

    std::vector<std::string> nameFunctions;
    for(auto function : publicFunctions){
        function->setClassArg(static_cast<StructType*>(structType->clone()));
        function->setClassFunction(true);
        function->setClassName(nameClass);
        nameFunctions.push_back(function->getName());
    }
    // Must be pointer only in the first argument of the function, not in general
    structType->setPointer(false); 

    publicFunctions.insert(publicFunctions.begin(), constructor); //not good for performance


    ClassType* classType = new ClassType(structType, nameFunctions);

    symbolClassType.push_back(static_cast<ClassType*>(classType->clone()));

    this->classType = classType;
    this->functions = publicFunctions;
}


void DefineClass::codegen(llvm::IRBuilder<> &builder) {
    llvm::outs() << "YOLO\n";
    llvm::LLVMContext& ctx = builder.getContext();

    StructType* structType = classType->getStructType();

    //todo --------------------------------------------------->
    llvm::StructType* pointType = llvm::StructType::create(ctx, classType->getNameClass());
    std::vector<llvm::Type*> members;
    for(auto member : structType->getMembers()){
        members.push_back(member.first->getLLVMType(ctx));
    }
    pointType->setBody(members);
    //todo generalize this part end the codegene parte of the DefineStruct: same code
    //todo ---------------------------------------------------<

    for(auto function : functions){
        function->codegen(builder);
    }

    generateAllocFunction(builder, classType->getNameClass(), pointType);
    generateFreeFunction(builder, classType->getNameClass(), pointType);
}

ClassDecl::ClassDecl(std::string nc, std::string vcn, std::vector<Expr *> a) {
    bool check = false;
    for(auto st : symbolClassType){
        if(st->getNameClass() == nc){
            check = true;
            break;
        }
    }
    if (!check)
        throw std::runtime_error("ClassDecl: Undefined Class with name '"+nc+"'");
    
    this->nameClass = nc;
    this->varClassName = vcn;
    this->args = a;
}

void ClassDecl::codegen(llvm::IRBuilder<> &builder) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Function* func = builder.GetInsertBlock()->getParent();

    bool checkVariable = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(varClassName);
        if (found != it->end()) {
            checkVariable = true;
            break;
        }
    }
    
    if(checkVariable) throw std::runtime_error("Variable already declared: " + varClassName);

    /*
    ClassType can never be nullpt since it has
    been checked in the constructor of this class
    before, so no need to add additional checks
    */
    ClassType* ClassType;      
    for (auto type : symbolClassType) {
        if (type->getNameClass() == nameClass) {
            ClassType = type; // Just use to reading
            break;
        }
    }

    // Allocation Class
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    //llvm::Type* llvmClassType = ClassType->getLLVMType(ctx);
    //llvm::AllocaInst* ptrToStruct = builder.CreateAlloca(llvmClassType, nullptr, varClassName);

    //Malloc-----------------------<>
    std::string class_Alloc = nameClass + "_alloc";
    llvm::Function* d_mallocFuncClass = module->getFunction(class_Alloc);
    if (!d_mallocFuncClass) {
        throw std::runtime_error("Function not found: " + class_Alloc);
    }
    llvm::Value* ptrToStruct = builder.CreateCall(d_mallocFuncClass, {}, varClassName);
    //Malloc-----------------------<\>

    llvm::Type* llvmPointerClassType = llvm::PointerType::getUnqual(classType->getLLVMType(ctx));
    llvm::AllocaInst* ptrToPointer= builder.CreateAlloca(llvmPointerClassType, nullptr, varClassName);

    builder.SetInsertPoint(currentBlock);
    builder.CreateStore(ptrToStruct, ptrToPointer);
    
    symbolTable.back()[varClassName] = {ptrToPointer,  new PointerType(classType->clone())};
    
    //Call the constructor;   
    SymbolFunction* functionStruct = nullptr;

    bool checkFunc = false;
    for (auto& function : symbolFunctions) {
        if (function.first == nameClass){
            functionStruct = &function.second;
            checkFunc = true;
            break;
        }
    }
    if(!checkFunc)
        throw std::runtime_error("Constructor '" + nameClass + "' not found");

    if(functionStruct->argType.size() != args.size() + 1){ 
        throw std::runtime_error("Argument count mismatch for Constructor" + nameClass);
    }

    llvm::Function* callee = functionStruct->func;

    // Generate argument values
    std::vector<llvm::Value*> argValues;
    argValues.push_back(ptrToStruct); //is a pointer

    for(auto* arg : args) {
        bool isVar = false;
        
        if (Var* varPtr = dynamic_cast<Var*>(arg)) {
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        else if (StructVar* structVarPtr = dynamic_cast<StructVar*>(arg)) {
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        Value* value  = arg->codegen(builder, isVar);

        llvm::Value* llvmVal = nullptr; 
        if (functionStruct->argType.at(argValues.size())->isPointer() && !value->getType()->isPointer()) {
            throw std::runtime_error(
                "Constructor " +
                nameClass +
                " argument " +
                std::to_string(argValues.size() + 1) +
                " wants a reference pass, insert a"  +
                " variable as an argument"
            );
        }
        
        if(!(*value->getType() ==  *functionStruct->argType.at(argValues.size())  )){
            throw std::runtime_error("Type mismatch in argument " + std::to_string(argValues.size() + 1));
        }
        argValues.push_back(!llvmVal ?
            value->getLLVMValue() : llvmVal);
        delete value;
    }

    //It's Void type
    builder.CreateCall(callee, argValues);
}

DefineStruct::DefineStruct(std::string ns, std::vector<std::pair<Type*, std::string>> m) : nameStruct(ns), members(m) {
    StructType* structType = new StructType(nameStruct, members); //todo channge the logic of private member of DefineStruct, complete useless now, just remove and use this object like private for the codegen
    for(auto st : symbolStructsType){
        if(st == structType || structType->equalName(*st)){
            delete structType;
            throw std::runtime_error("The Struct has already been defined: \n" + st->toString());
        }
    } 
    symbolStructsType.push_back(structType);
}

void DefineStruct::codegen(llvm::IRBuilder<> &builder) {

    llvm::LLVMContext& ctx = builder.getContext();

    StructType* structType;
    for(auto typeValue : symbolStructsType){
        if(typeValue->getNameStruct() == nameStruct){
            structType = typeValue;
            break;
        }
    }

    llvm::StructType* pointType = llvm::StructType::create(ctx, nameStruct);

    std::vector<llvm::Type*> members;
    for(auto member : this->members){
        members.push_back(member.first->getLLVMType(ctx));
    }
    pointType->setBody(members);

    //structType->setLLVMType(pointType);
}

StructDecl::StructDecl(std::string ns, std::string vrs, std::vector<Expr*> me) {
    
    bool check = false;
    for(auto st : symbolStructsType){
        if(st->getNameStruct() == ns){
            if(st->getMembers().size() == me.size()){
                check = true;
            }
            else{
                throw std::runtime_error(
                    "Struct '" + ns + "' expects " + 
                    std::to_string(st->getMembers().size()) + 
                    " members, but " + 
                    std::to_string(me.size()) + 
                    " were provided."
                );
            }
            break;
        }
    }
    if (!check)
        throw std::runtime_error("Undefined struct with name '"+ns+"'");
    
    this->nameStruct = ns;
    this->varStructName = vrs;
    this->membersExpr = me;
}

void StructDecl::codegen(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Function* func = builder.GetInsertBlock()->getParent();

    bool checkVariable = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(varStructName);
        if (found != it->end()) {
            checkVariable = true;
            break;
        }
    }
    
    if(checkVariable) throw std::runtime_error("Variable already declared: " + varStructName);

    StructType* structType;      
    for (auto type : symbolStructsType) {
        if (type->getNameStruct() == nameStruct) {
            structType = type; // Just use to reading
            break;
        }
    }

    // Allocation Struct
    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();

    builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    llvm::Type* llvmStructType = structType->getLLVMType(ctx);
    llvm::AllocaInst* ptrToStruct = builder.CreateAlloca(llvmStructType, nullptr, varStructName);

    builder.SetInsertPoint(currentBlock);

    const auto& structMembers = structType->getMembers();
    for (size_t i = 0; i < structMembers.size(); ++i) {
        const auto& member = structMembers[i];
        Expr* memberExpr = membersExpr[i];

        llvm::Value* fieldIGEP = builder.CreateStructGEP(llvmStructType, ptrToStruct, i, member.second);
        Value* memberValue = memberExpr->codegen(builder);
        if (memberValue->getType()->isPointer()){
            throw std::runtime_error(
                "You can't assign a ptr to one member of the struct without using ref"
            );
        }
        if(!(*member.first == *memberValue->getType())){
            if(!memberValue->getType()->isCastTo(member.first)){
                throw std::runtime_error(
                    "The type of struct member '" +
                    member.second + "' (expected '" +
                    member.first->toString() +
                    "') is not compatible with the provided value of type '" + 
                    memberValue->getType()->toString() + "'"
                );
            }
        
            Value* newVal = memberValue->castTo(member.first, builder);
            delete memberValue;
            memberValue = newVal;
        }
        builder.CreateStore(memberValue->getLLVMValue(), fieldIGEP);
        delete memberValue;
    }

    // Insert struct variable in the vector of variables
    symbolTable.back()[varStructName] = {ptrToStruct, structType->clone()};
}

VarStructUpdt::VarStructUpdt(std::string nv, std::vector<std::string>  mc, Expr *v) : nameVar(nv), memberChain(mc), value(v) {}

void VarStructUpdt::codegen(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();
    bool checkVariable = false;
    llvm::Value* ptrToStruct;
    StructType* type;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            ptrToStruct = found->second.alloca;
            type = dynamic_cast<StructType*>(found->second.type);
            checkVariable = true;
            break;
        }
    }

    if(!checkVariable) 
        throw std::runtime_error(
            "Undeclared variable: "
            + nameVar
        );
    if(!type) 
        throw std::runtime_error(
            "variable: " +
             nameVar +
            " is not a Struct variable but '" +
            type->toString() +
            "'"
        );
    auto [fieldPtr, memberType] = getStructMemberGEP(builder, ptrToStruct, type, memberChain);
    Value* memberValue = value->codegen(builder);
    if (memberValue->getType()->isPointer()){
        throw std::runtime_error(
            "You can't Update a member to of the struct to a ptr"
        );
    }

    if(!(*memberType == *memberValue->getType())){
        if(!memberValue->getType()->isCastTo(memberType)){
            throw std::runtime_error(
                "The type of struct member '" +
                memberChain.back() + "' (expected '" +
                memberType->toString() +
                    "') is not compatible with the provided value of type '" + 
                memberValue->getType()->toString() + "'"
            );
        }
        Value* newVal = memberValue->castTo(memberType, builder);
        delete memberValue;
        memberValue = newVal;
    }
    
    builder.CreateStore(memberValue->getLLVMValue(), fieldPtr);
    delete memberValue;
}

Function::Function(
    Type* tf, 
    const std::string nf,
    const std::vector<std::pair<Type*, std::string>> p,
    std::vector<Statement*> b,
    bool classFunction,
    std::string className
) 
: typeFunc(tf),
  nameFunc(nf),
  parameters(p),
  body(b) {}

void Function::codegen(llvm::IRBuilder<> &builder) {
    llvm::outs() <<"llvm::namefunction-> " + nameFunc + "   is classFunction: " << classFunction <<"\n\n\n";
    llvm::LLVMContext& ctx = builder.getContext();

    std::vector<llvm::Type*> argLLVMTypes;
    std::vector<Type*> argTypes;
    for (const auto& param : parameters) {
        argLLVMTypes.push_back(param.first->getLLVMType(ctx));
        argTypes.push_back(param.first->clone());
    }

    llvm::Type* returnType = typeFunc->getLLVMType(ctx);

    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, argLLVMTypes, false);

    std::string realName = nameFunc;
    if(classFunction){
        if(className == nameFunc){
            nameFunc = nameFunc + "_Create_Default";
            //llvm::outs() <<"llvm::namefunction8888-> " + nameFunc + "\n\n\n";
        }
        else{
            nameFunc = className + "_" + nameFunc;
            //llvm::outs() <<"llvm::namefunction999-> " + nameFunc + "\n\n\n";
        }
    }

    llvm::Function* function = module->getFunction(nameFunc);

    if (function) throw std::runtime_error("Redefinition of function: " + nameFunc);

    //llvm::outs() <<"llvm::namefunction-> " + nameFunc + "\n\n\n";

    function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, nameFunc, module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(builder.getContext(), "entry", function);
    builder.SetInsertPoint(entry);
    symbolTable.emplace_back();

    llvm::Function::arg_iterator argIt = function->arg_begin();
    for (const auto& param : parameters) {
        llvm::Argument* arg = &*argIt++;
        arg->setName(param.second);
        if (param.first->isPointer()) {
            // If it's a pointer, save the argument directly --> Become not Pointer
            Type* t = param.first->clone();
            t->setPointer(false);
            symbolTable.back()[param.second] = {arg, t};
        } 
        else {
            llvm::AllocaInst* alloca = builder.CreateAlloca(arg->getType(), nullptr, param.second);
            builder.CreateStore(arg, alloca);
            symbolTable.back()[param.second] = {alloca, param.first->clone()};
        }
    }
    symbolFunctions.emplace_back(
        realName,
        SymbolFunction{
            typeFunc->clone(),
            argTypes,
            function,
            classFunction,
            className,
        }
    );

    size_t i = 0;
    for (Statement* stm : body) {
        stm->codegen(builder);
    }

    for (auto& [name, info] : symbolTable.back()) {
        delete info.type;
    }
    symbolTable.pop_back();
}

void Function::setClassArg(StructType* argType) {
    parameters.insert(parameters.begin(), {argType, "this"});
}

void Function::setClassFunction(bool value) {
    
    this->classFunction = value;
}

void Function::setClassName(std::string name){
    this->className = name;
}

ClassCallVoidFunc::ClassCallVoidFunc(
    const std::string fvn,
    const std::vector<std::string> mn,
    std::string noc,
    std::vector<Expr *> a
):
    firstVariableName(fvn),
    memberChain(mn),
    nameOfClass(noc),
    args(a)
{}

void ClassCallVoidFunc::codegen(llvm::IRBuilder<>& builder) {
    generateClassFunctionCall(
        builder,
        firstVariableName,
        memberChain,
        nameOfClass,
        args,
        false);
    return;
}

CallFuncStatement::CallFuncStatement(std::string nf, std::vector<Expr *> a) : funcName(nf), args(a) {}

void CallFuncStatement::codegen(llvm::IRBuilder<> &builder){
    SymbolFunction* functionStruct = nullptr;

    bool checkFunc = false;
    for (auto& function : symbolFunctions) {
        if (function.first == funcName && !function.second.classFunction) {
            functionStruct = &function.second;
            checkFunc = true;
            break;
        }
    }
    if(!checkFunc)
        throw std::runtime_error("Function not found: " + funcName);

    if(functionStruct->argType.size() != args.size()){
        throw std::runtime_error("Argument count mismatch for " + funcName);
    }
    if(dynamic_cast<VoidType*>(functionStruct->returnType)  == nullptr){
        throw std::runtime_error(
            "The function '"
            + funcName +
            "'is not a Void, Can't call without store the return value "
        );
    }

    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Function* callee = functionStruct->func;

    // Generate argument values
    std::vector<llvm::Value*> argValues;
    for(auto* arg : args) {
        bool isVar = false;
        
        if (Var* varPtr = dynamic_cast<Var*>(arg)) {
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        else if (StructVar* structVarPtr = dynamic_cast<StructVar*>(arg)) {
            if(functionStruct->argType.at(argValues.size())->isPointer()){
                isVar = true;
            }
        }
        Value* value  = arg->codegen(builder, isVar);
        llvm::Value* llvmVal = nullptr; 
        
        if (functionStruct->argType.at(argValues.size())->isPointer() && !value->getType()->isPointer()) {
            
            throw std::runtime_error(
                "Function " +
                funcName +
                " argument " +
                std::to_string(argValues.size() + 1) +
                " wants a reference pass, insert a"  +
                " variable as an argument"
            );
        }
        
        if(!(*value->getType() ==  *functionStruct->argType.at(argValues.size())  )){
            throw std::runtime_error("Type mismatch in argument " + std::to_string(argValues.size() + 1));
        }
        argValues.push_back(!llvmVal ?
            value->getLLVMValue() : llvmVal);
        delete value;
    }

    builder.CreateCall(callee, argValues);
    return;
}

ReturnVoid::ReturnVoid(std::string fn, std::string noc) : funcName(fn), nameOfClass(noc) {}

void ReturnVoid::codegen(llvm::IRBuilder<> &builder){
    Type* returnType = nullptr;
    for (const auto& func : symbolFunctions) {
        if (func.first == funcName &&
            func.second.className == nameOfClass    
        ){    
            returnType = func.second.returnType->clone();
            break;
        }
    }
    if(returnType == nullptr)
        throw std::runtime_error(
            "Return error No function '" + funcName + "' found in symbolFunctions"
        );
    
    if (dynamic_cast<VoidType*>(returnType) == nullptr) {
        throw std::runtime_error(
            "The function '"+funcName+"' is Void expected  'return;'"
        );
    }
    builder.CreateRetVoid();
}



Return::Return(Expr* e, std::string fn, std::string noc) : expr(e), funcName(fn), nameOfClass(noc) {}

void Return::codegen(llvm::IRBuilder<> &builder) {
    Type* returnType = nullptr;
    for (const auto& func : symbolFunctions) {
        if (func.first == funcName &&
            func.second.className == nameOfClass    
        ){  
            returnType = func.second.returnType->clone();
            break;
        }
    }
    if(returnType == nullptr)
        throw std::runtime_error(
            "Return error No function '" + funcName + "' found in symbolFunctions"
        );

    // Handled the fact that it could be a pointer
    bool pointer = returnType->isPointer() ? true : false;
    Value* retVal;
    if(pointer){
        //DEVO VERIFICARE SE I RISULTATI SONO DEI PUNTATORI
        if (auto* varExpr = dynamic_cast<Var*>(expr)) {
            retVal = expr->codegen(builder, true);
        }

        else if (auto* structVarExpr = dynamic_cast<StructVar*>(expr)) {
            retVal = expr->codegen(builder, true);
        }

        else if (auto* callFuncExpr = dynamic_cast<CallFunc*>(expr)) {
            retVal = expr->codegen(builder, false);
            if(!retVal->getType()->isPointer()){
                throw std::runtime_error(
                    "Retun of '"+
                    funcName +
                    "' You cannot assign a value to a reference that is not a pointer."
                );   
            }
        }

        else if (auto* classCallFuncExpr = dynamic_cast<ClassCallFunc*>(expr)) {
            retVal = expr->codegen(builder, false);
            if(!retVal->getType()->isPointer()){
                throw std::runtime_error(
                    "The Retun of '"+
                    funcName +
                    "' You cannot assign a value to a reference that is not a pointer."
                );    
            }
        }
        else {
            throw std::runtime_error(
                "Return error: the return value is not a pointer"
            );
        }
    }
    if(!pointer){
        retVal = expr->codegen(builder);
        if(retVal->getType()->isPointer()){
            throw std::runtime_error(
                "Retun of '"+
                funcName +
                "' You cannot assign a value to a reference is a pointer."
            );   
        }
    }

    if(retVal == nullptr){
        throw std::runtime_error(
            "THE RUNNING CODE MUST NOT ENTER HERE"
        );
    }

    if(!(*retVal->getType() == *returnType))
        throw std::runtime_error(
            "The returned type '" +
            retVal->getType()->toString() + 
            "' is not compatible with the type that the function '" +
            funcName +
            "', is supposed to return '"+returnType->toString()+"'"
        );
    builder.CreateRet(retVal->getLLVMValue());
}

WhileStm::WhileStm(Expr* c, std::vector<Statement*> w) : cond(c), whileExpr(w) {}

void WhileStm::codegen(llvm::IRBuilder<>& builder) {
    llvm::Function* func = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock* condWhileBB = llvm::BasicBlock::Create(builder.getContext(), "condWhile", func);
    llvm::BasicBlock* bodyWhileBB = llvm::BasicBlock::Create(builder.getContext(), "bodyWhile", func);
    llvm::BasicBlock* nextBB = llvm::BasicBlock::Create(builder.getContext(), "mergeWhile", func);

    builder.CreateBr(condWhileBB);

    builder.SetInsertPoint(condWhileBB);
    Value* condVal = cond->codegen(builder);
    // Check if the condition is already a boolean value
    if(!dynamic_cast<const BoolType*>(condVal->getType())){
        Value* newCondVal = condVal->getBoolValue(builder);  // Convert to boolean
        delete condVal;                                      // Free old value
        condVal = newCondVal;                                // Update pointer
    }
    builder.CreateCondBr(condVal->getLLVMValue(), bodyWhileBB, nextBB);
    delete condVal;

    symbolTable.emplace_back();

    builder.SetInsertPoint(bodyWhileBB);
    for (Statement* stm : whileExpr) {
        stm->codegen(builder);
    }
    builder.CreateBr(condWhileBB);

    for (auto& [name, info] : symbolTable.back()) {
        delete info.type;
    }
    symbolTable.pop_back();

    builder.SetInsertPoint(nextBB);
}

IfStm::IfStm(Expr* c, std::vector<Statement*> t, std::vector<Statement*> e) : cond(c), thenExpr(t), elseExpr(e) {}

void IfStm::codegen(llvm::IRBuilder<>& builder) {
    Value* condVal = cond->codegen(builder);
    // Check if the condition is already a boolean value
    if(!dynamic_cast<const BoolType*>(condVal->getType())){
        Value* newCondVal = condVal->getBoolValue(builder);  // Convert to boolean
        delete condVal;                                      // Free old value
        condVal = newCondVal;                                // Update pointer
    }

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(ctx, "then", func);
    llvm::BasicBlock* elseBB = !elseExpr.empty() ? llvm::BasicBlock::Create(ctx, "else", func) : nullptr;
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx, "merge",func);

    if(!elseExpr.empty()){
        builder.CreateCondBr(condVal->getLLVMValue(), thenBB, elseBB);
    }
    else{
        builder.CreateCondBr(condVal->getLLVMValue(), thenBB, mergeBB);
    }

    delete condVal;

    symbolTable.emplace_back();
    
    builder.SetInsertPoint(thenBB);
    for (Statement* stm : thenExpr) {
        stm->codegen(builder);
    }

    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(mergeBB);
    }

    for (auto& [name, info] : symbolTable.back()) {
        delete info.type;
    }
    symbolTable.pop_back();
    symbolTable.emplace_back();

    if (!elseExpr.empty()) {
        builder.SetInsertPoint(elseBB);
        for (Statement* stm : elseExpr) {
            stm->codegen(builder);
        }
        if (!builder.GetInsertBlock()->getTerminator()) {
            builder.CreateBr(mergeBB);
        }
    }

    for (auto& [name, info] : symbolTable.back()) {
        delete info.type;
    }
    symbolTable.pop_back();

    builder.SetInsertPoint(mergeBB);
}

VarUpdt::VarUpdt(const std::string n, Expr* v) : nameVar(n), value(v) {}

void VarUpdt::codegen(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();
    bool checkVariable = false;
    llvm::Value* alloca;
    Type* type;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            alloca = found->second.alloca;
            type = found->second.type;
            checkVariable = true;
            break;
        }
    }

    if(!checkVariable) throw std::runtime_error("Undeclared variable: " + nameVar);

    Value* val = value->codegen(builder);
    if (val->getType()->isPointer()){
        throw std::runtime_error(
            "You can't update a variable into a ptr"
        );
    }
    
    if(!(*val->getType() == *type)){
            if(!val->getType()->isCastTo(type)){
                throw std::runtime_error(
                "VarUpdt::Type mismatch for variable '" + 
                nameVar + 
                "': expected " + 
                type->toString() + 
                ", got " + 
                val->getType()->toString()
                );
            }
            Value* newVal = val->castTo(type, builder);
            delete val;
            val = newVal;
    }
    builder.CreateStore(val->getLLVMValue(), alloca);
    delete val;
}

VarDecl::VarDecl(const std::string n, Type* t, Expr* v) : nameVar(n), type(t), value(v) {}

void VarDecl::codegen(llvm::IRBuilder<> &builder)
{

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();

    bool checkVariable = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            checkVariable = true;
            break;
        }
    }
    if(checkVariable) throw std::runtime_error("Variable already declared: " + nameVar);
 
    Value* val = value->codegen(builder);
    if (val->getType()->isPointer()){
        throw std::runtime_error(
            "You can't assign a variable a ptr without using ref"
        );
    }

    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
    llvm::Type* typeVar;
    if(!type){
        typeVar = val->getType()->getLLVMType(ctx);
        type = val->getType()->clone();
    }
    else{
        if(!(*val->getType() == *type)){
            if(!val->getType()->isCastTo(type)){
                throw std::runtime_error(
                    "VarDecl::Type mismatch for variable '" + 
                    nameVar + 
                    "': expected " + 
                    type->toString() + 
                    ", got " + 
                    val->getType()->toString()
                );
            }
            Value* newVal = val->castTo(type, builder);
            delete val;
            val = newVal;
        }
        typeVar = type->getLLVMType(ctx);
    }
    
    /*
    If Cast is performed, the block may be
    different from the saved one,
    so continue on the current one
      */
    if (builder.GetInsertBlock() != currentBlock) { 
        currentBlock = builder.GetInsertBlock();
    }
    
    builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
    llvm::AllocaInst* alloca = builder.CreateAlloca(typeVar, nullptr, nameVar);
    
    builder.SetInsertPoint(currentBlock);
    builder.CreateStore(val->getLLVMValue(), alloca);

    llvm::outs() << "The variable allocated with name: " << alloca->getName() << "\n";

    symbolTable.back()[nameVar] = {alloca, val->getType()->clone()};

    delete val;
}

RefDecl::RefDecl(const std::string n, Type* t, Expr* v) : nameVar(n), type(t), value(v) {}

void RefDecl::codegen(llvm::IRBuilder<> &builder) {

    llvm::Function* func = builder.GetInsertBlock()->getParent();
    llvm::LLVMContext& ctx = builder.getContext();

    bool checkVariable = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            checkVariable = true;
            break;
        }
    }
    if(checkVariable) throw std::runtime_error("Variable already declared: " + nameVar);

    llvm::Value* alloca;
    llvm::Type* typellvm;
    Value* resultCodegen;
 
    bool isCallFunc = false;
    bool isClassCallFunc = false;
    bool isVar = false;
    bool isStructVar = false;
    
    if (auto* varExpr = dynamic_cast<Var*>(value)) {
        isVar = true;
    }
    else if (auto* callFuncExpr = dynamic_cast<CallFunc*>(value)) {
        isCallFunc = true;
    }
    else if (auto* classCallFuncExpr = dynamic_cast<ClassCallFunc*>(value)) {
        isClassCallFunc = true;
    }
    else if (auto* structVarExpr = dynamic_cast<StructVar*>(value)) {
        isStructVar = true;
    }
    else {
        throw std::runtime_error(
            "You cannot assign a value to a reference that is not a pointer."
        );
    }

    if(isCallFunc || isClassCallFunc){
        resultCodegen = value->codegen(builder, false);
        if(!resultCodegen->getType()->isPointer()){
            throw std::runtime_error(
                "You cannot assign a value to a reference that is not a pointer."
            );   
        }
        alloca = resultCodegen->getLLVMValue();
        
        if(!(*resultCodegen->getType() == *type)){
            throw std::runtime_error(
                "You cannot assign a value to a reference with difference type: '" +
                resultCodegen->getType()->toString() + 
                "' into '"+ type->toString() +
                "'"
            );
        }
        resultCodegen->getType()->setPointer(false);
        symbolTable.back()[nameVar] = {alloca, resultCodegen->getType()->clone()};
        delete resultCodegen;
        return;
    }    

    if(isVar || isStructVar){
        resultCodegen = value->codegen(builder, true);
        alloca = resultCodegen->getLLVMValue();
        typellvm = resultCodegen->getType()->getLLVMType(ctx);
        
        if(!(*resultCodegen->getType() == *type)){
            throw std::runtime_error(
                "You cannot assign a value to a reference with difference type: '" +
                resultCodegen->getType()->toString() + 
                "' into '"+ type->toString() +
                "'"
            );
        }
        resultCodegen->getType()->setPointer(false);
        symbolTable.back()[nameVar] = {alloca, resultCodegen->getType()->clone()};
        delete resultCodegen;
        return;
    }
}

DeleteVar::DeleteVar(std::string v) : var(v) {}

void DeleteVar::codegen(llvm::IRBuilder<> &builder){
    llvm::LLVMContext& ctx = builder.getContext();
    bool checkVariable = false;
    llvm::Value* alloca;
    Type* type;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(var);
        if (found != it->end()) {
            alloca = found->second.alloca;
            type = found->second.type;
            checkVariable = true;

            it->erase(found); //I need to fix it
            break;
        }
    }

    if(!checkVariable) throw std::runtime_error("Undeclared variable: " + var);

    if(!dynamic_cast<ClassType* >(type)){
        throw std::runtime_error("You can't Delete a variable '" + var + "' because isn't an object");
    }
    auto classType = dynamic_cast<ClassType* >(type);
    std::string class_Free =  classType->getNameClass()+ "_free";
    llvm::Function* d_freeFuncClass = module->getFunction(class_Free);
    if (!d_freeFuncClass) {
        throw std::runtime_error("Function not found: " + class_Free);
    }
    llvm::Value* ptrToStruct = builder.CreateCall(d_freeFuncClass, {alloca});

    delete type;
    return;
}
