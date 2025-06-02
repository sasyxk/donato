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

DefineStruct::DefineStruct(StructType* structType) {
    
    for(auto st : symbolStructsType){
        if(st->getNameStruct() == structType->getNameStruct()){
            throw std::runtime_error("The Struct has already been defined: " + st->toString());
        }
    } 
    for(auto ct : symbolClassType){
        if(ct->getNameClass() == structType->getNameStruct()){
            throw std::runtime_error("You cannto define a Struct with the dame name of: " + ct->toString());
        }
    }
    symbolStructsType.push_back(structType);
    this->structType = static_cast<StructType*>(structType->clone());
}

void DefineStruct::codegen(llvm::IRBuilder<> &builder) {

    llvm::LLVMContext& ctx = builder.getContext();
    llvm::StructType* pointType = llvm::StructType::create(ctx, structType->getNameStruct());

    std::vector<llvm::Type*> members;
    for(auto member : structType->getMembers()){
        members.push_back(member.first->getLLVMType(ctx));
    }
    pointType->setBody(members);

    generateAllocFunction(builder, structType->getNameStruct(), pointType);
    generateFreeFunction(builder, structType->getNameStruct(), pointType);
}

VarStructUpdt::VarStructUpdt(std::string nv, std::vector<std::string>  mc, Expr *v) : nameVar(nv), memberChain(mc), value(v) {}

void VarStructUpdt::codegen(llvm::IRBuilder<>& builder) {
    
    llvm::LLVMContext& ctx = builder.getContext();
    bool checkVariable = false;
    llvm::Value* ptrToStruct;
    Type* typeT = nullptr;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            ptrToStruct = found->second.alloca;
            typeT = found->second.type;
            checkVariable = true;
            break;
        }
    }
    if(!checkVariable) 
        throw std::runtime_error(
            "Undeclared variable: "
            + nameVar
        );

    StructType* type = dynamic_cast<StructType*>(typeT);
    if(type == nullptr){
        throw std::runtime_error(
            "variable: " +
             nameVar +
            " is not a Struct variable but '" +
            typeT->toString() +
            "'"
        );
    } 
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
        }
        else{
            nameFunc = className + "_" + nameFunc;
        }
    }

    llvm::Function* function = module->getFunction(nameFunc);

    if (function) throw std::runtime_error("Redefinition of function: " + nameFunc);

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
    /*llvm::BasicBlock* block = builder.GetInsertBlock();
    if (!block->getTerminator()) {
        throw std::runtime_error(
            "The function " + nameFunc + " does not have a Return"
        );
    } */

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
        else if(auto* DeferenceExpr = dynamic_cast<DereferenceOp*>(expr)){
            retVal = expr->codegen(builder, true);
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
            "You can't update a variable with value IsPointer()= true"
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

    bool checkVariable = false;
    for (auto it = symbolTable.rbegin(); it != symbolTable.rend(); ++it) {
        auto found = it->find(nameVar);
        if (found != it->end()) {
            checkVariable = true;
            break;
        }
    }
    if(checkVariable) throw std::runtime_error("Variable already declared: " + nameVar);

    if(!dynamic_cast<AddressOp*>(value)){
        //implicit &x ---> ref int y = &x  where int x = 5;
        value = new AddressOp(value);
        /*throw std::runtime_error(
            "When declaring with ref, you must pass the memory address, variable:  " + nameVar
        );*/
    }

    // It will always be PointerValue
    Value* result = value->codegen(builder);
    PointerType* resultType = static_cast<PointerType*>(result->getType());
    
    if(!(*resultType->getTypePointed() == *type)){
         throw std::runtime_error(
            "You cannot assign a value to a reference with difference type: '" +
            resultType->getTypePointed()->toString() + 
            "' into '"+ type->toString() +
            "'"
        );
    }
    llvm::Value* alloca = result->getLLVMValue();
    symbolTable.back()[nameVar] = {alloca, resultType->getTypePointed()->clone()};

    delete result;
}

DeleteVar::DeleteVar(Expr* v) : value(v) {}

void DeleteVar::codegen(llvm::IRBuilder<> &builder){
    llvm::LLVMContext& ctx = builder.getContext();
    Value* result = value->codegen(builder);

    auto pointerType = dynamic_cast<PointerType*>(result->getType());
    auto classType = pointerType ? dynamic_cast<ClassType*>(pointerType->getTypePointed()) : nullptr;
    auto structType = pointerType ? dynamic_cast<StructType*>(pointerType->getTypePointed()) : nullptr;

    if (!classType && !structType) {
        throw std::runtime_error(
            "You can't Delete a variable that"
            " isn't an pointer to a Class or Struct"
        );
    }
    std::string name = classType? classType->getNameClass() : structType->getNameStruct();
    std::string object_Free =  name+ "_free";
    llvm::Function* d_freeFuncObj = module->getFunction(object_Free);
    if (!d_freeFuncObj) {
        throw std::runtime_error("Function not found: " + object_Free);
    }

    // This value will have already been loaded from the pointer,
    // or placed by the AddressOp ready to be read.
    llvm::Value* alloca = result->getLLVMValue();

    builder.CreateCall(d_freeFuncObj, {alloca});
}


PrintVar::PrintVar(Expr* v) : value(v) {}

void PrintVar::codegen(llvm::IRBuilder<> &builder){
    llvm::LLVMContext& ctx = builder.getContext();

    Value* result = value->codegen(builder);

    std::string printFunctionName = "d_print";
    llvm::Function* d_printFunction = module->getFunction(printFunctionName);
    if (!d_printFunction) {
        throw std::runtime_error("Function not found: " + printFunctionName);
    }

    builder.CreateCall(d_printFunction, {result->getLLVMValue()});
}