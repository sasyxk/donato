#include "statement.h"

DefineClass::DefineClass(
    std::string nameClass,
    std::vector<std::pair<Type *, std::string>> privateMembers,
    std::vector<std::pair<TypeInfo, std::string>> constructorArgs,
    std::vector<Statement *> ConstructorBodyStatemets,
    std::vector<Function *> publicFunctions,
    ClassType* classType
){
    StructType* structType = classType->getStructType();
    constructorArgs.insert(constructorArgs.begin(), {{structType, true}, "this"});
    Function* constructor = new Function(
        {TypeManager::instance().getVoidType(), false},
        nameClass,  
        constructorArgs,
        ConstructorBodyStatemets
    );
    constructor->setClassFunction(true);
    constructor->setClassName(nameClass);

    std::vector<std::string> nameFunctions;
    for(auto function : publicFunctions){
        function->setClassArg(static_cast<StructType*>(structType));
        function->setClassFunction(true);
        function->setClassName(nameClass);
        nameFunctions.push_back(function->getName());
    }

    publicFunctions.insert(publicFunctions.begin(), constructor); //not good for performance

    this->classType = static_cast<ClassType*>(classType);
    this->functions = publicFunctions;

}


void DefineClass::codegen(llvm::IRBuilder<> &builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    StructType* structType = classType->getStructType();

    llvm::StructType* pointType = llvm::StructType::create(ctx, classType->getNameClass());
    std::vector<llvm::Type*> members;
    for(auto member : structType->getMembers()){
        members.push_back(member.first->getLLVMType(ctx));
    }
    pointType->setBody(members);

    generateAllocFunction(builder, classType->getNameClass(), pointType);
    generateFreeFunction(builder, classType->getNameClass(), pointType);

    for(auto function : functions){
        function->codegen(builder);
    }
}

DefineStruct::DefineStruct(StructType* structType) {
    this->structType = structType;
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
    
    if (memberValue->getLLVMValue() == nullptr){
        memberValue->loadLLVMValue(memberChain.back(), builder);
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
    TypeInfo tf, 
    const std::string nf,
    const std::vector<std::pair<TypeInfo, std::string>> p,
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
    std::vector<TypeInfo> argTypes;
    for (const auto& [typeInfoparam, nameArg] : parameters) {
        argLLVMTypes.push_back(typeInfoparam.getLLVMType(ctx));
        argTypes.push_back(typeInfoparam);
    }

    llvm::Type* returnType = typeFunc.getLLVMType(ctx);

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

    llvm::Function* func = builder.GetInsertBlock()->getParent();

    llvm::Function::arg_iterator argIt = function->arg_begin();
    for (const auto& [typeInfoparam, nameArg] : parameters) {
        llvm::Argument* arg = &*argIt++;
        arg->setName(nameArg);
        llvm::Value* alloca;
        if (typeInfoparam.isReference) {
            alloca = arg;
        } 
        else {
            llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
            builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());
            alloca = builder.CreateAlloca(arg->getType(), nullptr, nameArg);
            builder.SetInsertPoint(currentBlock);
            builder.CreateStore(arg, alloca);
        }
        symbolTable.back()[nameArg] = {alloca, typeInfoparam.type};
    }
    symbolFunctions.emplace_back(
        realName,
        SymbolFunction{
            typeFunc,
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

    symbolTable.pop_back();
}

void Function::setClassArg(StructType* argType) {
    parameters.insert(parameters.begin(), {{argType, true}, "this"});
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

void CallFuncStatement::codegen(llvm::IRBuilder<> &builder) {
    FunctionCallParams params;
    params.functionName = funcName;
    params.requiresVoidReturn = true;
    
    auto [functionStruct, argValues] = prepareAndValidateFunctionCall(params, args, builder);
    
    llvm::Function* callee = functionStruct->func;
    builder.CreateCall(callee, argValues);
}

ReturnVoid::ReturnVoid(std::string fn, std::string noc) : funcName(fn), nameOfClass(noc) {}

void ReturnVoid::codegen(llvm::IRBuilder<> &builder){
    TypeInfo returnType = {nullptr, false};
    for (const auto& func : symbolFunctions) {
        if (func.first == funcName &&
            func.second.className == nameOfClass    
        ){    
            returnType = func.second.returnType;
            break;
        }
    }
    if(returnType.type == nullptr)
        throw std::runtime_error(
            "Return error No function '" + funcName + "' found in symbolFunctions"
        );
    
    if (dynamic_cast<VoidType*>(returnType.type) == nullptr) {
        throw std::runtime_error(
            "The function '"+funcName+"' is Void expected  'return;'"
        );
    }
    builder.CreateRetVoid();
}


Return::Return(Expr* e, std::string fn, std::string noc) : expr(e), funcName(fn), nameOfClass(noc) {}

void Return::codegen(llvm::IRBuilder<> &builder) {
    TypeInfo returnType = {nullptr, false};
    for (const auto& func : symbolFunctions) {
        if (func.first == funcName &&
            func.second.className == nameOfClass    
        ){  
            returnType = func.second.returnType;
            break;
        }
    }
    if(returnType.type == nullptr){
        throw std::runtime_error(
            "Return error No function '" + funcName + "' found in symbolFunctions"
        );
    }

    Value* retVal = expr->codegen(builder);

    if(!(*retVal->getType() == *returnType.type)){
        throw std::runtime_error(
            "The returned type '" +
            retVal->getType()->toString() + 
            "' is not compatible with the type that the function '" +
            funcName +
            "', is supposed to return '"+returnType.type->toString()+"'"
        );
    }

    llvm::Value* retValLLVM;
    if(returnType.isReference){
        retValLLVM = retVal->getAllocation();
        if(retValLLVM == nullptr){
            throw std::runtime_error(
                "The Retun of '"+
                funcName +
                "' You cannot return a value that is not a variable."
            );    
        }
    }
    else {
        retValLLVM = retVal->getLLVMValue();
        if(retValLLVM == nullptr){
            llvm::Value* loadedVal = builder.CreateLoad(
                retVal->getType()->getLLVMType(builder.getContext()),
                retVal->getAllocation(),
                "load_val"
            );
            retValLLVM = loadedVal;
        }
    }
    builder.CreateRet(retValLLVM);

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

    symbolTable.pop_back();

    builder.SetInsertPoint(nextBB);
}

IfStm::IfStm(Expr* c, std::vector<Statement*> t, std::vector<Statement*> e) : cond(c), thenExpr(t), elseExpr(e) {}

void IfStm::codegen(llvm::IRBuilder<>& builder) {
    Value* condVal = cond->codegen(builder);
    if (condVal->getLLVMValue() == nullptr){
        condVal->loadLLVMValue("if_cond_stm", builder);
    }
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
    if (val->getLLVMValue() == nullptr){
        val->loadLLVMValue(nameVar, builder);
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

void VarDecl::codegen(llvm::IRBuilder<> &builder) {

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
    if (val->getLLVMValue() == nullptr){
       val->loadLLVMValue(nameVar, builder);
    }

    llvm::BasicBlock* currentBlock = builder.GetInsertBlock();
    llvm::Type* typeVar;
    if(!type){
        typeVar = val->getType()->getLLVMType(ctx);
        type = val->getType();
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

    symbolTable.back()[nameVar] = {alloca, val->getType()};

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

    llvm::outs() << "REF declaration: " << nameVar <<"\n";
    Value* result = value->codegen(builder);

    // The alloca Value must be != nullptr
    llvm::Value* alloca = result->getAllocation();
    if(alloca == nullptr){
        throw std::runtime_error(
            "Reference assignment cannot be done with constant values, variable: '" + nameVar +"'"
        );
    }
    
    Type* resultType = result->getType();
    if(!(*result->getType() == *type)){
         throw std::runtime_error(
            "You cannot assign a value to a reference with difference type: '" +
            resultType->toString() + 
            "' into '"+ type->toString() +
            "'"
        );
    }
    symbolTable.back()[nameVar] = {alloca, resultType};

    delete result;
}

DeleteVar::DeleteVar(Expr* v) : value(v) {}

void DeleteVar::codegen(llvm::IRBuilder<> &builder){
    llvm::LLVMContext& ctx = builder.getContext();
    Value* result = value->codegen(builder);
    if (result->getLLVMValue() == nullptr){
        result->loadLLVMValue("delete_ptr", builder);
    }

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
    if (result->getLLVMValue() == nullptr){
        result->loadLLVMValue("print", builder);
    }

    std::string printFunctionName = "d_print";
    llvm::Function* d_printFunction = module->getFunction(printFunctionName);
    if (!d_printFunction) {
        throw std::runtime_error("Function not found: " + printFunctionName);
    }

    builder.CreateCall(d_printFunction, {result->getLLVMValue()});
}