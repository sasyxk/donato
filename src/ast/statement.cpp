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
        new BoolType(), //todo create A Void Type to change it
        nameClass,      // WARNING maybe i have to manage if the name should be with the first in lowarcase
        constructorArgs,
        ConstructorBodyStatemets
    );
    std::vector<std::string> nameFunctions;
    for(auto function : publicFunctions){
        function->setClassArg(static_cast<StructType*>(structType->clone()));
        nameFunctions.push_back(function->getName());
    }

    publicFunctions.push_back(constructor); // In theory even if the constructor is generated later, it shouldn't be a problem
    //nameFunctions.push_back(nameClass);  Even the constructor name function????

    ClassType* classType = new ClassType(structType, nameFunctions);

    symbolClassType.push_back(static_cast<ClassType*>(classType->clone()));

    this->classType = classType;
    this->functions = publicFunctions;
}

void DefineClass::codegen(llvm::IRBuilder<> &builder) {
    llvm::outs() << "YOLO\n";
}

DefineStruct::DefineStruct(std::string ns, std::vector<std::pair<Type*, std::string>> m) : nameStruct(ns), members(m) {
    StructType* structType = new StructType(nameStruct, members); //todo channge the logi of private member of DefineStruct, complete useless now, just remove and use this object like private for the codegen
    for(auto st : symbolStructsType){
        if(st == structType || structType->equalName(*st)){
            delete structType;
            throw std::runtime_error("The Struct has already been defined: \n" + st->toString());
        }
    } 
    symbolStructsType.push_back(static_cast<StructType*>(structType->clone()));
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

    structType->setLLVMType(pointType);
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

VarStructUpdt::VarStructUpdt(std::string nv, std::string nm, Expr *v) : nameVar(nv), NameMember(nm), value(v) {}

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
    size_t i = 0;
    bool checkMember = false;
    Type* memberType;
    for(auto member : type->getMembers()){
        if(member.second == NameMember){
           checkMember = true;
           memberType = member.first;
           break; 
        }
        i++;
    }
    if(!checkMember) 
        throw std::runtime_error(
            "member: " +
            NameMember +
            " is not a member from the '"+
            type->getNameStruct()+"' struct"
        );

    llvm::Value* fieldIGEP = builder.CreateStructGEP(type->getLLVMType(ctx), ptrToStruct, i, NameMember);
    Value* memberValue = value->codegen(builder);

    if(!(*memberType == *memberValue->getType())){
        if(!memberValue->getType()->isCastTo(memberType)){
            throw std::runtime_error(
                "The type of struct member '" +
                NameMember + "' (expected '" +
                memberType->toString() +
                    "') is not compatible with the provided value of type '" + 
                memberValue->getType()->toString() + "'"
            );
        }
        Value* newVal = memberValue->castTo(memberType, builder);
        delete memberValue;
        memberValue = newVal;
    }
    /*
    if(!(*memberType == *memberValue->getType())){
        throw std::runtime_error(
            "The type of struct member '" +
            NameMember + "' (expected '" +
            memberType->toString() +
                "') is not compatible with the provided value of type '" + 
            memberValue->getType()->toString() + "'"
        );
    }*/
    builder.CreateStore(memberValue->getLLVMValue(), fieldIGEP);
    delete memberValue;
}

Function::Function(Type* tf,const std::string nf, const std::vector<std::pair<Type*, std::string>> p,
    std::vector<Statement*> b) : typeFunc(tf), nameFunc(nf), parameters(p) ,body(b) {}

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
    llvm::Function* function = module->getFunction(nameFunc);

    if (function) throw std::runtime_error("Redefinition of function: " + nameFunc); //&& function->getFunctionType() == funcType

    function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, nameFunc, module);

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(builder.getContext(), "entry", function);
    builder.SetInsertPoint(entry);
    symbolTable.emplace_back();

    llvm::Function::arg_iterator argIt = function->arg_begin();
    for (const auto& param : parameters) {
        llvm::Argument* arg = &*argIt++;
        arg->setName(param.second);
        if (arg->getType()->isPointerTy()) {
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
        nameFunc,
        SymbolFunction{typeFunc->clone(), argTypes, function}
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

Return::Return(Expr* e, std::string fn) : expr(e), funcName(fn) {}

void Return::codegen(llvm::IRBuilder<> &builder) {
    Value* retVal = expr->codegen(builder);
    Type* returnType;
    for (const auto& func : symbolFunctions) {
        if (func.first == funcName) {  
            returnType = func.second.returnType->clone();
            break;
        }
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
    /*if(!(*val->getType() == *type)){
        throw std::runtime_error(
            "Updated value of variable '"+
            nameVar+
            "' not compatible with the type of the variable itself"
        );
    }*/
    if(!(*val->getType() == *type)){
            if(!val->getType()->isCastTo(type)){
                throw std::runtime_error(
                "Type mismatch for variable '" + 
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
                "Type mismatch for variable '" + 
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
    // If Cast is performed, the block may be different from the saved one, so continue on the current one
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