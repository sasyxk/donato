#include "type.h"
#include "llvm/IR/IRBuilder.h"
#include "double_value.h"
#include "signed_int_value.h"
#include "bool_value.h"
#include "struct_value.h"
#include "pointer_value.h"

// DoubleType----------------------------------------
DoubleType::DoubleType(bool isPointer) {
    pointer = isPointer;
}

llvm::Type *DoubleType::getLLVMType(llvm::LLVMContext &ctx) const {
    return llvm::Type::getDoubleTy(ctx);
}

Value *DoubleType::createValue(llvm::Value* llvmVal, llvm::LLVMContext &ctx, bool isReference) {
    Value* value =  new DoubleValue(this);
    if(isReference){
        value->setAlloca(llvmVal, this, ctx);
        return value;
    }
    value->setLLVMValue(llvmVal, this, ctx);
    return value;
}

bool DoubleType::operator==(const Type &other) const
{
    return dynamic_cast<const DoubleType*>(&other) != nullptr;
}

bool DoubleType::isCastTo(Type *other) const {
    return false;
}

//SignedIntType-------------------------------------
SignedIntType::SignedIntType(unsigned bits, bool isPointer) {
    if(bits != 8 && bits != 16 && bits != 32 && bits != 64)
        throw std::invalid_argument("Unsupported bit width for SignedIntType: " + std::to_string(bits));
    this->bits = bits;
    this->pointer = isPointer;
}

llvm::Type* SignedIntType::getLLVMType(llvm::LLVMContext &ctx) const {
    llvm::Type* baseType;

    switch (bits) {
        case 8:
            baseType = llvm::Type::getInt8Ty(ctx);
            break;
        case 16:
            baseType = llvm::Type::getInt16Ty(ctx);
            break;
        case 32:
            baseType = llvm::Type::getInt32Ty(ctx);
            break;
        case 64:
            baseType = llvm::Type::getInt64Ty(ctx);
            break;
        default:
            throw std::invalid_argument("Unsupported bit width for SignedIntType: " + std::to_string(bits));
    }
    return baseType;
}
Value *SignedIntType::createValue(llvm::Value* llvmVal, llvm::LLVMContext &ctx, bool isReference) {
    Value* value =  new SignedIntValue(this);
    if(isReference){
        value->setAlloca(llvmVal, this, ctx);
        return value;
    }
    value->setLLVMValue(llvmVal, this, ctx);
    return value;
}


bool SignedIntType::operator==(const Type &other) const {
    if (const SignedIntType* otherInt = dynamic_cast<const SignedIntType*>(&other)) {
        return otherInt->getBits() == bits;
    }
    return false;
}

bool SignedIntType::isCastTo(Type *other) const {
    if (dynamic_cast<const SignedIntType*>(other)) {
        return true;
    }
    return false;
}

// BoolType------------------------------------------

BoolType::BoolType(bool isPointer) {
    pointer = isPointer;
}

llvm::Type* BoolType::getLLVMType(llvm::LLVMContext &ctx) const {
    return llvm::Type::getInt1Ty(ctx);
}

Value *BoolType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx, bool isReference) {
    Value* value =  new BoolValue(this);
    if(isReference){
        value->setAlloca(llvmVal, this, ctx);
        return value;
    }
    value->setLLVMValue(llvmVal, this, ctx);
    return value;
}

bool BoolType::operator==(const Type &other) const {
    return dynamic_cast<const BoolType*>(&other) != nullptr;
}

bool BoolType::isCastTo(Type *other) const {
    return false;
}


//VoidType------------------------------------------
VoidType::VoidType() {}

bool VoidType::operator==(const Type& other) const {
    return dynamic_cast<const VoidType*>(&other) != nullptr;
}

llvm::Type* VoidType::getLLVMType(llvm::LLVMContext& ctx) const {
    return llvm::Type::getVoidTy(ctx);
}

// Creazione di un valore VoidValue
Value* VoidType::createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference) {
    throw std::runtime_error("VoidType does not support createValue.");
}

bool VoidType::isCastTo(Type* other) const {
    return false;
}

//StructType----------------------------------------
StructType::StructType(std::string ns, std::vector<std::pair<Type*, std::string>> m) {
    this->nameStruct = ns;
    this->members = m;
}

llvm::Type* StructType::getLLVMType(llvm::LLVMContext &ctx) const { 
    return llvm::StructType::getTypeByName(ctx, this->nameStruct);
}

bool StructType::operator==(const Type &other) const {
    if(auto structType = dynamic_cast<const StructType*>(&other)){
        return structType->nameStruct == this->nameStruct;
    }

    if(auto specialType = dynamic_cast<const SpecialType*>(&other)){
        return specialType->getNameSymbol() == this->nameStruct;
    }
    return false;

    /*
    if (!otherType){
        llvm::outs() << "2)\n";
        return false;
    }
        

    if(this->nameStruct != otherType->nameStruct){
        llvm::outs() << "3)\n";
        return false;
    }
    
    if (members.size() != otherType->members.size()){
        llvm::outs() << "4)\n";
        return false;
    }

    for (size_t i = 0; i < members.size(); ++i) {
        const auto& [thisType, thisName] = members[i];
        const auto& [otherTypeMember, otherName] = otherType->members[i];

        if (!(*thisType == *otherTypeMember)){
            llvm::outs() << "5)\n";
            return false;
        }
           

        if (thisName != otherName){
            llvm::outs() << "6)\n";
            return false;
        }
            
    }
    return true;
    */
}

std::string StructType::toString() const {
    std::string result =  this->nameStruct + "struct";
    return result;
}

bool StructType::equalName(const StructType &other) {
    if(this->nameStruct == other.nameStruct) return true;
    return false;
}

Value* StructType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx, bool isReference) {
    Value* value =  new StructValue(this);
    if(isReference){
        value->setAlloca(llvmVal, this, ctx);
        return value;
    }
    value->setLLVMValue(llvmVal, this, ctx);
    return value;
}

//ClassType----------------------------------------

ClassType::ClassType(StructType* structType, std::vector<std::string> nameFunctions) {
    this->structType = structType;
    this->nameFunctions = nameFunctions;
}

llvm::Type* ClassType::getLLVMType(llvm::LLVMContext &ctx) const {
    return this->structType->getLLVMType(ctx);
}

Value *ClassType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx, bool isReference) {
    Value* value =  new StructValue(this);
    if(isReference){
        value->setAlloca(llvmVal, this, ctx);
        return value;
    }
    value->setLLVMValue(llvmVal, this, ctx);
    return value;
}

bool ClassType::operator==(const Type &other) const {
    if(auto classType = dynamic_cast<const ClassType*>(&other)){
        if(classType->getNameClass() == this->getNameClass())
            return true;
    }

    if(dynamic_cast<const PointerType*>(&other)){
        return other == *this;
    }
    
    return false;
}

bool ClassType::isCastTo(Type *other) const {
    return false;
}

bool ClassType::isFuctionOfClass(std::string nameFunc) {
    for(auto& name : nameFunctions){
        if(name == nameFunc) return true;
    }
    return false;
}

//PointerType--------------------------------------
PointerType::PointerType(Type* typePointed) {
    this->typePointed = typePointed;
}

llvm::Type* PointerType::getLLVMType(llvm::LLVMContext &ctx) const {
    return llvm::PointerType::getUnqual(typePointed->getLLVMType(ctx));
}

Value* PointerType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx, bool isReference) {
    Value* value =  new PointerValue(this);
    if(isReference){
        value->setAlloca(llvmVal, this, ctx);
        return value;
    }
    value->setLLVMValue(llvmVal, this, ctx);
    return value;
}

bool PointerType::operator==(const Type &other) const {
    if(auto pointerType = dynamic_cast<const PointerType*>(&other)){
        if(*this->getTypePointed() == *pointerType->getTypePointed()){
            return true;
        }
    }
    return false;
}

bool PointerType::isCastTo(Type *other) const {
    return false;
}

std::string PointerType::toString() const {
    return "PointerType to " + typePointed->toString();
}

//SpecialType----------------------------------------

SpecialType::SpecialType(std::string nameSymbol, Type* symbolTypeREF) {
    this->nameSymbol = nameSymbol;
    this->symbolTypeREF = symbolTypeREF;
}

bool SpecialType::operator==(const Type& other) const {
    if(auto otherType =  dynamic_cast<const StructType*>(&other)){
        return this->nameSymbol == otherType->getNameStruct();
    }
    if(auto otherType =  dynamic_cast<const ClassType*>(&other)){
        return this->nameSymbol == otherType->getNameClass();
    }
    if(auto otherType = dynamic_cast<const SpecialType*>(&other)){
        return this->nameSymbol == otherType->nameSymbol;
    }

    return false;
}

llvm::Type* SpecialType::getLLVMType(llvm::LLVMContext& ctx) const {
    return  this->symbolTypeREF->getLLVMType(ctx);
}

Value* SpecialType::createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference) {

    return this->symbolTypeREF->createValue(llvmVal, ctx, isReference);
}

bool SpecialType::isCastTo(Type* other) const {
    return this->symbolTypeREF->isCastTo(other);
}
