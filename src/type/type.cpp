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
    llvm::Type* baseType = llvm::Type::getDoubleTy(ctx); 
    if (pointer) {
        return llvm::PointerType::getUnqual(baseType);
    }
    return baseType;
}

Value *DoubleType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx) {
    return new DoubleValue(this->clone(), llvmVal, ctx);
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
    if (pointer) {
        return llvm::PointerType::getUnqual(baseType);
    }
    return baseType;
}

Value *SignedIntType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx) {
    return new SignedIntValue(this->clone(), llvmVal, ctx);
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
    llvm::Type* baseType = llvm::Type::getInt1Ty(ctx);
    if (pointer) {
        return llvm::PointerType::getUnqual(baseType);
    }
    return baseType;
}

Value *BoolType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx) {
    return new BoolValue(this->clone(), llvmVal, ctx);
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
Value* VoidType::createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) {
    throw std::runtime_error("VoidType does not support createValue.");
}

bool VoidType::isCastTo(Type* other) const {
    return false;
}

bool VoidType::isPointer() const {
    throw std::runtime_error("VoidType does not support 'isPointer()'");
}

void VoidType::setPointer(bool ptr) {
    throw std::runtime_error("VoidType does not support 'setPointer()'");
}

//StructType----------------------------------------
StructType::StructType(std::string ns, std::vector<std::pair<Type*, std::string>> m) {
    this->nameStruct = ns;
    this->members = m;
}

Type* StructType::clone() const {
    std::vector<std::pair<Type*, std::string>> clonedMembers;
    for (const auto& [typePtr, name] : this->members) {
        clonedMembers.push_back({typePtr->clone(), name});
    }
    StructType* newStructType = new StructType(this->nameStruct, clonedMembers);
    newStructType->setPointer(this->pointer);
    
    return newStructType;
}

llvm::Type* StructType::getLLVMType(llvm::LLVMContext &ctx) const { 
    llvm::StructType* structType = llvm::StructType::getTypeByName(ctx, this->nameStruct);
    if(pointer){
        return llvm::PointerType::getUnqual(structType);
    }
    return structType;
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

Value* StructType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx) {
    return new StructValue(this->clone(), llvmVal, ctx);
}

//ClassType----------------------------------------

ClassType::ClassType(StructType* structType, std::vector<std::string> nameFunctions) {
    this->structType = structType;
    this->nameFunctions = nameFunctions;
}

Type* ClassType::clone() const {
    ClassType* newClassType = new ClassType(static_cast<StructType* >(this->structType->clone()), this->nameFunctions);
    return newClassType;
}

llvm::Type* ClassType::getLLVMType(llvm::LLVMContext &ctx) const {
    return this->structType->getLLVMType(ctx);
}

Value *ClassType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx) {
    return new StructValue(this->clone(), llvmVal, ctx);
    //return new BoolValue(new BoolType, llvmVal, ctx);  //todo fix
    throw std::invalid_argument("Unsupported ClassType::createValue");
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
    llvm::Type* baseType = llvm::PointerType::getUnqual(typePointed->getLLVMType(ctx));
    if (pointer) {
        return llvm::PointerType::getUnqual(baseType);
    }
    return baseType;
}

Value* PointerType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx) {
    return new PointerValue(this->clone(), llvmVal, ctx);
}

bool PointerType::operator==(const Type &other) const {
    if(auto pointerType = dynamic_cast<const PointerType*>(&other)){
        if(*this->getTypePointed() == *pointerType->getTypePointed()){ // f(pointer == false && other.isPointer() == true){ ,mmmmmm
            return true;
        }
    }

    if(*this->getTypePointed() == other){
        if(!pointer && other.isPointer()){
            return true;
        }
    }
    return false;
}

bool PointerType::isCastTo(Type *other) const {
    return false;
}

Type* PointerType::clone() const {
    auto* p = new PointerType(typePointed->clone());
    p->setPointer(pointer);
    return p;
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
    //const StructType* otherType = dynamic_cast<const StructType*>(&other);

    /*Degug
    if(auto cc = dynamic_cast<StructType*>(this->symbolTypeREF)){
        llvm::outs() << "cc->getMembersSize(): " <<cc->getMembersSize() << "\n"; 
        for(auto& [type, name] : cc->getMembers()){
            llvm::outs() << "name: " << name << "  type: " << type->toString() << "\n";
            if(auto cc2 = dynamic_cast<PointerType*>(type)){
                auto cc3 = dynamic_cast<SpecialType*>(cc2->getTypePointed());
                auto cc4 = dynamic_cast<StructType*> (cc3->symbolTypeREF);
                llvm::outs() << "cc3->getMembersSize(): " <<cc4->getMembersSize() << "\n"; 
                for(auto& [type, name] : cc4->getMembers()){
                    llvm::outs() << "3name: " << name << "  type: " << type->toString() << "\n";
                }
            }
        }
    }*/
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
    return this->symbolTypeREF->getLLVMType(ctx);
}

Value* SpecialType::createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) {
    return this->symbolTypeREF->createValue(llvmVal, ctx);
}

bool SpecialType::isCastTo(Type* other) const {
    return this->symbolTypeREF->isCastTo(other);
}

bool SpecialType::isPointer() const {
    throw std::runtime_error("SpecialType does not support 'isPointer()'");
}

void SpecialType::setPointer(bool ptr) {
    throw std::runtime_error("SpecialType does not support 'setPointer()'");
}
