#pragma once
#include <string>
#include "llvm/IR/IRBuilder.h"

class Value;
//todo set the poiter defaul value to all class

class Type {
public:
    virtual ~Type() = default;
    
    virtual bool operator==(const Type& other) const = 0;

    virtual llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const = 0;
    virtual Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference = false) = 0;
    virtual std::string toString() const = 0;
    virtual bool isCastTo(Type* other) const= 0;
    virtual llvm::Type* getAllocaType(llvm::LLVMContext& ctx) const = 0;

};

struct TypeInfo {
    Type* type;
    bool isReference;
    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const {
        return isReference ? type->getAllocaType(ctx) : type->getLLVMType(ctx);
    }
};


class DoubleType : public Type{
    bool pointer = false;
public:
    DoubleType(bool isPointer = false);
    ~DoubleType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference = false) override;
    std::string toString() const override {return "double";}
    bool isCastTo(Type* other) const override;
    llvm::Type* getAllocaType(llvm::LLVMContext& ctx) const override {
        llvm::Type* baseType = getLLVMType(ctx); 
        return llvm::PointerType::getUnqual(baseType);
    }

}; 

class SignedIntType  : public Type{
    unsigned bits;
    bool pointer = false;
public:
    SignedIntType (unsigned bits, bool isPointer = false);
    ~SignedIntType () override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference = false) override;
    std::string toString() const override { return "int" + std::to_string(bits); }
    bool isCastTo(Type* other) const override;
    llvm::Type* getAllocaType(llvm::LLVMContext& ctx) const override {
        llvm::Type* baseType = getLLVMType(ctx); 
        return llvm::PointerType::getUnqual(baseType);
    }

    unsigned getBits() const {return bits;}
}; 

class BoolType : public Type{
    bool pointer = false;
public:
    BoolType(bool isPointer = false);
    ~BoolType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference = false) override;
    std::string toString() const override {return "bool";}
    bool isCastTo(Type* other) const override;
    llvm::Type* getAllocaType(llvm::LLVMContext& ctx) const override {
        llvm::Type* baseType = getLLVMType(ctx); 
        return llvm::PointerType::getUnqual(baseType);
    }
}; 

class VoidType : public Type{
public:
    VoidType();
    ~VoidType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference = false) override;
    std::string toString() const override {return "void";}
    bool isCastTo(Type* other) const override;
    llvm::Type* getAllocaType(llvm::LLVMContext& ctx) const override {
        llvm::Type* baseType = getLLVMType(ctx); 
        return llvm::PointerType::getUnqual(baseType);
    }

};

class StructType : public Type{
    std::string nameStruct;
    std::vector<std::pair<Type*, std::string>> members;
    bool pointer = false;
public:
    StructType(std::string nameStruct) { this->nameStruct = nameStruct;}
    StructType(std::string ns, std::vector<std::pair<Type*, std::string>> m);
    ~StructType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference = false) override;
    std::string toString() const override;
    bool isCastTo(Type* other) const override {return false;}
    llvm::Type* getAllocaType(llvm::LLVMContext& ctx) const override {
        llvm::Type* baseType = getLLVMType(ctx); 
        return llvm::PointerType::getUnqual(baseType);
    }

    bool equalName(const StructType& other);
    std::string getNameStruct() const {return nameStruct;}
    std::vector<std::pair<Type*, std::string>> getMembers() {return members;}
    size_t getMembersSize() {return members.size();}

    void setMembers( std::vector<std::pair<Type*, std::string>> members) {this->members = members;}

};

class ClassType : public Type {
    //std::string nameClass;
    StructType* structType;
    std::vector<std::string> nameFunctions;
public:
    ClassType(StructType* structType){ this->structType = structType;}
    ClassType(StructType* structType, std::vector<std::string> nameFunctions);
    ~ClassType() override = default;
    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference = false) override;
    std::string toString() const override {return "class" + structType->getNameStruct();}
    bool isCastTo(Type* other) const override;
    llvm::Type* getAllocaType(llvm::LLVMContext& ctx) const override {
        llvm::Type* baseType = getLLVMType(ctx); 
        return llvm::PointerType::getUnqual(baseType);
    }

    std::string getNameClass() const {return structType->getNameStruct();}
    StructType* getStructType() {return structType;}
    bool isFuctionOfClass(std::string nameFunc);
    
    void setMembers(std::vector<std::pair<Type*, std::string>> members) {this->structType->setMembers(members);}
    void setNameFunctions(std::vector<std::string> nameFunctions) {this->nameFunctions = nameFunctions;}
    std::vector<std::string> GetnameFunctions() {return this->nameFunctions ;} 

};

class PointerType : public Type {
    Type* typePointed;
    bool pointer = false;
public:
    PointerType(Type* typePointed);
    ~PointerType() override = default;
    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx, bool isReference = false) override;
    std::string toString() const override;
    bool isCastTo(Type* other) const override;
    llvm::Type* getAllocaType(llvm::LLVMContext& ctx) const override {
        llvm::Type* baseType = getLLVMType(ctx); 
        return llvm::PointerType::getUnqual(baseType);
    }

    Type* getTypePointed() const {return typePointed;}
};


class SpecialType : public Type {
    Type* symbolTypeREF;
    std::string nameSymbol;
    bool pointer = false;
public:
    SpecialType(std::string nameSymbol,  Type* symbolTypeREF);
    ~SpecialType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx , bool isReference = false) override;
    std::string toString() const override {return "S_" + symbolTypeREF->toString();}
    bool isCastTo(Type* other) const override;
    llvm::Type* getAllocaType(llvm::LLVMContext& ctx) const override {
        llvm::Type* baseType = getLLVMType(ctx); 
        return llvm::PointerType::getUnqual(baseType);
    }

    std::string getNameSymbol() const {return nameSymbol;}
    Type* getSybolREF() const {return symbolTypeREF;}

};


