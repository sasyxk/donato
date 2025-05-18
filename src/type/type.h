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
    virtual Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) = 0;
    virtual std::string toString() const = 0;
    virtual Type* clone() const = 0;
    virtual bool isCastTo(Type* other) const= 0;
    virtual bool isPointer() const = 0;
    virtual void setPointer(bool ptr) = 0;

};

class DoubleType : public Type{
    bool pointer;
public:
    DoubleType(bool isPointer = false);
    ~DoubleType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) override;
    std::string toString() const override {return "double";}
    Type* clone() const override { return new DoubleType(*this); }
    bool isCastTo(Type* other) const override;
    bool isPointer() const override {return pointer;}
    void setPointer(bool ptr) override {pointer = ptr;}

}; 

class SignedIntType  : public Type{
    unsigned bits;
    bool pointer;
public:
    SignedIntType (unsigned bits, bool isPointer = false);
    ~SignedIntType () override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) override;
    std::string toString() const override { return "int" + std::to_string(bits); }
    Type* clone() const override { return new SignedIntType(*this); }
    bool isCastTo(Type* other) const override;
    bool isPointer() const override {return pointer;}
    void setPointer(bool ptr) override {pointer = ptr;}

    unsigned getBits() const {return bits;}
}; 

class BoolType : public Type{
    bool pointer;
public:
    BoolType(bool isPointer = false);
    ~BoolType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) override;
    std::string toString() const override {return "bool";}
    Type* clone() const override { return new BoolType(*this); }
    bool isCastTo(Type* other) const override;
    bool isPointer() const override {return pointer;}
    void setPointer(bool ptr) override {pointer = ptr;}
}; 

class StructType : public Type{
    std::string nameStruct;
    std::vector<std::pair<Type*, std::string>> members;
    llvm::Type* typeLLVM;
    bool pointer;
public:
    StructType(std::string ns, std::vector<std::pair<Type*, std::string>> m);
    ~StructType() override {
        for(auto memeber : members){
            delete memeber.first;
        }
    }

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) override;//todo
    std::string toString() const override;
    Type* clone() const override;
    bool isCastTo(Type* other) const override {return false;}  //todo
    bool isPointer() const override {return pointer;}
    void setPointer(bool ptr) override {pointer = ptr;}

    bool equalName(const StructType& other);
    std::string getNameStruct() const {return nameStruct;}
    std::vector<std::pair<Type*, std::string>> getMembers() {return members;}
    void setLLVMType(llvm::Type* t) {typeLLVM = t;};
   
};


class ClassType : public Type {
    //std::string nameClass;
    StructType* structType;
    std::vector<std::string> nameFunctions; 
    bool pointer;
public:
    ClassType(StructType* structType, std::vector<std::string> nameFunctions);
    ~ClassType() override {
        delete structType;
    }
    bool operator==(const Type& other) const override {return false;} //to fix

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override {return nullptr;} //to fix
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) override {return nullptr;} //to fix
    std::string toString() const override {return "class" + structType->getNameStruct();}
    Type* clone() const override;
    bool isCastTo(Type* other) const override {return false;} //to fix
    bool isPointer() const override {return pointer;}
    void setPointer(bool ptr) override {pointer = ptr;}

    std::string getNameClass() const {return structType->getNameStruct();}
    //void setLLVMType(llvm::Type* t) {structType->setLLVMType(t);};
};

