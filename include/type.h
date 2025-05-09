#pragma once
#include <string>
#include "llvm/IR/IRBuilder.h"

class Value;

class Type {
public:
    virtual ~Type() = default;
    
    virtual bool operator==(const Type& other) const = 0;

    virtual llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const = 0;
    virtual Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) = 0;
    virtual std::string toString() const = 0;
    virtual Type* clone() const = 0;

};

class DoubleType : public Type{
public:
    DoubleType() {};
    ~DoubleType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) override;
    std::string toString() const override {return "double";}
    Type* clone() const override { return new DoubleType(*this); }
}; 

class SignedIntType  : public Type{
    unsigned bits;
public:
    SignedIntType (unsigned bits);
    ~SignedIntType () override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) override;
    std::string toString() const override { return "int" + std::to_string(bits); }
    Type* clone() const override { return new SignedIntType(*this); }
    unsigned getBits() const {return bits;}
}; 

class BoolType : public Type{
public:
    BoolType() {};
    ~BoolType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    Value* createValue(llvm::Value* llvmVal, llvm::LLVMContext& ctx) override;
    std::string toString() const override {return "bool";}
    Type* clone() const override { return new BoolType(*this); }
}; 




    