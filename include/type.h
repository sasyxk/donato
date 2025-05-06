#pragma once
#include <string>
#include "llvm/IR/IRBuilder.h"

class Type {
public:
    virtual ~Type() = default;

    static Type* mapLLVMTypeToType(llvm::Type* llvmType);
    
    virtual bool operator==(const Type& other) const = 0;

    virtual llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const = 0;
    virtual std::string toString() const = 0;
    virtual Type* clone() const = 0;

};

class DoubleType : public Type{
public:
    DoubleType() {};
    ~DoubleType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    std::string toString() const override {return "double";}
    Type* clone() const override { return new DoubleType(*this); }
}; 

class BoolType : public Type{
public:
    BoolType() {};
    ~BoolType() override = default;

    bool operator==(const Type& other) const override;

    llvm::Type* getLLVMType(llvm::LLVMContext& ctx) const override;
    std::string toString() const override {return "bool";}
    Type* clone() const override { return new BoolType(*this); }
}; 




    