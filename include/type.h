#pragma once
#include <string>
#include <memory>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include "llvm/IR/Value.h"
#include "llvm/IR/IRBuilder.h"

class Type  {
public:
    virtual ~Type() = default;

    static Type* bestFitForString(const std::string &stringValue);
    static Type* fromString(const std::string &str);
    static Type* operateWith(llvm::IRBuilder<>& builder, Type* left, Type* right, const std::string& op);

    virtual bool operator==(const Type& other) const = 0;
    virtual Type* clone() const = 0;

    virtual llvm::Type* toLLVMType(llvm::LLVMContext& ctx) const = 0;
    virtual bool isSigned() const = 0;
    virtual bool isPointer() const = 0;
    virtual size_t getSize() const = 0;
    virtual bool canCastTo(const Type& other) const = 0;
    virtual bool canOperateWith(const Type& other, const std::string& op) const = 0;
    virtual std::string toString() const = 0;
    virtual llvm::Value* generateCast(llvm::IRBuilder<>& builder, llvm::Value* val, const Type& to) const = 0;
    virtual llvm::Value* getLLVMValue() const = 0;
    virtual void setLLVMValue(llvm::Value* val) = 0;
    virtual Type* makeOperation(llvm::IRBuilder<>& builder, llvm::Value* val,const std::string &op) = 0;
};

class IntType : public Type {
    unsigned bitWidth;
    bool signedFlag;
    llvm::Value* llvmValue; 
public:
    IntType(unsigned bits, bool isSigned) : bitWidth(bits), signedFlag(isSigned), llvmValue(nullptr){}

    bool operator==(const Type& other) const override;
    Type* clone() const override {return new IntType(*this);}

    llvm::Type* toLLVMType(llvm::LLVMContext& ctx) const override;
    bool isSigned() const override { return signedFlag; }
    bool isPointer() const override { return false; }
    size_t getSize() const override { return bitWidth / 8; }
    bool canCastTo(const Type& other) const override;
    bool canOperateWith(const Type& other, const std::string& op) const override;
    std::string toString() const override;
    llvm::Value* generateCast(llvm::IRBuilder<>& builder, llvm::Value* val, const Type& to) const override;
    llvm::Value* getLLVMValue() const override {return llvmValue;}
    void setLLVMValue(llvm::Value* val) override { llvmValue = val;}
    Type* makeOperation(llvm::IRBuilder<>& builder, llvm::Value* val,const std::string &op) override;
};

class DoubleType : public Type {
    llvm::Value* llvmValue;
public:
    DoubleType() : llvmValue(nullptr){}

    bool operator==(const Type& other) const override;
    Type* clone() const override {return new DoubleType(*this);}

    llvm::Type* toLLVMType(llvm::LLVMContext& ctx) const override;
    bool isSigned() const override { return true; }
    bool isPointer() const override { return false; }
    size_t getSize() const override { return 8; }
    bool canCastTo(const Type& other) const override;
    bool canOperateWith(const Type& other, const std::string& op) const override;
    std::string toString() const override { return "double"; }
    llvm::Value* generateCast(llvm::IRBuilder<>& builder, llvm::Value* val, const Type& to) const override;
    llvm::Value* getLLVMValue() const override {return llvmValue;}
    void setLLVMValue(llvm::Value* val) override { llvmValue = val;}
    Type* makeOperation(llvm::IRBuilder<>& builder, llvm::Value* val,const std::string &op) override;
};

class BoolType : public Type {
    llvm::Value* llvmValue;
public:
    BoolType() : llvmValue(nullptr){}

    bool operator==(const Type& other) const override;
    Type* clone() const override {return new BoolType(*this);}

    llvm::Type* toLLVMType(llvm::LLVMContext& ctx) const override;
    bool isSigned() const override { return false; }
    bool isPointer() const override { return false; }
    size_t getSize() const override { return 1; }
    bool canCastTo(const Type& other) const override;
    bool canOperateWith(const Type& other, const std::string& op) const override;
    std::string toString() const override { return "bool"; }
    llvm::Value* generateCast(llvm::IRBuilder<>& builder, llvm::Value* val, const Type& to) const override;
    llvm::Value* getLLVMValue() const override {return llvmValue;}
    void setLLVMValue(llvm::Value* val) override { llvmValue = val;}
    Type* makeOperation(llvm::IRBuilder<>& builder, llvm::Value* val,const std::string &op) override;
};

/*
class VoidType : public Type {
    llvm::Value* llvmValue;
public:
    llvm::Type* toLLVMType(llvm::LLVMContext& ctx) const override;
    bool isSigned() const override { return false; }
    bool isPointer() const override { return false; }
    size_t getSize() const override { return 0; }
    bool canCastTo(const Type& other) const override;
    bool canOperateWith(const Type& other, const std::string& op) const override;
    std::string toString() const override { return "void"; }
    llvm::Value* generateCast(llvm::IRBuilder<>& builder, llvm::Value* val, const Type& to) const override;
    llvm::Value* getLLVMValue() const override {return llvmValue;}
    void setLLVMValue(llvm::Value* val) override { llvmValue = val;}
    void makeOperation(llvm::IRBuilder<>& builder, llvm::Value* val,const std::string &op) override;
};
*/