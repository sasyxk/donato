#pragma once
#include "value.h"
#include "type.h"
#include "type_manager.h"

class PointerValue : public Value {
    Type* type;
    llvm::Value* value = nullptr;
    llvm::Value* alloca = nullptr;
public:
    PointerValue(Type* type);
    ~PointerValue() = default;

    Type* getType() const override;
    llvm::Value* getLLVMValue() const override;
    llvm::Value* getAllocation() const override {return alloca;}
    bool isReference() const override {
        if(alloca != nullptr && value == nullptr) return true;
        return false;
    }
    void setAlloca(llvm::Value* value, Type* type, llvm::LLVMContext &ctx) override {
        checkTypeCompatibility(type, value , ctx, true);
        this->alloca = value;
    }
    void setLLVMValue(llvm::Value* value, Type* type, llvm::LLVMContext &ctx) override {
        checkTypeCompatibility(type, value , ctx, false);
        this->value = value;
    }
    void loadLLVMValue(std::string name, llvm::IRBuilder<>& builder) override {
        Value::loadLLVMValueDefault(name, builder, this);
    }

    Value* add(Value* other, llvm::IRBuilder<>& builder) override;
    Value* sub(Value* other, llvm::IRBuilder<>& builder) override;
    Value* mul(Value* other, llvm::IRBuilder<>& builder) override;
    Value* div(Value* other, llvm::IRBuilder<>& builder) override;
    Value* eq(Value* other,  llvm::IRBuilder<>& builder) override;
    Value* neq(Value* other, llvm::IRBuilder<>& builder) override;
    Value* lt(Value* other,  llvm::IRBuilder<>& builder) override;
    Value* lte(Value* other, llvm::IRBuilder<>& builder) override;
    Value* gt(Value* other,  llvm::IRBuilder<>& builder) override;
    Value* gte(Value* other, llvm::IRBuilder<>& builder) override;

    Value* neg(llvm::IRBuilder<>& builder) override;
    Value* getBoolValue(llvm::IRBuilder<>& builder) override;

    Value* castTo(Type* other, llvm::IRBuilder<> &builder) override;
};
