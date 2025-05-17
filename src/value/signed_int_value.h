#pragma once
#include "value.h"
#include "type.h"

class SignedIntValue : public Value {
    Type* type;
    llvm::Value* value;
public:
    SignedIntValue(Type* type, llvm::Value* value, llvm::LLVMContext &ctx);
    ~SignedIntValue() override {
        delete type;
    };

    Type* getType() const override;
    llvm::Value* getLLVMValue() const override;

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
