#include "bool_value.h"

BoolValue::BoolValue(Type* type, llvm::Value* value, llvm::LLVMContext &ctx) {
    Value::checkTypeCompatibility(type, value, ctx);
    this->type = type;
    this->value = value;
}

Type* BoolValue::getType() const {
    return type;
}

llvm::Value* BoolValue::getLLVMValue() const {
    return value;
}

Value* BoolValue::add(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const BoolType*>(other->getType())) {
        llvm::Value* result = builder.CreateOr(this->getLLVMValue(), other->getLLVMValue(), "addbooltmp");
        return new BoolValue(this->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for addition (logical or): " +
        this->getType()->toString() +
        " !+ " +
        other->getType()->toString());
}

Value* BoolValue::sub(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Substraction is not supported for boolean values.");
}

Value* BoolValue::mul(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const BoolType*>(other->getType())) {
        llvm::Value* result = builder.CreateAnd(this->getLLVMValue(), other->getLLVMValue(), "mulbooltmp");
        return new BoolValue(this->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for multiplication (logical and): " +
        this->getType()->toString() +
        " !* " +
        other->getType()->toString());
}

Value* BoolValue::div(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    throw std::runtime_error("Division is not supported for boolean values.");
}


Value* BoolValue::eq(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const BoolType*>(other->getType())) {
        llvm::Value* result = builder.CreateICmpEQ(this->getLLVMValue(), other->getLLVMValue(), "eqbooltmp");
        return new BoolValue(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for equality comparison: " +
        this->getType()->toString() +
        " == " +
        other->getType()->toString());
}

Value* BoolValue::neq(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const BoolType*>(other->getType())) {
        llvm::Value* result = builder.CreateICmpNE(this->getLLVMValue(), other->getLLVMValue(), "neqbooltmp");
        return new BoolValue(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for inequality comparison: " +
        this->getType()->toString() +
        " != " +
        other->getType()->toString());
}

Value* BoolValue::lt(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Less-than comparison is not supported for boolean values.");
}

Value* BoolValue::lte(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Less-than-or-equal comparison is not supported for boolean values.");
}

Value* BoolValue::gt(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Greater-than comparison is not supported for boolean values.");
}

Value* BoolValue::gte(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Greater-than-or-equal comparison is not supported for boolean values.");
}

Value* BoolValue::neg(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Value* result = builder.CreateNot(this->getLLVMValue(), "negbooltmp");
    return new BoolValue(this->getType()->clone(), result, ctx);
}

Value* BoolValue::getBoolValue(llvm::IRBuilder<> &builder)
{
    return new BoolValue(new BoolType(), this->getLLVMValue(), builder.getContext());
}