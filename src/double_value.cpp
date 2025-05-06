#include "double_value.h"
#include "bool_value.h"

DoubleValue::DoubleValue(Type* type, llvm::Value* value, llvm::LLVMContext &ctx) {
    Value::checkTypeCompatibility(type, value, ctx);
    this->type = type;
    this->value = value;
}

Type* DoubleValue::getType() const {
    return type;
}

llvm::Value* DoubleValue::getLLVMValue() const {
    return value;
}

Value* DoubleValue::add(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())){
        llvm::Value* result = builder.CreateFAdd(this->getLLVMValue(), other->getLLVMValue(), "addtmp");
        return new DoubleValue(this->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for addition " + 
        this->getType()->toString() +
        " !+ " + 
        other->getType()->toString());
}

Value* DoubleValue::sub(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())) {
        llvm::Value* result = builder.CreateFSub(this->getLLVMValue(), other->getLLVMValue(), "subtmp");
        return new DoubleValue(this->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for subtraction " +
        this->getType()->toString() +
        " !- " +
        other->getType()->toString());
}

Value* DoubleValue::mul(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())) {
        llvm::Value* result = builder.CreateFMul(this->getLLVMValue(), other->getLLVMValue(), "multmp");
        return new DoubleValue(this->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for multiplication " +
        this->getType()->toString() +
        " !* " +
        other->getType()->toString());
}

Value* DoubleValue::div(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())) {
        llvm::Value* result = builder.CreateFDiv(this->getLLVMValue(), other->getLLVMValue(), "divtmp");
        return new DoubleValue(this->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for division " +
        this->getType()->toString() +
        " !/ " +
        other->getType()->toString());
}

Value* DoubleValue::eq(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())) {
        llvm::Value* result = builder.CreateFCmpOEQ(this->getLLVMValue(), other->getLLVMValue(), "eqtmp");
        return new BoolValue(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for equality comparison: " +
        this->getType()->toString() +
        " == " +
        other->getType()->toString());
}

Value* DoubleValue::neq(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())) {
        llvm::Value* result = builder.CreateFCmpONE(this->getLLVMValue(), other->getLLVMValue(), "netmp");
        return new BoolValue(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for inequality comparison: " +
        this->getType()->toString() +
        " != " +
        other->getType()->toString());
}

Value* DoubleValue::lt(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())) {
        llvm::Value* result = builder.CreateFCmpOLT(this->getLLVMValue(), other->getLLVMValue(), "ltmp");
        return new BoolValue(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for less-than comparison: " +
        this->getType()->toString() +
        " < " +
        other->getType()->toString());
}

Value* DoubleValue::lte(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())) {
        llvm::Value* result = builder.CreateFCmpOLE(this->getLLVMValue(), other->getLLVMValue(), "leqtmp");
        return new BoolValue(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for less-than-or-equal comparison: " +
        this->getType()->toString() +
        " <= " +
        other->getType()->toString());
}

Value* DoubleValue::gt(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())) {
        llvm::Value* result = builder.CreateFCmpOGT(this->getLLVMValue(), other->getLLVMValue(), "gtmp");
        return new BoolValue(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for greater-than comparison: " +
        this->getType()->toString() +
        " > " +
        other->getType()->toString());
}

Value* DoubleValue::gte(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(other->getType())) {
        llvm::Value* result = builder.CreateFCmpOGE(this->getLLVMValue(), other->getLLVMValue(), "geqtmp");
        return new BoolValue(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for greater-than-or-equal comparison: " +
        this->getType()->toString() +
        " >= " +
        other->getType()->toString());
}

Value* DoubleValue::neg(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Value* result = builder.CreateFNeg(this->getLLVMValue(), "negtmp");
    return new DoubleValue(this->getType()->clone(), result, ctx);
}

Value* DoubleValue::getBoolValue(llvm::IRBuilder<> &builder)
{
    llvm::Value* result = builder.CreateFCmpONE(
        this->getLLVMValue(),
        llvm::ConstantFP::get(builder.getContext(),
        llvm::APFloat(0.0)),
        "ifconf"
    );

    return new BoolValue(new BoolType(), result, builder.getContext());
}
