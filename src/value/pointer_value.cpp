#include "pointer_value.h"
#include "bool_value.h"

PointerValue::PointerValue(Type* type) {
    this->type = type;
}

Type* PointerValue::getType() const {
    return type;
}

llvm::Value* PointerValue::getLLVMValue() const {
    return value;
}

Value* PointerValue::add(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Add is not supported for pointer values.");
}

Value* PointerValue::sub(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Substraction is not supported for pointer values.");
}

Value* PointerValue::mul(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("mul is not supported for pointer values.");
}

Value* PointerValue::div(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Division is not supported for pointer values.");
}

Value* PointerValue::eq(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (auto pointerTypeother = dynamic_cast<const PointerType*>(other->getType())) {
        if(*pointerTypeother == *this->getType()){
            llvm::Value* result = builder.CreateICmpEQ(this->getLLVMValue(), other->getLLVMValue(), "eqPointertmp");
            Type* boolType = new BoolType();
            return boolType->createValue(result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for equality comparison: " +
        this->getType()->toString() +
        " == " +
        other->getType()->toString()
    );
}

Value* PointerValue::neq(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (auto pointerTypeother = dynamic_cast<const PointerType*>(other->getType())) {
        if(*pointerTypeother == *this->getType()){
            llvm::Value* result = builder.CreateICmpNE(this->getLLVMValue(), other->getLLVMValue(), "neqPointertmp");
            Type* boolType = new BoolType();
            return boolType->createValue(result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for equality comparison: " +
        this->getType()->toString() +
        " == " +
        other->getType()->toString()
    );
}

Value* PointerValue::lt(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Less-than comparison is not supported for pointer values.");
}

Value* PointerValue::lte(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Less-than-or-equal comparison is not supported for pointer values.");
}

Value* PointerValue::gt(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Greater-than comparison is not supported for pointer values.");
}

Value* PointerValue::gte(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Greater-than-or-equal comparison is not supported for pointer values.");
}

Value* PointerValue::neg(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Value* result = builder.CreateNot(this->getLLVMValue(), "negbooltmp");
    Type* boolType = new BoolType();
    return boolType->createValue(result, ctx);
}

Value* PointerValue::getBoolValue(llvm::IRBuilder<> &builder) {
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* ptr = this->getLLVMValue();
    llvm::Value* result = builder.CreateICmpNE(
        ptr,
        llvm::Constant::getNullValue(ptr->getType()),
        "ptr_non_null"
    );

    Type* boolType = new BoolType();
    return boolType->createValue(result, ctx);
}

Value *PointerValue::castTo(Type *other, llvm::IRBuilder<> &builder) {
    throw std::runtime_error("Impossible cast BoolValue");
}
