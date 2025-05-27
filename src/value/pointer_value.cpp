#include "pointer_value.h"
#include "bool_value.h"

PointerValue::PointerValue(Type* type, llvm::Value* value, llvm::LLVMContext &ctx) {
    Value::checkTypeCompatibility(type, value, ctx);
    this->type = type;
    this->value = value;
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
            return new BoolValue(new BoolType(), result, ctx);
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
    throw std::runtime_error("neq comparison is not supported for pointer values.");
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
    return new BoolValue(this->getType()->clone(), result, ctx);
}

Value* PointerValue::getBoolValue(llvm::IRBuilder<> &builder) {
    return new BoolValue(new BoolType(), this->getLLVMValue(), builder.getContext());
}

Value *PointerValue::castTo(Type *other, llvm::IRBuilder<> &builder) {
    throw std::runtime_error("Impossible cast BoolValue");
}
