#include "struct_value.h"
#include "bool_value.h"

StructValue::StructValue(Type* type, llvm::Value* value, llvm::LLVMContext &ctx) {
    Value::checkTypeCompatibility(type, value, ctx);
    this->type = type;
    this->value = value;
}

Type* StructValue::getType() const {
    return type;
}

llvm::Value* StructValue::getLLVMValue() const {
    return value;
}

Value* StructValue::add(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported add for Struct");
}

Value* StructValue::sub(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported sub for Struct");
}

Value* StructValue::mul(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported mul for Struct");
}

Value* StructValue::div(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported div for Struct");
}

Value* StructValue::eq(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported eq for Struct");
}

Value* StructValue::neq(Value* other, llvm::IRBuilder<>& builder) {
   throw std::runtime_error("Unsupported neq for Struct");
}

Value* StructValue::lt(Value* other, llvm::IRBuilder<>& builder) {
   throw std::runtime_error("Unsupported lt for Struct");
}

Value* StructValue::lte(Value* other, llvm::IRBuilder<>& builder) {
   throw std::runtime_error("Unsupported lte for Struct");
}

Value* StructValue::gt(Value* other, llvm::IRBuilder<>& builder) {
   throw std::runtime_error("Unsupported gt for Struct");
}

Value* StructValue::gte(Value* other, llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported gte for Struct");
}

Value* StructValue::neg(llvm::IRBuilder<>& builder) {
    throw std::runtime_error("Unsupported neg for Struct");
}

Value* StructValue::getBoolValue(llvm::IRBuilder<> &builder) {
    throw std::runtime_error("Unsupported getBoolValue for Struct");
}

Value *StructValue::castTo(Type *other, llvm::IRBuilder<> &builder) {
   throw std::runtime_error("Unsupported castTo for Struct");
}

