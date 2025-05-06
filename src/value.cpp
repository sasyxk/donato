#include "value.h"
#include "double_value.h"
#include "bool_value.h"

void Value::checkTypeCompatibility(Type* type, llvm::Value* value, llvm::LLVMContext& ctx) {
    if(type->getLLVMType(ctx) != value->getType()) {
        std::string expectedStr;
        llvm::raw_string_ostream expectedOS(expectedStr);
        type->getLLVMType(ctx)->print(expectedOS);

        std::string actualStr;
        llvm::raw_string_ostream actualOS(actualStr);
        value->getType()->print(actualOS);

        throw std::runtime_error(
            "Type mismatch:\nExpected: " + expectedOS.str() + 
            "\nActual: " + actualOS.str() +
            "\nType*: '" + type->toString() + "'"
        );
    }
}

Value* Value::createValue(Type *type, llvm::Value *llvmVal, llvm::LLVMContext &ctx)
{
    if (dynamic_cast<DoubleType*>(type)) {
        return new DoubleValue(type, llvmVal, ctx);
    } 
    else if (dynamic_cast<BoolType*>(type)) {
        return new BoolValue(type, llvmVal, ctx);
    } 
    throw std::runtime_error("Unsupported type in Value::createValue: " + type->toString());
}

/*
Value* addValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) { 
        llvm::Value* result = builder.CreateFAdd(l->getLLVMValue(), r->getLLVMValue(), "addtmp");
        return new Value(l->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for addition " + 
        l->getType()->toString() +
        " !+ " + 
        r->getType()->toString());
}

Value* subValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) {
        llvm::Value* result = builder.CreateFSub(l->getLLVMValue(), r->getLLVMValue(), "subtmp");
        return new Value(l->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for subtraction " + 
        l->getType()->toString() +
        " !- " + 
        r->getType()->toString());
}

Value* mulValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) {
        llvm::Value* result = builder.CreateFMul(l->getLLVMValue(), r->getLLVMValue(), "multmp");
        return new Value(l->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for multiplication " + 
        l->getType()->toString() +
        " !* " + 
        r->getType()->toString());
}

Value* divValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) {
        llvm::Value* result = builder.CreateFDiv(l->getLLVMValue(), r->getLLVMValue(), "divtmp");
        return new Value(l->getType()->clone(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for division " + 
        l->getType()->toString() +
        " !/ " + 
        r->getType()->toString());
}

Value* negValue(const Value* v, llvm::IRBuilder<>& builder){
    llvm::LLVMContext& ctx = builder.getContext();
    if (dynamic_cast<const DoubleType*>(v->getType())){
        llvm::Value* result = builder.CreateFNeg(v->getLLVMValue(), "negtmp");
        return new Value(v->getType()->clone(), result, ctx);
    }
    throw std::runtime_error(
        "Unsupported types for negation " + 
        v->getType()->toString() +
        " !-x ");
}

Value* eqValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) {
        llvm::Value* result = builder.CreateFCmpOEQ(l->getLLVMValue(), r->getLLVMValue(), "eqtmp");
        return new Value(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for equality comparison: " +
        l->getType()->toString() + 
        " == " + 
        r->getType()->toString());
}

Value* neqValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) {
        llvm::Value* result = builder.CreateFCmpONE(l->getLLVMValue(), r->getLLVMValue(), "netmp");
        return new Value(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for inequality comparison: " +
        l->getType()->toString() + 
        " != " + 
        r->getType()->toString());
}

Value* ltValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) {
        llvm::Value* result = builder.CreateFCmpOLT(l->getLLVMValue(), r->getLLVMValue(), "ltmp");
        return new Value(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for less-than comparison: " +
        l->getType()->toString() +
        " < " +
        r->getType()->toString());
}

Value* lteValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) {
        llvm::Value* result = builder.CreateFCmpOLE(l->getLLVMValue(), r->getLLVMValue(), "leqtmp");
        return new Value(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for less-than-or-equal comparison: " +
        l->getType()->toString() +
        " <= " + 
        r->getType()->toString());
}

Value* gtValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) {
        llvm::Value* result = builder.CreateFCmpOGT(l->getLLVMValue(), r->getLLVMValue(), "gtmp");
        return new Value(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for greater-than comparison: " +
        l->getType()->toString() +
        " > " +
        r->getType()->toString());
}

Value* gteValues(const Value* l, const Value* r, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (dynamic_cast<const DoubleType*>(l->getType()) &&
        dynamic_cast<const DoubleType*>(r->getType())) {
        llvm::Value* result = builder.CreateFCmpOGE(l->getLLVMValue(), r->getLLVMValue(), "geqtmp");
        return new Value(new BoolType(), result, ctx);
    }

    throw std::runtime_error(
        "Unsupported types for greater-than-or-equal comparison: " +
        l->getType()->toString() +
        " >= " +
        r->getType()->toString());
}
*/