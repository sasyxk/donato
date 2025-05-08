#include "signed_int_value.h"
#include "bool_value.h"

SignedIntValue::SignedIntValue(Type* type, llvm::Value* value, llvm::LLVMContext &ctx) {
    Value::checkTypeCompatibility(type, value, ctx);
    this->type = type;
    this->value = value;
}

Type* SignedIntValue::getType() const {
    return type;
}

llvm::Value* SignedIntValue::getLLVMValue() const {
    return value;
}

Value* SignedIntValue::add(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            /*llvm::Value* result = builder.CreateAdd(this->getLLVMValue(), other->getLLVMValue(), "addtmp");
            return new SignedIntValue(this->getType()->clone(), result, ctx);*/
            llvm::Value* result = createCheckedIntegerArithmetic(
                llvm::Intrinsic::sadd_with_overflow,
                this->getLLVMValue(),
                other->getLLVMValue(),
                builder,
                "addtmp_ok",
                "addtmp_overflow"
            );
            return new SignedIntValue(this->getType()->clone(), result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for addition " + 
        this->getType()->toString() +
        " !+ " + 
        other->getType()->toString()
    );
}

Value* SignedIntValue::sub(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            /*llvm::Value* result = builder.CreateSub(this->getLLVMValue(), other->getLLVMValue(), "subtmp");
            return new SignedIntValue(this->getType()->clone(), result, ctx);*/
            llvm::Value* result = createCheckedIntegerArithmetic(
                llvm::Intrinsic::ssub_with_overflow,
                this->getLLVMValue(),
                other->getLLVMValue(),
                builder,
                "addtmp_ok",
                "addtmp_overflow"
            );
            return new SignedIntValue(this->getType()->clone(), result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for subtraction " +
        this->getType()->toString() +
        " !- " +
        other->getType()->toString()
    );
}

Value* SignedIntValue::mul(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            /*llvm::Value* result = builder.CreateMul(this->getLLVMValue(), other->getLLVMValue(), "multmp");
            return new SignedIntValue(this->getType()->clone(), result, ctx);*/
            llvm::Value* result = createCheckedIntegerArithmetic(
                llvm::Intrinsic::smul_with_overflow,
                this->getLLVMValue(),
                other->getLLVMValue(),
                builder,
                "addtmp_ok",
                "addtmp_overflow"
            );
            return new SignedIntValue(this->getType()->clone(), result, ctx);
        }
    }
   
    throw std::runtime_error(
        "Unsupported types for multiplication " +
        this->getType()->toString() +
        " !* " +
        other->getType()->toString()
    );
}

Value* SignedIntValue::div(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            llvm::Value* lhs = this->getLLVMValue();
            llvm::Value* rhs = other->getLLVMValue();
            llvm::Type* intTy = lhs->getType();
            llvm::Module* module = builder.GetInsertBlock()->getModule();
            llvm::Function* currentFunc = builder.GetInsertBlock()->getParent();

            // Constants: INT_MIN and -1
            llvm::Value* intMin = llvm::ConstantInt::get(intTy, llvm::APInt::getSignedMinValue(intTy->getIntegerBitWidth()));
            llvm::Value* minusOne = llvm::ConstantInt::getSigned(intTy, -1);

            // Comparisons
            llvm::Value* isMin = builder.CreateICmpEQ(lhs, intMin, "ismin");
            llvm::Value* isNegOne = builder.CreateICmpEQ(rhs, minusOne, "isnegone");
            llvm::Value* willOverflow = builder.CreateAnd(isMin, isNegOne, "will_overflow");

            llvm::BasicBlock* okBlock = llvm::BasicBlock::Create(ctx, "div_ok", currentFunc);
            llvm::BasicBlock* errBlock = llvm::BasicBlock::Create(ctx, "div_overflow", currentFunc);

            builder.CreateCondBr(willOverflow, errBlock, okBlock);

            // Error Block
            builder.SetInsertPoint(errBlock);
            builder.CreateCall(llvm::Intrinsic::getDeclaration(module, llvm::Intrinsic::trap));
            builder.CreateUnreachable();

            // Continue Block
            builder.SetInsertPoint(okBlock);
            llvm::Value* result = builder.CreateSDiv(this->getLLVMValue(), other->getLLVMValue(), "divtmp");
            return new SignedIntValue(this->getType()->clone(), result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for division " +
        this->getType()->toString() +
        " !/ " +
        other->getType()->toString()
    );
}

Value* SignedIntValue::eq(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            llvm::Value* result = builder.CreateICmpEQ(this->getLLVMValue(), other->getLLVMValue(), "eqtmp");
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

Value* SignedIntValue::neq(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            llvm::Value* result = builder.CreateICmpNE(this->getLLVMValue(), other->getLLVMValue(), "netmp");
            return new BoolValue(new BoolType(), result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for inequality comparison: " +
        this->getType()->toString() +
        " != " +
        other->getType()->toString()
    );
}

Value* SignedIntValue::lt(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            llvm::Value* result = builder.CreateICmpSLT(this->getLLVMValue(), other->getLLVMValue(), "ltmp");
            return new BoolValue(new BoolType(), result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for less-than comparison: " +
        this->getType()->toString() +
        " < " +
        other->getType()->toString()
    );
}

Value* SignedIntValue::lte(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            llvm::Value* result = builder.CreateICmpSLE(this->getLLVMValue(), other->getLLVMValue(), "leqtmp");
            return new BoolValue(new BoolType(), result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for less-than-or-equal comparison: " +
        this->getType()->toString() +
        " <= " +
        other->getType()->toString()
    );
}

Value* SignedIntValue::gt(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            llvm::Value* result = builder.CreateICmpSGT(this->getLLVMValue(), other->getLLVMValue(), "gtmp");
            return new BoolValue(new BoolType(), result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for greater-than comparison: " +
        this->getType()->toString() +
        " > " +
        other->getType()->toString()
    );
}

Value* SignedIntValue::gte(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            llvm::Value* result = builder.CreateICmpSGE(this->getLLVMValue(), other->getLLVMValue(), "geqtmp");
            return new BoolValue(new BoolType(), result, ctx);
        }
    }

    throw std::runtime_error(
        "Unsupported types for greater-than-or-equal comparison: " +
        this->getType()->toString() +
        " >= " +
        other->getType()->toString()
    );
}

Value* SignedIntValue::neg(llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    //llvm::Value* result = builder.CreateNeg(this->getLLVMValue(), "negtmp");
    llvm::Value* zero = llvm::ConstantInt::get(this->getType()->getLLVMType(ctx), 0);
    llvm::Value* result = createCheckedIntegerArithmetic(
        llvm::Intrinsic::ssub_with_overflow,
        zero,  // 0 - x
        this->getLLVMValue(),
        builder,
        "negtmp_ok",
        "negtmp_overflow"
    );
    return new SignedIntValue(this->getType()->clone(), result, ctx);
}

Value* SignedIntValue::getBoolValue(llvm::IRBuilder<> &builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Value* result = builder.CreateICmpNE(
        this->getLLVMValue(),
        llvm::ConstantInt::get(ctx,
        llvm::APInt(32, 0)),
        "ifconf"
    );
    return new BoolValue(new BoolType(), result, ctx);
}
