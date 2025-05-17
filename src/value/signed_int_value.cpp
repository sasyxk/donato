#include "signed_int_value.h"
#include "bool_value.h"
#include "runtime_errors.h"

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

llvm::Value* makeOperation(
    const SignedIntValue* l,
    const SignedIntValue* r,
    llvm::Intrinsic::ID op,
    llvm::IRBuilder<>& builder,
    std::string name
    ){
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* lv = l->getLLVMValue();
    llvm::Value* rv = r->getLLVMValue();

    // Estendi a 64 bit se necessario
    llvm::Type* i64Ty = llvm::Type::getInt64Ty(ctx);

    if (auto* lt = dynamic_cast<SignedIntType*>(l->getType()); lt && lt->getBits() < 64) {
        lv = builder.CreateSExt(lv, i64Ty, "sext_lhs");
    }

    if (auto* rt = dynamic_cast<SignedIntType*>(r->getType()); rt && rt->getBits() < 64) {
        rv = builder.CreateSExt(rv, i64Ty, "sext_rhs");
    }

    // Use intrinsic sadd_with_overflow
    llvm::Value* result = Value::createCheckedIntegerArithmetic(
        llvm::Intrinsic::sadd_with_overflow,
        lv,
        rv,
        builder,
        name + "_ok",
        name + "_overflow"
    );

    return result;
}

Value* SignedIntValue::add(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (auto* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {

        auto* otherValue = dynamic_cast<const SignedIntValue*>(other);
        llvm::Value* result = makeOperation(this, otherValue, llvm::Intrinsic::sadd_with_overflow, builder,"addtmp");
        Type* newType = new SignedIntType(64);
        return new SignedIntValue(newType, result, ctx); 
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
        auto* otherValue = dynamic_cast<const SignedIntValue*>(other);
        llvm::Value* result = makeOperation(this, otherValue, llvm::Intrinsic::ssub_with_overflow, builder,"subtmp");
        Type* newType = new SignedIntType(64);
        return new SignedIntValue(newType, result, ctx);
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
        auto* otherValue = dynamic_cast<const SignedIntValue*>(other);
        llvm::Value* result = makeOperation(this, otherValue, llvm::Intrinsic::smul_with_overflow, builder,"multmp");
        Type* newType = new SignedIntType(64);
        return new SignedIntValue(newType, result, ctx);
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
        
        llvm::Value* lhs = this->getLLVMValue();
        llvm::Value* rhs = other->getLLVMValue();
         llvm::Type* i64Ty = llvm::Type::getInt64Ty(ctx);

        if (auto* lt = dynamic_cast<SignedIntType*>(this->getType()); lt && lt->getBits() < 64) {
            lhs = builder.CreateSExt(lhs, i64Ty, "sext_lhs");
        }

        if (auto* rt = dynamic_cast<SignedIntType*>(other->getType()); rt && rt->getBits() < 64) {
            rhs = builder.CreateSExt(rhs, i64Ty, "sext_rhs");
        }

        llvm::Module* module = builder.GetInsertBlock()->getModule();
        llvm::Function* currentFunc = builder.GetInsertBlock()->getParent();

        // Constants: INT_MIN , -1 AND 0
        llvm::Value* intMin = llvm::ConstantInt::get(i64Ty, llvm::APInt::getSignedMinValue(i64Ty->getIntegerBitWidth()));
        llvm::Value* minusOne = llvm::ConstantInt::getSigned(i64Ty, -1);
        llvm::Value* zero = llvm::ConstantInt::get(i64Ty, 0);

        // Division by zero check
        llvm::Value* isZero = builder.CreateICmpEQ(rhs, zero, "iszero");

        // Overflow control division INT_MIN / -1
        llvm::Value* isMin = builder.CreateICmpEQ(lhs, intMin, "ismin");
        llvm::Value* isNegOne = builder.CreateICmpEQ(rhs, minusOne, "isnegone");
        llvm::Value* willOverflow = builder.CreateAnd(isMin, isNegOne, "will_overflow");

        llvm::BasicBlock* divZeroBlock = llvm::BasicBlock::Create(ctx, "div_zero", currentFunc);
        llvm::BasicBlock* overflowBlock = llvm::BasicBlock::Create(ctx, "div_overflow", currentFunc);
        llvm::BasicBlock* okBlock = llvm::BasicBlock::Create(ctx, "div_ok", currentFunc);
        llvm::BasicBlock* errBlock = llvm::BasicBlock::Create(ctx, "div_overflow_error", currentFunc);

        builder.CreateCondBr(isZero, divZeroBlock, overflowBlock);

        // Div Zero Error Block
        builder.SetInsertPoint(divZeroBlock);
        llvm::FunctionCallee errorFn = module->getFunction("llvm_error");
        llvm::Value* errorCodeZero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), ERROR_DIVISION_BY_ZERO);
        builder.CreateCall(errorFn, { errorCodeZero });
        builder.CreateUnreachable();

        // Overflow Block
        builder.SetInsertPoint(overflowBlock);
        builder.CreateCondBr(willOverflow, errBlock, okBlock);

        // Error overflow Block
        builder.SetInsertPoint(errBlock);
        llvm::Value* errorCodeOverflow = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), ERROR_SIGNED_OVERFLOW);
        builder.CreateCall(errorFn, { errorCodeOverflow });
        builder.CreateUnreachable();

        // Continue Block
        builder.SetInsertPoint(okBlock);
        llvm::Value* result = builder.CreateSDiv(lhs, rhs, "divtmp");
        Type* newType = new SignedIntType(64);
        return new SignedIntValue(newType, result, ctx);
        
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

Value* SignedIntValue::neg(llvm::IRBuilder<>& builder) { //todo try to fix this using makeoperation
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


Value *SignedIntValue::castTo(Type *other, llvm::IRBuilder<> &builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other)) {
        const SignedIntType* thisType = dynamic_cast<const SignedIntType*>(this->getType());
        if (thisType->getBits() > otherType->getBits()) {
            //todo Add runtime check for truncation whether or not there is data loss
            /*llvm::Value* newValue = builder.CreateTrunc(this->getLLVMValue(), otherType->getLLVMType(ctx), "tronc_right");
            return new SignedIntValue(otherType->clone(), newValue, ctx);
            */
            llvm::Value* val = this->getLLVMValue();
            llvm::Type* srcTy = val->getType();
            llvm::Type* dstTy = otherType->getLLVMType(ctx);

            llvm::Value* truncated = builder.CreateTrunc(val, dstTy, "trunc_val");
            llvm::Value* reextended = builder.CreateSExt(truncated, srcTy, "reext_val");
            llvm::Value* isSame = builder.CreateICmpEQ(reextended, val, "check_lossless");

            llvm::Function* func = builder.GetInsertBlock()->getParent();
            llvm::BasicBlock* okBlock = llvm::BasicBlock::Create(ctx, "trunc_ok", func);
            llvm::BasicBlock* trapBlock = llvm::BasicBlock::Create(ctx, "trunc_trap", func);

            builder.CreateCondBr(isSame, okBlock, trapBlock);

            builder.SetInsertPoint(trapBlock);
            
            llvm::FunctionCallee errorFn = builder.GetInsertBlock()->getModule()->getFunction("llvm_error");
            llvm::Value* errorCode = llvm::ConstantInt::get(
                llvm::Type::getInt32Ty(ctx),
                ERROR_TRUNCATION_LOSS);

            builder.CreateCall(errorFn, { errorCode });
            builder.CreateUnreachable();

            builder.SetInsertPoint(okBlock);
            return new SignedIntValue(otherType->clone(), truncated, ctx);
        } 
        else if (thisType->getBits() < otherType->getBits()) {
            llvm::Value* newValue = builder.CreateSExt(this->getLLVMValue(), otherType->getLLVMType(ctx), "sext_left_casting");
            return new SignedIntValue(otherType->clone(), newValue, ctx);
        }
    }
    throw std::runtime_error(
        "Unsupported cast Operation: " +
        this->getType()->toString() +
        " to " +
        other->toString()
    );
}
