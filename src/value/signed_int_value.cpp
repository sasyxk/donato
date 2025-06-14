#include "signed_int_value.h"
#include "bool_value.h"
#include "runtime_errors.h"

SignedIntValue::SignedIntValue(Type* type) {
    this->type = type;
}

Type* SignedIntValue::getType() const {
    return type;
}

llvm::Value* SignedIntValue::getLLVMValue() const {
    return value;
}

void extendValue(
    llvm::IRBuilder<>& builder,
    llvm::Value*& left,
    llvm::Value*& right,
    Type* leftType,
    Type* rightType
){
    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Type* i64Ty = llvm::Type::getInt64Ty(ctx);

    if (auto* lt = dynamic_cast<SignedIntType*>(leftType); lt && lt->getBits() < 64) {
        left = builder.CreateSExt(left, i64Ty, "sext_lhs");
    }

    if (auto* rt = dynamic_cast<SignedIntType*>(rightType); rt && rt->getBits() < 64) {
        right = builder.CreateSExt(right, i64Ty, "sext_rhs");
    }

    return;
}

Value* SignedIntValue::add(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (auto* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        auto* otherValue = dynamic_cast<const SignedIntValue*>(other);
        llvm::Value* left = this->getLLVMValue();
        llvm::Value* right = otherValue->getLLVMValue();
        Type* leftType = this->getType();
        Type* rightType = otherValue->getType();
        extendValue(builder, left, right, leftType, rightType);
        Type* newType = TypeManager::instance().getSignedIntType(64);
        llvm::Value* result;
        if(CompilerFlags::instance().overflowCheck){
            std::string name = "addtmp";
            result = Value::createCheckedIntegerArithmetic(
                llvm::Intrinsic::sadd_with_overflow,
                left,
                right,
                builder,
                name + "_ok",
                name + "_overflow"
            );
        }
        else{
            result = builder.CreateAdd(left, right, "addtmp");
        }
        return newType->createValue(result, ctx);
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
        llvm::Value* left = this->getLLVMValue();
        llvm::Value* right = otherValue->getLLVMValue();
        Type* leftType = this->getType();
        Type* rightType = otherValue->getType();
        extendValue(builder, left, right, leftType, rightType);
        llvm::Value* result;
        Type* newType = TypeManager::instance().getSignedIntType(64);
        if(CompilerFlags::instance().overflowCheck){ 
            std::string name = "subtmp";
            result = Value::createCheckedIntegerArithmetic(
                llvm::Intrinsic::ssub_with_overflow,
                left,
                right,
                builder,
                name + "_ok",
                name + "_overflow"
            );
        }
        else{
            result = builder.CreateSub(left, right, "subtmp");
        }
        return newType->createValue(result, ctx);
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
        llvm::Value* left = this->getLLVMValue();
        llvm::Value* right = otherValue->getLLVMValue();
        Type* leftType = this->getType();
        Type* rightType = otherValue->getType();
        extendValue(builder, left, right, leftType, rightType);
        llvm::Value* result;
        Type* newType = TypeManager::instance().getSignedIntType(64);
        if(CompilerFlags::instance().overflowCheck){ 
            std::string name = "multmp";
            result = Value::createCheckedIntegerArithmetic(
                llvm::Intrinsic::smul_with_overflow,
                left,
                right,
                builder,
                name + "_ok",
                name + "_overflow"
            );            
        }
        else {
            result = builder.CreateMul(left, right, "multmp");
        }
        return newType->createValue(result, ctx);
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
        extendValue(builder, lhs, rhs,  this->getType(), other->getType());
        if(CompilerFlags::instance().overflowCheck){
            llvm::Type* i64Ty = llvm::Type::getInt64Ty(ctx);

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
        }
        Type* newType = TypeManager::instance().getSignedIntType(64);
        llvm::Value* result = builder.CreateSDiv(lhs, rhs, "divtmp");
        
        return newType->createValue(result, ctx); 
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
            Type* boolType = TypeManager::instance().getBoolType();
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

Value* SignedIntValue::neq(Value* other, llvm::IRBuilder<>& builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if (const SignedIntType* otherType = dynamic_cast<const SignedIntType*>(other->getType())) {
        if (*this->getType() == *other->getType()) {
            llvm::Value* result = builder.CreateICmpNE(this->getLLVMValue(), other->getLLVMValue(), "netmp");
            Type* boolType = TypeManager::instance().getBoolType();
            return boolType->createValue(result, ctx);
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
            Type* boolType = TypeManager::instance().getBoolType();
            return boolType->createValue(result, ctx);
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
            Type* boolType = TypeManager::instance().getBoolType();
            return boolType->createValue(result, ctx);
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
            Type* boolType = TypeManager::instance().getBoolType();
            return boolType->createValue(result, ctx);
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
            Type* boolType = TypeManager::instance().getBoolType();
            return boolType->createValue(result, ctx);
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

    llvm::Type* i64Ty = llvm::Type::getInt64Ty(ctx);
    llvm::Value* value = this->getLLVMValue();
    if (auto* lt = dynamic_cast<SignedIntType*>(this->getType()); lt && lt->getBits() < 64) {
        value = builder.CreateSExt(value, i64Ty, "sext_lhs");
    }
    Type* newType = TypeManager::instance().getSignedIntType(64);
    llvm::Value* result;
    if(CompilerFlags::instance().overflowCheck){
        llvm::Value* zero = llvm::ConstantInt::get(i64Ty, 0);
        result = createCheckedIntegerArithmetic(
            llvm::Intrinsic::ssub_with_overflow,
            zero,  // 0 - x
            value,
            builder,
            "negtmp_ok",
            "negtmp_overflow"
        );
    }
    else {
        result = builder.CreateNeg(value, "negtmp");
    }
    return newType->createValue(result, ctx);
}

Value* SignedIntValue::getBoolValue(llvm::IRBuilder<> &builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Value* result = builder.CreateICmpNE(
        this->getLLVMValue(),
        llvm::ConstantInt::get(ctx,
        llvm::APInt(32, 0)),
        "ifconf"
    );
    Type* boolType = TypeManager::instance().getBoolType();
    return boolType->createValue(result, ctx);
}

 
Value *SignedIntValue::castTo(Type *other, llvm::IRBuilder<> &builder) {
    llvm::LLVMContext& ctx = builder.getContext();

    if ( SignedIntType* otherType = dynamic_cast< SignedIntType*>(other)) {
        const SignedIntType* thisType = dynamic_cast<const SignedIntType*>(this->getType());
        bool bitCond = thisType->getBits() > otherType->getBits();
        llvm::Value* result;
        Type* type = otherType;
        llvm::Value* val = this->getLLVMValue();
        llvm::Type* dstTy = otherType->getLLVMType(ctx);
        if (bitCond && CompilerFlags::instance().truncateEnabled) {
           
            llvm::Type* srcTy = val->getType();
            
            // result is the truncated value
            result = builder.CreateTrunc(val, dstTy, "trunc_val"); 
            llvm::Value* reextended = builder.CreateSExt(result, srcTy, "reext_val");
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
        
        } 
        else if(!bitCond){
            result = builder.CreateSExt(val, dstTy, "sext_left_casting");
        }
        else{ // CompilerFlags::instance().truncateEnabled == false && bitCond
            result = builder.CreateTrunc(val, dstTy, "trunc_val");
        }
        return type->createValue(result, ctx);
    }
    throw std::runtime_error(
        "Unsupported cast Operation: " +
        this->getType()->toString() +
        " to " +
        other->toString()
    );
}
