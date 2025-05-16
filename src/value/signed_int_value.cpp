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

std::tuple<llvm::Value*, llvm::Value*, bool> SignedIntValue::promoteOperands(
    Value* left,
    Value* right,
    llvm::IRBuilder<>& builder
){
    llvm::LLVMContext& ctx = builder.getContext();

    const SignedIntType* leftType = dynamic_cast<const SignedIntType*>(left->getType());
    const SignedIntType* rightType = dynamic_cast<const SignedIntType*>(right->getType()); 

    if (leftType->getBits() > rightType->getBits()) {
        llvm::Value* newRight = builder.CreateSExt(right->getLLVMValue(), leftType->getLLVMType(ctx), "sext_right");
        return {left->getLLVMValue(), newRight, true};
    } else if (leftType->getBits() < rightType->getBits()) {
        llvm::Value* newLeft = builder.CreateSExt(left->getLLVMValue(), rightType->getLLVMType(ctx), "sext_left_aura");
        return {newLeft, right->getLLVMValue(), false};
    } else {
        return {left->getLLVMValue(), right->getLLVMValue(), true};
    } 
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

Value* SignedIntValue::div(Value* other, llvm::IRBuilder<>& builder) { //todo try to fix using the 64
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
            llvm::Value* newValue = builder.CreateTrunc(this->getLLVMValue(), otherType->getLLVMType(ctx), "tronc_right");
            return new SignedIntValue(otherType->clone(), newValue, ctx);
        } else if (thisType->getBits() < otherType->getBits()) {
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
