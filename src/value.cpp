#include "value.h"
#include "double_value.h"
#include "bool_value.h"
#include "signed_int_value.h"

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
    else if (const SignedIntType* intType = dynamic_cast<const SignedIntType*>(type)) {
        return new SignedIntValue(type, llvmVal, ctx);
    } 

    throw std::runtime_error("Unsupported type in Value::createValue: " + type->toString());
}

llvm::Value *Value::createCheckedIntegerArithmetic(
    llvm::Intrinsic::ID op,
    llvm::Value* l,
    llvm::Value* r,
    llvm::IRBuilder<>& builder,
    const std::string& okBlockName,
    const std::string& errorBlockName
) {
    llvm::LLVMContext& ctx = builder.getContext();

    llvm::Type* intTy = l->getType();
    llvm::Module* module = builder.GetInsertBlock()->getModule();
    llvm::Function* currentFunction = builder.GetInsertBlock()->getParent();

    llvm::Function* intrinsic = llvm::Intrinsic::getDeclaration(module, op, intTy);
    llvm::Value* resultStruct = builder.CreateCall(intrinsic, { l, r }, okBlockName + "_with_overflow");

    llvm::Value* result = builder.CreateExtractValue(resultStruct, 0, "result");
    llvm::Value* overflow = builder.CreateExtractValue(resultStruct, 1, "overflow");

    llvm::BasicBlock* okBlock = llvm::BasicBlock::Create(ctx, okBlockName, currentFunction);
    llvm::BasicBlock* errorBlock = llvm::BasicBlock::Create(ctx, errorBlockName, currentFunction);

    builder.CreateCondBr(overflow, errorBlock, okBlock);

    // Error Block
    builder.SetInsertPoint(errorBlock);
    builder.CreateCall(llvm::Intrinsic::getDeclaration(module, llvm::Intrinsic::trap));
    builder.CreateUnreachable();

    // Continue Block
    builder.SetInsertPoint(okBlock);
    return result;
}