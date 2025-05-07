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