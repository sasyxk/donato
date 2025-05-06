#pragma once
#include "type.h"
//#include "double_value.h"
//#include "bool_value.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"

class Value {
public:
    virtual ~Value() = default;

    static void checkTypeCompatibility(Type* type, llvm::Value* value, llvm::LLVMContext& ctx);
    static Value* createValue(Type* type, llvm::Value* llvmVal, llvm::LLVMContext& ctx);

    virtual Type* getType() const = 0;
    virtual llvm::Value* getLLVMValue() const = 0;

    virtual Value* add(Value* other, llvm::IRBuilder<>& builder) = 0;  // +
    virtual Value* sub(Value* other, llvm::IRBuilder<>& builder) = 0;  // -
    virtual Value* mul(Value* other, llvm::IRBuilder<>& builder) = 0;  // *
    virtual Value* div(Value* other, llvm::IRBuilder<>& builder) = 0;  // /
    virtual Value* eq(Value* other,  llvm::IRBuilder<>& builder) = 0;  // ==
    virtual Value* neq(Value* other, llvm::IRBuilder<>& builder) = 0;  // !=
    virtual Value* lt(Value* other,  llvm::IRBuilder<>& builder) = 0;  // <
    virtual Value* lte(Value* other, llvm::IRBuilder<>& builder) = 0;  // <=
    virtual Value* gt(Value* other,  llvm::IRBuilder<>& builder) = 0;  // >
    virtual Value* gte(Value* other, llvm::IRBuilder<>& builder) = 0;  // >=

    virtual Value* neg(llvm::IRBuilder<>& builder) = 0;  // -x
    virtual Value* getBoolValue(llvm::IRBuilder<>& builder) = 0; 

};