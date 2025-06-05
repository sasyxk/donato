#pragma once
//#include "type.h"
//#include "double_value.h"
//#include "bool_value.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"

class Type;

struct TypeInfo;

class Value {
public:
    virtual ~Value() = default;

    static void checkTypeCompatibility(Type* type, llvm::Value* value, llvm::LLVMContext& ctx, bool alloca);
    static llvm::Value* createCheckedIntegerArithmetic(
        llvm::Intrinsic::ID op,
        llvm::Value* l,
        llvm::Value* r,
        llvm::IRBuilder<>& builder,
        const std::string& okBlockName = "arith_ok",
        const std::string& errorBlockName = "arith_overflow"
    );
    static void loadLLVMValueDefault(std::string& name, llvm::IRBuilder<>& builder, Value* value);

    virtual Type* getType() const = 0;
    virtual llvm::Value* getLLVMValue() const = 0;
    virtual llvm::Value* getAllocation() const = 0;
    virtual bool isReference() const = 0;
    virtual void setAlloca(llvm::Value* alloca, Type* type, llvm::LLVMContext &ctx) = 0;
    virtual void setLLVMValue(llvm::Value* value, Type* type, llvm::LLVMContext &ctx) = 0;
    virtual void loadLLVMValue(std::string name, llvm::IRBuilder<>& builder) = 0;

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
    
    virtual Value* castTo(Type* other, llvm::IRBuilder<> &builder) = 0;

};