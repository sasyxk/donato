#include "type.h"
#include <stdexcept>
#include <llvm/IR/DerivedTypes.h>


#include <limits>
#include <cstdint>
#include <cstdlib>
#include <stdexcept>
#include <regex>

Type* Type::bestFitForString(const std::string& stringValue) {
    // Double detection
    if (stringValue.find('.') != std::string::npos) {
        // Try parsing as double
        try {
            std::stod(stringValue);
            return new DoubleType();
        } catch (...) {
            throw std::runtime_error("Invalid floating-point stringValue: " + stringValue);
        }
    }

    // Integer stringValue
    try {
        bool isNegative = !stringValue.empty() && stringValue[0] == '-';

        if (isNegative) {
            int64_t val = std::stoll(stringValue);
            if (val >= std::numeric_limits<int8_t>::min() && val <= std::numeric_limits<int8_t>::max())
                return new IntType(8, true);
            if (val >= std::numeric_limits<int16_t>::min() && val <= std::numeric_limits<int16_t>::max())
                return new IntType(16, true);
            if (val >= std::numeric_limits<int32_t>::min() && val <= std::numeric_limits<int32_t>::max())
                return new IntType(32, true);
            return new IntType(64, true);
        } else {
            uint64_t val = std::stoull(stringValue);
            if (val <= std::numeric_limits<uint8_t>::max())
                return new IntType(8, false);
            if (val <= std::numeric_limits<uint16_t>::max())
                return new IntType(16, false);
            if (val <= std::numeric_limits<uint32_t>::max())
                return new IntType(32, false);
            return new IntType(64, false);
        }
    } catch (...) {
        throw std::runtime_error("Invalid integer stringValue: " + stringValue);
    }
}

Type* Type::fromString(const std::string& str) {
    if(str == "int" || str == "i32") return new IntType(32, true);
    if(str == "uint" || str == "u32") return new IntType(32, false);
    if(str == "double") return new DoubleType();
    if(str == "bool") return new BoolType();
   // if(str == "void") return new VoidType();

    throw std::runtime_error("Unknown type: " + str);
}

Type* Type::operateWith(llvm::IRBuilder<> &builder, Type *left, Type *right, const std::string &op)
{
    if (!left->canOperateWith(*right, op)) {
        throw std::runtime_error("Cannot operate from " + left->toString() + " to " + right->toString() + " with '" + op + "'");
    }

    llvm::LLVMContext& ctx = builder.getContext();
    llvm::Value* castedVal = left->getLLVMValue();

    if (!(*left == *right)) {
        if (!left->canCastTo(*right)) {
            if (!right->canCastTo(*left)) {
                throw std::runtime_error("Unsupported cast from " + left->toString() + " to " + right->toString());
            }
            std::swap(left, right);
        }
        // The value is cast only if the two types are not equal
        castedVal = left->generateCast(builder, left->getLLVMValue(), *right);
    }

    return right->makeOperation(builder, castedVal, op);
}

//  IntType -----------------------------------------------------------------------------------------

bool IntType::operator==(const Type &other) const {
    const IntType* otherType = dynamic_cast<const IntType*>(&other);
    if (!otherType) return false;
    return bitWidth == otherType->bitWidth && signedFlag == otherType->signedFlag;
}

llvm::Type* IntType::toLLVMType(llvm::LLVMContext& ctx) const {
    return llvm::IntegerType::get(ctx, bitWidth);
}

bool IntType::canCastTo(const Type& other) const { //Understand well how to manage if dream signed or unsigned !!!!
    if(auto intTy = dynamic_cast<const IntType*>(&other)) {
        return bitWidth <= intTy->bitWidth;
    }
    return dynamic_cast<const DoubleType*>(&other) != nullptr;
}

bool IntType::canOperateWith(const Type& other, const std::string& op) const {
    if(op == "+" || op == "-" || op == "*" || op == "/") {
        return dynamic_cast<const IntType*>(&other) || 
               dynamic_cast<const DoubleType*>(&other);
    }
    if(op == "==" || op == "!=" || op == "<" || op == ">" || op == ">=" || op == "<=") {
        return dynamic_cast<const IntType*>(&other) != nullptr;
    }
    return false;
}

std::string IntType::toString() const {
    return (signedFlag ? "int" : "uint") + std::to_string(bitWidth);
}

llvm::Value *IntType::generateCast(llvm::IRBuilder<> &builder, llvm::Value *val, const Type &to) const
{
    if (!this->canCastTo(to)) {
        throw std::runtime_error("Cannot cast from " + this->toString() + " to " + to.toString());
    }

    llvm::LLVMContext& ctx = builder.getContext();

    // Same types, return the value
    if (this->toLLVMType(ctx) == to.toLLVMType(ctx)) {
        return val;
    }

    if (auto intTo = dynamic_cast<const IntType*>(&to)) {
        if (this->getSize() < intTo->getSize()) {
            // Cast from smaller int to larger int
            if (this->isSigned()) {
                // Signed -> any 
                return builder.CreateSExt(val, to.toLLVMType(ctx));
            } else {
                // Unsigned -> any 
                return builder.CreateZExt(val, to.toLLVMType(ctx));
            }
        }
        else {
            // Cast from larger int to smaller int
            return builder.CreateTrunc(val, to.toLLVMType(ctx));
        }
    } 
    else if (auto doubleTo = dynamic_cast<const DoubleType*>(&to)) {
        // Cast from int to double
        if (this->isSigned()) {
            return builder.CreateSIToFP(val, doubleTo->toLLVMType(ctx));
        } 
        else {
            return builder.CreateUIToFP(val, doubleTo->toLLVMType(ctx)); 
        }
    }

    throw std::runtime_error("Unsupported cast from " + this->toString() + " to " + to.toString()); // <- delete??
}

Type* IntType::makeOperation(llvm::IRBuilder<> &builder, llvm::Value *val, const std::string &op)
{
    llvm::Value* L = this->getLLVMValue();
    llvm::Value* R = val;
    llvm::Value* result = nullptr;
    
    if (op == "+") 
        result = builder.CreateAdd(L, R, "addtmp");
    else if (op == "-") 
        result = builder.CreateSub(L, R, "subtmp");
    else if (op == "*") 
        result = builder.CreateMul(L, R, "multmp");
    else if (op == "/") 
        result = signedFlag ? builder.CreateSDiv(L, R, "divtmp") : builder.CreateUDiv(L, R, "divtmp");
    
    if(result){
        this->setLLVMValue(result);
        return this;
    }
    
    // FOR BOOLEAN COMPARISON
    if (op == "==")
        result = builder.CreateICmpEQ(L, R, "eqtmp");
    else if (op == "!=")
        result = builder.CreateICmpNE(L, R, "netmp");
    else if (op == "<")
        result = signedFlag ? builder.CreateICmpSLT(L, R, "ltmp") : builder.CreateICmpULT(L, R, "ltmp");
    else if (op == "<=")
        result = signedFlag ? builder.CreateICmpSLE(L, R, "leqtmp") : builder.CreateICmpULE(L, R, "leqtmp");
    else if (op == ">")
        result = signedFlag ? builder.CreateICmpSGT(L, R, "gtmp") : builder.CreateICmpUGT(L, R, "gtmp");
    else if (op == ">=")
        result = signedFlag ? builder.CreateICmpSGE(L, R, "geqtmp") : builder.CreateICmpUGE(L, R, "geqtmp");
    else
        throw std::runtime_error("Unknown operator: " + op);

    Type* t = Type::fromString("bool");
    t->setLLVMValue(result);
    return t;
}

// DoubleType -----------------------------------------------------------------------------------------

bool DoubleType::operator==(const Type &other) const {
    return dynamic_cast<const DoubleType*>(&other) != nullptr;
}

llvm::Type* DoubleType::toLLVMType(llvm::LLVMContext& ctx) const {
    return llvm::Type::getDoubleTy(ctx);
}

bool DoubleType::canCastTo(const Type& other) const {
    return dynamic_cast<const DoubleType*>(&other) != nullptr;
}

bool DoubleType::canOperateWith(const Type& other, const std::string& op) const {
    if(op == "+" || op == "-" || op == "*" || op == "/") {
        return dynamic_cast<const IntType*>(&other) || 
               dynamic_cast<const DoubleType*>(&other);
    }
    if(op == "==" || op == "!=" || op == "<" || op == ">" || op == ">=" || op == "<=") {
        return dynamic_cast<const DoubleType*>(&other)!= nullptr;
    }
    return false;
}

llvm::Value *DoubleType::generateCast(llvm::IRBuilder<> &builder, llvm::Value *val, const Type &to) const
{
    if (!this->canCastTo(to)) {
        throw std::runtime_error("Cannot cast from " + this->toString() + " to " + to.toString());
    }

    llvm::LLVMContext& ctx = builder.getContext();

    // Same types, return the value
    if (this->toLLVMType(ctx) == to.toLLVMType(ctx)) {
        return val;
    }

    // No implementation of casting from double to something else yet

    throw std::runtime_error("Unsupported cast from " + this->toString() + " to " + to.toString());
}

Type* DoubleType::makeOperation(llvm::IRBuilder<> &builder, llvm::Value *val, const std::string &op) {
    llvm::Value* L = this->getLLVMValue();
    llvm::Value* R = val;
    llvm::Value* result = nullptr;

    if (op == "+") 
        result = builder.CreateFAdd(L, R, "addtmp");
    else if (op == "-")
        result = builder.CreateFSub(L, R, "subtmp");
    else if (op == "*")
        result = builder.CreateFMul(L, R, "multmp");
    else if (op == "/")
        result = builder.CreateFDiv(L, R, "divtmp");

    if (result) {
        this->setLLVMValue(result);
        return this;
    }

    if (op == "==")
        result = builder.CreateFCmpOEQ(L, R, "eqtmp");
    else if (op == "!=")
        result = builder.CreateFCmpONE(L, R, "netmp");
    else if (op == "<")
        result = builder.CreateFCmpOLT(L, R, "ltmp");
    else if (op == "<=")
        result = builder.CreateFCmpOLE(L, R, "leqtmp");
    else if (op == ">")
        result = builder.CreateFCmpOGT(L, R, "gtmp");
    else if (op == ">=")
        result = builder.CreateFCmpOGE(L, R, "geqtmp");
    else
        throw std::runtime_error("Unknown operator: " + op);

    Type* t = Type::fromString("bool");
    t->setLLVMValue(result);
    llvm::outs() << "Valore Dopo il " << op << ": ";
    t->getLLVMValue()->getType()->print(llvm::outs());
    llvm::outs() << "\n";
    return t;
}

// BoolType

bool BoolType::operator==(const Type &other) const {
    return dynamic_cast<const BoolType*>(&other) != nullptr;
}

llvm::Type* BoolType::toLLVMType(llvm::LLVMContext& ctx) const {
    return llvm::Type::getInt1Ty(ctx);
}

bool BoolType::canCastTo(const Type& other) const {
    return dynamic_cast<const BoolType*>(&other) != nullptr;
}

bool BoolType::canOperateWith(const Type& other, const std::string& op) const {
    return (op == "==" || op == "!=") && dynamic_cast<const BoolType*>(&other);
}

llvm::Value *BoolType::generateCast(llvm::IRBuilder<> &builder, llvm::Value *val, const Type &to) const
{
    if (!this->canCastTo(to)) {
        throw std::runtime_error("Cannot cast from " + this->toString() + " to " + to.toString());
    }

    llvm::LLVMContext& ctx = builder.getContext();

    // Same types, return the value
    if (this->toLLVMType(ctx) == to.toLLVMType(ctx)) {
        return val;
    }

    // No implementation of casting from double to something else yet

    throw std::runtime_error("Unsupported cast from " + this->toString() + " to " + to.toString());
}

Type* BoolType::makeOperation(llvm::IRBuilder<> &builder, llvm::Value *val, const std::string &op) {
    llvm::Value* L = this->getLLVMValue();
    llvm::Value* R = val;
    llvm::Value* result = nullptr;
    
    if (op == "==")
        result = builder.CreateICmpEQ(L, R, "eqtmp");
    else if (op == "!=")
        result = builder.CreateICmpNE(L, R, "netmp");
    else
        throw std::runtime_error("Unsupported operator for bool: " + op);

    this->setLLVMValue(result);
    return this;
}

/*
// VoidType
llvm::Type* VoidType::toLLVMType(llvm::LLVMContext& ctx) const {
    return llvm::Type::getVoidTy(ctx);
}

bool VoidType::canCastTo(const Type& other) const {
    return false;
}

bool VoidType::canOperateWith(const Type& other, const std::string& op) const {
    return false;
}

llvm::Value *VoidType::generateCast(llvm::IRBuilder<> &builder, llvm::Value *val, const Type &to) const
{
    return nullptr;
}
*/