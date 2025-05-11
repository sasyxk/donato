#include "type.h"
#include "llvm/IR/IRBuilder.h"
#include "double_value.h"
#include "signed_int_value.h"
#include "bool_value.h"

//Type-----------------------------------------------

// DoubleType----------------------------------------
llvm::Type *DoubleType::getLLVMType(llvm::LLVMContext &ctx) const {
    return llvm::Type::getDoubleTy(ctx);
}

Value *DoubleType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx) {
    return new DoubleValue(this->clone(), llvmVal, ctx);
}

bool DoubleType::operator==(const Type &other) const {
    return dynamic_cast<const DoubleType*>(&other) != nullptr;
}

bool DoubleType::isCastTo(Type *other) const {
    return false;
}

//SignedIntType-------------------------------------
SignedIntType::SignedIntType(unsigned bits) {
    if(bits != 8 && bits != 16 && bits != 32 && bits != 64)
        throw std::invalid_argument("Unsupported bit width for SignedIntType: " + std::to_string(bits));
    this->bits = bits;
}

llvm::Type* SignedIntType::getLLVMType(llvm::LLVMContext &ctx) const {
    switch (bits) {
        case 8:
            return llvm::Type::getInt8Ty(ctx);
        case 16:
            return llvm::Type::getInt16Ty(ctx);
        case 32:
            return llvm::Type::getInt32Ty(ctx);
        case 64:
            return llvm::Type::getInt64Ty(ctx);
        default:
            throw std::invalid_argument("Unsupported bit width for SignedIntType: " + std::to_string(bits));
    }
}

Value *SignedIntType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx) {
    return new SignedIntValue(this->clone(), llvmVal, ctx);
}

bool SignedIntType::operator==(const Type &other) const {
    if (const SignedIntType* otherInt = dynamic_cast<const SignedIntType*>(&other)) {
        return otherInt->getBits() == bits;
    }
    return false;
}

bool SignedIntType::isCastTo(Type *other) const {
    if (dynamic_cast<const SignedIntType*>(other)) {
        return true;
    }
    return false;
}

// BoolType------------------------------------------
llvm::Type* BoolType::getLLVMType(llvm::LLVMContext &ctx) const {
    return llvm::Type::getInt1Ty(ctx);
}

Value *BoolType::createValue(llvm::Value *llvmVal, llvm::LLVMContext &ctx) {
    return new BoolValue(this->clone(), llvmVal, ctx);
}

bool BoolType::operator==(const Type &other) const {
    return dynamic_cast<const BoolType*>(&other) != nullptr;
}

bool BoolType::isCastTo(Type *other) const {
    return false;
}


//StructType----------------------------------------
StructType::StructType(std::string ns, std::vector<std::pair<Type*, std::string>> m) {
    this->nameStruct = ns;
    this->members = m;
}

llvm::Type* StructType::getLLVMType(llvm::LLVMContext &ctx) const {
    std::vector<llvm::Type*> llvmMembers;
    for (const auto& [memberType, memberName] : members) {
        llvmMembers.push_back(memberType->getLLVMType(ctx));
    }
    return llvm::StructType::get(ctx, llvmMembers);
}

bool StructType::operator==(const Type &other) const {
    const StructType* otherType = dynamic_cast<const StructType*>(&other);
    if (!otherType)
        return false;

    if(this->nameStruct != otherType->nameStruct){
        return false;
    }
    
    if (members.size() != otherType->members.size())
        return false;

    for (size_t i = 0; i < members.size(); ++i) {
        const auto& [thisType, thisName] = members[i];
        const auto& [otherTypeMember, otherName] = otherType->members[i];

        if (!(*thisType == *otherTypeMember))
            return false;

        if (thisName != otherName)
            return false;
    }

    return true;
}

std::string StructType::toString() const {
    std::string result = "struct " + this->nameStruct + " {\n";
    for(auto member : this->members){
        result += "  " +  member.first->toString() + " " + member.second +";\n";
    }
    result += "}";
    return result;
}

bool StructType::equalName(const StructType &other) {
    if(this->nameStruct == other.nameStruct) return true;
    return false;
}
