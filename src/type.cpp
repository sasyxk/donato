#include "type.h"
#include "llvm/IR/IRBuilder.h"

//Type-----------------------------------------------
Type* Type::mapLLVMTypeToType(llvm::Type* llvmType){
    
    if (llvmType->isDoubleTy()) {
        return new DoubleType();
    } else if (llvmType->isIntegerTy(1)) {
        return new BoolType();
    }

    throw std::runtime_error("Unsupported map llvm:Type to Type");
    
}


// DoubleType----------------------------------------
llvm::Type *DoubleType::getLLVMType(llvm::LLVMContext &ctx) const {
    return llvm::Type::getDoubleTy(ctx);
}

bool DoubleType::operator==(const Type &other) const {
    return dynamic_cast<const DoubleType*>(&other) != nullptr;
}

// BoolType------------------------------------------
llvm::Type *BoolType::getLLVMType(llvm::LLVMContext &ctx) const {
    return llvm::Type::getInt1Ty(ctx);
}

bool BoolType::operator==(const Type &other) const {
    return dynamic_cast<const BoolType*>(&other) != nullptr;
}
