#pragma once

#include <string>
#include <unordered_map>
#include "type.h"

class TypeManager {
public:
    static TypeManager& instance();

    ClassType* createClassType(const std::string& name);
    StructType* createStructType(const std::string& name);

    BoolType* getBoolType() const { return boolType; }
    DoubleType* getDoubleType() const { return doubleType; }
    SignedIntType* getSignedIntType(int size) const;
    VoidType* getVoidType() const {return voidType;}
    ClassType* getClassType(const std::string& name) const;
    StructType* getStructType(const std::string& name) const;
    PointerType* getPointerType(Type* pointedType);

private:
    TypeManager();
    ~TypeManager();

    TypeManager(const TypeManager&) = delete;
    TypeManager& operator=(const TypeManager&) = delete;

    std::unordered_map<std::string, ClassType*> classTypes;
    std::unordered_map<std::string, StructType*> structTypes;
    std::unordered_map<Type*, PointerType*> pointerTypes;

    BoolType* boolType = nullptr;
    DoubleType* doubleType = nullptr;
    SignedIntType* signedIntTypes[4] = {nullptr};
    VoidType* voidType = nullptr;
};
