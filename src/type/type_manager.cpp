#include "type_manager.h"

TypeManager& TypeManager::instance() {
    static TypeManager inst;
    return inst;
}

TypeManager::TypeManager() {
    boolType = new BoolType();
    doubleType = new DoubleType();
    voidType = new VoidType();

    signedIntTypes[0] = new SignedIntType(8);
    signedIntTypes[1] = new SignedIntType(16);
    signedIntTypes[2] = new SignedIntType(32);
    signedIntTypes[3] = new SignedIntType(64);
}

SignedIntType* TypeManager::getSignedIntType(int size) const {
    switch (size) {
        case 8: return signedIntTypes[0];
        case 16: return signedIntTypes[1];
        case 32: return signedIntTypes[2];
        default: return signedIntTypes[3];
    }
}

TypeManager::~TypeManager() {
    delete boolType;
    delete doubleType;

    for (int i = 0; i < 4; ++i) {
        delete signedIntTypes[i];
    }

    for (auto& p : classTypes) delete p.second;
    for (auto& p : structTypes) delete p.second;
    for (auto& p : pointerTypes) delete p.second;
}

ClassType* TypeManager::createClassType(const std::string& name) {
    auto it = classTypes.find(name);
    if (it != classTypes.end()) {
        throw std::runtime_error("The Class has already been defined: " + name);
    }

    StructType* structType = new StructType(name);
    ClassType* newClass = new ClassType(structType);
    classTypes[name] = newClass;
    return newClass;
}

ClassType* TypeManager::getClassType(const std::string& name) const {
    auto it = classTypes.find(name);
    if (it != classTypes.end()) {
        return it->second;
    }
    throw std::runtime_error("The class '" + name + "'has not been defined: ");
}

StructType* TypeManager::createStructType(const std::string& name) {
    auto it = structTypes.find(name);
    if (it != structTypes.end()) {
        throw std::runtime_error("The Struct has already been defined: " + name);
    }

    StructType* newStruct = new StructType(name);
    structTypes[name] = newStruct;
    return newStruct;
}

StructType* TypeManager::getStructType(const std::string& name) const {
    auto it = structTypes.find(name);
    if (it != structTypes.end()) {
        return it->second;
    }
    throw std::runtime_error("The struct '" + name + "' has not been defined.");
}

PointerType* TypeManager::getPointerType(Type* pointedType) {
    auto it = pointerTypes.find(pointedType);
    if (it != pointerTypes.end()) {
        return it->second;
    }

    PointerType* newPointerType = new PointerType(pointedType);
    pointerTypes[pointedType] = newPointerType;
    return newPointerType;
}