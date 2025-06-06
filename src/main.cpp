#include "parser.h"
#include "codegen.h"
#include "compiler_flags.h"
#include "llvm/IR/Verifier.h"
#include "setup_default_functions.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <iomanip>

llvm::Module* module = nullptr;

static size_t allocation_count = 0;
static size_t allocation_size = 0;

void* operator new(std::size_t size) {
    ++allocation_count;
    allocation_size += size;
    
    if (void* ptr = std::malloc(size))
        return ptr;
    throw std::bad_alloc();
}

void operator delete(void* ptr) noexcept {
    std::free(ptr);
}

void printAllocations(size_t count, size_t size_in_bytes) {
    std::cout << "allocation_count: " << count << "\n";

    constexpr const char* units[] = {"B", "KB", "MB", "GB", "TB"};
    double size = static_cast<double>(size_in_bytes);
    int unit_index = 0;

    while (size >= 1024 && unit_index < 4) {
        size /= 1024;
        ++unit_index;
    }

    std::cout << "allocation_size: " << std::fixed << std::setprecision(2)
              << size << " " << units[unit_index] << "\n";
}

int main(int argc, char** argv) {

    auto& flags = CompilerFlags::instance();

    // Parsing flags and checking input validity
    if (!flags.parseArgs(argc, argv)) {
        return 1;
    }

    std::ifstream file(flags.filename);

    if (!file) {
        std::cerr << "Error opening file: " << flags.filename << "\n";
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string code = buffer.str();
	
    // Parsing configuration
    Tokenizer tokenizer(code);
    Parser parser(tokenizer);
    std::vector<Statement*> ast;
    ast.reserve(10);

    // Generate ast
    try {
        do {
            ast.push_back(parser.parseCode());
        } while (parser.hasMoreTokens());
    } catch (const std::exception& e) {
        std::cerr << "Error in parsing:: " << e.what() << " " << tokenizer.getPos() << "\n"; 
        return 1;
    } 
    
    // LLVM configuration
    llvm::LLVMContext context;
    module = new llvm::Module("donato_program", context);
    llvm::IRBuilder<> builder(context);
    symbolTable.reserve(5);
    symbolTable.push_back({});

    // Generate code for default functions
    setupFunctions(builder, module);

    // Generate code
    try {
        for (Statement* stm : ast) {
            if(stm) stm->codegen(builder);
        }
    } catch (const std::exception& e) {
        std::cerr << "Error in codegen:: " << e.what() << "\n"; 
        return 1;
    } 
    
    // Verify and generate executable
    llvm::verifyModule(*module, &llvm::errs());
    generateExecutable(*module, flags.outputName);


    // Cleanup - clear vectors
    symbolTable.clear();
    symbolStructsType.clear();
    symbolClassType.clear();
    symbolFunctions.clear();

    // Cleanup - deallocating the AST
    for (Statement* stm : ast) {
        delete stm;
    }
    ast.clear();

    delete module;
    
    printAllocations(allocation_count,allocation_size);

    return 0;
}