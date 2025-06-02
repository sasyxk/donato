#include "parser.h"
#include "codegen.h"
#include "llvm/IR/Verifier.h"
#include "setup_default_functions.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

llvm::Module* module = nullptr;

int main(int argc, char** argv) {
    // Check if an argument (the input to compile) has been passed
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <file.donato>\n";
        return 1;
    }

    // Make sure the file name ends with ".donato"
    std::string filename = argv[1];
    const std::string ext = ".donato";
    if (filename.size() < ext.size() || filename.substr(filename.size() - ext.size()) != ext) {
        std::cerr << "Error: file extension must be " << ext << "\n";
        return 1;
    }

    std::ifstream file(argv[1]);
    if (!file) {
        std::cerr << "Error opening file: " << argv[1] << "\n";
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string code = buffer.str();
	
    // Parsing configuration
    Tokenizer tokenizer(code);
    Parser parser(tokenizer);
    std::vector<Statement*> ast;

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
    generateExecutable(*module, "output");

    // Cleanup - deallocation Functions
    /*for (auto& pair : symbolFunctions) {
        delete pair.second; 
    }
    symbolFunctions.clear();*/

    // Cleanup - deallocating Variables
    for (auto& scope : symbolTable) {
        for (auto& [name, info] : scope) {
            delete info.type;
        }
    }
    symbolTable.clear();

    /*
    for(auto& scope : symbolStructsType) {
        delete scope;
    }
    symbolStructsType.clear();
    */

    /*
    for(auto& scope : symbolClassType) {
        delete scope;
    }
    symbolClassType.clear();
    */

    // Cleanup - deallocating the AST
    for (Statement* stm : ast) {
        delete stm;
    }
    ast.clear();
    
    return 0;
}