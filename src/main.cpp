#include "parser.h"
#include "codegen.h"
#include "llvm/IR/Verifier.h"
#include <iostream>
#include <fstream>
#include <sstream>

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
	
    Tokenizer tokenizer(code);
    Parser parser(tokenizer);
    Statement* ast = parser.parseCode();
    
    // LLVM configuration
    llvm::LLVMContext context;
    module = new llvm::Module("my_module", context);
    llvm::IRBuilder<> builder(context);
    
    // Create main function
    //llvm::FunctionType* funcType = llvm::FunctionType::get(builder.getDoubleTy(), false);
    //llvm::Function* mainFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "eval", &module);
    
    // Code block
    //llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
    //builder.SetInsertPoint(entry);
    

    symbolTable.push_back({});

    // Generate code
    ast->codegen(builder);
    //llvm::Value* result = ast->codegen(builder);
    //builder.CreateRet(result);
    
    // Verify and generate executable
    llvm::verifyModule(*module, &llvm::errs());
    generateExecutable(*module, "output");
    
    return 0;
}