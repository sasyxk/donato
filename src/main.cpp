#include "parser.h"
#include "codegen.h"
#include "llvm/IR/Verifier.h"
#include <iostream>

int main(int argc, char** argv) {
    // Check if an argument (the input to compile) has been passed
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " \"<expression>\"\n";
        std::cerr << "Example: " << argv[0] << " \"let x = 5 in x * 2\"\n";
        return 1;
    }

    // Read input from the first argument
    std::string code = argv[1];
	
    Tokenizer tokenizer(code);
    Parser parser(tokenizer);
    Statement* ast = parser.parseCode();
    
    // LLVM configuration
    llvm::LLVMContext context;
    llvm::Module module("my_module", context);
    llvm::IRBuilder<> builder(context);
    
    // Create main function
    llvm::FunctionType* funcType = llvm::FunctionType::get(builder.getDoubleTy(), false);
    llvm::Function* mainFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "eval", &module);
    
    // Code block
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(entry);
    

    symbolTable.push_back({});

    // Generate code
    ast->codegen(builder);
    //llvm::Value* result = ast->codegen(builder);
    //builder.CreateRet(result);
    
    // Verify and generate executable
    llvm::verifyModule(module, &llvm::errs());
    generateExecutable(module, "output");
    
    return 0;
}