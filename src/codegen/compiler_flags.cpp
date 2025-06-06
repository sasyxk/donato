#include "compiler_flags.h"
#include <iostream>
#include <unistd.h>   
#include <cstring>    
#include <cstdlib>    

CompilerFlags& CompilerFlags::instance() {
    static CompilerFlags instance;
    return instance;
}

bool CompilerFlags::parseArgs(int argc, char** argv) {

    for (int i = 1; i < argc; ++i) {
        if (std::strcmp(argv[i], "--help") == 0) {
            std::cout << "Usage: " << argv[0] << " [options] <file.donato>\n"
                      << "Options:\n"
                      << "  -O <level>   Optimization level (0–3)\n"
                      << "  -t           Enable truncation error runtime\n"
                      << "  -f           Enable overflow error runtime\n"
                      << "  -o <name>    Output executable name\n"
                      << "  --help       Show this help message\n";
            return false;
        }
    }

    int opt;
    while ((opt = getopt(argc, argv, "O:tfo:")) != -1) {
        switch (opt) {
            case 'O':
                optimizationLevel = std::stoi(optarg);
                if (optimizationLevel < 0 || optimizationLevel > 3) {
                    std::cerr << "Error: optimization level must be between 0 and 3.\n";
                    return false;
                }
                break;
            case 't':
                truncateEnabled = true;
                break;
            case 'f':
                overflowCheck = true;
                break;
            case 'o':
                outputName = optarg;
                break;
            default:
                std::cerr << "Usage: " << argv[0]
                          << " [-O <level>] [-t] [-f] [-o <output>] <file.donato>\n";
                return false;
        }
    }

    // Check if an argument (the input to compile) has been passed
    if (optind >= argc) {
        std::cerr << "Usage: " << argv[0] << " <file.donato>\n";
        return false;
    }

    filename = argv[optind];

    // Make sure the file name ends with ".donato"
    const std::string ext = ".donato";
    if (filename.size() < ext.size() || filename.substr(filename.size() - ext.size()) != ext) {
        std::cerr << "Error: file extension must be " << ext << "\n";
        return false;
    }

    return true;
}
