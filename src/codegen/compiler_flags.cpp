#include "compiler_flags.h"
#include <charconv>
#include <iostream>
#include <string_view>
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
            case 'O': {
                std::string_view argument(optarg);
                int level = 0;
                const char* end = argument.data() + argument.size();
                auto result = std::from_chars(argument.data(), end, level);
                if (result.ec != std::errc{} || result.ptr != end ||
                    level < 0 || level > 3) {
                    std::cerr << "Error: -O requires a decimal integer between 0 and 3.\n";
                    return false;
                }
                optimizationLevel = level;
                break;
            }
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
