#pragma once

#include <string>

class CompilerFlags {
public:
    static CompilerFlags& instance();

    bool parseArgs(int argc, char** argv);

    // Flag
    int optimizationLevel = 0;
    bool truncateEnabled = false;
    bool overflowCheck = false;
    std::string outputName = "output";
    std::string filename;

private:
    CompilerFlags() = default;
    CompilerFlags(const CompilerFlags&) = delete;
    CompilerFlags& operator=(const CompilerFlags&) = delete;
};