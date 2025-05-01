cmake_minimum_required(VERSION 3.10)
project(mycompiler)

# Find ZLIB, CURL and ZSTD
find_package(ZLIB REQUIRED)
find_package(CURL REQUIRED)
find_package(zstd REQUIRED)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Add the "include" directory to the include paths
include_directories(${CMAKE_SOURCE_DIR}/include)

# Add executable
add_executable(mycompiler 
    src/main.cpp 
    src/parser.cpp 
    src/ast.cpp 
    src/codegen.cpp
    src/type.cpp
)

# Link all libraries
target_link_libraries(mycompiler 
    LLVMCore 
    LLVMSupport 
    LLVMIRReader 
    LLVMExecutionEngine 
    LLVMX86CodeGen 
    LLVMX86Desc 
    LLVMX86Info
    ${ZLIB_LIBRARIES}
    ${CURL_LIBRARIES}
    $<TARGET_NAME_IF_EXISTS:zstd::libzstd_shared>
)