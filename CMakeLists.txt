cmake_minimum_required(VERSION 3.10)
project(dtc)

cmake_policy(SET CMP0075 NEW)

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

configure_file(${CMAKE_SOURCE_DIR}/src/output_eval.c ${CMAKE_BINARY_DIR}/output_eval.c COPYONLY)

# Add executable
add_executable(dtc 
    src/main.cpp 
    src/parser.cpp 
    src/tokenizer.cpp 
    src/ast.cpp 
    src/codegen.cpp
    src/type.cpp
    src/value.cpp
    src/double_value.cpp
    src/bool_value.cpp
    src/signed_int_value.cpp
    src/struct_value.cpp
)

# Link all libraries
target_link_libraries(dtc 
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