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

configure_file(${CMAKE_SOURCE_DIR}/src/codegen/output_eval.c ${CMAKE_BINARY_DIR}/output_eval.c COPYONLY)

file(GLOB_RECURSE SOURCES
    "src/*.cpp"  
    "src/ast/*.cpp"          
    "src/parser/*.cpp"       
    "src/type/*.cpp"         
    "src/codegen/*.cpp"      
    "src/value/*.cpp"        
)

# Add executable
add_executable(dtc ${SOURCES})

# Add the "include" directory to the include paths
target_include_directories(dtc PRIVATE ${CMAKE_SOURCE_DIR}/src)

target_include_directories(dtc PRIVATE
    ${CMAKE_SOURCE_DIR}/src          
    ${CMAKE_SOURCE_DIR}/src/codegen  
    ${CMAKE_SOURCE_DIR}/src/ast     
    ${CMAKE_SOURCE_DIR}/src/parser   
    ${CMAKE_SOURCE_DIR}/src/value 
    ${CMAKE_SOURCE_DIR}/src/type  
    ${CMAKE_SOURCE_DIR}/src/error_handling    
    ${LLVM_INCLUDE_DIRS}
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