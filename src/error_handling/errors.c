#include "runtime_errors.h"

void llvm_error(int code) {
    switch(code) {
        case ERROR_DIVISION_BY_ZERO:
            fprintf(stderr, "Runtime Error: Division by zero\n");
            break;
        case ERROR_SIGNED_OVERFLOW:
            fprintf(stderr, "Runtime Error: Signed overflow detected\n");
            break;
        case ERROR_TRUNCATION_LOSS:
            fprintf(stderr, "Runtime Error: Truncation caused data loss\n");
            break;
        case ERROR_MALLOC_SIZE:
            fprintf(stderr, "Runtime Error: Invalid malloc size or allocation failed\n");
            break;
        default:
            fprintf(stderr, "Runtime Error: Unknown error (%d)\n", code);
    }
    exit(1);
}

void* d_malloc(int64_t size) {
    void* ptr = malloc((size_t)size);
    if (!ptr) {
        llvm_error(ERROR_MALLOC_SIZE);
    }
    return ptr;
}

void d_free(void* ptr) {
    free(ptr);
}