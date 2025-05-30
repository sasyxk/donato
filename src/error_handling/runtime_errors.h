#ifndef RUNTIME_ERRORS_H
#define RUNTIME_ERRORS_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>


typedef enum {
    ERROR_DIVISION_BY_ZERO = 1,
    ERROR_SIGNED_OVERFLOW  = 2,
    ERROR_TRUNCATION_LOSS  = 3,
    ERROR_MALLOC_SIZE      = 4
} RuntimeErrorCode;

void llvm_error(int code);

void* d_malloc(int64_t size);

void d_free(void* ptr);

#endif
