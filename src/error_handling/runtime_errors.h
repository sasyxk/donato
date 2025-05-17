#ifndef RUNTIME_ERRORS_H
#define RUNTIME_ERRORS_H

typedef enum {
    ERROR_DIVISION_BY_ZERO = 1,
    ERROR_SIGNED_OVERFLOW  = 2,
    ERROR_TRUNCATION_LOSS  = 3
} RuntimeErrorCode;

void llvm_error(int code);

#endif
