#include "runtime_errors.h"
#include <stdio.h>
#include <stdlib.h>

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
        default:
            fprintf(stderr, "Runtime Error: Unknown error (%d)\n", code);
    }
    exit(1);
}
