#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

int64_t eval();

int main(int argc, char** argv){

    int64_t result = eval();
    printf("%" PRId64 "\n", result);
}