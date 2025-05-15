#include <stdio.h>
#include <stdint.h>

int eval();

int main(int argc, char** argv){

    int8_t result = (int8_t)eval();
    printf("%d\n", result); 

}