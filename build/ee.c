#include <stdio.h>

double eval();

int main(int argc, char** argv){

    printf("%lf\n", eval());

}

/*
var x = 5;
var y = 2 +  let x = 1, z = 2 in x + 2 - if(z) then z + 1 else x;
if(y - 2) {
    var w = x + 2;
    var c = w - let z = 2 in 2 + z - y,
    if (w + c) {
        return x - 1;
    }
}
else {
    return x + y - 5 + let x =3, y= 1 in x + y;
}
return x;

 ./mycompiler "var x = 5; var y = 2 + (let x = 1, z = 2 in (x + 2 - (if (z) then (z + 1) else x))); if (y - 2) { var w = x + 2; var c = w - (let z = 2 in (2 + z - y)); if (w + c) { return x - 1; } } else { return x + y - 5 + (let x = 3, y = 1 in (x + y)); } return x;"
*/

//./mycompiler "$(cat program.txt)"