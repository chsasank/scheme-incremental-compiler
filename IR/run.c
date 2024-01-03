// run.c
#include <stdio.h>

extern int fun(int, int);

int main(){
    printf("result: %d\n", fun(7, 21));
}