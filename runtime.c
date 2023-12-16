#include <stdio.h>

#define fixnum_shift 2
#define fixnum_mask 0b11
#define fixnum_tag  0b00

#define char_shift  8
#define char_mask   0b11111111
#define char_tag    0b00001111

#define bool_shift  7
#define bool_mask   0b1111111
#define bool_tag    0b0011111

#define null_val 0b00101111

int main(int argc, char** argv){
    int val = scheme_entry();
    if ((val & fixnum_mask) == fixnum_tag){
        printf("%d\n", val >> fixnum_shift);
    } else if ((val & char_mask) == char_tag){
        printf("#\\%c\n", val >> char_shift);
    } else if ((val & bool_mask) == bool_tag){
        if (val >> bool_shift == 0){
            printf("#f\n");
        } else {
            printf("#t\n");
        }
    }
    else if (val == null_val){
        printf("()\n");
    }
    return 0;
}