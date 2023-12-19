#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>

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

/*Print values as tagged pointer representation*/
static void print_val(int val){
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
    else{
        printf("pointer: 0x%x\n", val);
    }
}

/*Allocate protected stack space and return pointer to
top of the stack*/ 
static char* allocate_protected_space(int size){
    // get memory page size (libc)
    int page = getpagesize();
    int status;
    // align size to multiples of page size
    int aligned_size = ((size + page -1) / page) * page;

    // create new protected virtual address space
    // with extra protection page above and below 
    char *p = mmap(0, aligned_size + 2 * page,
        PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE,
        0, 0);
    // exit if mmap failed
    if (p == MAP_FAILED){
        perror("mmap failed");
        exit(1);
    }
    // protect top of our stack
    status = mprotect(p, page, PROT_NONE);
    if (status != 0){
        perror("mprotect failed");
        exit(1);
    }

    // protect base of our stack
    status = mprotect(p + page + aligned_size, page, PROT_NONE);
    if (status != 0){
        perror("mprotect failed");
        exit(1);
    }

    // pointer to top of stack
    return (p + page);
}

/* Clear stack we created */
static char* deallocate_protected_space(char *p, int size){
    int page = getpagesize();
    int status;
    int aligned_size = ((size + page - 1) / page) * page;
    // remove virtual address space we created before
    status = munmap(p - page, aligned_size + 2 * page);
    if (status != 0){
        perror("munmap failed");
        exit(1);
    }
}

int main(int argc, char** argv){
    int stack_size = (16 * 4096);   /* holds 16k cells*/
    char *stack_top = allocate_protected_space(stack_size);
    char *stack_base = stack_top + stack_size;

    int heap_size = (160 * 4096);  /* holds 160k cells */
    char *heap_top = allocate_protected_space(heap_size);
    
    int val = scheme_entry(stack_base, heap_top);
    print_val(val);

    deallocate_protected_space(stack_top, stack_size);
    deallocate_protected_space(heap_top, heap_size);
    return 0;
}