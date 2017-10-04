#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "mfifo.h"


static void *__alloc(void *cc, size_t n) {
    return malloc(n);
}

static void __dealloc(void *cc, void *mem) {
    free(mem);
}

static void __print_int(void *to, void *v) {
    fprintf(to, "%02x ", *(int*)v );
}

void test_mfifo_create_1(void) {
    char mem[mfifo_size()];

    struct mfifo *fifo = mfifo_create( mem
                                     , sizeof(mem)
                                     , sizeof(int)
                                     , 10
                                     , 0
                                     , __alloc
                                     , __dealloc
                                     );


    size_t i = 0;
    for(; i < 10; i++ ) {

        int *v = mfifo_add(fifo);

        if( v ) {
            *v = (int)i;
        }
    }

    fprintf(stdout, "fifo: %s\n", fifo ? "exists" : "not exists");

    mfifo_iter_fwd(fifo, stdout, __print_int);
    fprintf(stdout, "\n");
    mfifo_iter_back(fifo, stdout, __print_int);
    fprintf(stdout, "\n");

    mfifo_destroy(fifo);
}


void test_mfifo_create_2(void) {
    char mem[mfifo_size()];

    struct mfifo *fifo = mfifo_create( mem
                                     , sizeof(mem)
                                     , sizeof(int)
                                     , 16
                                     , 0
                                     , __alloc
                                     , __dealloc
                                     );

    mfifo_dump_status(fifo);

    size_t q = 0;

    for(; q < 16; q++ ) {

        size_t i = 0;
        for(; i < q; i++ ) {

            int *v = mfifo_add(fifo);

            if( !v ) break;

            if( v ) {
                *v = (int)i;
            }
        }

        fprintf(stdout, "added %d\n", (int)i);
        mfifo_dump_status(fifo);

        int *z = 0;

        int l = 7;
        while( (z = mfifo_get(fifo)) )  {
            fprintf(stdout, "%02x ", *z);
            mfifo_drop(fifo, z);
            if( ! (--l ) ) break;
        }

        mfifo_shrink(fifo);
        fprintf(stdout, "\nafter shrink\n");
        mfifo_dump_status(fifo);

        z = 0;
        while( (z = mfifo_get(fifo)) )  {
            fprintf(stdout, "%02x ", *z);
            mfifo_drop(fifo, z);
        }

        fprintf(stdout, "\n");
    }

    mfifo_destroy(fifo);
}


void test_mfifo_drop_1(void) {
    char mem[mfifo_size()];

    struct mfifo *fifo = mfifo_create( mem
                                     , sizeof(mem)
                                     , sizeof(int)
                                     , 16
                                     , 0
                                     , __alloc
                                     , __dealloc
                                     );

    mfifo_dump_status(fifo);

    int *v = mfifo_add(fifo);
    *v = 10;

    mfifo_dump_status(fifo);

    int *q = mfifo_get(fifo);

    mfifo_shrink(fifo);
    mfifo_dump_status(fifo);

    fprintf(stdout, "still here: %d\n", *q);

    mfifo_drop(fifo, q);
    fprintf(stdout, "drop\n", *q);
    mfifo_dump_status(fifo);

    mfifo_destroy(fifo);

}

void test_mfifo_head_1(void) {
    char mem[mfifo_size()];

    struct mfifo *fifo = mfifo_create( mem
                                     , sizeof(mem)
                                     , sizeof(int)
                                     , 16
                                     , 0
                                     , __alloc
                                     , __dealloc
                                     );

    mfifo_dump_status(fifo);

    size_t i = 0;
    for(; i < 10; i++ ) {

        int *v = mfifo_add(fifo);

        if( !v ) break;

        if( v ) {
            *v = (int)i;
        }
    }


    fprintf(stdout, "\n");
    mfifo_iter_fwd(fifo, stdout, __print_int);
    fprintf(stdout, "\n");

    int *h = mfifo_head(fifo);

    (*h)++;
    (*h)++;
    (*h)++;
    (*h)++;

    mfifo_iter_fwd(fifo, stdout, __print_int);
    fprintf(stdout, "\n");

    mfifo_dump_status(fifo);
    mfifo_destroy(fifo);
}

