#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "prng.h"
#include "skiplist.h"

#define RECORD_SIZE 190

static uint64_t __randnum(void *c) {
    return (uint64_t)ranval(c);
}

static void *__alloc(void *cc, size_t n) {
    return malloc(n);
}

static void __dealloc(void *cc, void *c) {
    free(c);
}

static bool __u32_leq(void *a, void *b) {
    return *(uint32_t*)a <= *(uint32_t*)b;
}

static bool __u32_eq(void *a, void *b) {
    return *(uint32_t*)a == *(uint32_t*)b;
}

static void __u32_cpy(void *a, void *b) {
    *(uint32_t*)a = *(uint32_t*)b;
}

static void __print_u32(void *cc, void *v) {
    uint32_t *n = cc;

    (*n)++;

    fprintf(stdout, "%4u", *(uint32_t*)v);

    if( *n >= 16 ) {
        fprintf(stdout, "\n");
        *n = 0;
    }
}

static void __print_node_u32(void *cc, uint8_t l, int tp, void *v) {

    uint32_t *cnt = cc;

    if( tp < 0 ) {
        *cnt = 0;
        fprintf( stdout
               , "%2d:-|"
               , l );
    }

    if( tp == 0 && v) {
        fprintf(stdout, "%s%u", (*cnt)?",":"", *(uint32_t*)v);
        (*cnt)++;
    }

    if( tp > 0 ) {
        fprintf(stdout, "|+\n");
    }

}

void test_skiplist_1(void) {

    ranctx rctx;
    raninit(&rctx, 0xDEADBEEF);

    char mem[skiplist_size];

    struct skiplist *sl = skiplist_create( sizeof(mem)
                                         , mem
                                         , 8
                                         , sizeof(uint32_t)
                                         , __u32_leq
                                         , __u32_cpy
                                         , 0
                                         , __alloc
                                         , __dealloc
                                         , &rctx
                                         , __randnum
                                         );


    const size_t N = 32;
    size_t i = 0, r = 0;
    for(; i < N; i++ ) {
        r = (i+1)%16;
        uint32_t tmp = (ranval(&rctx) % (N));
        skiplist_insert(sl, &tmp);
        fprintf(stdout, "%4u%s", tmp, !r ? "\n" : "" );
    }

    fprintf(stdout, "\n");

    uint32_t n = 0;
    skiplist_enum(sl, &n, __print_u32);

    fprintf(stdout, "\n");

    uint32_t cnt = 0;
    skiplist_enum_debug(sl, &cnt, __print_node_u32);

    uint32_t rm[] = {0,0,25,26,14,8,2,9,9,13,13,23,10,12,18};
    for(i = 0; i < sizeof(rm)/sizeof(rm[0]); i++) {
        fprintf(stdout, "\nremove %u\n", rm[i]);
        skiplist_remove(sl, &rm[i], __u32_eq);
        cnt = 0;
        skiplist_enum_debug(sl, &cnt, __print_node_u32);

        fprintf(stdout, "\ncheck bottom is ok\n");
        uint32_t n = 0;
        skiplist_enum(sl, &n, __print_u32);
        fprintf(stdout, "\n");
    }

    skiplist_destroy(sl);
}


void test_skiplist_2(void) {

    ranctx rctx;
    raninit(&rctx, 0xDEADBEEF);

    char mem[skiplist_size];

    struct skiplist *sl = skiplist_create( sizeof(mem)
                                         , mem
                                         , 4
                                         , sizeof(uint32_t)
                                         , __u32_leq
                                         , __u32_cpy
                                         , 0
                                         , __alloc
                                         , __dealloc
                                         , &rctx
                                         , __randnum
                                         );


    const size_t N = 16;
    size_t i = 0;
    for(; i < N; i++ ) {
        uint32_t tmp = (ranval(&rctx) % (N));
        skiplist_insert(sl, &tmp);
    }

    uint32_t cnt = 0;
    skiplist_enum_debug(sl, &cnt, __print_node_u32);

    skiplist_destroy(sl);
}



