#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "hash_fixed.h"
#include "hash_uint32.h"
#include "const_mem_pool.h"

#include "hash_test_common.h"

#include "prng.h"

#define words(x) ((x)/sizeof(void*))


static void *__alloc(void *cc,size_t n) {
    return malloc(n);
}

static void __dealloc(void *cc,void *m) {
    free(m);
}

static bool __filt_even(void *c, void *k, void *v) {
    return !! ( *(uint32_t*)k % 2);
}

static void __print(void *c, void *k, void *v) {
    fprintf(stdout, "(%u,%u)\n", *(uint32_t*)k, *(uint32_t*)v);
}

void test_hash_fixed_1(void) {

    const size_t bkt = 32;

    char mem[hash_mem_size_fixed(bkt, 100, sizeof(uint32_t), sizeof(uint32_t))];

    fprintf(stderr, "mem size: %zu\n\n", sizeof(mem));
    fprintf(stderr, "const_mem_pool size: %zu\n\n", sizeof(struct const_mem_pool));
    fprintf(stderr, "hash_size %zu\n\n", hash_size);
    fprintf( stderr
           , "hash_minimal_mem_size %zu\n\n"
           , hash_minimal_mem_size(bkt, 100, sizeof(uint32_t), sizeof(uint32_t))
           );

    struct hash *h = hash_create_fixed( sizeof(mem)
                                      , mem
                                      , sizeof(uint32_t)
                                      , sizeof(uint32_t)
                                      , bkt
                                      , uint32_hash
                                      , uint32_eq
                                      , uint32_cpy
                                      , uint32_cpy );

    fprintf(stdout, "hash allocated? %s\n\n", h ? "yes" : "no");

    size_t i = 0;
    for(;;i++) {
        uint32_t k = i;
        uint32_t v = i;
        if( !hash_add(h, &k, &v) ) {
            break;
        }
    }

    fprintf(stdout, "added something? %s\n\n", i > 0 ? "yes" : "no");

    // platform-dependend
    fprintf(stderr, "added %zu\n\n", i);

    size_t j = 0;
    for(; j < i; j++ ) {
        uint32_t k = j;
        if( !hash_get(h, &k) ) {
            break;
        }
    }

    fprintf(stdout, "found all ? %s\n\n", i == j ? "yes" : "no");

    print_hash_stat(h);

    hash_filter(h, 0, 0);

    fprintf(stdout, "\nfiltered off\n\n");

    print_hash_stat(h);

    for(i=0;;i++) {
        uint32_t k = i;
        uint32_t v = i;
        if( !hash_add(h, &k, &v) ) {
            break;
        }
    }

    // platform-dependend
    fprintf(stdout, "\nadded: %zu\n\n", i);

    print_hash_stat(h);

    hash_filter(h, 0, __filt_even);

    fprintf(stdout, "\nfiltered even\n\n");

    print_hash_stat(h);

    fprintf(stdout, "\ntrying shrink -- should make no effect\n\n");

    hash_shrink(h, true);

    print_hash_stat(h);

    hash_destroy(h);
}

static void __alter_u32_u32(void *cc, void *k, void *v, bool n) {
    *(uint32_t*)v = *(uint32_t*)cc;
}

struct nested_1_alter_cc {
    ranctx *gen;
    size_t  fhs;
};

static void __hash_fixed_nested_1_alter(void *cc_, void *k, void *v, bool n) {
    struct hash *h = 0;

    struct nested_1_alter_cc *cc = cc_;

    if( n )  {
        h = hash_create_fixed( cc->fhs
                             , v
                             , sizeof(uint32_t)
                             , sizeof(uint32_t)
                             , 64
                             , uint32_hash
                             , uint32_eq
                             , uint32_cpy
                             , uint32_cpy
                             );
    } else {
        h = v;
    }

    assert(h);

    size_t i = 0;
    for(; i < 20; i++ ) {
        uint32_t key = i;
        uint32_t val = ranval(cc->gen) % 100000;
        if( hash_alter(h, true, &key, &val, __alter_u32_u32) ) {
            uint32_t *v_ = hash_get(h, &key);
            fprintf(stdout, "%u (%u,%u)\n", *(uint32_t*)k, key, *v_);
        }
    }

}

static void __fixed_nested_1_enum_2(void *cc, void *k, void *v) {
    fprintf(stdout, "%u (%u,%u)\n", *(uint32_t*)cc, *(uint32_t*)k, *(uint32_t*)v);
}

static void __fixed_nested_1_enum_1(void *cc, void *k, void *v) {
    hash_enum(v, k, __fixed_nested_1_enum_2);
}

void test_hash_fixed_nested_1(void) {

    ranctx rgen;
    raninit(&rgen, 0x010101010101);

    char mem[hash_size];

    const size_t fhs = hash_mem_size_fixed(64, 8, sizeof(uint32_t), sizeof(uint32_t));

    struct hash *c = hash_create( sizeof(mem)
                                , mem
                                , sizeof(uint32_t)
                                , fhs
                                , 64
                                , uint32_hash
                                , uint32_eq
                                , uint32_cpy
                                , 0 // not a POD structure, no copy
                                , 0
                                , __alloc
                                , __dealloc
                                );

    const size_t N = 10;
    size_t i = 0;

    struct nested_1_alter_cc cc = { .gen = &rgen, .fhs = fhs };

    for(; i < N; i++ ) {
        uint32_t tmp= i;
        hash_alter(c, true, &tmp, &cc, __hash_fixed_nested_1_alter);
    }

    fprintf(stdout, "\n---\n");

    hash_enum(c, 0, __fixed_nested_1_enum_1);

    hash_destroy(c);
}


