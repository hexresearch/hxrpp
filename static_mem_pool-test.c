#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>

#include "hash.h"
#include "hash_uint32.h"
#include "static_mem_pool.h"
#include "prng.h"

static void *__alloc(void *cc, size_t n) {
    void *mem = malloc(n);
/*    fprintf(stderr, "alloc %p\n", mem);*/
    return mem;
}

static void __dealloc(void *cc, void *mem) {
/*    fprintf(stderr, "free %p\n", mem);*/
    free(mem);
}

void nomem(void *cc) {
    fprintf(stderr, "out of memory\n");
}

void test_static_mem_pool_1(void) {

    ranctx rctx;
    raninit(&rctx, 0x128e437);

    struct static_mem_pool pool;

    struct hash *hs[20] = { 0 };
    const size_t HSIZE = sizeof(hs)/sizeof(hs[0]);

    void *pp = static_mem_pool_init( &pool
                                   , 8192 - sizeof(struct static_mem_pool)
                                   , 0
                                   , nomem
                                   , 0
                                   , 0
                                   , 0 );

    if( !pp ) {
        fprintf(stderr, "Can't setup memory pool\n");
        return;
    }

    size_t i = 0;
    for(; i < HSIZE; i++ ) {
        hs[i] = hash_create( hash_size
                           , static_mem_pool_alloc(&pool, hash_size)
                           , sizeof(uint32_t)
                           , sizeof(uint32_t)
                           , 32
                           , uint32_hash
                           , uint32_eq
                           , uint32_cpy
                           , uint32_cpy
                           , 0
                           , __alloc
                           , __dealloc );
    }

    for(i = 0; i < 100500; i++ ) {
        size_t hnum = ranval(&rctx) % HSIZE;
        uint32_t k = ranval(&rctx) % 1000001;
        uint32_t v = ranval(&rctx) % 99999999;
        hash_add(hs[hnum], &k, &v);
    }

    for(i = 0; i < HSIZE; i++ ) {

        size_t cap = 0, used = 0, cls = 0, maxb = 0;
        hash_stats(hs[i], &cap, &used, &cls, &maxb);

        fprintf( stdout
               , "\nhash#%zu\n"
                 "capacity:         %zu\n"
                 "used:             %zu\n"
                 "collisions (avg): %zu\n"
                 "max. row:         %zu\n"
               , i
               , cap
               , used
               , cls
               , maxb
               );


        hash_destroy(hs[i]);
    }

    static_mem_pool_destroy(&pool);
}

