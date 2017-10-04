#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <string.h>

#include "const_mem_pool.h"
#include "hash.h"
#include "hash_uint32.h"

#define words(x) ((x)/sizeof(void*))

void test_const_mem_pool_1(void) {

    char mem[2048];
    struct const_mem_pool *pool = const_mem_pool_create(sizeof(mem), mem);

    assert(pool);

    fprintf(stdout, "available mem in pool (words): %zu\n", words(const_mem_pool_avail(pool)));

    struct hash *h = hash_create( hash_size
                                , const_mem_pool_alloc(pool, hash_size)
                                , sizeof(uint32_t)
                                , sizeof(uint32_t)
                                , 32
                                , uint32_hash
                                , uint32_eq
                                , uint32_cpy
                                , uint32_cpy
                                , pool
                                , const_mem_pool_alloc
                                , const_mem_pool_dealloc );


    fprintf(stdout, "hash allocated? %s\n", h ? "yes" : "no");

    fprintf(stdout, "available mem in pool (words): %zu\n", words(const_mem_pool_avail(pool)));

    size_t i = 0;
    for(;;i++) {
        uint32_t k = i;
        uint32_t v = i;
        if( !hash_add(h, &k, &v) ) {
            break;
        }
    }

    fprintf(stdout, "added something? %s\n", i > 0 ? "yes" : "no");

    // platform-dependend
    fprintf(stderr, "added %zu\n", i);

    // should be not
    fprintf(stdout, "available mem in pool (words): %zu\n", words(const_mem_pool_avail(pool)));

    size_t j = 0;
    for(; j < i; j++ ) {
        uint32_t k = j;
        if( !hash_get(h, &k) ) {
            break;
        }
    }

    fprintf(stdout, "found all ? %s\n", i == j ? "yes" : "no");

    hash_destroy(h);
    const_mem_pool_destroy(pool);
}

