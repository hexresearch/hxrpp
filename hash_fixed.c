#include <assert.h>

#include "hash_fixed.h"
#include "slist.h"
#include "const_mem_pool.h"
#include "miscdata.h"

#include <stdio.h>

#define DECL_FIXED_HASH_STRUCT(name) \
    struct name {\
        char hash_mem[hash_size];\
        struct slist_allocator sa;\
    }

struct slist_allocator {
    slist *free;
    size_t size;
    struct const_mem_pool mp;
};

size_t hash_mem_size_fixed(size_t bkt, size_t n, size_t k, size_t v) {
    DECL_FIXED_HASH_STRUCT(fixed);
    return   sizeof(struct fixed)
           + hash_minimal_mem_size(bkt, n, k, v)
           - hash_size;
}

static void *__alloc_slist(void *cc, size_t n) {

    struct slist_allocator *sa = cc;

    if( sa->free && n <= sa->size ) {
        void *mem = slist_uncons(&sa->free);
        return mem;
    } else {
        return const_mem_pool_alloc(&sa->mp, MAX(sa->size, n));
    }

    return 0;
}

static void __dealloc_slist(void *cc, void *mem) {
    struct slist_allocator *sa = cc;
    sa->free = slist_cons(mem, sa->free);
}

struct hash *hash_create_fixed( size_t size
                              , void *mem
                              , size_t keysize
                              , size_t valsize
                              , size_t nbuckets
                              , uint32_t (*hashfun)(void *)
                              , bool     (*keycmp)(void *, void *)
                              , void     (*keycopy)(void *, void *)
                              , void     (*valcopy)(void *, void *) ) {

    DECL_FIXED_HASH_STRUCT(fixed_hash);

    const size_t fhs = sizeof(struct fixed_hash);

    if( size < fhs ) {
        return 0;
    }

    struct fixed_hash *fx = mem;

    const size_t ch = MAX(slist_size(0), hash_chunk_size(keysize, valsize));

    const size_t mpsize = size - fhs + sizeof(struct const_mem_pool);

    if( !const_mem_pool_create(mpsize, &fx->sa.mp) ) {
        return 0;
    }

    fx->sa.size = ch;
    fx->sa.free = 0;

    struct hash *h = hash_create( sizeof(fx->hash_mem)
                                , fx->hash_mem
                                , keysize
                                , valsize
                                , nbuckets
                                , hashfun
                                , keycmp
                                , keycopy
                                , valcopy
                                , &fx->sa
                                , __alloc_slist
                                , __dealloc_slist );

    if( !h ) {
        return 0;
    }

    // never rehash fixed hash
    hash_set_rehash_values(h, 0, 0);

    assert( h == mem );

    return h;
}

