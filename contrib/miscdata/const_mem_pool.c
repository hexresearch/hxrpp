#include "const_mem_pool.h"

void const_mem_pool_destroy(struct const_mem_pool *p) {
}

struct const_mem_pool *const_mem_pool_create(size_t size, void *mem) {

    if( size < sizeof(struct const_mem_pool) ) {
        return 0;
    }

    size_t pool_size = size - sizeof(struct const_mem_pool);

    struct const_mem_pool *pool = mem;

    pool->p  = &pool->data[0];
    pool->pe = &pool->data[pool_size];

    return pool;
}

void *const_mem_pool_alloc(void *cc, size_t size) {
    struct const_mem_pool *pool = cc;

    if( pool->p + size <= pool->pe ) {
        void *mem = pool->p;
        pool->p += size;
        return mem;
    }

    return 0;
}

void  const_mem_pool_dealloc(void *cc, void *mem) {
}

size_t const_mem_pool_avail(struct const_mem_pool *poo) {
    return poo->pe - poo->p;
}

