#ifndef __const_mem_pool_h
#define __const_mem_pool_h

#include <stddef.h>
#include <stdint.h>

struct const_mem_pool {
    void *p;
    void *pe;
    char data[0];
};

struct const_mem_pool *const_mem_pool_create(size_t, void*);
void const_mem_pool_destroy(struct const_mem_pool *p);

void *const_mem_pool_alloc(void *, size_t);
void  const_mem_pool_dealloc(void*,void*);

size_t const_mem_pool_avail(struct const_mem_pool*);

#endif
