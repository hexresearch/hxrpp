#ifndef __static_mem_pool_h
#define __static_mem_pool_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct static_mem_pool {
    size_t init_size;

    void *chunks;

    void *nomemcc;
    void (*nomem)(void*cc);

    void *allocator;
    void *(*alloc)(void*,size_t);
    void  (*dealloc)(void*,void*);
};

struct static_mem_pool *static_mem_pool_init( struct static_mem_pool *p
                                            , size_t init_size
                                            , void *nomemcc
                                            , void (*nomem)(void*)
                                            , void *allocator
                                            , void *(*alloc)(void*,size_t)
                                            , void  (dealloc)(void*,void*) );

void static_mem_pool_destroy(struct static_mem_pool*);

void *static_mem_pool_alloc(void *, size_t n);
void  static_mem_pool_dealloc(void*,void*);

#endif
