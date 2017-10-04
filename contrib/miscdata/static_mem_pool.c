#include <assert.h>
#include <stdlib.h>

#include "miscdata.h"
#include "static_mem_pool.h"

#define PAGESIZE 4096

static void *__alloc_malloc(void *cc, size_t n) {
    return malloc(n);
}

static void __dealloc_free(void *cc, void *mem) {
    free(mem);
}

struct mem_chunk {
    struct mem_chunk *next;
    void *p;
    void *pe;
    char data[0];
};

static size_t mem_chunk_size(size_t extra) {
    return sizeof(struct mem_chunk) + extra;
}

static struct mem_chunk *mem_chunk_init( struct static_mem_pool *p
                                       , struct mem_chunk *c
                                       , size_t data_size
                                       ) {

    if( !c ) {
        safecall(unit, p->nomem, p->nomemcc);
        return 0;
    }

    c->next = 0;
    c->p = c->data;
    c->pe = c->data + data_size;

    return c;
}

struct static_mem_pool *static_mem_pool_init( struct static_mem_pool *p
                                            , size_t init_size
                                            , void *nomemcc
                                            , void (*nomem)(void*)
                                            , void *allocator
                                            , void *(*alloc)(void*,size_t)
                                            , void  (dealloc)(void*,void*) ) {

    p->init_size = init_size;
    p->nomemcc = nomemcc;
    p->nomem = nomem;

    p->allocator = allocator;

    p->alloc = alloc ? alloc : __alloc_malloc;
    p->dealloc = dealloc ? dealloc : __dealloc_free;

    p->chunks = mem_chunk_init( p
                              , p->alloc(p->allocator, mem_chunk_size(init_size))
                              , init_size );

    return p->chunks ? p : 0;
}

void *static_mem_pool_alloc(void *cc, size_t n) {
    struct static_mem_pool *m = cc;

    struct mem_chunk *head = m->chunks;

    if( head->pe - head->p >= n ) {
        void *mem = head->p;
        head->p = head->p + n;
        return mem;
    } else {
        size_t demand = mem_chunk_size(n);
        size_t page   = PAGESIZE;
        size_t size   = MAX(page, demand);

        struct mem_chunk *nc = mem_chunk_init( m
                                              , m->alloc(m->allocator, size)
                                              , size - mem_chunk_size(0) );

        if( !nc ) {
            safecall(unit, m->nomem, m->nomemcc);
            return 0;
        }

        nc->next = m->chunks;
        m->chunks = nc;
        head = m->chunks;

        if( head->pe - head->p >= n ) {
            void *mem = head->p;
            head->p = head->p + n;
            return mem;
        }
    }

    assert(0);
    return 0;
}

void  static_mem_pool_dealloc(void *cc, void *mem) {
}

void static_mem_pool_destroy(struct static_mem_pool *m) {

    struct mem_chunk *e = m->chunks;

    while(e) {
        void *z = e;
        e = e->next;
        m->dealloc(m->allocator, z);
    }
}

