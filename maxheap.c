#include <string.h>
#include <stdint.h>
#include "heap.h"

#ifndef MAX
#define MAX(a,b) ((a) >= (b) ? (a) : (b))
#endif

struct heap {
    size_t n;
    size_t size;
    size_t item_size;
    bool (*leq)(void*,void*);
    void (*cpy)(void*,void*);
    char   data[0];
};

struct heap *heap_create( void *mem
                        , size_t memsize
                        , size_t item_size
                        , bool  (*item_leq)(void*,void*)
                        , void  (*item_cpy)(void*,void*)
                        ) {

    const size_t headsz = sizeof(struct heap);
    const size_t minsz = 3*item_size + headsz;

    if( memsize < minsz ) {
        return 0;
    }

    struct heap *h = mem;
    h->n = 0;
    h->size = (memsize - headsz) / item_size;
    h->item_size = item_size;
    h->leq = item_leq;
    h->cpy = item_cpy;

    return h;
}

static inline size_t L(size_t i) {
    return (i*2 + 1);
}

static inline size_t R(size_t i) {
    return (i*2 + 2);
}

static inline size_t P(size_t i) {
    return (i-1)/2;
}

static inline void* pitem(struct heap *h, size_t i) {
    return &h->data[i*h->item_size];
}

static inline void swap(struct heap *h, size_t i, size_t j) {
    char tmp[h->item_size];

    void *pi = pitem(h, i);
    void *pj = pitem(h, j);

    h->cpy(tmp, pi);
    h->cpy(pi, pj);
    h->cpy(pj, tmp);
}

static inline bool leq(struct heap *h, size_t i, size_t j) {
    return h->leq(pitem(h,i), pitem(h,j));
}

static void sift_down(struct heap *h, size_t i) {
    while( L(i) < h->n ) {
        size_t l = L(i);
        size_t r = R(i);
        size_t j = l;
        if( r < h->n && leq(h, r, l) ) {
            j = r;
        }
        if( leq(h, i, j) ) {
            break;
        }
        swap(h, i, j);
        i = j;
    }
}

static void sift_up(struct heap *h, size_t i) {
    for(; i && leq(h, i, P(i)); i = P(i) ) {
        swap(h, i, P(i));
    }
}

size_t heap_size(struct heap *h) {
    return h->size;
}

size_t heap_items(struct heap *h) {
    return h->n;
}

size_t heap_mem_size(size_t n, size_t chunk_size) {
    return (sizeof(struct heap) + MAX(n,3)*chunk_size);
}

bool heap_empty(struct heap *h) {
    return (0 == h->n);
}

bool heap_full(struct heap* h) {
    return h->n == h->size;
}

bool heap_add(struct heap *h, void *v) {
    if( h->n >= h->size ) {
        return false;
    }

    size_t i = h->n++;

    h->cpy(pitem(h,i), v);
    sift_up(h, i);

    return true;
}

void *heap_get(struct heap *h) {

    if( !h->n ) {
        return 0;
    }

    return pitem(h, 0);
}

void *heap_pop(struct heap *h) {

    if( !h->n ) {
        return 0;
    }

    if( h->n == 1 ) {
        h->n--;
        return pitem(h, 0);
    }

    size_t l = --h->n;
    swap(h, 0, l);
    sift_down(h, 0);

    return pitem(h, l);
}


