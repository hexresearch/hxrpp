#ifndef __heap_h
#define __heap_h

#include <stddef.h>
#include <stdbool.h>

struct heap;

// FIXED SIZE BINARY HEAP

struct heap * heap_create( void *mem
                         , size_t memsize
                         , size_t item_size
                         , bool (*item_leq)(void*,void*)
                         , void (*item_cpy)(void*,void*)
                         );

size_t heap_mem_size(size_t n, size_t chunk_size);
size_t heap_size(struct heap*);
size_t heap_items(struct heap*);
bool heap_full(struct heap*);
bool heap_empty(struct heap*);
bool heap_add(struct heap *, void *);
void *heap_get(struct heap*);
void *heap_pop(struct heap*);

#endif
