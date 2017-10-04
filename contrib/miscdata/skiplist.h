#ifndef __skiplist_h
#define __skiplist_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct skiplist;

extern const size_t skiplist_size;

struct skiplist * skiplist_create( size_t memsize
                                 , void *mem
                                 , size_t maxlevel
                                 , size_t itemsize
                                 , bool (*leq)(void*,void*)
                                 , void (*cpy)(void*,void*)
                                 , void *allocator
                                 , void *(*alloc)(void*,size_t)
                                 , void  (*dealloc)(void*,void*)
                                 , void *rndgen
                                 , uint64_t (*rnd)(void*)
                                 );


void skiplist_destroy(struct skiplist *sl);

bool skiplist_insert(struct skiplist *sl, void *v);

void *skiplist_find( struct skiplist *sl
                   , void *v
                   , bool (*eq)(void*,void*)
                   );

bool skiplist_remove( struct skiplist *sl
                    , void *v
                    , bool (*eq)(void*,void*)
                    );

void skiplist_enum( struct skiplist *sl
                  , void *cc
                  , void (*fn)(void*,void*) );

void skiplist_enum_debug( struct skiplist *sl
                        , void *cc
                        , void (*fn)( void *cc
                                    , uint8_t level
                                    , int tp // -1, 0, +1 for -inf, regular, +inf nodes
                                    , void *v ) );

#endif
