#ifndef __SLIST_H__
#define __SLIST_H__

#include <stddef.h>
#include <stdbool.h>

typedef struct slist_ {
    struct slist_ *next;
    char value[0];
} slist;

#define slist_size(psize) ((psize) + sizeof(slist))

#define slist_nil() NULL

#define slist_value(t, it) ((t)((it)->value))

#define slist_set_value(t,l,v) (*(t*)((l)->value)) = v

slist *slist_cons(slist *item, slist *next);

slist *slist_uncons(slist **pitem);

slist *slist_pool(void *mem, size_t chunk_size, size_t mem_size);

slist* slist_pool_( slist *root
                  , void *mem
                  , size_t chunk_size
                  , size_t mem_size);

slist *slist_alloc( slist **pool
                  , void *init_ctx
                  , void (*init)(void *init_ctx, char *item_value));

size_t slist_length(slist *head);

void slist_foreach(slist*, void *cc, void (*)(void*, void*));

void slist_reverse(slist **r);

// keeps order
void slist_filt_destructive( slist **xs
                           , void *cc
                           , bool (*filt)(void*, void*)
                           , void *ccd
                           , void (*destroy)(void*, slist*)
                           );

void slist_partition_destructive( slist **xs
                                , slist **notmatch
                                , void  *cc
                                , bool (*filt)(void*, void*)
                                );
#endif

