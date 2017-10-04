#include <assert.h>
#include "slist.h"

slist *slist_cons(slist *item, slist *next)
{
    assert(item != NULL);

    item->next = next;

    return item;
}

slist *slist_uncons(slist **pitem)
{
    slist *item = *pitem;

    if( item != NULL ) {
        *pitem = item->next;
    }

    return item;
}

slist* slist_pool(void *mem, size_t chunk_size, size_t mem_size)
{
    slist *pool = slist_nil();
    return slist_pool_(pool, mem, chunk_size, mem_size);
}

slist* slist_pool_(slist *root, void *mem, size_t chunk_size, size_t mem_size)
{
    const size_t chunks = mem_size / chunk_size;
    char *p = (char *) mem;
    char *e = (char *) mem + chunks * chunk_size;

    for( ; p < e; p += chunk_size ) {
        root = slist_cons((slist *) p, root);
    }

    return root;
}

slist* slist_alloc( slist **pool
                  , void *init_ctx
                  , void (*init)(void *init_ctx, char *item_value))
{
    slist *item = slist_uncons(pool);

    if( item != NULL ) {
        item->next = (slist *) slist_nil();

        if( init != NULL ) {
            init(init_ctx, item->value);
        }
    }

    return item;
}

size_t slist_length(slist *head)
{
    size_t length = 0;

    while( head != NULL ) {
        head = head->next;
        ++length;
    }

    return length;
}

void slist_foreach(slist *e, void *cc, void (*cb)(void*, void*) ) {
    slist *it = e;
    for(; it; it = it->next) {
        cb(cc, it->value);
    }
}

void slist_filt_destructive( slist **xs
                           , void *cc
                           , bool (*cb)(void*, void*)
                           , void *ccd
                           , void (*destroy)(void*, slist*)
                           ) {

    slist *new_l = slist_nil();
    slist *phead = slist_nil();

    if( !xs )
        return;

    while( xs ) {
        slist *i = slist_uncons(xs);

        if( i == NULL ) break;

        i->next = slist_nil();

        if( cb && cb(cc, i->value) ) {
            if( new_l == NULL ) {
                new_l = slist_cons(i, new_l);
                phead = new_l;
                continue;
            }

            phead->next = i;
            phead = phead->next;

        } else {
            if( destroy ) {
                destroy(ccd, i);
            }
        }
    }

    *xs = new_l;
}

void slist_partition_destructive( slist **xs
                                , slist **notmatch
                                , void  *cc
                                , bool (*filt)(void*, void*)
                                ) {

    if( !xs )
        return;

    if( !notmatch )
        return;

    slist *new_l = slist_nil();
    slist *phead = slist_nil();

    slist *new_l2 = slist_nil();
    slist *phead2 = slist_nil();

    while( xs ) {
        slist *i = slist_uncons(xs);

        if( i == NULL ) break;

        i->next = slist_nil();

        if( filt && filt(cc, i->value) ) {
            if( new_l == NULL ) {
                new_l = slist_cons(i, new_l);
                phead = new_l;
                continue;
            }

            phead->next = i;
            phead = phead->next;

        } else {
            if( new_l2 == NULL ) {
                new_l2 = slist_cons(i, new_l2);
                phead2 = new_l2;
                continue;
            }

            phead2->next = i;
            phead2 = phead2->next;
        }
    }

    *xs = new_l;
    *notmatch = new_l2;
}

void slist_reverse(slist **r) {
    slist *hd = 0;
    while( r && *r ) {
        hd = slist_cons(slist_uncons(r), hd);
    }
    *r = hd;
}

