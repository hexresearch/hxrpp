#include "mfifo.h"
#include "miscdata.h"

struct chunk {
    struct chunk *n;
    struct chunk *p;
    char data[0];
};

struct mfifo {
    size_t chunk_size;
    size_t free_chunks_max;
    size_t free_chunks_num;

    struct chunk *head;
    struct chunk *tail;
    struct chunk *used;
    struct chunk *free;

    // allocator
    void *allocator;
    void *(*alloc)(void *, size_t);
    void (*dealloc)(void*,void*);
};

const size_t mfifo_size(void) {
    return sizeof(struct mfifo);
}

#define full_chunk_size(fifo) (sizeof(struct chunk) + (fifo)->chunk_size)

void mfifo_destroy(struct mfifo *fifo) {

    struct chunk *it = fifo->head;

    while( it ) {
        struct chunk *tmp = it;
        it = it->n;
        fifo->dealloc(fifo->allocator, tmp);
    }

    it = fifo->free;

    while( it ) {
        struct chunk *tmp = it;
        it = it->n;
        fifo->dealloc(fifo->allocator, tmp);
    }

    it = fifo->used;

    while( it ) {
        struct chunk *tmp = it;
        it = it->n;
        fifo->dealloc(fifo->allocator, tmp);
    }


    fifo->used = 0;
    fifo->free = 0;
    fifo->head = 0;
    fifo->tail = 0;
    fifo->free_chunks_num = 0;
}

void mfifo_shrink( struct mfifo *fifo) {

    struct chunk *it = fifo->free;

    for(; it && fifo->free_chunks_num > fifo->free_chunks_max; fifo->free_chunks_num-- ) {
        struct chunk *tmp = it;
        it = it->n;
        fifo->dealloc(fifo->allocator, tmp);
    }
}


struct mfifo *mfifo_create( void   *mem
                          , size_t memsize
                          , size_t chunk_size
                          , size_t free_chunks_max
                          , void *allocator
                          , void *(*alloc)(void *, size_t)
                          , void (*dealloc)(void*,void*)
                          ) {


    if( memsize < mfifo_size() ) {
        return 0;
    }

    struct mfifo *fifo = mem;

    fifo->chunk_size = chunk_size;
    fifo->free_chunks_max = free_chunks_max;
    fifo->free_chunks_num = 0;
    fifo->allocator = allocator;
    fifo->alloc = alloc;
    fifo->dealloc = dealloc;

    fifo->head = 0;
    fifo->tail = 0;
    fifo->free = 0;
    fifo->used = 0;

    size_t i = 0;
    for(; i < fifo->free_chunks_max/2; i++ ) {
        struct chunk *ch = fifo->alloc(fifo->allocator, full_chunk_size(fifo));

        if( !ch ) {
            break;
        }

        ch->p = 0;
        ch->n = fifo->free;
        fifo->free = ch;
        fifo->free_chunks_num++;
    }

    return fifo;
}

void *mfifo_add( struct mfifo *fifo ) {

    const size_t n = full_chunk_size(fifo);

    struct chunk *oldtail = fifo->tail;

    struct chunk *ch = 0;

    if( fifo->free ) {
        ch = fifo->free;
        fifo->free = fifo->free->n;
        fifo->free_chunks_num--;
    }

    if( !ch ) {
        ch = fifo->alloc(fifo->allocator, n);
    }

    if( !ch ) {
        return 0;
    }

    fifo->tail = ch;
    ch->n = 0;
    ch->p = oldtail;

    if( oldtail ) {
        oldtail->n = ch;
    }

    if( !fifo->head ) {
        fifo->head = ch;
    }

    return ch->data;
}

void *mfifo_get( struct mfifo *fifo ) {

    if( !fifo->head ) {
        return 0;
    }

    struct chunk *h = fifo->head;

    fifo->head = fifo->head->n;

    if( fifo->head ) {
        fifo->head->p = 0;
    }

    if( h ) {
        h->n = fifo->used;
        fifo->used = h;
        return h->data;
    }

    return 0;
}

void mfifo_drop( struct mfifo *fifo, void *data ) {
    struct chunk *ch = data - sizeof(struct chunk);
    ch->n = fifo->free;
    fifo->free = ch;
    fifo->free_chunks_num++;
}

void *mfifo_head( struct mfifo *fifo ) {
    if( !fifo->head )
        return 0;

    return fifo->head->data;
}

void mfifo_iter_fwd( struct mfifo *fifo, void *cc, void (*fn)(void*,void*) ) {

    if( !fn ) {
        return;
    }

    struct chunk *it = fifo->head;
    for(; it; it = it->n ) {
        fn(cc, it->data);
    }
}

void mfifo_iter_back( struct mfifo *fifo, void *cc, void (*fn)(void*,void*) ) {

    if( !fn ) {
        return;
    }

    struct chunk *it = fifo->tail;
    for(; it; it = it->p ) {
        fn(cc, it->data);
    }
}


#ifdef MISCDATA_RT_ENABLE

#include <stdio.h>

void mfifo_dump_status(struct mfifo *fifo) {
    fprintf(stdout, "mfifo_dump_status\n"
                    "  chunk_size: %d\n"
                    "  free_chunks_max: %d\n"
                    "  free_chunks_num: %d\n"
                    "  head: %s\n"
                    "  tail: %s\n"
                    "  free: %s\n"

                  , (int)fifo->chunk_size
                  , (int)fifo->free_chunks_max
                  , (int)fifo->free_chunks_num
                  , fifo->head ? "xxxx" : "nil"
                  , fifo->tail ? "xxxx" : "nil"
                  , fifo->free ? "xxxx" : "nil"
                  );
}

#endif
