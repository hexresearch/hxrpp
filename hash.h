#ifndef __BIGHASH_H__
#define __BIGHASH_H__

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

struct hash;

extern const size_t hash_size;

struct hash *hash_create( size_t memsize
                        , void  *mem
                        , size_t keysize
                        , size_t valsize
                        , size_t nbuckets
                        , uint32_t (*hashfun)(void *)
                        , bool     (*keycmp)(void *, void *)
                        , void     (*keycopy)(void *, void *)
                        , void     (*valcopy)(void *, void *)
                        , void   *allocator
                        , void   *(*alloc)(void*,size_t)
                        , void    (dealloc)(void*,void*)
                        );

void hash_set_rehash_values(struct hash *c, uint8_t fill, size_t n);

void hash_rehash_end(struct hash *c);

void hash_destroy(struct hash*);

bool hash_shrink(struct hash *, bool);

bool hash_add(struct hash *c, void *k, void *v);

void *hash_get(struct hash *c, void *k);

void hash_find( struct hash *c
              , void *k
              , void *cc
              , void (*cb) (void *cc, void *v));

void hash_del( struct hash *c, void *k);

bool hash_alter( struct hash* c
               , bool add
               , void *k
               , void *ctx
               , void (*) (void *, void *, void *, bool));


void hash_enum( struct hash *c
              , void *cc
              , void (*)(void *, void *, void *));

void hash_filter( struct hash *c
                , void *cc
                , bool (*cb)(void *, void *, void *));

void hash_stats( struct hash *c
               , size_t *capacity
               , size_t *used
               , size_t *collisions
               , size_t *maxbuck
               );

size_t hash_chunk_size(size_t k, size_t v);
size_t hash_minimal_mem_size(size_t bkt, size_t n, size_t k, size_t v);

#endif
