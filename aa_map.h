#ifndef __aa_map_h
#define __aa_map_h

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

struct aa_tree;
extern const size_t aa_tree_size;

struct aa_tree *aa_tree_create( size_t memsize
                              , void *mem
                              , size_t itemsize
                              , int cmp(void*,void*)
                              , void (*cpy)(void*,void*)
                              , void *allocator
                              , void *(*alloc)(void*,size_t)
                              , void  (*dealloc)(void*,void*) );

void aa_tree_destroy(struct aa_tree *t, void *cc, void (*fn)(void*,void*));

bool aa_tree_insert(struct aa_tree *t, void *v);

void aa_tree_remove_with( struct aa_tree *t
                        , void *v
                        , void *cc
                        , void (*fn)(void*,void*));

void aa_tree_remove(struct aa_tree *t, void *v);

void *aa_tree_find( struct aa_tree *t, void *v );

void aa_tree_enum( struct aa_tree *t
                 , void *cc_
                 , void (*fn)(void*, void*));

struct aa_map;
extern const size_t aa_map_size;

struct aa_map *aa_map_create( size_t memsize
                            , void *mem
                            , size_t keysize
                            , size_t valsize
                            , int cmp(void*,void*) // compare KEYS!
                            , void (*keycopy)(void*,void*)
                            , void (*valcopy)(void*,void*)
                            , void *allocator
                            , void *(*alloc)(void*,size_t)
                            , void  (*dealloc)(void*,void*) );


bool aa_map_add(struct aa_map *m, void *k, void *v);
void *aa_map_find(struct aa_map *m, void *k);
void aa_map_del(struct aa_map *m, void *k);

bool aa_map_alter( struct aa_map *m
                 , bool create
                 , void *k
                 , void *cc
                 , void (*fn)( void*   // cc
                             , void*   // k
                             , void*   // v
                             , bool)); // is new


void aa_map_enum( struct aa_map *, void *cc, void (*fn)(void*,void*,void*));

bool aa_map_filter( struct aa_map *, void *cc, bool (*fn)(void*,void*,void*));

void aa_map_destroy(struct aa_map *);

// for debugging
struct aa_node_info {
    size_t level;
    void  *value;
};

void aa_tree_enum_debug( struct aa_tree *t
                       , void *cc_
                       , void (*fn)( void*
                                   , struct aa_node_info *p
                                   , struct aa_node_info *n ) );

#endif

