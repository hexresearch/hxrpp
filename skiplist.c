#include "skiplist.h"

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

struct skipnode;

struct skiplist {

    struct skipnode *top;
    struct skipnode *bottom;

    size_t  itemsize;
    uint8_t maxlevel;
    uint8_t level;

    bool (*leq)(void*, void*);
    void (*cpy)(void*, void*);

    void *allocator;
    void *(*alloc)(void*,size_t);
    void (*dealloc)(void*,void*);

    void *rndgen;
    uint64_t (*rnd)(void*);
};

const size_t skiplist_size = sizeof(struct skiplist);

static void *const __minus_inf = (void*)(0);
static void *const __plus_inf  = (void*)(-1);

struct skipnode {
    struct skipnode *next;
    struct skipnode *down;
    void *value;
    char data[0];
};

static void skiplist_add_levels(struct skiplist *sl, uint8_t n);

static struct skipnode* skipnode_init( struct skiplist*
                                     , bool
                                     , struct skipnode *
                                     , void *);

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
                                 , uint64_t (*rnd)(void*) ) {

    if( memsize < skiplist_size ) {
        return 0;
    }

    struct skiplist *sl = mem;

    sl->top = 0;
    sl->bottom = 0;
    sl->level = 0;
    sl->maxlevel = maxlevel;
    sl->itemsize = itemsize;
    sl->leq = leq;
    sl->cpy = cpy;
    sl->allocator = allocator;
    sl->alloc = alloc;
    sl->dealloc = dealloc;
    sl->rndgen = rndgen;
    sl->rnd = rnd;

    skiplist_add_levels(sl, 1);

    return sl;
}

void skiplist_destroy(struct skiplist *sl) {
    while( sl->top ) {
        while( sl->top->next ) {
            struct skipnode *n = sl->top->next;
            sl->top->next = n->next;
            sl->dealloc(sl->allocator, n);
        }
        struct skipnode *n = sl->top;
        sl->top = n->down;
        sl->dealloc(sl->allocator, n);
    }
}

static struct skipnode *skipnode_init( struct skiplist *sl
                                     , bool cpy
                                     , struct skipnode *n
                                     , void *v ) {

    if( !n ) {
        return 0;
    }

    n->next = 0;
    n->down = 0;

    if( !cpy ) {
        n->value = v;
    } else {
        sl->cpy(n->data, v);
        n->value = n->data;
    }
    return n;
}

static struct skipnode *skipnode_insert_next(struct skipnode *p, struct skipnode *n) {
    if( p && n ) {
        n->next = p->next;
        p->next = n;
        return n;
    }
    return 0;
}

static inline void adjust_bottom(struct skiplist *sl) {
    if( !sl->bottom ) {
        sl->bottom = sl->top;
    }
    for(; sl->bottom->down; sl->bottom = sl->bottom->down);
}

static
struct skipnode * skipnode_insert_up( struct skipnode **t
                                    , struct skipnode *n) {

    if( n ) {
        n->down = *t;
        *t = n;
        return n->down;
    }

    return 0;
}

static bool skipnode_value_leq( struct skiplist *sl
                              , void *a
                              , void *b ) {


    if( a == __minus_inf ) {
        return true;
    } else if( a == __plus_inf ) {
        return false;
    }

    if( b == __minus_inf || b == __plus_inf ) {
        return false;
    }

    return sl->leq(a,b);
}

static inline bool __safe_eq( bool (*eq)(void*,void*), void *a, void *b ) {

    if( a == __minus_inf || a == __plus_inf )
        return false;

    if( b == __minus_inf || b == __plus_inf )
        return false;

    return eq(a,b);
}

static void skipnode_find( struct skiplist *sl
                         , struct skipnode *from
                         , void *v
                         , void *cc
                         , bool (*fn)( void*
                                     , struct skipnode*
                                     , struct skipnode*) ) {

    struct skipnode *p = from;

    for(; p; ) {
        struct skipnode *prev = 0;
        for(; skipnode_value_leq(sl, p->next->value, v); p = p->next) {
            prev = p;
        }
        if( !fn(cc, prev, p) ) {
            return;
        }
        p = p->down;
    }
}

static uint8_t skiplist_new_level(struct skiplist *sl) {

    uint8_t lvl = 1;

    while( lvl <= sl->maxlevel && (sl->rnd(sl->rndgen) & 0xFFFF) < (0xFFFF/2) ) {
        lvl++;
    }

    return (uint8_t)MIN(lvl, sl->maxlevel);
}

static struct skipnode * skipnode_create(struct skiplist *sl, void *v, size_t sz) {
    return skipnode_init( sl
                        , !!sz
                        , sl->alloc(sl->allocator, (sizeof(struct skipnode) + sz))
                        , v );
}

static void skiplist_add_levels(struct skiplist *sl, uint8_t n) {

    for(; n; n--, sl->level++ ) {
        skipnode_insert_up( &sl->top
                          , skipnode_create(sl, __minus_inf, 0));

        skipnode_insert_next(sl->top, skipnode_create(sl, __plus_inf, 0));
    }

    adjust_bottom(sl);
}

struct skipnode_ins_cc {
    void *v;
    struct skiplist *sl;
    uint8_t l;
    uint8_t promote_l;
    struct skipnode *up;
    struct skipnode *top;
    void *newvalue;
};

static bool skipnode_ins_fn( void *cc_
                           , struct skipnode *prev
                           , struct skipnode *n) {

    struct skipnode_ins_cc *cc = cc_;

    if( cc->promote_l == cc->l ) {

        struct skipnode *nn = 0;

        if( n->down ) {
            nn = skipnode_insert_next(n, skipnode_create( cc->sl, 0, 0 ));
        } else {
            nn = skipnode_insert_next(n, skipnode_create( cc->sl
                                                        , cc->v
                                                        , cc->sl->itemsize));
            cc->newvalue = nn->value;
        }

        if( !nn ) {
            // FIXME: memory allocation
            return false;
        }

        if( !cc->up ) {
            cc->up = nn;
            cc->top = cc->up;
        } else {
            cc->up->down = nn;
            cc->up = nn;
        }

        cc->promote_l--;
    }

    cc->l--;

    return true;
}


bool skiplist_insert(struct skiplist *sl, void *v) {

    uint8_t lvl = skiplist_new_level(sl);

    if( lvl > sl->level ) {
        skiplist_add_levels(sl, lvl - sl->level);
    }

    struct skipnode_ins_cc cc = { .sl = sl
                                , .l  = sl->level
                                , .promote_l = lvl
                                , .v = v
                                , .up = 0
                                , .newvalue = 0
                                };

    skipnode_find( sl
                 , sl->top
                 , v
                 , &cc
                 , skipnode_ins_fn );


    struct skipnode *i = cc.top;

    for(; i; i = i->down ) {
        if( !i->value ) {
            i->value = cc.newvalue;
        }
    }

    return true;
}

struct find_match_cc {
    struct skipnode *n;
    void *v;
    bool (*eq)(void*,void*);
};

static bool __find_match(void *cc_, struct skipnode *p, struct skipnode *n) {

    struct find_match_cc *cc = cc_;

    if( !n || n->value == __minus_inf ) {
        return true;
    }

    if( __safe_eq(cc->eq, n->value, cc->v) ) {
        cc->n = n;
        return false;
    }

    // should not happen
    return true;
}

void *skiplist_find( struct skiplist *sl
                   , void *v
                   , bool (*eq)(void*,void*)
                   ) {

    struct find_match_cc cc = { .n  = 0
                              , .v  = v
                              , .eq = eq
                              };

    skipnode_find(sl, sl->top, v, &cc, __find_match);

    return cc.n ? cc.n->value : 0;
}


struct skipnode_rm_cc {
    void  *v;
    bool  (*eq)(void*,void*);
    struct skipnode *p;
};

static bool __skiplist_rm_fn( void *cc_
                            , struct skipnode *prev
                            , struct skipnode *n) {

    struct skipnode_rm_cc *cc = cc_;

    if( n && !__safe_eq(cc->eq, cc->v, n->value) ) {
        cc->p = n;
        return false;
    }

    if( prev ) {
        cc->p = prev;
        return false;
    }

    return true;
}

static inline bool empty_level( struct skipnode *n ) {
    return n->value == __minus_inf && n->next->value == __plus_inf;
}

static void prune_levels(struct skiplist *sl) {

    struct skipnode *p    = sl->top;

    struct skipnode *ntop = 0;

    while( p ) {
        struct skipnode *c = p;
        p = p->down;

        if( empty_level(c) ) {
            while( c ) {
                struct skipnode *tmp = c->next;
                sl->dealloc(sl->allocator, c);
                c = tmp;
            }
        } else {
            c->down = ntop;
            ntop = c;
        }
    }

    sl->bottom = 0;
    sl->top = 0;
    sl->level = 0;

    while( ntop ) {
        struct skipnode *c = ntop;
        ntop = ntop->down;
        c->down = sl->top;
        sl->top = c;
        sl->level++;
    }

    adjust_bottom(sl);
}

bool skiplist_remove( struct skiplist *sl
                    , void *v
                    , bool (*eq)(void*,void*)
                    ) {

    size_t rm = 0;

    struct skipnode_rm_cc cc = { .p = 0, .v = v, .eq = eq } ;

    struct skipnode *p = sl->top;

    for(; p; ) {

        skipnode_find( sl
                     , p
                     , v
                     , &cc
                     , __skiplist_rm_fn );


        if( cc.p->next && __safe_eq(eq, cc.p->next->value, v) ) {
            struct skipnode *tmp = cc.p->next;
            cc.p->next = cc.p->next->next;
            sl->dealloc(sl->allocator, tmp);
            rm++;
        }

        p = cc.p ? cc.p->down : 0;
    }

   prune_levels(sl);

    return (rm > 0);
}

void skiplist_enum( struct skiplist *sl
                  , void *cc
                  , void (*fn)(void*,void*) ) {

    // should not never ever been nil
    struct skipnode *p = sl->bottom->next;
    for(; p && p->value != __plus_inf; p = p->next) {
        fn(cc, p->value);
    }
}

static int __value_tp( void *n ) {
    if( n == __minus_inf )
        return -1;

    if( n == __plus_inf )
        return 1;

    return 0;
}

void skiplist_enum_debug( struct skiplist *sl
                        , void *cc
                        , void (*fn)( void *cc
                                    , uint8_t level
                                    , int tp
                                    , void *v ) ) {

    uint8_t lvl = sl->level;
    struct skipnode *tp = sl->top;
    for(; tp; tp = tp->down, lvl-- ) {
        struct skipnode *p = tp;
        for(; p; p = p->next ) {
            int ttp = __value_tp(p->value);
            fn(cc, lvl, ttp, !ttp ? p->value : 0);
        }
    }

}

