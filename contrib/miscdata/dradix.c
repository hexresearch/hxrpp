#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "dradix.h"

static char *const nilptr   = "{nil}";
static char *const emptyptr = "{empty}";

#define safecall(v, f, ...)  ((f) ? (f(__VA_ARGS__)) : (v))
#define unit {}

bool rtrie_emptyval(void *v)
{
    return v == emptyptr;
}

static inline size_t rtrie_klen(char *ka, char *ke)
{
    return (size_t) (ke - ka);
}

static size_t rtrie_prefix_len(char *s, char *ka, char *ke)
{
    size_t sl = strlen(s);
    char *se = s + sl;
    size_t pl = 0;

    for( ; s < se && ka < ke && *s == *ka; s++, ka++ ) {
        pl++;
    }

    return pl < sl ? pl : sl;
}

static bool rtrie_new_key(rtrie *t, char *ka, char *ke)
{
    size_t kl = rtrie_klen(ka, ke);

    free(t->keymem);
    t->keymem = malloc(kl + 1);

    if( t->keymem == NULL ) {
        return false;
    }

    memcpy(t->keymem, ka, kl);
    t->keymem[kl] = '\0';
    t->ka = t->keymem;
    t->ke = t->keymem + kl;

    return true;
}

static rtrie *rtrie_new(char *ka, char *ke, void *v, bool copy)
{
    rtrie *n = malloc(sizeof(rtrie));

    if( n != NULL ) {
        n->link = NULL;
        n->sibling = NULL;
        n->ka = ka;
        n->ke = ke;
        n->v = v;
        n->keymem = NULL;

        if( copy ) {
            if( !rtrie_new_key(n, ka, ke) ) {
                free(n);

                return NULL;
            }
        }
    }

    return n;
}

rtrie *rtrie_nil()
{
    return rtrie_new(nilptr, nilptr + strlen(nilptr), emptyptr, false);
}

static rtrie *rtrie_assign(rtrie *t, char *ka, char *ke, void *v, bool copy)
{
    assert(t != NULL);

    t->ka = ka;
    t->ke = ke;
    t->v  = v;

    if( copy ) {
        if( !rtrie_new_key(t, ka, ke) ) {
            free(t);

            return NULL;
        }
    }

    return t;
}

bool rtrie_null(rtrie *t)
{
    return t->ka == nilptr;
}

bool rtrie_empty(rtrie *t)
{
    return t->v == emptyptr;
}

void rtrie_add(rtrie *t, char *sa, size_t len, void *v)
{
    assert(t != NULL);

    if( sa[0] == '\0' ) {
        return; // prune empty strings
    }

    size_t pl = rtrie_prefix_len(sa, t->ka, t->ke);
    size_t kl = rtrie_klen(t->ka, t->ke);
    char *se = sa + len;

    if( rtrie_null(t) ) { // need mem to keep the key
        rtrie_assign(t, sa, se, v, true);

        return;
    }

    if( pl == 0 ) { // not a prefix
        // add at sibling
        if( t->sibling == NULL ) {
            rtrie *n = rtrie_new(sa, se, v, true);

            n->sibling = t->sibling;
            t->sibling = n;

            return;
        }

        rtrie_add(t->sibling, sa, len, v);

        return;
    }

    if( pl == len && kl == len ) {
        t->v = v;

        return;
    }

    if( pl < kl ) { // new node
        // need mem to keep the key... or it does not?
        rtrie *n = rtrie_new(t->ka + pl, t->ke, t->v, false);

        n->link = t->link;
        t->link = n;

        rtrie_assign(t, t->ka, t->ka + pl, emptyptr, false);

        // prune empty strings
        if( sa[pl] == '\0' ) {
            t->v = v;

            return;
        }
    }

    if( t->link == NULL ) {
        t->link = rtrie_nil();
    }

    rtrie_add(t->link, sa + pl, len - pl, v);
}

void rtrie_bfs_with_node(rtrie *t, void *cc, rtrie_node_cb cb)
{
    if( t != NULL ) {
        safecall(unit, cb, cc, t);
        rtrie_bfs_with_node(t->sibling, cc, cb);
        rtrie_bfs_with_node(t->link, cc, cb);
    }
}

void rtrie_bfs(rtrie *t, void *cc, rtrie_cb cb)
{
    if( t != NULL ) {
        safecall(unit, cb, cc, t->ka, t->ke, t->v);
        rtrie_bfs(t->sibling, cc, cb);
        rtrie_bfs(t->link, cc, cb);
    }
}

void rtrie_dfs(rtrie *t, void *cc, rtrie_cb cb)
{
    if( t != NULL ) {
        safecall(unit, cb, cc, t->ka, t->ke, t->v);
        rtrie_dfs(t->link, cc, cb);
        rtrie_dfs(t->sibling, cc, cb);
    }
}

void rtrie_free(rtrie *t, void *cc, rtrie_cb cb)
{
    if( t != NULL ) {
        if( !rtrie_emptyval(t->v) ) {
            safecall(unit, cb, cc, t->ka, t->ke, t->v);
        }

        rtrie_free(t->link, cc, cb);
        rtrie_free(t->sibling, cc, cb);

        free(t->keymem);
        free(t);
    }
}

static bool rtrie_lookup_with_parent( rtrie *t
                                    , char *key
                                    , size_t len
                                    , rtrie **p
                                    , rtrie **l
                                    , void *cc
                                    , rtrie_cb cb)
{
    char *s  = (char *) key;

    if( t == NULL || key[0] == '\0' ) {
        return false;
    }

    size_t pl = rtrie_prefix_len(s, t->ka, t->ke);

    if( pl == 0 ) {
        if( p != NULL ) {
            *p = t;
        }

        return rtrie_lookup_with_parent(t->sibling, key, len, p, l, cc, cb);
    }

    // partial match

    size_t kl = rtrie_klen(t->ka, t->ke);

    if( pl == kl && !rtrie_empty(t) ) {
        if( l != NULL ) {
            *l = t;
        }

        safecall(unit, cb, cc, t->ka, t->ke, t->v);
    }

    // full match
    if( len == kl && pl == kl && !rtrie_empty(t) ) {
        return true;
    }

    if( p != NULL ) {
        *p = t;
    }

    return rtrie_lookup_with_parent(t->link, s + pl, len - pl, p, l, cc, cb);
}

bool rtrie_lookup( rtrie *t
                 , char *key
                 , size_t len
                 , rtrie **l
                 , void* cc
                 , rtrie_cb cb)
{
    return rtrie_lookup_with_parent(t, key, len, NULL, l, cc, cb);
}

// TODO: delete node
void rtrie_del(rtrie *t, char *s, size_t len, void *cc, rtrie_cb clean)
{
    assert(t != NULL);

    rtrie *l = 0, *p = 0;

    if( !rtrie_lookup_with_parent(t, s, len, &p, &l, cc, NULL) || !l ) {
        return;
    }

    // cleanup node anyway
    safecall(unit, clean, cc, l->ka, l->ke, l->v);
    l->v = emptyptr;

    if( p == NULL ) {
        // ... and it's root
        if( l->link == NULL ) {
            free(l->keymem);
            l->keymem = NULL;
            rtrie_assign(t, nilptr, nilptr, emptyptr, false);
        }

        return;
    }

    if( l->link == NULL ) {
        // ... and it's leaf
        if( p->sibling == l ) {
            p->sibling = l->sibling;
        } else if( p->link == l ) {
            p->link = l->sibling;
        }

        free(l->keymem);
        free(l);
    } else if( l->link->link == NULL && l->link->sibling == NULL ) {
        // ... and it's not leaf (try merge keys)
        rtrie *ll = l->link;
        size_t l1 = rtrie_klen(l->ka, l->ke);
        size_t l2 = rtrie_klen(ll->ka, ll->ke);
        char *b = malloc(l1 + l2 + 1);

        memcpy(b, l->ka, l1);
        memcpy(b + l1, ll->ka, l2);
        b[l1 + l2] = '\0';
        free(l->keymem);
        l->keymem = b;
        rtrie_assign(l, b, b + l1 + l2, ll->v, false);
        free(ll->keymem);
        safecall(unit, clean, cc, ll->ka, ll->ke, ll->v);
        free(ll);
        l->link = NULL;
    }
}

char* rtrie_tocstring(char *buf, size_t len, char *sa, char *se)
{
    char *p = buf;
    size_t l = 0;

    for( ; sa < se && l < len; sa++, p++) {
        *p = *sa;
    }

    *p = 0;

    return buf;
}

// TODO: iterate with prefix?
