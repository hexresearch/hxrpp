#ifndef __DRADIX_H__
#define __DRADIX_H__

#include <stddef.h>

typedef struct rtrie_ {
    struct rtrie_ *link;
    struct rtrie_ *sibling;
    char          *ka;
    char          *ke;
    void          *v;
    char          *keymem;
} rtrie;

typedef void (*rtrie_cb)(void*, char *, char *, void *);
typedef void (*rtrie_node_cb)(void *, rtrie *);

char* rtrie_tocstring(char *buf, size_t len, char *sa, char *se);
bool rtrie_emptyval(void *);

rtrie *rtrie_nil();

void rtrie_free(rtrie *,void *, rtrie_cb);
bool rtrie_null(rtrie *);

void rtrie_add(rtrie *, char *, size_t, void *);
void rtrie_del(rtrie *, char *, size_t, void *, rtrie_cb);
void rtrie_bfs(rtrie *, void *, rtrie_cb);
void rtrie_dfs(rtrie *, void *, rtrie_cb);
void rtrie_bfs_with_node(rtrie *, void *, rtrie_node_cb);

bool rtrie_lookup(rtrie *, char *, size_t, rtrie **, void *, rtrie_cb);

#endif

