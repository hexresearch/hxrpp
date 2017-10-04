#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include "dradix.h"

////// TESTS

void dump_node(void *cc, char *sa, char *se, void *v);

void test_dradix_1(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
          {  "A",   31 }
        , {  "AB",  32 }
        , {  "AAC", 33 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_1_1(void) // regression found
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
         {  "ABAK",         1 }
       , {  "AB",           2 }
       , {  "AB2",          3 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        if( buf[i].v > 0 ) {
            rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
        }
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_2(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
          {  "AB",     1 }
        , {  "A",      2 }
        , {  "AB",     3 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_3(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
          {  "AAAAK",  1 }
        , {  "AAAAKZ", 2 }
        , {  "AAAAKM", 3 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_4(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
           {  "AAAAB",  1 }
         , {  "JOPA",   2 }
         , {  "KITA",   3 }
         , {  "AK47",   4 }
         , {  "JORA",   5 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_5(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
         {  "AABA", 1 }
       , {  "AAB",  2 }
       , {  "CCD",  3 }
       , {  "AABC", 4 }
       , {  "",     5 }
       , {  "",     7 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_6(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
         {  "AC",    1 }
       , {  "AABA",  2 }
       , {  "AC",    3 }
       , {  "AC",    4 }
       , {  "AC",    5 }
       , {  "A",     6 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_7(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
         {  "JOPA",    1 }
       , {  "KITA",    2 }
       , {  "PECHEN",  3 }
       , {  "TRESKI",  4 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
    }

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        char k[32];

        rtrie *n = NULL;

        bool r = rtrie_lookup(t, buf[i].k, strlen(buf[i].k), &n, NULL, NULL);
        int  v = -1;

        if( n != NULL ) {
            rtrie_tocstring(k, sizeof(k), n->ka, n->ke);
            v = (n->v != 0) ? *(int *) n->v : -1;
        }

        printf( "FOUND %s: %s (%s,%d) \n"
              , r ? "TRUE" : "FALSE"
              , buf[i].k
              , k
              , v);
    }

    rtrie_free(t, NULL, NULL);
}

void test_dradix_8(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
         {  "A",       1 }
       , {  "ABAK",    2 }
       , {  "BOO",    10 }
       , {  "ABAKAN",  3 }
       , {  "BABA",    4 }
       , {  "AB",      0 }
       , {  "A",       0 }
       , {  "B",      -2 }
       , {  "AR",     -1 }
       , {  "ARBAN",  -2 }
       , {  "MOMOMO", -3 }
       , {  "ZBABA",  -4 }
       , {  "ABAKANBABAI", -5 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        if( buf[i].v > 0 ) {
            rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
        }
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        char k[32];

        rtrie *n = NULL;

        bool r = rtrie_lookup(t, buf[i].k, strlen(buf[i].k), &n, NULL, NULL);
        int  v = -1;

        if( n != NULL ) {
            rtrie_tocstring(k, sizeof(k), n->ka, n->ke);
            v = (n->v != 0) ? *(int *) n->v : -1;
        }

        printf( "FOUND %s: %s (%s,%d)\n"
              , r ? "TRUE" : "FALSE"
              , buf[i].k
              , k
              , v);
    }

    rtrie_free(t, NULL, NULL);
}


struct test9_scan_cc {
    char *s;
    char *p;
    char *pe;
};

void test9_scan(void *cc_, char *sa, char *se, void *v)
{
    struct test9_scan_cc *cc = cc_;

    assert(cc != NULL);

    char *p = cc->p;
    char *pe = cc->pe;

    for( ; p < pe && sa < se; p++, sa++ ) {
        *p = *sa;
    }

    *p = 0;

    printf("test9_scan: (%s)\n", cc->s);
    // rtrie_tocstring(tmp, 255, sa, se));
}

void test10_cb(void *cc_, char *sa, char *se, void *v_)
{
    int v = v_ ? *(int *) v_ : -1;

    printf("found partial match : %d\n", v);
}

void test_dradix_10(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[64]; int v; } buf[] = {
         {  ".",             1 }
       , {  ".moc",          2 }
       , {  ".moc.elgoog",   3 }
       , {  ".moc.elgoog.a", 4 }
       , {  ".ur.xednay",    6 }
       , {  ".ur",           5 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
    }

    struct q { char q[64]; } qq[] = {
         {  ".ur.ay" }
       , {  ".moc.oob" }
       , {  ".gro.fbo" }
       , {  ".moc.abcd" }
       , {  ".moc.elgoog.c" }
       , {  ".moc.elgoog.a.d" }
       , {  ".oof" }
       , {  ".fsf.gro" }
    };

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);
    printf("\n\n");

    for( i = 0; i < sizeof(qq)/sizeof(qq[0]); i++ ) {
        char k[64];

        rtrie *n = NULL;

        printf("LOOKUP %s\n", qq[i].q);

        bool r = rtrie_lookup(t, qq[i].q, strlen(qq[i].q), &n, 0, test10_cb);
        int  v = -1;

        if( n != NULL ) {
            rtrie_tocstring(k, sizeof(buf), n->ka, n->ke);
            v = (n->v != 0) ? *(int *) n->v : -1;
        }

        printf( "FOUND %s: %s (%s,%d)\n"
              , r ? "TRUE" : "FALSE"
              , qq[i].q
              , k
              , v);
    }

    rtrie_free(t, NULL, NULL);
}

void test_dradix_11(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[64]; int v; } buf[] = {
         {  ".moc.elgoog",   1 }
       , {  ".ur.xednay",    2 }
       , {  ".",             3 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
    }

    struct q { char q[64]; } qq[] = {
         {  ".fsf.gro" }
    };

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);
    printf("\n\n");

    for( i = 0; i < sizeof(qq)/sizeof(qq[0]); i++ ) {
        char k[64];
        rtrie *n = NULL;

        printf("LOOKUP %s\n", qq[i].q);

        bool r = rtrie_lookup( t
                             , qq[i].q
                             , strlen(qq[i].q)
                             , &n
                             , NULL
                             , test10_cb);
        int  v = -1;

        if( n != NULL ) {
            rtrie_tocstring(k, sizeof(buf), n->ka, n->ke);
            v = (n->v != 0) ? *(int *) n->v : -1;
        }

        printf( "FOUND %s: %s (%s,%d)\n"
              , r ? "TRUE" : "FALSE"
              , qq[i].q
              , k
              , v);
    }

    rtrie_free(t, NULL, NULL);
}

void test_dradix_12_cb(void *cc, rtrie *t)
{
    printf("node (%s, %s, %s)\n", t->ka, t->ke, t->keymem);
}

void test_dradix_12(void)
{
    rtrie *t = rtrie_nil();
    char *s[] = { "ABAK"
                , "ZHABA"
                , "ABAKAN"
                , "ABA"
                , "XPEH"
                , "XPEHOBYХА"
                };

    rtrie_add(t, s[0], strlen(s[0]), NULL);
    rtrie_add(t, s[1], strlen(s[1]), NULL);
    rtrie_add(t, s[2], strlen(s[2]), NULL);
    rtrie_add(t, s[3], strlen(s[3]), NULL);
    rtrie_add(t, s[4], strlen(s[4]), NULL);
    rtrie_add(t, s[5], strlen(s[5]), NULL);

    rtrie_bfs_with_node(t, NULL, test_dradix_12_cb);
    rtrie_free(t, NULL, NULL);
}

void test_dradix_13(void)
{
    rtrie *t = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
          {  "ABAK",         1 }
        , {  "ABAKAN",       2 }
        , {  "ZHLOB",        3 }
        , {  "KIT",          4 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        if( buf[i].v > 0 ) {
            rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
        }
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    // removing a leaf must be easy
    rtrie_del(t,buf[1].k,strlen(buf[1].k), NULL, NULL);

    rtrie_bfs(t, tmp, dump_node);

    rtrie_del(t,buf[2].k,strlen(buf[2].k), NULL, NULL);

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_14_cb(void *cc, char *sa, char *se, void *v)
{
    rtrie_add((rtrie *) cc, sa, se - sa, v);
}

void test_dradix_14(void)
{
    rtrie *t = rtrie_nil();
    rtrie *t2 = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
          {  "AB",         1 }
        , {  "AC",         2 }
        , {  "AD",         3 }
        , {  "AE",         4 }
        , {  "ACM",        5 }
        , {  "ACK",        6 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        if( buf[i].v > 0 ) {
            rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
        }
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_del(t, buf[2].k, strlen(buf[2].k), NULL, NULL);

    rtrie_bfs(t, tmp, dump_node);

    rtrie_del(t, buf[0].k, strlen(buf[0].k), NULL, NULL);

    rtrie_bfs(t, tmp, dump_node);

    rtrie_del(t, buf[1].k, strlen(buf[1].k), NULL, NULL);

    rtrie_bfs(t, tmp, dump_node);

    printf("new trie:\n");

    rtrie_dfs(t, t2, test_dradix_14_cb);

    rtrie_bfs(t2, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
    rtrie_free(t2, NULL, NULL);
}

void test_dradix_15(void)
{
    rtrie *t  = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
           {  "AB",         1 }
         , {  "AC",        -2 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        if( buf[i].v > 0 ) {
            rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
        }
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_del(t, buf[0].k, strlen(buf[0].k), NULL, NULL);

    rtrie_bfs(t, tmp, dump_node);

    rtrie_add(t, buf[1].k, strlen(buf[1].k), &buf[1].v);

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_16(void)
{
    rtrie *t  = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
           {  "A" ,         1 }
         , {  "AB",         2 }
         , {  "ABC",        3 }
         , {  "ABCD",       4 }
         , {  "ABCDE",      5 }
         , {  "ABCDEF",     6 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        if( buf[i].v > 0 ) {
            rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
        }
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie_del(t, buf[4].k, strlen(buf[4].k), NULL, NULL);

    fprintf(stdout, "\n");

    rtrie_bfs(t, tmp, dump_node);

    rtrie_del(t, buf[3].k, strlen(buf[3].k), NULL, NULL);

    fprintf(stdout, "\n");

    rtrie_bfs(t, tmp, dump_node);

    rtrie_free(t, NULL, NULL);
}

void test17_cb(void *cc_, char *sa, char *se, void *v_)
{
    int v = (v_ != NULL) ? *(int *) v_ : -1;

    printf("found partial match : %d\n", v);
/*    char buf[128];*/
/*    fprintf(stdout, "%s\n", rtrie_tocstring(buf, 128, sa, se));*/
}

void test_dradix_17(void)
{
    rtrie *t  = rtrie_nil();

    struct kv { char k[32]; int v; } buf[] = {
        { ".moc"             , 1 }
      , { ".moc.elgoog"      , 2 }
      , { ".moc.elgoog.tset" , 3 }
      , { "."                , 10}
      , { ".ur"              ,-2 }
      , { ".moc.wtf"         ,-3 }
    };

    int i = 0;

    for( i = 0; i < sizeof(buf)/sizeof(buf[0]); i++ ) {
        if( buf[i].v > 0 ) {
            rtrie_add(t, buf[i].k, strlen(buf[i].k), &buf[i].v);
        }
    }

    char tmp[256];

    rtrie_bfs(t, tmp, dump_node);

    rtrie *n = NULL;
    char key[] = ".moc.elgoog.tset.4a";
    char kl = strlen(key);
    bool r = rtrie_lookup(t, key, kl, &n, 0, test17_cb);
    int v = (n != NULL && n->v != 0) ? *(int *) n->v : -1;

    fprintf(stdout, "%sFOUND MATCH %d\n", r ? "" : "NOT ", v);

    rtrie_free(t, NULL, NULL);
}

void test_dradix_18(void)
{
    fprintf(stdout, "delete empty rtrie\n");

    rtrie *t = rtrie_nil();

    rtrie_free(t, NULL, NULL);
    fprintf(stdout, "done\n");
}

void dump_node(void *cc, char *sa, char *se, void *v)
{
    char *buf = (char *) cc;
    int vv = v != NULL && !rtrie_emptyval(v) ? *(int *) v : -1;

    fprintf(stdout, "%s#%d\n", rtrie_tocstring(buf, 128, sa, se), vv);
}

