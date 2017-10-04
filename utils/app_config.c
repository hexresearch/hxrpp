#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>
#include <errno.h>

#include "miscdata/slist.h"
#include "miscdata/const_mem_pool.h"
#include "miscdata/hash_cstring.h"

#include "miscdata/hash.h"
#include "miscdata/dradix.h"

#include "utils/app_config.h"

#ifndef APP_CONFIG_OPTS
#define APP_CONFIG_OPTS "app_config_opts_gen.inc"
#endif

struct app_opt_val {
    union {
        unsigned int  w[4];
        bool        v_bool;
        long        v_int;
        char       *v_cstr;
        char      **v_argv;
        slist      *v_strs;
        struct {
            void *vs[32];
            unsigned int i;
        } v_mult;
    } u;
};


struct str_list_item {
    const char *s;
};

struct app_config {
    struct hash  *opt_hash;

    slist *str_list;
    slist *str_free;
    char str_list_mem[1024*slist_size(sizeof(struct str_list_item))];

    slist *args_list;
    slist *args_free;
    char args_list_mem[1024*slist_size(sizeof(struct str_list_item))];

    void *alloc_cc;
    void *(*alloc)(void*, size_t);
    void (*dealloc)(void*, void*);

    char  mempool[512];
};

const size_t app_config_size = sizeof(struct app_config);

static void* def_alloc(void *cc, size_t l) {
    return malloc(l);
}

static void def_dealloc(void *cc, void *mem) {
    free(mem);
}

static bool opt_key_cmp(void *a, void *b) {
    return (0 == strncmp(a, b, APP_OPT_HASH_KEY_LEN));

}

static void opt_key_cpy(void *a, void *b) {
    strncpy(a, b, APP_OPT_HASH_KEY_LEN);
}

static void opt_val_cpy(void *a, void *b) {
    memcpy(a, b, sizeof(struct app_opt_val));
}

struct app_config *app_config_create( size_t len
                                    , void *mem
                                    , void *alloc_cc
                                    , void *(*alloc)(void*,size_t)
                                    , void (*dealloc)(void*,void*)
                                    ) {

    if( len < app_config_size ) {
        return 0;
    }

    struct app_config *cfg = mem;

    struct const_mem_pool *pool = const_mem_pool_create( sizeof(cfg->mempool)
                                                       , cfg->mempool);

    cfg->alloc_cc = alloc_cc;
    cfg->alloc = alloc ? alloc : def_alloc;
    cfg->dealloc = dealloc ? dealloc : def_dealloc;

    cfg->opt_hash = hash_create( hash_size
                               , const_mem_pool_alloc(pool, hash_size)
                               , APP_OPT_HASH_KEY_LEN
                               , sizeof(struct app_opt_val)
                               , 64
                               , hash_cstring
                               , opt_key_cmp
                               , opt_key_cpy
                               , opt_val_cpy
                               , cfg->alloc_cc
                               , cfg->alloc
                               , cfg->dealloc
                               );

    assert( cfg->opt_hash );

    cfg->str_free = slist_pool( cfg->str_list_mem
                              , slist_size(sizeof(struct str_list_item))
                              , sizeof(cfg->str_list_mem));

    cfg->args_free = slist_pool( cfg->args_list_mem
                               , slist_size(sizeof(struct str_list_item))
                               , sizeof(cfg->args_list_mem));

    return cfg;
}

struct opt_decl {

    void (*init)(void*, void*, void*, bool);

    void (*parser)( struct app_config*
                  , struct opt_decl *
                  , void *cc
                  , const char *(*)(void*));

    char *opt;
    char *name;
    const char *env;
    void *def; // ??
    const char *desc;

    /* for options with custom handler */
    void (*custom_fn)(const char *, const char *, void *);
    void *custom_cc;
};


struct app_config_opt_parser {
    int pos;
    int argc;
    char **argv;
    struct app_config *cfg;
};

const char *app_config_opt_next(void *cc) {
    struct app_config_opt_parser *p = cc;
    if( p->pos < p->argc ) {
        p->pos++;
        return p->argv[p->pos];
    }
    return 0;
}

const char *app_config_opt_env(void *cc) {
    return cc;
}

void app_config_opt_set_nah(void *cc, void *k, void *v, bool n) {
}

void app_config_opt_set_bool(void *cc, void *k, void *v, bool n) {
    struct app_opt_val *o = v;
    o->u.v_bool = (bool)cc;
}

void app_config_opt_set_str(void *cc, void *k, void *v, bool n) {
    struct app_opt_val *o = v;
    o->u.v_cstr = cc;
}

void app_config_opt_set_int(void *cc, void *k, void *v, bool n) {
    struct app_opt_val *o = v;
    o->u.v_int = (long)cc;
}

void app_config_opt_set_argv(void *cc, void *k, void *v, bool n) {
    struct app_opt_val *o = v;
    o->u.v_argv = (char**)v;
}

void app_config_opt_set_str_list(void *cc, void *k, void *v, bool n) {
    struct app_opt_val *o = v;
    o->u.v_strs = (slist*)cc;
}

void app_config_opt_set_mult(void *cc, void *k, void *v, bool n) {
    struct app_opt_val *o = v;

    if( n ) {
        o->u.v_mult.i = 0;
    } else if( o->u.v_mult.i ==
              (sizeof(o->u.v_mult.vs) / sizeof(o->u.v_mult.vs[0])) ) {
        fprintf(stderr, "too many instances for option %s\n", (char *)k);
    }

    o->u.v_mult.vs[o->u.v_mult.i++] = cc;
}

void app_config_bool_atom_opt( struct app_config *cfg
                             , struct opt_decl *d
                             , void *cc
                             , const char *(*next)(void*cc)) {

    hash_alter( cfg->opt_hash
              , true
              , (void*)(d->name)
              , (void*)true
              , d->init);
}

void app_config_str_opt( struct app_config *cfg
                       , struct opt_decl *d
                       , void *cc
                       , const char *(*next)(void*cc)) {

    const char *v = next(cc);
    if( v ) {
        hash_alter( cfg->opt_hash
                  , true
                  , (void*)(d->name)
                  , (void*)v
                  , d->init);
    }
}


void app_config_str_list_opt( struct app_config *cfg
                            , struct opt_decl *d
                            , void *cc
                            , const char *(*next)(void*cc)) {

    struct app_config_opt_parser *p = cc;

    const char *s = 0;

    int i = 0;

    slist *e = slist_nil();
    slist *re = slist_nil();

    for(s = next(cc); s; s = next(cc) ) {

        slist *n = slist_uncons(&cfg->str_free);

        if( !n ) {
            break;
        }

        slist_value(struct str_list_item*, n)->s = s;
        e = slist_cons(n, e);
    }

    slist *tmp = slist_nil();
    while(e) {
        re = slist_cons(slist_uncons(&e), re);
    }

    hash_alter( cfg->opt_hash
              , true
              , (void*)(d->name)
              , (void*)re
              , d->init);

}

void app_config_argv_opt( struct app_config *cfg
                        , struct opt_decl *d
                        , void *cc
                        , const char *(*next)(void*cc)) {

    struct app_config_opt_parser *p = cc;

    hash_alter( cfg->opt_hash
              , true
              , (void*)(d->name)
              , (void*)&p->argv[p->pos+1]
              , d->init);
}


void app_config_int_opt( struct app_config *cfg
                       , struct opt_decl *d
                       , void *cc
                       , const char *(*next)(void*cc)) {

    const char *v = next(cc);
    if( v ) {

        char *ep = 0;

        errno = 0;
        long int iv = strtol(v, &ep, 10);

        // FIXME: error notification

        if( ep > v && !errno ) {

            hash_alter( cfg->opt_hash
                      , true
                      , (void*)(d->name)
                      , (void*)iv
                      , d->init);
        }
    }
}

#undef CFG_OPTION
#define CFG_OPTION(i, h, o, e, d, c) {i, (h), "--"o, (o), (e), (d), (c), NULL, NULL},

#undef OPT_SET_NAH
#define OPT_SET_NAH app_config_opt_set_nah

#undef OPT_SET_BOOL
#define OPT_SET_BOOL app_config_opt_set_bool

#undef  OPT_SET_STR
#define OPT_SET_STR  app_config_opt_set_str

#undef  OPT_SET_ARGV
#define OPT_SET_ARGV app_config_opt_set_argv

#undef  OPT_SET_STR_LIST
#define OPT_SET_STR_LIST app_config_opt_set_str_list

#undef  OPT_SET_INT
#define OPT_SET_INT  app_config_opt_set_int

#undef  OPT_SET_MULT
#define OPT_SET_MULT  app_config_opt_set_mult

#undef OPT_BOOL_ATOM
#define OPT_BOOL_ATOM  app_config_bool_atom_opt

#undef OPT_STR
#define OPT_STR        app_config_str_opt

#undef OPT_INT
#define OPT_INT        app_config_int_opt

#undef OPT_ARGV
#define OPT_ARGV     app_config_argv_opt

#undef OPT_STR_LIST
#define OPT_STR_LIST  app_config_str_list_opt

bool app_config_load(struct app_config *cfg, int argc, char **argv) {

/*    if( access(cfg->db_file_name, R_OK) == -1 ) {*/
/*        // TODO: log error*/
/*        return false;*/
/*    }*/

    rtrie *opts = rtrie_nil();

    struct opt_decl decls[] = {
#include APP_CONFIG_OPTS
        { 0 }
    };

    int i = 0;
    for(; decls[i].opt; i++ ) {
        struct opt_decl *d = &decls[i];
        if( d->init && d->init != OPT_SET_NAH ) {
            hash_alter(cfg->opt_hash, true, d->name, d->def, d->init);
        }
        char *env  = 0;
        if( d->env && (env = getenv(d->env)) && d->parser) {
            d->parser(cfg, d, env, app_config_opt_env);
        }
        rtrie_add(opts, d->opt, strlen(d->opt), d);
    }

    struct app_config_opt_parser p = { .pos  = 0
                                     , .argc = argc
                                     , .argv = argv
                                     , .cfg  = cfg
                                     };

    slist *tmp = slist_nil();

    for(p.pos = 1; p.pos < p.argc; p.pos++) {
        rtrie *match = 0;
        char *s = argv[p.pos];
        if( rtrie_lookup(opts, s, strlen(s), &match, 0, 0) ) {
            struct opt_decl *d = match->v;
            assert( d && !rtrie_emptyval(match->v) );
            d->parser(cfg, d, &p, app_config_opt_next);
        } else {

            slist *n = slist_uncons(&cfg->args_free);

            if( !n ) {
                fprintf(stderr, "Too many arguments: stop\n");
                return false;
            }

            slist_value(struct str_list_item*, n)->s = s;
            tmp = slist_cons(n, tmp);
        }
    }

    cfg->args_list = 0;
    while(tmp) {
        cfg->args_list = slist_cons(slist_uncons(&tmp), cfg->args_list);
    }

    rtrie_free(opts, 0, 0);

    bool help = false;
    if( app_config_opt_get_bool(cfg, "help", &help) && help) {
        fprintf(stderr, "Usage: %s OPTIONS ARGS\n", basename(argv[0]));
        fprintf(stderr, "Where OPTIONS\n");
        int i = 0;
        for(; decls[i].opt; i++ ) {
            struct opt_decl *d = &decls[i];
            fprintf(stderr, "%s\t$%s   %s\n"
                          , d->opt  ? d->opt : ""
                          , d->env  ? d->env : "\b\b"
                          , d->desc ? d->desc : ""
                          );
        }
        exit(0);
    }

    return true;
}

void app_config_destroy(struct app_config *cfg) {
    hash_destroy(cfg->opt_hash);
}

bool app_config_opt_get_bool( struct app_config *cfg
                              , const char *k
                              , bool *r) {

    struct app_opt_val *d = hash_get(cfg->opt_hash, (void*)k);
    if( d ) {
        *r = d->u.v_bool;
        return true;
    }
    return false;
}

bool app_config_opt_get_argv( struct app_config *cfg
                              , const char *k
                              , char ***r) {

    struct app_opt_val *d = hash_get(cfg->opt_hash, (void*)k);

    if( d ) {
        *r = d->u.v_argv;
        return true;
    }
    return false;
}

bool app_config_opt_get_str_list( struct app_config *cfg
                                  , const char *k
                                  , void *cc
                                  , void (*fn)(void*, const char *s) ) {

    struct app_opt_val *d = hash_get(cfg->opt_hash, (void*)k);

    if( !d ) {
        return false;
    }

    if( !fn ) {
        return true;
    }

    slist *l = d->u.v_strs;
    for(; l; l = l->next ) {
        struct str_list_item *it = slist_value(struct str_list_item*, l);
        fn(cc, it->s);
    }

    return  true;
}

bool app_config_opt_get_str( struct app_config *cfg
                             , const char *k
                             , char **r) {

    struct app_opt_val *d = hash_get(cfg->opt_hash, (void*)k);
    if( d ) {
        *r = (char*)d->u.v_cstr;
        return true;
    }
    return false;
}

bool app_config_opt_get_int( struct app_config *cfg
                             , const char *k
                             , long *r) {

    struct app_opt_val *d = hash_get(cfg->opt_hash, (void*)k);

    if( d ) {
        *r = d->u.v_int;
        return true;
    }
    return false;
}

bool app_config_opt_get_args_list( struct app_config *cfg
                                 , void *cc
                                 , void (*fn)(void*, const char *s) ) {

    if( !cfg->args_list ) {
        return false;
    }

    if( !fn ) {
        return true;
    }

    slist *l = cfg->args_list;
    for(; l; l = l->next ) {
        struct str_list_item *it = slist_value(struct str_list_item*, l);
        fn(cc, it->s);
    }

    return  true;
}

bool app_config_opt_get_mult( struct app_config *cfg
                            , const char *k
                            , void *cc
                            , void (*fn)(void *, const void *) ) {

    struct app_opt_val *d = hash_get(cfg->opt_hash, (void *)k);
    unsigned int i;

    if( !d || !d->u.v_mult.i ) {
        return false;
    }

    if( !fn ) {
        return true;
    }

    for( i = 0; i < d->u.v_mult.i; i++ ) {
        fn(cc, d->u.v_mult.vs[i]);
    }

    return true;
}

