#ifndef __app_config_h
#define __app_config_h

#include <stdbool.h>
#include <stdint.h>

#define APP_OPT_HASH_KEY_LEN 32

struct app_config;

struct app_opt_val;

struct app_config_opt_parser;

extern const size_t app_config_size;

struct app_config *app_config_create( size_t
                                        , void*
                                        , void*
                                        , void *(*alloc)(void*,size_t)
                                        , void (*dealloc)(void*,void*) );

void app_config_destroy(struct app_config *cfg);

bool app_config_load(struct app_config *cfg, int argc, char **argv);

bool app_config_opt_get_bool( struct app_config*
                              , const char*
                              , bool*);

bool app_config_opt_get_str( struct app_config*
                             , const char*
                             , char **);

bool app_config_opt_get_int( struct app_config*
                             , const char*
                             , long*); // dont' ask me why

bool app_config_opt_get_argv( struct app_config *
                              , const char *
                              , char ***);


bool app_config_opt_get_str_list( struct app_config *cfg
                                  , const char *k
                                  , void *cc
                                  , void (*fn)(void*, const char *s) );

bool app_config_opt_get_args_list( struct app_config *cfg
                                 , void *cc
                                 , void (*fn)(void*, const char *s) );

bool app_config_opt_get_mult( struct app_config *cfg
                            , const char *k
                            , void *cc
                            , void (*fn)(void *, const void *) );

#define CFG_OPTION(m, h, o, e, d, c)

#define I(n) ((void*)(n))

#endif
