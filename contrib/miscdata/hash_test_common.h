#ifndef __hash_test_common_h
#define __hash_test_common_h

#include <stdio.h>
#include <stddef.h>

#include "hash.h"

static inline void print_hash_stat(struct hash *c) {
    size_t cap = 0, used = 0, cls = 0, maxb = 0;
    hash_stats(c, &cap, &used, &cls, &maxb);

    fprintf( stdout
           , "capacity:         %zu\n"
             "used:             %zu\n"
             "collisions (avg): %zu\n"
             "max. row:         %zu\n"
           , cap
           , used
           , cls
           , maxb
           );

}

#endif
