#include "hash_cstring.h"

uint32_t hash_cstring(void *k)
{
    char *s = k;
    uint32_t hash = 5381;
    int c = 0;

    while( (c = *s++) != '\0' ) {
        hash = ((hash << 5) + hash) + c;
    }

    return hash;
}

