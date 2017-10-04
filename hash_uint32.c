#include "hash_uint32.h"

uint32_t uint32_hash(void *a) {
    return *(uint32_t*)a;
}

bool uint32_eq(void *a, void *b) {
    return *(uint32_t*)a == *(uint32_t*)b;
}

void uint32_cpy(void *a, void *b) {
    *(uint32_t*)a = *(uint32_t*)b;
}

