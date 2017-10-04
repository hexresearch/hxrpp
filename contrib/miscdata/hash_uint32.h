#ifndef __hash_uint32_h
#define __hash_uint32_h

#include <stdbool.h>
#include <stdint.h>

uint32_t uint32_hash(void *a);
bool     uint32_eq(void *a, void *b);
void     uint32_cpy(void *a, void *b);

#endif
