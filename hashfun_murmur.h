#ifndef __hashfun_murmur_h
#define __hashfun_murmur_h

#include <stdint.h>

uint32_t hash_murmur3_32( const uint8_t *key
                        , uint32_t len
                        , uint32_t seed);

#endif
