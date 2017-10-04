#include "hexsockaddr.h"

#include <string.h>

void hexsockaddr_new(int family, uint16_t port, hexsockaddr_t *hsa) {
    memset(hsa, 0, sizeof(hexsockaddr_t));
    hsa->sa.sa_family = family;

    if( hexsockaddr_ipv4(hsa) ) {
        hsa->s4.sin_addr.s_addr = INADDR_ANY;
        hsa->s4.sin_port = htons(port);
        return;
    }

    if( hexsockaddr_ipv6(hsa) ) {
        memset(&hsa->s6.sin6_addr.s6_addr, 0, sizeof(hsa->s6.sin6_addr.s6_addr));
        hsa->s6.sin6_port = htons(port);
        return;
    }
}

bool hexsockaddr_eq(hexsockaddr_t *a, hexsockaddr_t *b) {

    if( hexsockaddr_ipv4(a) && hexsockaddr_ipv4(b) ) {
        return (0 == memcmp(&a->s4, &b->s4, sizeof(a->s4)));
    }

    if( hexsockaddr_ipv6(a) && hexsockaddr_ipv6(b) ) {
        return (0 == memcmp(&a->s6, &b->s6, sizeof(a->s6)));
    }

    return false;
}

bool hexsockaddr_eq_addr(hexsockaddr_t *a, hexsockaddr_t *b) {

    if( hexsockaddr_ipv4(a) && hexsockaddr_ipv4(b) ) {
        return (0 == memcmp(&a->s4.sin_addr, &b->s4.sin_addr, sizeof(a->s4.sin_addr)));
    }

    if( hexsockaddr_ipv6(a) && hexsockaddr_ipv6(b) ) {
        return (0 == memcmp(&a->s6.sin6_addr, &b->s6.sin6_addr, sizeof(a->s6.sin6_addr)));
    }

    return false;
}


void hexsockaddr_cpy(hexsockaddr_t *a, hexsockaddr_t *b) {
    memcpy(a, b, sizeof(hexsockaddr_t));
}

const char *hexsockaddr_fmt(char *buf, size_t len, bool port, hexsockaddr_t *sa) {

    char tmp[512] = { 0 };

    if( hexsockaddr_ipv4(sa) ) {
        inet_ntop(AF_INET, (struct sockaddr*)&sa->s4.sin_addr, tmp, sizeof(tmp));
    }

    if( hexsockaddr_ipv6(sa) ) {
        inet_ntop(AF_INET6, (struct sockaddr*)&sa->s6.sin6_addr, tmp, sizeof(tmp));
    }

    char pbuf[16] = { 0 };
    snprintf(pbuf, sizeof(pbuf), "%s%d", hexsockaddr_ipv4(sa) ? ":" : "#"
                                       , hexsockaddr_port(sa));
    snprintf(buf, len, "%s%s", tmp, port ? pbuf : "" );

    return buf;
}

bool hexsockaddr_parse(hexsockaddr_t *dst, bool port, const char *src) {
    int family = -1;
    const char *p = src;
    char tmp[256];

    char *pt = tmp;
    char *pte = tmp + sizeof(tmp)-1;

    for(p = src; *p; p++, pt++ ) {
        if( *p == '.' && family < 0 ) {
            family = AF_INET;
        }
        if( *p == ':' && family < 0 ) {
            family = AF_INET6;
        }

        if( pt < pte ) {
            *pt = *p;
        }

        if( family == AF_INET && (*p == ':' || *p == '#') ) {
            if( pt < pte ) *pt = 0;
            break;
        }
        if( family == AF_INET6 && *p == '#' ) {
            if( pt < pte ) *pt = 0;
            break;
        }
    }

    if( family < 0 ) {
        return false;
    }

    *pt = 0;

    if( family == AF_INET ) {
        hexsockaddr_new(AF_INET, 0, dst);
        inet_pton(family, tmp, &dst->s4.sin_addr);
    } else if( family == AF_INET6 ) {
        hexsockaddr_new(AF_INET6, 0, dst);
        inet_pton(family, tmp, &dst->s6.sin6_addr);
    }

    if( !port ) {
        return true;
    }

    uint16_t pnum = 0;
    for(p++; *p && isdigit(*p); p++ ) {
        pnum *= 10;
        pnum += *p - '0';
    }

    if( family == AF_INET ) {
        dst->s4.sin_port = htons(pnum);
    } else if( family == AF_INET6 ) {
        dst->s6.sin6_port = htons(pnum);
    }

    return true;
}

static inline uint32_t __fletcher32( uint16_t const *data, size_t words ) {
    uint32_t sum1 = 0xffff, sum2 = 0xffff;

    while (words) {
        unsigned tlen = words > 359 ? 359 : words;
        words -= tlen;

        do {
                sum2 += sum1 += *data++;
        } while (--tlen);

        sum1 = (sum1 & 0xffff) + (sum1 >> 16);
        sum2 = (sum2 & 0xffff) + (sum2 >> 16);
    }

    /* Second reduction step to reduce sums to 16 bits */
    sum1 = (sum1 & 0xffff) + (sum1 >> 16);
    sum2 = (sum2 & 0xffff) + (sum2 >> 16);
    return sum2 << 16 | sum1;
}


uint32_t hexsockaddr_hash(void *a) {
    hexsockaddr_t *sa = a;
    return __fletcher32((uint16_t*)sa, sizeof(*sa)/sizeof(uint16_t));
}

uint32_t hexsockaddr_hash_addr(void *a) {
    hexsockaddr_t *sa = a;

    if( hexsockaddr_ipv4(sa) ) {
        return (uint32_t)sa->s4.sin_addr.s_addr;
    }

    if( hexsockaddr_ipv6(sa) ) {
        return  __fletcher32( (uint16_t *)(sa->s6.sin6_addr.s6_addr)
                            , sizeof(sa->s6.sin6_addr.s6_addr) / sizeof(uint16_t) );
    }

    return 0;
}

