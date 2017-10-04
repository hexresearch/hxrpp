#ifndef __hexsockaddr_h
#define __hexsockaddr_h

#ifdef HEXSOCKADDR_PLATFORM_DEFAULT
#include "hexsockaddr_platform_default.h"
#else
#include "hexsockaddr_platform.h"
#endif

typedef union {
  struct sockaddr         sa;
  struct sockaddr_in      s4;
  struct sockaddr_in6     s6;
  struct sockaddr_storage ss;
} hexsockaddr_t;

void hexsockaddr_new(int family, uint16_t port, hexsockaddr_t *hsa);

bool hexsockaddr_eq(hexsockaddr_t *a, hexsockaddr_t *b);

bool hexsockaddr_eq_addr(hexsockaddr_t *a, hexsockaddr_t *b);

void hexsockaddr_cpy(hexsockaddr_t *a, hexsockaddr_t *b);

const char *hexsockaddr_fmt(char *buf, size_t len, bool port, hexsockaddr_t *sa);

static inline bool hexsockaddr_ipv6( hexsockaddr_t *sa ) {
    return sa->sa.sa_family == AF_INET6;
}

static inline bool hexsockaddr_ipv4( hexsockaddr_t *sa ) {
    return sa->sa.sa_family == AF_INET;
}

static inline bool hexsockaddr_eq_(void *a, void *b) {
    return hexsockaddr_eq(a,b);
}

static inline bool hexsockaddr_eq_addr_(void *a, void *b) {
    return hexsockaddr_eq_addr(a,b);
}

static inline void hexsockaddr_cpy_(void *a, void *b) {
    hexsockaddr_cpy(a,b);
}

static inline uint16_t hexsockaddr_port(hexsockaddr_t *sa) {
    return hexsockaddr_ipv4(sa) ? htons(sa->s4.sin_port)
                                : htons(sa->s6.sin6_port);
}

bool hexsockaddr_parse(hexsockaddr_t *dst, bool port, const char *src);

uint32_t hexsockaddr_hash(void *a);
uint32_t hexsockaddr_hash_addr(void *a);


#endif
