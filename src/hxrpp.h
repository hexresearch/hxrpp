#ifndef __hxrpp_h
#define __hxrpp_h

#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include "hexsockaddr/hexsockaddr.h"

typedef struct hxrpp_usec {
    useconds_t u;
} hxrpp_usec_t;

typedef union hxrpp_session_init_msg {
    char raw[256];
} hxrpp_session_init_msg_t;


bool hxrpp_send_pkt_pairs( int socket
                         , hexsockaddr_t *dst
                         , int size
                         , hxrpp_usec_t *gap
                         , hxrpp_usec_t *due
                         , void *pkt_init_cc
                         , void (*pkt_data_init)(void *, int size, char *pkt ) );

#endif
