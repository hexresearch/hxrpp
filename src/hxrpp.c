#define _GNU_SOURCE

#include <netinet/ip.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include "hxrpp.h"

hxrpp_usec_t gettimeofday_usec(void) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME_COARSE, &ts);
    useconds_t u1 = ((useconds_t)ts.tv_sec) * 1000000;
    useconds_t u2 = ((useconds_t)ts.tv_nsec) / 1000;
    return (hxrpp_usec_t){ .u = u1 + u2 };
}

bool hxrpp_send_pkt_pairs( int sock
                         , hexsockaddr_t *dst
                         , int size
                         , hxrpp_usec_t *gap
                         , hxrpp_usec_t *due
                         , void *pkt_init_cc
                         , void (*pkt_data_init)(void *, int size, char *pkt ) ) {

    bool ok = true;

    fprintf(stdout, "hxrpp_send_pkt_pairs \n");

    socklen_t ssize = hexsockaddr_ipv4(dst) ? sizeof(struct sockaddr_in)
                                            : sizeof(struct sockaddr_in6);

    if(connect(sock, (struct sockaddr*)dst, ssize ) < 0 ) {
       char tmp[256];
       fprintf(stderr, "cant' connect to %d %s\n"
                     , sock
                     , hexsockaddr_fmt(tmp, sizeof(tmp), true, dst));
       return false;
    }


    char pkt[size];
    if( pkt_data_init ) {
        pkt_data_init(pkt_init_cc, size, pkt);
    } else {
        memset(pkt, '0', size);
    }

    struct mmsghdr msg[1];
    struct iovec msg1[2];

    memset(msg1, 0, sizeof(msg1));
    msg1[0].iov_base = pkt;
    msg1[0].iov_len = size;
    msg1[1].iov_base = pkt;
    msg1[1].iov_len = size;

    memset(msg, 0, sizeof(msg));
    msg[0].msg_hdr.msg_iov = msg1;
    msg[0].msg_hdr.msg_iovlen = 2;

    hxrpp_usec_t started = gettimeofday_usec();
    hxrpp_usec_t now     = started;

    for(; now.u - started.u < due->u ;) {
        if( sendmmsg(sock, msg, 2, 0) == -1 ) {
            ok = false;
            break;
        }
        usleep(gap->u);
        now = gettimeofday_usec();
    }

    return ok;

}

