#define _GNU_SOURCE

#include <arpa/inet.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <fcntl.h>
#include <linux/filter.h>
#include <linux/if.h>
#include <linux/if_packet.h>
#include <linux/ip.h>
#include <linux/limits.h>
#include <net/ethernet.h>
#include <poll.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>
#include <inttypes.h>

#include "hxrpp.h"
#include "hxrpp_server.h"
#include "utils/app_config.h"

struct hxrpp_server_cfg {
    bool fake;
    struct app_config *cfg;
    char cfg_mem[0];
};


#define APP_CONFIG_SIZE ((sizeof(struct hxrpp_server_cfg)) + app_config_size)

static void * __alloc(void *cc, size_t n) {
    return malloc(n);
}

static void __dealloc(void *cc, void *mem) {
    free(mem);
}


static struct hxrpp_server_cfg *init_app_config(void *mem, size_t size) {
    assert( size >= APP_CONFIG_SIZE );

    memset(mem, 0, size);

    struct hxrpp_server_cfg *app = mem;
    app->cfg = (void*)app->cfg_mem;

    struct app_config *cfg =
        app_config_create( app_config_size
                         , app->cfg_mem
                         , 0
                         , __alloc
                         , __dealloc
                         );

    return app;
}


static bool create_app_socket( struct hxrpp_server_cfg *app
                             , hexsockaddr_t *sa
                             , int *sock ) {

    const uint16_t port = 8765;

    hexsockaddr_new(AF_INET, port, sa);

    if ((*sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
       perror("socket()");
       exit(-1);
    }

    int opt = 1;

    if( setsockopt( *sock
                  , SOL_SOCKET
                  , SO_REUSEADDR
                  , &opt, sizeof(opt)) != 0 ) {
       perror("sock: reuse");
       exit(-1);
    }

    if( fcntl(*sock, F_SETFL, O_NONBLOCK, 1) == -1 ) {
       perror("sock: nonblock");
       exit(-1);
    }

    return true;
}

int main(int argc, char **argv)
{
    // TODO: init with command line args
    char conf_mem[APP_CONFIG_SIZE];

    struct hxrpp_server_cfg *app = init_app_config(conf_mem, sizeof(conf_mem));

    if( !app_config_load(app->cfg, argc, argv) ) {
        exit(-1);
    }

    // TODO: setup signals

    // TODO: create socket

    hexsockaddr_t sa;
    int sock = -1;
    if( !create_app_socket(app, &sa, &sock) ) {
        fprintf(stderr, "can't create socket\n");
        exit(-1);
    }

    // TODO: bind to UDP socket

    if (bind(sock, (struct sockaddr *)&sa, sizeof(sa)) < 0) {
        fprintf(stderr, "can't bind to socket\n");
        exit(-1);
    }

    // TODO: listen/poll UDP socket
    struct pollfd fds[2];
    memset(fds, 0, sizeof(fds));
    fds[1].fd = sock;
    fds[1].events = POLLIN;

    // FIXME: remove log message
    fprintf(stdout, "polling...\n");

    for( ;;) {
        int ret = poll(fds, sizeof(fds)/sizeof(fds[0]), -1);

        // TODO: when got UDP message

        char data[256] = { 0 };
        hexsockaddr_t orig = { 0 };

        socklen_t orig_size = sizeof(hexsockaddr_t);

        hxrpp_session_init_msg_t hxrmsg = { 0 };

        struct sockaddr saa = { 0 };

        int len = recvfrom( fds[ret].fd
                          , hxrmsg.raw
                          , sizeof(hxrmsg.raw)
                          , MSG_DONTWAIT
                          , (struct sockaddr*)&orig
                          , &orig_size );
        data[len] = 0;

        char tmp[256];
        fprintf(stdout, "got message %d %d %d %d %d %s\n"
                      , ret
                      , sock
                      , fds[ret].fd
                      , orig_size
                      , len
                      , hexsockaddr_fmt(tmp, sizeof(tmp), true, &orig)
                      );

        // TODO:   read UDP message

        // TODO:   start packet pairs generation with
        //         given length and given delay
        //         during the given time

        const int packet_size = 1024;
        hxrpp_usec_t pair_gap = { .u = 100000 };
        hxrpp_usec_t period   = { .u = 5 * 1000000 };

        hxrpp_send_pkt_pairs( fds[ret].fd
                            , &orig
                            , packet_size
                            , &pair_gap
                            , &period
                            , 0
                            , 0 );

    }


    return 0;
}


