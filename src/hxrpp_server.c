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

    size_t max_packets;
    uint16_t packet_size;
    hxrpp_usec_t train_gap;
    hxrpp_usec_t period;

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

    if( fcntl(*sock, F_SETFL, O_NONBLOCK, 1) == -1 ) {
       perror("sock: nonblock");
       exit(-1);
    }

    return true;
}

void wait_client_and_send( struct hxrpp_server_cfg *app ) {
    fprintf(stderr, "wait_client_and_send\n");


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

    int ret = poll(fds, sizeof(fds)/sizeof(fds[0]), -1);

    // TODO: when got UDP message

    char data[256] = { 0 };
    hexsockaddr_t orig = { 0 };

    socklen_t orig_size = sizeof(hexsockaddr_t);

    hxrpp_session_init_msg_t hxrmsg = { 0 };

    int len = recvfrom( fds[ret].fd
                      , hxrmsg.raw
                      , sizeof(hxrmsg.raw)
                      , MSG_DONTWAIT
                      , (struct sockaddr*)&orig
                      , &orig_size );

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

    hxrpp_send_pkt_pairs( fds[ret].fd
                        , &orig
                        , app->packet_size
                        , &app->train_gap
                        , &app->period
                        , 0
                        , 0 );
    close(sock);
}

void receive_packets_from_remote( struct hxrpp_server_cfg *app, hexsockaddr_t *remote) {
    char tmp[256];
    fprintf(stderr, "receive_packets_from_remote %s\n", hexsockaddr_fmt(tmp, sizeof(tmp), true, remote));

    hexsockaddr_t sa;

    int sock = -1;
    if( !create_app_socket(app, &sa, &sock) ) {
        fprintf(stderr, "can't create socket\n");
        exit(-1);
    }

    int val = 1;
    int err = setsockopt(sock, SOL_SOCKET, SO_TIMESTAMPNS, &val, sizeof(val));

    fprintf(stderr, "err %d %08x\n", err, val);

    socklen_t ssize = hexsockaddr_ipv4(remote) ? sizeof(struct sockaddr_in)
                                               : sizeof(struct sockaddr_in6);

    if(connect(sock, (struct sockaddr*)remote, ssize ) < 0 ) {
       char tmp[256];
       fprintf(stderr, "cant' connect to %d %s\n"
                     , sock
                     , hexsockaddr_fmt(tmp, sizeof(tmp), true, remote));
        exit(-1);
    }

/*    if( err < 0 ) {*/
        fprintf(stderr, "wtf %d\n", err);
/*    }*/

    hxrpp_session_init_msg_t init = { 0 };

    struct timespec timeout;
    timeout.tv_sec  = 5;
    timeout.tv_nsec = 0;

    struct iovec iov[1];
    iov[0].iov_base = &init;
    iov[0].iov_len  = sizeof(init);

    struct msghdr hdr;
    memset(&hdr, 0, sizeof(hdr));
    hdr.msg_name    = remote;
    hdr.msg_namelen = ssize;
    hdr.msg_iov     = iov;
    hdr.msg_iovlen  = 1;

    sendmsg(sock, &hdr, 0);

    // TODO: listen/poll UDP socket
    struct pollfd fds[2];
    memset(fds, 0, sizeof(fds));
    fds[1].fd = sock;
    fds[1].events = POLLIN;


    fprintf(stdout, "# gap: %ld\n", app->train_gap.u);

    for(;;) {

        const int train = 2;
        const int BUFSIZE = 2048;
        const int TIMESTAMP_LEN = 40;

        struct mmsghdr msgs[train];
        struct iovec iovecs[train];
        char bufs[train][BUFSIZE];
        char tss[train][TIMESTAMP_LEN];

        memset(msgs, 0, sizeof(msgs));

        for(int i = 0; i < train; i++) {
            iovecs[i].iov_base         = &bufs[i];
            iovecs[i].iov_len          = BUFSIZE;
            msgs[i].msg_hdr.msg_iov    = &iovecs[i];
            msgs[i].msg_hdr.msg_iovlen = 1;
            msgs[i].msg_hdr.msg_control = &tss[i];
            msgs[i].msg_hdr.msg_controllen = TIMESTAMP_LEN;
        }

        int ret = poll(fds, sizeof(fds)/sizeof(fds[0]), 5000);

        int retval = recvmmsg(sock, msgs, train, 0, &timeout);

        if( !retval ) {
            break;
        }

        if(retval == -1) {
            fprintf(stderr, "recvmmsg() screwed\n\n");
            exit(-1);
        }

        for(int i = 0; i < retval; i++ ) {

            int level, type;
            struct cmsghdr *cm;
            struct timespec *ts;

            for (cm = CMSG_FIRSTHDR(&msgs[i].msg_hdr); cm != NULL; cm = CMSG_NXTHDR(&msgs[i].msg_hdr, cm)) {
                level = cm->cmsg_level;
                type  = cm->cmsg_type;
                if (SOL_SOCKET == level && SO_TIMESTAMPNS == type) {
                    ts = (struct timespec *) CMSG_DATA(cm);
                    fprintf(stdout, "SW TIMESTAMP[%d] %ld.%09ld\n", i, (long)ts[0].tv_sec, (long)ts[0].tv_nsec);
/*                    printf("TIMESTAMP[%d] %ld\n", i, timespec_to_useconds(ts));*/
                }
            }
        }

    }

    close(sock);
}

int main(int argc, char **argv)
{
    // TODO: init with command line args
    char conf_mem[APP_CONFIG_SIZE];

    struct hxrpp_server_cfg *app = init_app_config(conf_mem, sizeof(conf_mem));

    if( !app_config_load(app->cfg, argc, argv) ) {
        exit(-1);
    }

    app->max_packets  = 3000;
    app->packet_size  = 1432;
    app->period.u     = 90000000;
    app->train_gap.u  = app->period.u / (app->max_packets / 2);

    // TODO: setup signals

    long serv_port = 0;
    app_config_opt_get_int(app->cfg, SERVE_ON_PORT, &serv_port);

    fprintf(stderr, "serve %s %d\n", serv_port > 1 ? "yes" : "no", (int)serv_port );

    bool send = false;
    app_config_opt_get_bool(app->cfg, SEND_PACKETS, &send);

    if( serv_port > 0 && send ) {
        wait_client_and_send(app);
    }

    char *remote_str = 0;
    app_config_opt_get_str(app->cfg, REMOTE, &remote_str);
    hexsockaddr_t *sa = 0, sa_mem;

    if( remote_str ) {

        sa = hexsockaddr_parse(&sa_mem, true, remote_str) ? &sa_mem : 0;

        if( remote_str && !sa ) {
            fprintf(stderr, "bad remote addr: %s\n", remote_str);
            exit(-1);
        }

    }

    bool receive = false;
    app_config_opt_get_bool(app->cfg, RECEIVE, &receive);

    if( receive && sa ) {
        receive_packets_from_remote(app, sa);
    }

    fprintf(stderr, "no idea\n");
    exit(-1);


    return 0;
}


