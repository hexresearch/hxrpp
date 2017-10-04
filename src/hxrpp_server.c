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

#include "utils/app_config.h"
#include "hxrpp_server.h"

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

int main(int argc, char **argv)
{
    // TODO: init with command line args
    char conf_mem[APP_CONFIG_SIZE];

    struct hxrpp_server_cfg *app = init_app_config(conf_mem, sizeof(conf_mem));

    if( !app_config_load(app->cfg, argc, argv) ) {
        exit(-1);
    }


    // TODO: bind to UDP socket
    // TODO: listen UDP socket
    // TODO: if got UDP message
    // TODO:   start packet pairs generation with
    //         given length and given delay
    //         during the given time

    return 0;
}




