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

int main(int argc, char **argv)
{
    fprintf(stdout, "JOPA\n\n");

    // TODO: bind to UDP socket
    // TODO: listen UDP socket
    // TODO: if got UDP message
    // TODO:   start packet pairs generation with
    //         given length and given delay
    //         during the given time

    return 0;
}
