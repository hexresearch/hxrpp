#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "clos.h"
#include "slist.h"

static void dummy0(void *cc) {
    fprintf(stdout, "dummy0\n");
}

static void dummy2(void *cc, int x) {
    fprintf(stdout, "dummy2 %d!\n", x + *(int*)cc);
}

void test_clos_1(void) {
    clos_mem_t clos1;
    call_clos0(void (*)(void *), mk_clos(&clos1, dummy0, 0));

    {
        clos_mem_t clos2;
        int tmp = 10;
        call_clos(void (*)(void *, int), mk_clos(&clos2, dummy2, &tmp), 4);
    }

    {
        clos_mem_t clos3;
        int tmp = 66;
        call_clos(void (*)(void *, int), mk_clos(&clos3, dummy2, &tmp), 22);
    }
}

