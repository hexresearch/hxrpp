#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "t/test-suite.h"

static struct test_ {
    void (*test_fun)(void);
    const char *name;
} tests[] = {
     { test_hexsockaddr4_new_1,    "test_hexsockaddr4_new_1"  }
    ,{ test_hexsockaddr4_new_2,    "test_hexsockaddr4_new_2"  }
    ,{ test_hexsockaddr6_new_1,    "test_hexsockaddr6_new_1"  }
    ,{ test_hexsockaddr6_new_2,    "test_hexsockaddr6_new_2"  }
    ,{ test_hexsockaddr_parse_1,   "test_hexsockaddr_parse_1" }
    ,{ test_hexsockaddr_eq_1,      "test_hexsockaddr_eq_1"    }
    ,{ test_hexsockaddr_cpy_1,     "test_hexsockaddr_cpy_1"   }
    ,{ test_hexsockaddr_hash_1,    "test_hexsockaddr_hash_1"  }
    ,{ 0,        ""         }
};

void tests_run(int n) {
    int k = sizeof(tests)/sizeof(tests[0]);
    int i = n >= 0 ? n % k : 0;
    int l = n >= 0 ? i + 1 : k;
    for(; i < l && tests[i].test_fun; i++) {
        fprintf(stderr, "\n=== TEST STARTED  (%s)\n", tests[i].name );
        tests[i].test_fun();
        fprintf(stderr, "\n=== TEST FINISHED (%s)\n", tests[i].name );
    }
}

int main(int argc, char **argv) {

    if( argc < 2 ) {
        tests_run(-1);
        return 0;
    }

    if( !strncmp("list", argv[1], strlen("list")) ) {
        int i = 0;
        int k = sizeof(tests)/sizeof(tests[0]);
        for(i = 0; i < k && tests[i].test_fun; i++) {
            fprintf(stderr, "%3d %s\n", i, tests[i].name );
        }
        return 0;
    }

    char *e = argv[1];
    long n = strtol(argv[1], &e, 10);
    if( e > argv[1] ) {
        tests_run(n);
        return 0;
    }

    return -1;
}


