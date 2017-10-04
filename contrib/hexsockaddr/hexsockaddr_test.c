#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>


#include "hexsockaddr.h"

void test_hexsockaddr4_new_1(void) {
    hexsockaddr_t sa;
    hexsockaddr_new(AF_INET, 0, &sa);

    char tmp[256];
    fprintf(stdout, "%s\n", hexsockaddr_fmt(tmp, sizeof(tmp), false, &sa));
    fprintf(stdout, "%s\n", hexsockaddr_fmt(tmp, sizeof(tmp), true, &sa));
}


void test_hexsockaddr4_new_2(void) {
    hexsockaddr_t sa;
    hexsockaddr_new(AF_INET, 53, &sa);

    char tmp[256];
    fprintf(stdout, "%s\n", hexsockaddr_fmt(tmp, sizeof(tmp), true, &sa));
}


void test_hexsockaddr6_new_1(void) {
    hexsockaddr_t sa;
    hexsockaddr_new(AF_INET6, 0, &sa);

    char tmp[256];
    fprintf(stdout, "%s\n", hexsockaddr_fmt(tmp, sizeof(tmp), false, &sa));
    fprintf(stdout, "%s\n", hexsockaddr_fmt(tmp, sizeof(tmp), true, &sa));
}

void test_hexsockaddr6_new_2(void) {
    hexsockaddr_t sa;
    hexsockaddr_new(AF_INET6, 1053, &sa);

    char tmp[256];
    fprintf(stdout, "%s\n", hexsockaddr_fmt(tmp, sizeof(tmp), true, &sa));
}


struct {
    bool port;
    char *s;
} addr[]  = {
    {false, "0.0.0.0"}
  , {false, "127.0.0.1"}
  , {false, "192.168.1.1"}
  , {false,"::1"}
  , {false,"::"}
  , {false,"3a00:1450:401b:802::200e"}
  , {true, "127.0.0.1:53"}
  , {true, "127.0.0.1:1053"}
  , {true, "127.0.0.1:"}
  , {true,"::1"}
  , {true,"2a00:1450:401b:802::200e#53"}
  , {true,""}
  , {true,"vae0giQu"}
};


void test_hexsockaddr_parse_1(void) {

    for(size_t i = 0; i < sizeof(addr)/sizeof(addr[0]); i++ ) {
        hexsockaddr_t sa;
        hexsockaddr_new(AF_INET, 0, &sa);
        bool ok = hexsockaddr_parse(&sa, addr[i].port, addr[i].s);
        char tmp[256];
        fprintf(stdout, "%s %s %-32s %-32s\n"
                      , addr[i].port ? "p" : "n"
                      , ok ? "+" : "-"
                      , addr[i].s
                      , hexsockaddr_fmt(tmp, sizeof(tmp), true, &sa));
    }

}


void test_hexsockaddr_eq_1(void) {

    for(size_t pass = 0; pass < 3; pass++ ) {

        fprintf(stdout, "pass %ld\n", pass);

        for(size_t i = 0; i < sizeof(addr)/sizeof(addr[0]); i++ ) {
            for(size_t j = 0; j < sizeof(addr)/sizeof(addr[0]); j++ ) {
                hexsockaddr_t sa1;
                hexsockaddr_new(AF_INET, 0, &sa1);
                hexsockaddr_parse(&sa1, addr[i].port, addr[i].s);

                hexsockaddr_t sa2;
                hexsockaddr_new(AF_INET, 0, &sa2);
                hexsockaddr_parse(&sa2, addr[j].port, addr[j].s);

                char ip1[256];
                char ip2[256];

                bool eq   = hexsockaddr_eq(&sa1, &sa2);
                bool eq_a = hexsockaddr_eq_addr(&sa1, &sa2);

                if( (pass == 0 && eq_a) || (pass == 1 && !eq && eq_a) || (pass == 2 && eq && eq_a)  ) {
                    fprintf(stdout, "%s %-32s %s %-32s\n"
                                  , addr[i].port ? "p" : "n"
                                  , hexsockaddr_fmt(ip1, sizeof(ip1), true, &sa1)
                                  , eq ? "==" : "!="
                                  , hexsockaddr_fmt(ip2, sizeof(ip2), true, &sa2)
                                  );
                }

            }
        }

    }
}


void test_hexsockaddr_cpy_1(void) {
    for(size_t i = 0; i < sizeof(addr)/sizeof(addr[0]); i++ ) {
        hexsockaddr_t sa1;
        hexsockaddr_new(AF_INET, 0, &sa1);
        hexsockaddr_parse(&sa1, addr[i].port, addr[i].s);

        hexsockaddr_t sa2;
        hexsockaddr_new(AF_INET, 0, &sa2);
        hexsockaddr_cpy(&sa2, &sa1);

        char ip1[256];
        char ip2[256];

        bool eq   = hexsockaddr_eq(&sa1, &sa2);

        fprintf(stdout, " %-32s %s %-32s\n"
                      , hexsockaddr_fmt(ip1, sizeof(ip1), true, &sa1)
                      , eq ? "==" : "!="
                      , hexsockaddr_fmt(ip2, sizeof(ip2), true, &sa2)
                      );

    }
}


void test_hexsockaddr_hash_1(void) {
    for(size_t i = 0; i < sizeof(addr)/sizeof(addr[0]); i++ ) {
        hexsockaddr_t sa1;
        hexsockaddr_new(AF_INET, 0, &sa1);
        hexsockaddr_parse(&sa1, addr[i].port, addr[i].s);

        char ip1[256];

        fprintf(stdout, "%s %-32s %08x %08x\n"
                      , addr[i].port ? "p" : "n"
                      , hexsockaddr_fmt(ip1, sizeof(ip1), true, &sa1)
                      , hexsockaddr_hash(&sa1)
                      , hexsockaddr_hash_addr(&sa1)
                      );

    }
}

