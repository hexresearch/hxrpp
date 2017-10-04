#include <assert.h>
#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "substr.h"

static bool __eq_simple(char a, char b) {
    return tolower(a) == tolower(b);
}

static size_t __off(const char *const a, const char *const b) {
    return (size_t)(b - a);
}

void test_substr_1(void) {

    struct {
        const char *src;
        const char *pattern;
    } t[] = { { "ABA", "BA" }
            , { "aba", "Ba" }
            , { "ABABABA", "AZAZA" }
            , { "ABABAazazaAJHSJHAS AJSH ", "AzAzA" }
            , { "", "KitA" }
            , { "", "" }
            , { "a1.b2.c4-fff.googlevideo.com", "googlevideo" }
            , { "a1.b2.c4-fff.fcdn.akamai.cn",  "fcdn" }
            , { "a1.b2.c4-fff.fcdn.akamai.cn",  "a1" }
            , { "a1.b2.c4-fff.fcdn.akamai.cn",  ".cn" }
            };

    size_t i = 0;
    for(; i < sizeof(t)/sizeof(t[0]); i++ ) {
        const char *ss = find_substr(t[i].src, t[i].pattern, __eq_simple);
        fprintf(stdout, "%s '%s' in '%s' at %zu\n"
                      , ss ? "found" : "not found"
                      , t[i].pattern
                      , t[i].src
                      , ss ? __off(t[i].src, ss) : strlen(t[i].src)
                      );
    }

}
