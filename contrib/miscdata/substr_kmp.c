#include <stddef.h>
#include <string.h>

#include "substr.h"

const char *find_substr( const char *text
                       , const char *pattern
                       , bool (*eq)(char,char) ) {

    int T[(strlen(pattern)+1) * sizeof(int)];
    int i, j;
    const char *result = 0;

    T[0] = -1;

    if (pattern[0] == '\0')
        return text;

    for (i=0; pattern[i] != '\0'; i++) {
        T[i+1] = T[i] + 1;
        while (T[i+1] > 0 && !eq(pattern[i], pattern[T[i+1]-1]) )
            T[i+1] = T[T[i+1]-1] + 1;
    }

    /* Perform the search */
    for (i=j=0; text[i] != '\0'; ) {
        if (j < 0 || eq(text[i], pattern[j]) ) {
            ++i, ++j;
            if (pattern[j] == '\0') {
                result = text+i-j;
                break;
            }
        }
        else j = T[j];
    }

    return result;
}

