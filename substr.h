#ifndef __substr_h
#define __substr_h

#include <stdbool.h>

const char *find_substr( const char *src
                       , const char *pattern
                       , bool (*)(char,char)
                       );


#endif
