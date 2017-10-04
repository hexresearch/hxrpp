#ifndef __miscdata_h
#define __miscdata_h

#ifndef MIN
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#endif

#define safecall(v, f, ...) ((f) ? (f(__VA_ARGS__)) : (v))
#define unit ({})

#endif
