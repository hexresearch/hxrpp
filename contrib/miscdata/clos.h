#ifndef __miscdata_clos_h
#define __miscdata_clos_h

// Very poor man's closures

struct clos {
    void *fn;
    void *cc;
};

#define clos_size (sizeof(struct clos))

typedef struct {
    union {
        struct clos c;
        char mem[0];
    } x;
} clos_mem_t;

#define call_clos0(c_t, c) (((c_t)((c)->fn))((c)->cc))

#define call_clos(c_t, c, ...) (((c_t)((c)->fn))((c)->cc, __VA_ARGS__))

struct clos *mk_clos(void *mem, void *fn, void *cc);


#endif
