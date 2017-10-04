#include "clos.h"

struct clos *mk_clos(void *mem, void *fn, void *cc) {
    struct clos *c = mem;
    c->fn = fn;
    c->cc = cc;
    return (struct clos*)mem;
}
