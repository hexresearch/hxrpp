#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "heap.h"

bool u32_leq(void *a, void *b) {
    return *(uint32_t*)a <= *(uint32_t*)b;
}

void u32_cpy(void *a, void *b) {
    *(uint32_t*)a = *(uint32_t*)b;
}

void test_heap_test_1(void) {

    char heap_mem[heap_mem_size(16, sizeof(uint32_t))];
    struct heap *h = heap_create( heap_mem
                                , sizeof(heap_mem)
                                , sizeof(uint32_t)
                                , u32_leq
                                , u32_cpy );

    fprintf(stdout, "# heap_size: %ld\n", heap_size(h));
    fprintf(stdout, "# heap_empty: %s\n", heap_empty(h) ? "yes" : "no");

    uint32_t vs[] = {9,100,7,2,5,200,1,20,8,8,8,150};
    size_t i = 0;
    for(; i < sizeof(vs)/sizeof(vs[0]); i++ ) {
        if( !heap_add(h, &vs[i]) ) {
            break;
        }
    }

    fprintf(stdout, "# heap_empty: %s\n", heap_empty(h) ? "yes" : "no");

    while( !heap_empty(h) ) {
        uint32_t *mp = heap_pop(h);
        if( !mp )
            break;
        fprintf(stdout, "# %d\n", *mp);
    }
}

static void __test_head_add_gt(struct heap *h, uint32_t v) {
    if( !heap_full(h) ) {
        heap_add(h, &v);
    } else {
        uint32_t *min = heap_get(h);
        if( *min < v ) {
            heap_pop(h);
            heap_add(h, &v);
        }
    }
}

void test_heap_test_2(void) {

    char heap_mem[heap_mem_size(10, sizeof(uint32_t))];
    struct heap *h = heap_create( heap_mem
                                , sizeof(heap_mem)
                                , sizeof(uint32_t)
                                , u32_leq
                                , u32_cpy );

    fprintf(stdout, "# heap_size: %ld\n", heap_size(h));

    size_t N = 3000;

    size_t n = 0;
    for(n = N; n; n-- ) {
        __test_head_add_gt(h, n);
    }

    fprintf(stdout, "\n");
    while( !heap_empty(h) ) {
        uint32_t *mp = heap_pop(h);
        if( !mp )
            break;
        fprintf(stdout, "# %d\n", *mp);
    }

    for(n = 0; n <= N; n++ ) {
        __test_head_add_gt(h, n);
    }

    fprintf(stdout, "\n");
    while( !heap_empty(h) ) {
        uint32_t *mp = heap_pop(h);
        if( !mp )
            break;
        fprintf(stdout, "# %d\n", *mp);
    }
}

struct cw {
    uint16_t cat;
    uint32_t weight;
} __attribute__ ((aligned));

static bool __cw_leq(void *a_, void *b_) {
    struct cw *a = a_;
    struct cw *b = b_;
    return a->weight <= b->weight;
}

static void __cw_cpy(void *a, void *b) {
    memcpy(a, b, sizeof(struct cw));
}

void test_heap_test_3(void) {

    char heap_mem[heap_mem_size(5, sizeof(struct cw))];
    struct heap *h = heap_create( heap_mem
                                , sizeof(heap_mem)
                                , sizeof(struct cw)
                                , __cw_leq
                                , __cw_cpy );

    fprintf(stdout, "# heap_size: %ld\n", heap_size(h));

    struct cw cats[] = { {  1, 1 }
                       , {  2, 1 }
                       , {  1, 2 }
                       , {  3, 1 }
                       , { 12, 3 }
                       , {  5, 1 }
                       , { 31, 2 }
                       , {  6, 2 }
                       , {  7, 1 }
                       , {  7, 1 }
                       , { 10, 5 }
                       };

    fprintf(stdout, "\n");

    size_t i = 0;
    for(; i < sizeof(cats)/sizeof(cats[0]); i++ ) {
        fprintf(stdout, "# {%d, %d}\n", cats[i].cat, cats[i].weight);
        if( heap_full(h) ) {
            struct cw *min = heap_get(h);
            if( __cw_leq(min, &cats[i]) ) {
                heap_pop(h);
            }
        }
        heap_add(h, &cats[i]);
    }

    fprintf(stdout, "\n");
    while( !heap_empty(h) ) {
        struct cw *c = heap_pop(h);
        fprintf(stdout, "# {%d, %d}\n", c->cat, c->weight);
    }
}

void test_heap_test_4(void) {

    size_t memsize = heap_mem_size(1, sizeof(struct cw));

    fprintf(stderr, "memsize: %ld\n", memsize);

    char heap_mem[heap_mem_size(1, sizeof(struct cw))];
    struct heap *h = heap_create( heap_mem
                                , sizeof(heap_mem)
                                , sizeof(struct cw)
                                , __cw_leq
                                , __cw_cpy );

    fprintf(stderr, "heap: %p\n", h);

    fprintf(stdout, "# heap_size: %ld\n", heap_size(h));

    struct cw cats[] = { {  1, 1 }
                       , {  2, 1 }
                       , {  1, 2 }
                       , {  3, 1 }
                       , { 12, 3 }
                       , {  5, 1 }
                       , { 31, 2 }
                       , {  6, 2 }
                       , {  7, 1 }
                       , {  7, 1 }
                       , { 10, 5 }
                       };

    fprintf(stdout, "\n");


    size_t i = 0;
    for(; i < sizeof(cats)/sizeof(cats[0]); i++ ) {
        fprintf(stdout, "# {%d, %d}\n", cats[i].cat, cats[i].weight);
        if( heap_full(h) ) {
            struct cw *min = heap_get(h);
            if( __cw_leq(min, &cats[i]) ) {
                heap_pop(h);
            }
        }
        heap_add(h, &cats[i]);
    }

    fprintf(stdout, "\nheap_items %ld\n", heap_items(h));

    fprintf(stdout, "\n");
    while( !heap_empty(h) ) {
        struct cw *c = heap_pop(h);
        fprintf(stdout, "# {%d, %d}\n", c->cat, c->weight);
    }
}

