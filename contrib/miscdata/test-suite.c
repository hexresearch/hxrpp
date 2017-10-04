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
     {  test_slist_1,    "test_slist_1"    }
    ,{  test_slist_2,    "test_slist_2"    }
    ,{  test_dradix_1,   "test_dradix_1"   }
    ,{  test_dradix_1_1, "test_dradix_1_1" }
    ,{  test_dradix_2,   "test_dradix_2"   }
    ,{  test_dradix_3,   "test_dradix_3"   }
    ,{  test_dradix_4,   "test_dradix_4"   }
    ,{  test_dradix_5,   "test_dradix_5"   }
    ,{  test_dradix_6,   "test_dradix_6"   }
    ,{  test_dradix_7,   "test_dradix_7"   }
    ,{  test_dradix_8,   "test_dradix_8"   }
    ,{  test_dradix_10,  "test_dradix_10"  }
    ,{  test_dradix_11,  "test_dradix_11"  }
    ,{  test_dradix_12,  "test_dradix_12"  }
    ,{  test_dradix_13,  "test_dradix_13"  }
    ,{  test_dradix_14,  "test_dradix_14"  }
    ,{  test_dradix_15,  "test_dradix_15"  }
    ,{  test_dradix_16,  "test_dradix_16"  }
    ,{  test_dradix_17,  "test_dradix_17"  }
    ,{  test_dradix_18,  "test_dradix_18"  }
    ,{  test_slist_filt_destructive_1,  "test_slist_filt_destructive_1" }
    ,{  test_slist_filt_destructive_2,  "test_slist_filt_destructive_2" }
    ,{  test_slist_partition_destructive_1,  "test_slist_partition_destructive_1" }
    ,{  test_slist_char_array_1,  "test_slist_char_array_1" }
    ,{  test_hash_create_1,  "test_hash_create_1" }
    ,{  test_hash_create_2,  "test_hash_create_2" }
    ,{  test_hash_create_3,  "test_hash_create_3" }
    ,{  test_hash_rehash_1,  "test_hash_rehash_1" }
    ,{  test_hash_shrink_1,  "test_hash_shrink_1" }
    ,{  test_hash_shrink_2,  "test_hash_shrink_2" }
    ,{  test_clos_1,       "test_clos_1" }
    ,{  test_heap_test_1,  "test_heap_test_1" }
    ,{  test_heap_test_2,  "test_heap_test_2" }
    ,{  test_heap_test_3,  "test_heap_test_3" }
    ,{  test_heap_test_4,  "test_heap_test_4" }
    ,{  test_substr_1,      "test_substr_1" }
    ,{  test_skiplist_1,    "test_skiplist_1" }
    ,{  test_skiplist_2,    "test_skiplist_2" }
    ,{  test_aa_tree_create_1,   "test_aa_tree_create_1"    }
    ,{  test_aa_tree_remove_1_0, "test_aa_tree_remove_1_0"  }
    ,{  test_aa_tree_remove_1_1, "test_aa_tree_remove_1_1"  }
    ,{  test_aa_tree_remove_1_2, "test_aa_tree_remove_1_2"  }
    ,{  test_aa_tree_remove_2,   "test_aa_tree_remove_2"    }
    ,{  test_aa_tree_remove_3,   "test_aa_tree_remove_3"    }
    ,{  test_aa_tree_lookup_1,   "test_aa_tree_lookup_1"    }
    ,{  test_aa_tree_clinical_1, "test_aa_tree_clinical_1"  }
    ,{  test_aa_map_basic_1,     "test_aa_map_basic_1"  }
    ,{  test_aa_map_alter_1,     "test_aa_map_alter_1"  }
    ,{  test_aa_map_filter_1,    "test_aa_map_filter_1" }
    ,{  test_aa_map_arbitrary_kv_1, "test_aa_map_arbitrary_kv_1" }
    ,{  test_aa_map_arbitrary_kv_2, "test_aa_map_arbitrary_kv_2" }
    ,{  test_aa_map_arbitrary_kv_3, "test_aa_map_arbitrary_kv_3" }
    ,{  test_aa_map_arbitrary_kv_4, "test_aa_map_arbitrary_kv_4" }
    ,{  test_aa_map_arbitrary_kv_5, "test_aa_map_arbitrary_kv_5" }
    ,{  test_aa_map_no_val_copy_1, "test_aa_map_no_val_copy_1" }
    ,{  test_static_mem_pool_1,    "test_static_mem_pool_1" }
    ,{  test_const_mem_pool_1,    "test_const_mem_pool_1" }
    ,{  test_hash_fixed_1,    "test_hash_fixed_1" }
    ,{  test_hash_minimal_mem_size,  "test_hash_minimal_mem_size" }
    ,{  test_hash_fixed_nested_1,  "test_hash_fixed_nested_1" }
    ,{  test_hash_shrink_str_1,  "test_hash_shrink_str_1" }
    ,{  test_slist_set_value_1,  "test_slist_set_value_1" }
    ,{  test_slist_reverse_1,  "test_slist_reverse_1" }
    ,{  test_mfifo_create_1,  "test_mfifo_create_1" }
    ,{  test_mfifo_create_2,  "test_mfifo_create_2" }
    ,{  test_mfifo_drop_1,  "test_mfifo_drop_1" }
    ,{  test_mfifo_head_1,  "test_mfifo_head_1" }
    ,{  0,        ""         }
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

