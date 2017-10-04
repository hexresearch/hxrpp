.PHONY: ctags clean baseline check

TESTSRC=dradix-test.c hash-test.c slist-test.c clos-test.c heap-test.c mfifo-test.c
TESTSRC+=substr-test.c
TESTSRC+=skiplist-test.c
TESTSRC+=aa_map-test.c
TESTSRC+=static_mem_pool-test.c
TESTSRC+=const_mem_pool-test.c
TESTSRC+=hash_fixed-test.c

SOURCES := slist.c dradix.c hash.c clos.c maxheap.c substr_kmp.c
SOURCES += hash_uint32.c
SOURCES += hash_cstring.c
SOURCES += hash_fletcher32.c
SOURCES += hash_murmur.c
SOURCES += hash_fixed.c
SOURCES += skiplist.c
SOURCES += aa_map.c
SOURCES += mfifo.c
SOURCES += static_mem_pool.c
SOURCES += const_mem_pool.c
SOURCES += test-suite.c
SOURCES += $(TESTSRC)

SOURCES+=
all: build-tests

build-tests:
	echo "#ifndef __all_tests_h" > t/test-suite.h
	echo "#define __all_tests_h" >> t/test-suite.h
	echo $(TESTSRC)
	cat $(TESTSRC) | egrep -o 'void\s+test_.*\s*\(\s*void\s*\)' | awk '{printf("%s %s;\n",$$1,$$2)}' >> t/test-suite.h
	echo "#endif" >> t/test-suite.h
	gcc -DMISCDATA_RT_ENABLE=1 -g -Wall $(SOURCES) -o test-suite

ctags:
	ctags *.c

clean:
	rm -f test-suite
	rm -f t/*.h

baseline: build-tests
	./test-suite list 2>&1 | xargs -L 1 t/scripts/mkbaseline.sh

check: build-tests
	./test-suite list 2>&1 | xargs -L 1 t/scripts/checkbaseline.sh

