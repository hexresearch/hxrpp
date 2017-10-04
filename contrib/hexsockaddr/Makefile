.PHONY: ctags clean baseline check

TESTSRC:=hexsockaddr_test.c

SOURCES += test.c hexsockaddr.c
SOURCES += $(TESTSRC)

CFLAGS+=-DHEXSOCKADDR_PLATFORM_DEFAULT=1

all: build-tests

build-tests:
	echo "#ifndef __all_tests_h" > t/test-suite.h
	echo "#define __all_tests_h" >> t/test-suite.h
	echo $(TESTSRC)
	cat $(TESTSRC) | egrep -o 'void\s+test_.*\s*\(\s*void\s*\)' | awk '{printf("%s %s;\n",$$1,$$2)}' >> t/test-suite.h
	echo "#endif" >> t/test-suite.h
	gcc $(CFLAGS) -g -Wall $(SOURCES) -o test-suite

ctags:
	ctags *.c

clean:
	rm -f test-suite
	rm -f t/*.h

baseline: build-tests
	./test-suite list 2>&1 | xargs -L 1 t/scripts/mkbaseline.sh

check: build-tests
	./test-suite list 2>&1 | xargs -L 1 t/scripts/checkbaseline.sh

