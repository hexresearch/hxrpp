.PHONY: all clean executables
.PRECIOUS: %.c %.h

ifeq "$(.DEFAULT_GOAL)" ""
.DEFAULT_GOAL=all
endif

ALLEXECUTABLES=

MISCDATA_DIR=$(TOP)/contrib/miscdata
UTILS_DIR=$(TOP)/utils

INCDIRS+=$(TOP)/contrib
INCDIRS+=$(TOP)

INC=$(addprefix -I, $(INCDIRS))

BUILDDIR=_build

CFLAGS+=$(INC) $(DEBUG) --std=gnu99

DEBUG=-g
CC=gcc -O2

objfiles=$(addprefix $(BUILDDIR)/,$(patsubst %.c,%.o,$(1)))

define add_source_dir
ALLSOURCES+=$2
vpath %.c $1
endef

define miscdata_sources
ALLSOURCES+=$(1)
vpath %.c $(MISCDATA_DIR)
endef

define utils_sources
ALLSOURCES+=$(1)
vpath %.c $(UTILS_DIR)
endef

define add_include_dir
INCDIRS+=$1
endef

define ExecFile
ALLEXECUTABLES+=$(BUILDDIR)/$1
$(BUILDDIR)/$1: $$(VERSION_HDR) $$(call objfiles,$2)
	$(CC) $$(filter-out $$(VERSION_HDR),$$^) -o $$@ $(LDFLAGS)
endef

define VersionHdr
VERSION:=$(shell cat VERSION)
GIT_VERSION:=$(shell [[ ! -z "$(GIT_VERSION)" ]] && echo "$(GIT_VERSION)" || git rev-parse HEAD)

VERSION_HDR:=$(1)_version.h
.PHONY: $$(VERSION_HDR)
$$(VERSION_HDR):
	@echo "#ifndef __$(2)_VERSION_H__" > $$(VERSION_HDR)
	@echo "#define __$(2)_VERSION_H__" >> $$(VERSION_HDR)
	@echo "#define $(2)_VERSION \""$$(VERSION)"\"" >> $$(VERSION_HDR)
	@echo "#define $(2)_GIT_VERSION \""$$(GIT_VERSION)"\"" >> $$(VERSION_HDR)
	@echo "#endif" >> $$(VERSION_HDR)
endef

vpath %.o $(BUILDDIR)
vpath %.c .

.SECONDEXPANSION: all

all: $(ALLEXECUTABLES) ;

.PHONY: builddir
%: builddir ;

builddir: $(BUILDDIR) ;

$(BUILDDIR)/%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
	@$(CC) $(CFLAGS) \
		-MT $(addprefix $(BUILDDIR)/, $(patsubst %.c,%.o,$<)) \
		-MM $< > $(patsubst %.o,%.d,$@)

.PHONY: $(BUILDDIR)
$(BUILDDIR):
	@mkdir -p $(BUILDDIR)

clean: clean_extra
clean:
	@echo Cleaning up...
	@rm  -rf $(BUILDDIR)/*
	@rm  -f $(VERSION_HDR)

.PHONY: ctags
ctags: $(OBJECTFILES)
	cat $(BUILDDIR)/*.d | sed 's/^.*://g' | sed 's/\s/\n/g' | egrep '^(\w|\.\.)' | sort -u | xargs ctags
	ctags -a $(MISCDATA_DIR)/*.c


ifneq "$(wildcard $(BUILDDIR))" ""
-include $(OBJECTFILES:.o=.d)
endif
