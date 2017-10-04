TOP=..

INCDIRS+=.

include $(TOP)/make/top.mk

OPTS_GEN := app_config_opts_gen.inc

MISCDATA_SOURCES+=slist.c
MISCDATA_SOURCES+=hash.c
MISCDATA_SOURCES+=dradix.c
MISCDATA_SOURCES+=const_mem_pool.c
MISCDATA_SOURCES+=hash.c
MISCDATA_SOURCES+=hash_cstring.c
MISCDATA_SOURCES+=hash_fletcher32.c

UTILS_SOURCES+=app_config.c

$(eval $(call miscdata_sources, $(MISCDATA_SOURCES)))
$(eval $(call add_source_dir, $(TOP)/contrib/hexsockaddr, hexsockaddr.c))

HEXBWCLIENT_SOURCES+=hxrpp_server.c

HEXBWCLIENT_SOURCES+= $(HEXBWCLIENT_LIB_SOURCES)

$(eval $(call utils_sources, $(UTILS_SOURCES)))

$(eval $(call ExecFile,hxrpp_server,$(HEXBWCLIENT_SOURCES) $(ALLSOURCES)))

$(OPTS_GEN):
	@echo Generate $@
	@egrep -sho '^\s*CFG_OPTION\(.*\)' `ls -1 *.h | grep -v $(OPTS_GEN)` >> $@

all: $(ALLEXECUTABLES)

show:
	@echo $(VPATH)
	@echo $(ALLEXECUTABLES)

clean_extra:
	@echo clean extra...
	$(RM) $(OPTS_GEN)

-include _build/*.d
