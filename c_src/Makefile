# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT = brotli

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")

OUT ?= $(BASEDIR)/priv

# System type and C compiler/flags.
#

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS += -O3 -std=c99 -finline-functions -Wall
	LDFLAGS ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC = /usr/bin/clang
	CFLAGS += -O3 -std=c99 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS += -O3 -std=c99 -finline-functions -Wall
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I ./include/

LDFLAGS += -shared -rdynamic

BROTLI_SRCS = $(wildcard common/*.c) $(wildcard dec/*.c) $(wildcard enc/*.c)
BROTLI_OBJS = $(patsubst %.c,%.o,$(BROTLI_SRCS))

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

lib: $(OUT)/$(PROJECT).so

$(OUT)/$(PROJECT).so: brotli_nif.o $(BROTLI_OBJS)
	@mkdir -p $(@D)
	$(link_verbose) $(CC) $^ $(LDFLAGS) $(LDLIBS) -o $@

tool: $(OUT)/brotli

$(OUT)/brotli: $(BROTLI_OBJS) tools/brotli.o
	$(link_verbose) $(CC) $^ -lm $(LDLIBS) -o $@

tools/brotli.o: tools/brotli.c
	$(c_verbose) $(CC) -O2 -I ./include/ $(CPPFLAGS) -c $< -o $@

%.o: %.c
	$(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

clean:
	@rm -f $(OUT)/$(PROJECT).so \
		$(OUT)/brotli $(BROTLI_OBJS) \
		tools/brotli.o brotli_nif.o

.PHONY: lib tool
