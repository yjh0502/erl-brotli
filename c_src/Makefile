# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")

C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).so

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CXXFLAGS ?= -O3 -std=c++11 -arch x86_64 -finline-functions -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC = /usr/bin/clang
	CXX = /usr/bin/clang++
	CXXFLAGS ?= -O3 -std=c++11 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CXX ?= g++
	CXXFLAGS ?= -O3 -std=c++11 -finline-functions -Wall
endif

CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) 

LDLIBS += -lstdc++
LDFLAGS += -shared -rdynamic

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(C_SRC_OUTPUT): brotli_nif.o brotli_encoder/libbrotli.a
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $^ $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT)

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

brotli_encoder/libbrotli.a:
	$(MAKE) -C brotli_encoder CXX=$(CXX)

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS) && $(MAKE) clean -C brotli_encoder
