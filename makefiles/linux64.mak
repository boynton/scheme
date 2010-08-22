###################################################################
###################################################################
#
# LeeScheme makefile for Linux
#
###################################################################
###################################################################

NAME = scheme

# installation directories
BIN_DIR = $(HOME)/bin
LIB_DIR = $(HOME)/lib
SCM_DIR = $(LIB_DIR)/scheme

# flags, configuration
CONFIG_FLAGS = -g -Wall -ansi  -DLINUX -DAMD64
#CONFIG_FLAGS = -Wall -ansi -D__GCC__ -DLINUX
#CONFIG_FLAGS = -g -Wall -ansi -D__GCC__ -DLINUX -DPOINTERS

#this is as fast as we go. No useful for debugging
#OPTIMIZATIONS = -O3 -DNO_TYPE_CHECK -DNO_INTERRUPT_CHECK -fomit-frame-pointer

#OPTIMIZATIONS = -march=athlon-xp -O3 -fomit-frame-pointer
OPTIMIZATIONS = -march=athlon64 -O3 -fomit-frame-pointer
#this is useful for scheme debugging, but no C debugging
#OPTIMIZATIONS = -O3 -fomit-frame-pointer
#OPTIMIZATIONS = -g

CFLAGS = $(OPTIMIZATIONS) $(CONFIG_FLAGS) $(EXTENSIONS) $(VERSION)
LDFLAGS = $(CFLAGS)
LIBS=-lm
CC = gcc


# source

CFILES = scheme.c heap.c runtime.c compiler.c macros.c \
	 logical.c symbol.c string.c number.c list.c vector.c proc.c \
	 port.c io.c \
	 sysunix.c edlin.c

HFILES = scheme.h object.h sys.h heap.h runtime.h io.h compiler.h \
	macros.h logical.h symbol.h string.h number.h list.h vector.h \
	proc.h port.h

SCMFILES = scheme.scm r4rs.scm repl.scm macros.scm config.scm \
	autoload.scm pretty.scm sort.scm apropos.scm struct.scm \
	test.scm bench.scm


###################################################################

OFILES = $(CFILES:.c=.o)

DERIVED = $(NAME) *.o

.c.o:
	$(CC) $(CFLAGS) -c $*.c

$(NAME): $(OFILES)
	$(CC) $(LDFLAGS) -o $@ $(OFILES) $(LIBS)

profile::
	$(MAKE) "CFLAGS=-pg -Wall -ansi  -DLINUX -DINSTRUMENT_OPS"

$(OFILES): $(HFILES)

clean::
	/bin/rm -f $(DERIVED) *~ *.bak

$(SCM_DIR) $(BIN_DIR):
	mkdir -p $@

install:: $(SCM_DIR) $(BIN_DIR) $(NAME) $(SCMFILES)
	cp -p $(SCMFILES) $(SCM_DIR)
	install -s $(NAME) $(BIN_DIR)
