###################################################################
###################################################################
#
# LeeScheme makefile for Sun OS 4.x + gcc.
#
###################################################################
###################################################################

NAME = scheme

# installation directories
BIN_DIR = $(HOME)/bin
SCM_DIR = $(HOME)/lib/scheme

# flags, configuration
#CONFIG_FLAGS = -g -DSUNOS4X -DSPARC -DPROFILE -D__GCC__
CONFIG_FLAGS = -g -DSUNOS4X -DSPARC -D__GCC__
OPTIMIZATIONS = -O
CFLAGS = $(OPTIMIZATIONS) $(CONFIG_FLAGS) $(EXTENSIONS) $(VERSION)
LDFLAGS = $(CFLAGS)
LIBS = -lm
CC = gcc


# source

CFILES = scheme.c heap.c runtime.c compiler.c macros.c \
	 logical.c symbol.c string.c number.c list.c vector.c proc.c \
	 port.c io.c console.c \
	 sysunix.c

HFILES = scheme.h object.h sys.h heap.h runtime.h io.h compiler.h \
	macros.h logical.h symbol.h string.h number.h list.h vector.h \
	proc.h port.h

SCMFILES = scheme.scm r4rs.scm repl.scm edlin.scm macros.scm config.scm \
	autoload.scm pretty.scm sort.scm apropos.scm struct.scm \
	synchk.scm test.scm bench.scm


###################################################################

OFILES = $(CFILES:.c=.o)

DERIVED = $(NAME) *.o

.c.o:
	$(CC) $(CFLAGS) -c $*.c

$(NAME): $(OFILES)
	$(CC) $(LDFLAGS) -o $@ $(OFILES) $(LIBS)

$(OFILES): $(HFILES)

clean::
	/bin/rm -f $(DERIVED) *~ *.bak

$(SCM_DIR) $(BIN_DIR):
	mkdir $@

install:: $(SCM_DIR) $(BIN_DIR) $(NAME) $(SCMFILES)
	cp -p $(SCMFILES) $(SCM_DIR)
	install -s $(NAME) $(BIN_DIR)
