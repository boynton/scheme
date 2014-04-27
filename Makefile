###################################################################
###################################################################
#
# LeeScheme makefile for Apple OSX
#
###################################################################
###################################################################

NAME = scheme
EXECUTABLE = $(HOME)/bin/$(NAME)
LIBRARY = $(HOME)/lib/scheme

# flags, configuration
CONFIG_FLAGS = -DAPPLE -arch x86_64 -DX86_64 -DSCMLIB=$(LIBRARY)
OPTIMIZATIONS = -O3
CFLAGS = $(OPTIMIZATIONS) $(CONFIG_FLAGS) $(EXTENSIONS) $(VERSION)
LDFLAGS = $(CFLAGS)
CC = gcc

CFILES = scheme.c heap.c runtime.c compiler.c macros.c \
	logical.c symbol.c string.c number.c list.c vector.c proc.c \
	port.c io.c edlin.c \
	table.c \
	sysunix.c

HFILES = scheme.h object.h sys.h heap.h runtime.h io.h compiler.h \
	macros.h logical.h symbol.h string.h number.h list.h vector.h \
	proc.h port.h

SCMFILES = scheme.scm r4rs.scm repl.scm macros.scm config.scm \
	autoload.scm pretty.scm apropos.scm\
	contrib/*.scm


###################################################################

OFILES = $(CFILES:.c=.o)

DERIVED = $(NAME) *.o lib

.c.o:
	$(CC) $(CFLAGS) -c $*.c

all:: $(EXECUTABLE) $(LIBRARY)

$(NAME): $(OFILES)
	$(CC) $(LDFLAGS) -o $@ $(OFILES) $(LIBS)

$(EXECUTABLE): $(NAME)
	cp -p $(NAME) $(EXECUTABLE)

$(OFILES): $(HFILES)

$(LIBRARY)::
	mkdir -p $(LIBRARY)
	cp -p $(SCMFILES) $(LIBRARY)

clean::
	/bin/rm -rf $(DERIVED) *~ *.bak

