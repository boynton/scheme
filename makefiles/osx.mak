###################################################################
###################################################################
#
# LeeScheme makefile for Apple OSX
#
###################################################################
###################################################################

NAME = scheme

# installation directories
BIN_DIR = $(HOME)/bin
SCM_DIR = $(HOME)/lib/scheme

# flags, configuration
CONFIG_FLAGS = -g -DAPPLE -arch x86_64 -DX86_64
OPTIMIZATIONS =  -O3 -pipe -fomit-frame-pointer
CFLAGS = $(OPTIMIZATIONS) $(CONFIG_FLAGS) $(EXTENSIONS) $(VERSION)
LDFLAGS = $(CFLAGS)
CC = cc

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
