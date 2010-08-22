/* LeeScheme/scheme.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"

const char *version = VERSION_STRING;
const char *copyright = COPYRIGHT_STRING;

static void primop_version(long argc) {
	*--sp = make_string((char *)version);
}

static void primop_copyright(long argc) {
	char buf[256];
	char *b = buf;
	const char *p = version;
	while ((*b++ = *p++) != '\0') {}
	b[-1] = ','; *b++ = ' ';
	p = copyright;
	while ((*b++ = *p++) != '\0') {}
	*--sp = make_string(buf);
}

void init_scheme(long heap_kbytes,long stack_kbytes,long code_kbytes) {
	init_heap(heap_kbytes,stack_kbytes,code_kbytes);
	init_runtime();
	init_procedure();
	init_symbol();
	init_port();
	init_system();
	init_io();
	init_logical();
	init_list();
	init_vector();
	init_math();
	init_string();
	init_macros();
	init_compiler();
	init_table();
	define_primop("system:version",primop_version,0,0);
	define_primop("system:copyright",primop_copyright,0,0);
}

object execute_file(char *filename) {
	object o = null_object;
	PUSH_GC_PROTECT(o);
	o = open_input_file(filename);
	define("system:*current-input-port*",o);
	o = read_object_from_port(o);
	o = macro_expand(o);
	o = compile(o);
	o = execute(o);
	POP_GC_PROTECT(1);
	return o;
}


