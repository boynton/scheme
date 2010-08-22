/* LeeScheme/io.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include <string.h>

#define PORT_TABLE_SIZE (256)

object curin_port(void) {
	return global("system:*current-input-port*");
}

object curout_port(void) {
	return global("system:*current-output-port*");
}

object scm(object o) {
	write_object(curout_port(),o,1);
	write_string(curout_port(),newline_string(curout_port()));
	return o;
}

/* The global port enumeration */

object port_vector;

void add_port_to_port_vector(object p) {
	long i, port_vector_size = VECTOR_LENGTH(port_vector);
	object *v = VECTOR_ELEMENTS(port_vector);
	for (i=0; i<port_vector_size; i++) {
		if (VOID_P(v[i])) {
			break;
		}
	}
	if (i < port_vector_size) {
		p = weak_binding(p);
		VECTOR_ELEMENTS(port_vector)[i] = p;
	} else {
		fatal_error("Cannot create port - port vector full");
	}
}

void close_stale_port(object p) {
	long i, port_vector_size = VECTOR_LENGTH(port_vector);
	object *v = VECTOR_ELEMENTS(port_vector);
	for (i=0; i<port_vector_size; i++) {
		if (WEAK_P(v[i]) && WEAK_VALUE(v[i]) == p) {
			v[i] = void_object;
			return;
		}
	}
}

void close_stale_ports(void) {
	long i, port_vector_size = VECTOR_LENGTH(port_vector);
	object *v = VECTOR_ELEMENTS(port_vector);
	for (i=0; i<port_vector_size; i++) {
		object p = v[i];
		if (WEAK_P(p) && !WEAK_BOUND(p)) {
			object o = WEAK_VALUE(p);
			(PORT_CLOSE_PROC(o))(o);
			v[i] = void_object;
		}
	}
}


/* Creating ports */
object make_port(void * stream, long flags, char *name) {
	char *n = name? name : "";
	long i = strlen(name) + 1 + sizeof(struct port_heap_structure);
	object p = make_heap_object(PORT_TYPE,i);
	PORT_STREAM(p) = stream;
	PORT_FLAGS(p) = flags;
	PORT_BUFFER(p) = void_object;
	PORT_POSITION(p) = 0;
	PORT_CHAR_BUF(p) = -1;
	strcpy(PORT_NAME(p),n);
	add_port_to_port_vector(p);
	return p;
}

static object make_string_port(object buf, long flags) {
	long i = sizeof(struct port_heap_structure);
	object p;
	PUSH_GC_PROTECT(buf);
	p = make_heap_object(PORT_TYPE,i);
	PORT_FLAGS(p) = flags;
	PORT_BUFFER(p) = buf;
	PORT_POSITION(p) = 0;
	PORT_CHAR_BUF(p) = -1;
	PORT_STREAM(p) = 0;
	POP_GC_PROTECT(1);
	return p;
}

/* String ports */

void string_port_close(object p) {
    PORT_FLAGS(p) |= PORT_FLAG_CLOSED;
}

int string_port_getc(object p) {
	long max = STRING_LENGTH(PORT_BUFFER(p));
	long pos = PORT_POSITION(p);
	if (pos >= max) return -1;
	PORT_POSITION(p) = pos + 1;
	return STRING_VALUE(PORT_BUFFER(p))[pos];
}

void string_port_putc(object inport, int c) {
	long max = STRING_LENGTH(PORT_BUFFER(inport));
	long pos = PORT_POSITION(inport);
	char *p;
	if (pos >= max) {
		object newbuf;
		PUSH_GC_PROTECT(inport);
		newbuf = make_string_of_size(max*2,1);
		POP_GC_PROTECT(1);
		strcpy(STRING_VALUE(newbuf), STRING_VALUE(PORT_BUFFER(inport)));
		PORT_BUFFER(inport) = newbuf;
	}
	p = STRING_VALUE(PORT_BUFFER(inport));
	p[pos++] = c;
	PORT_POSITION(inport) = pos;
}

void string_port_flush(object p) {
}

int string_port_ready_p(object p) {
	return 1;
}

object open_input_string(object buf) {
	object p = make_string_port(buf,PORT_FLAG_STRING|PORT_FLAG_READ);
	PORT_CLOSE_PROC(p) = &string_port_close;
	PORT_GETC_PROC(p) = &string_port_getc;
	PORT_READY_PROC(p) = &string_port_ready_p;
	PORT_PUTC_PROC(p) = NULL;
	PORT_FLUSH_PROC(p) = NULL;
	return p;
}

object open_output_string(object buf) {
	object p = make_string_port(buf,PORT_FLAG_STRING|PORT_FLAG_WRITE);
	PORT_CLOSE_PROC(p) = &string_port_close;
	PORT_PUTC_PROC(p) = &string_port_putc;
	PORT_FLUSH_PROC(p) = &string_port_flush;
	PORT_GETC_PROC(p) = NULL;
	PORT_READY_PROC(p) = NULL;
	return p;
}

static void primop_close_output_port(long argc) {
	object tmp = *sp;
	TYPE_CHECK(OUTPUT_PORT_P(tmp),1,"output-port",tmp);
	(PORT_CLOSE_PROC(tmp))(tmp);
}


static void primop_input_port_p(long argc) {
	if (!INPUT_PORT_P(*sp)) *sp = false_object;
}

static void primop_open_input_file(long argc) {
	object tmp = *sp;
	TYPE_CHECK(STRING_P(tmp),1,"string",tmp);
	*sp = open_input_file(STRING_VALUE(tmp));
}

static void primop_open_input_string(long argc) {
	object tmp = *sp;
	TYPE_CHECK(STRING_P(tmp),1,"string",tmp);
	*sp = open_input_string(tmp);
}

static void primop_string_port_buffer(long argc) {
	object p = *sp;
	TYPE_CHECK(PORT_P(p) && PORT_STRING_P(p),1,"string-port",p);
	*sp = PORT_BUFFER(p);
}

static void primop_string_port_position(long argc) {
	object p = *sp;
	TYPE_CHECK(PORT_P(p) && PORT_STRING_P(p),1,"string-port",p);
	*sp = MAKE_FIXNUM(PORT_POSITION(p));
}

static void primop_string_port_p(long argc) {
	if (!PORT_P(*sp) || !PORT_STRING_P(*sp))
		*sp = false_object;
}

static void primop_close_input_port(long argc) {
	object tmp = *sp;
	TYPE_CHECK(INPUT_PORT_P(tmp),1,"input-port",tmp);
	PORT_CLOSE_PROC(tmp)(tmp);
}

static void primop_output_port_p(long argc) {
	if (!OUTPUT_PORT_P(*sp))
		*sp = false_object;
}

static void primop_open_output_file(long argc) {
	object tmp = *sp;
	TYPE_CHECK(STRING_P(tmp),1,"string",tmp);
	*sp = open_output_file(STRING_VALUE(tmp));
}

static void primop_open_output_string(long argc) {
	object tmp = *sp;
	TYPE_CHECK(STRING_P(tmp),1,"string",tmp);
	*sp = open_output_string(tmp);
}

void init_port(void) {
	port_vector = make_vector(PORT_TABLE_SIZE,void_object);
	PUSH_GC_PROTECT(port_vector);
	define_primop("output-port?",primop_output_port_p,1,1);
	define("system:*current-output-port*",false_object);
	define("system:*current-input-port*",false_object);
	define_primop("open-output-file",primop_open_output_file,1,1);
	define_primop("close-output-port",primop_close_output_port,1,1);
	define_primop("input-port?",primop_input_port_p,1,1);
	define_primop("open-input-file",primop_open_input_file,1,1);
	define_primop("close-input-port",primop_close_input_port,1,1);
	define_primop("system:open-input-string",primop_open_input_string,1,1);
	define_primop("system:open-output-string",primop_open_output_string,1,1);
	define_primop("system:string-port?",primop_string_port_p,1,1);
	define_primop("system:string-port-buffer",primop_string_port_buffer,1,1);
	define_primop("system:string-port-position",primop_string_port_position,1,1);
}
