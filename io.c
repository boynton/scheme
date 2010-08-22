/* LeeScheme/io.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static object scratch_buffer;

#define PORT_IS_OPEN_P(p) (!(PORT_FLAGS(p) & PORT_FLAG_CLOSED))


/*
 * Reading
 */

extern int edlin_getc(object ioport);

static void primop_port_edlin(long argc) {
	object p = *sp++;
	int b = 0;
	TYPE_CHECK(INPUT_PORT_P(p)&OUTPUT_PORT_P(p),1,"I/O port",p);
	b = (*sp++ != false_object);
	if (PORT_IS_OPEN_P(p)) {
		if (b)
			PORT_FLAGS(p) = PORT_FLAGS(p) | PORT_FLAG_COOKED;
		else
			PORT_FLAGS(p) = PORT_FLAGS(p) & ~PORT_FLAG_COOKED;
	}
	*--sp = void_object;
}


static int get_character(object inport) {
	int c = PORT_CHAR_BUF(inport);
	if (c >= 0) {
		PORT_CHAR_BUF(inport) = -1;
	} else {
		if (PORT_FLAGS(inport) & PORT_FLAG_COOKED)
			c = edlin_getc(inport);
		else
			c = (*(PORT_GETC_PROC(inport)))(inport);
	}
	return c;
}

static void unget_character(int c,object port) {
	PORT_CHAR_BUF(port) = c;
}

static object read_object(object port);

static object read_comment(object port) {
	char c;
	do {
		c = get_character(port);
	} while (c != '\n' && c != -1);
	return (c == -1)? eof_object : true_object;
}

static object read_list(object port) {
	object the_list, the_tail, temp, element;
	char c;
	long dot_state = 0;
	the_list = null_object;
	the_tail = null_object;
	PUSH_GC_PROTECT(the_list);
	PUSH_GC_PROTECT(the_tail);
	while ((c = get_character(port)) != -1) {
		if (WHITESPACE_P(c)) continue;
		if (COMMENT_P(c)) {
			if (EOF_P(read_comment(port))) {
				the_list = eof_object;
				break;
			} else
				continue;
		}
		if (c == ')') {
			if (dot_state == 1)
				error(unbound_object,
					  "read: expected element after dot in list");
			break;
		}
		if (dot_state > 1)
			error(unbound_object,
				  "read: more than one element after dot in list");
		unget_character(c,port);
		element = read_object(port);
		if (EOF_P(element)) {
			the_list = element;
			break;
		} else if (DOT_P(element)) {
			if (NULL_P(the_list))
				error(unbound_object,"read: dot in null list");
			else if (!dot_state)
				dot_state++;
			else
				error(unbound_object,"read: more than one dot in list");
		} else if (dot_state == 1) {
			if (!NULL_P(the_tail)) {
				dot_state++;
				CDR(the_tail) = element;
			} else
				error(unbound_object,
					  "read: expected element before dot in list");
		} else {
			temp = cons(element, null_object);
			if (!NULL_P(the_tail)) {
				CDR(the_tail) = temp;
			} else
				the_list = temp;
			the_tail = temp;
		}
	}
	POP_GC_PROTECT(2);
	return (c == -1)? eof_object : the_list;
}

static int named_character(char *name) {
	if (!strcmpci(name,"space"))
		return CHAR_SPACE;
	else if (!strcmpci(name,"newline"))
		return CHAR_NEWLINE;
	else if (!strcmpci(name,"return"))
		return CHAR_RETURN;
	else if (!strcmpci(name,"backspace"))
		return CHAR_BACKSPACE;
	else if (!strcmpci(name,"tab"))
		return CHAR_TAB;
	else if (!strcmpci(name,"escape"))
		return CHAR_ESCAPE;
	else if (!strcmpci(name,"rubout"))
		return CHAR_RUBOUT;
	else if (!strcmpci(name,"null"))
		return CHAR_NULL;
	else if (!strcmpci(name,"f1"))
		return CHAR_F1;
	else if (!strcmpci(name,"f2"))
		return CHAR_F2;
	else if (!strcmpci(name,"f3"))
		return CHAR_F3;
	else if (!strcmpci(name,"f4"))
		return CHAR_F4;
	else if (!strcmpci(name,"f5"))
		return CHAR_F5;
	else if (!strcmpci(name,"f6"))
		return CHAR_F6;
	else if (!strcmpci(name,"f7"))
		return CHAR_F7;
	else if (!strcmpci(name,"f8"))
		return CHAR_F8;
	else if (!strcmpci(name,"f9"))
		return CHAR_F9;
	else if (!strcmpci(name,"f10"))
		return CHAR_F10;
	else if (!strcmpci(name,"f11"))
		return CHAR_F11;
	else if (!strcmpci(name,"f12"))
		return CHAR_F12;
	else if (!strcmpci(name,"left"))
		return CHAR_LEFT;
	else if (!strcmpci(name,"down"))
		return CHAR_DOWN;
	else if (!strcmpci(name,"up"))
		return CHAR_UP;
	else if (!strcmpci(name,"right"))
		return CHAR_RIGHT;
	else if (!strcmpci(name,"print"))
		return CHAR_PRINT;
	else if (!strcmpci(name,"pause"))
		return CHAR_PAUSE;
	else if (!strcmpci(name,"home"))
		return CHAR_HOME;
	else if (!strcmpci(name,"end"))
		return CHAR_END;
	else if (!strcmpci(name,"pageup"))
		return CHAR_PAGEUP;
	else if (!strcmpci(name,"pagedown"))
		return CHAR_PAGEDOWN;
	else if (!strcmpci(name,"insert"))
		return CHAR_INSERT;
	else if (!strcmpci(name,"delete"))
		return CHAR_DELETE;
	else
		return -1;
}

static object read_string(object port) {
	long size = 0, escape = 0, max = BUFFER_CAPACITY(scratch_buffer);
	char *buf = BUFFER_DATA(scratch_buffer);
	int c, c2;
	PUSH_GC_PROTECT(port);
	while ((c = get_character(port)) != -1) {
		if (escape) {
			escape = 0;
			switch (c) {
			  case '\\':
			  case '"':
				buf[size++] = c;
				break;
			  case 'e':
				buf[size++] = CHAR_ESCAPE;
				break;
			  case 'n':
				buf[size++] = CHAR_NEWLINE;
				break;
			  case 't':
				buf[size++] = CHAR_TAB;
				break;
			  case 'r':
				buf[size++] = CHAR_RETURN;
				break;
			  case 'C':
			  case 'c':
				c2 = get_character(port);
				if (c2 == '-') {
					c2 = get_character(port);
					if (c2 >= 'a' && c2 <= 'z') c2 -= ('a' - 'A');
					if (c2 > '@' && c2 <= 'Z') {
						buf[size++] = c2 - '@';
						break;
					}
				}
				/* fall through to error case */
			  default:
				error(unbound_object,
					  "read: bad escape character in string: \\%c",c);
			}
		} else if (c == '"')
			break;
		else if (c == '\\')
			escape = 1;
		else {
			escape = 0;
			buf[size++] = c;
		}
		if (size >= max) {
			max *= 2;
			BUFFER_LENGTH(scratch_buffer) = max;
			scratch_buffer = grow_buffer(scratch_buffer,max);
			buf = BUFFER_DATA(scratch_buffer);
		}
	}
	POP_GC_PROTECT(1);
	if (c == -1)
		return eof_object;
	else {
		object s;
		buf[size] = '\0';
		s = make_string(buf);
		return s;
	}
}

static object read_vector(object port) {
	object buffer = make_buffer(32);
	long size = 0, max = BUFFER_CAPACITY(buffer) / sizeof(object);
	char c;
	object element = null_object;
	object *buf = NULL;
	PUSH_GC_PROTECT(buffer);
	PUSH_GC_PROTECT(port);
	PUSH_GC_PROTECT(element);
	while ((c = get_character(port)) != -1) {
		if (WHITESPACE_P(c)) continue;
		if (COMMENT_P(c)) {
			if (EOF_P(read_comment(port)))
				return eof_object;
			else
				continue;
		}
		if (c == ')') break;
		unget_character(c,port);
		element = read_object(port);
		if (EOF_P(element)) {
			POP_GC_PROTECT(2);
			return element;
		}
		if (size >= max) {
			max *= 2;
			BUFFER_LENGTH(buffer) = size * sizeof(object);
			buffer = grow_buffer(buffer,max*sizeof(object));
		}
		buf = (object *)BUFFER_DATA(buffer);
		buf[size++] = element;
	}
	if (c == -1) {
		POP_GC_PROTECT(3);
		return eof_object;
	} else {
		object v = make_vector(size,null_object);
		object *src = (object *)BUFFER_DATA(buffer), *dst = VECTOR_ELEMENTS(v);
		while (size--)
			*dst++ = *src++;
		POP_GC_PROTECT(3);
		return v;
	}
}

object parse_rational(char *src, long radix, long exact) {
	char c, *p, *d, buf[MAX_ATOM+1];
	double val;
	if (*src == '/') return void_object;
	strcpy(buf,src);
	d = buf;
	d = strchr(buf,'/');
	*d++ = '\0';
	if (!*d) return void_object;
	p = buf;
	while ((c = *p++) != 0) if (!DIGIT_P(c)) return void_object;
	p = d;
	while ((c = *p++) != 0) if (!DIGIT_P(c)) return void_object;
	/* Rationals are read in as real numbers, since they aren't supported. */
	val = atof(d);
	if (val == 0)
		error(intern(src),"Rational number cannot have 0 as denominator");
	val = atof(buf)/val;
	return make_flonum(val);
}

object parse_number(char *buf, long radix, long exact) {
	char c, *p;
	long dot = 0, exponent = 0;
	double val;
	long digits = 0;
	p = buf;
	if (strchr(p,'/')) return parse_rational(buf,radix,exact);
	if (*p == '+' || *p == '-') p++;
	while ((c = *p++) != 0) {
		if (c == '.') {
			if (dot++) return void_object;
		} else if (c == 'e') {
			if (!digits) return void_object;
			if (exponent++) return void_object;
			dot = digits = 0;
			if (*p == '+' || *p == '-') p++;
		} else if (!(radix == 16 && HEXDIGIT_P(c)) && !DIGIT_P(c))
			return void_object;
		else
			digits++;
	}
	if (!digits) return void_object;
	if (radix == 10 || dot) {
		val = atof(buf);
	} else if (radix == 16) {
		long n;
		sscanf(buf,"%lx",&n);
		val = n;
	} else if (radix == 8) {
		long n;
		sscanf(buf,"%lo",&n);
		val = n;
	} else {
		val = 0;
		error(MAKE_FIXNUM(radix),"unsupported radix");
	}
	if (!dot && !exponent && val <= MAX_FIXNUM && val >= MIN_FIXNUM)
		return MAKE_FIXNUM((long)val);
	else
		return make_flonum(val);
}

static object read_character(object port) {
	char ctmp, c = get_character(port);
	if (c == -1) return eof_object;
	ctmp = get_character(port);
	if (c == -1) return eof_object;
	if (WHITESPACE_P(ctmp) || DELIMITER_P(ctmp)) {
		/* a normal character literal */
		unget_character(ctmp,port);
	} else {
		/* maybe a named character */
#define BUFSIZE 64
		int i = BUFSIZE-2;
		char buf[BUFSIZE];
		char *p = buf;
		*p++ = '#'; *p++ = '\\'; *p++ = c; *p++ = ctmp; *p = '\0';
		c = get_character(port);
		while (ALPHA_P(c) || DIGIT_P(c)) {
			*p++ = c;
			*p = '\0';
			if (!i--) break;
			c = get_character(port);
		}
		if (c == -1) return eof_object;
		if (c == '-') {
			/* i.e. ctrl-U */
			*p++ = c; *p = '\0';
			if (strcmpci(buf+2,"ctrl-")) goto bad_char_syntax;
			c = get_character(port);
			if (c == -1) return eof_object;
			*p++ = c; *p = '\0';
			if (c >= 'a' && c <= 'z') c -= ('a' - 'A');
			c -= '@';
			if (c < 0 || c >= 32) goto bad_char_syntax;
			ctmp = get_character(port);
			if (ctmp == -1) return eof_object;
			*p++ = ctmp; *p = '\0';
			if (!WHITESPACE_P(ctmp) && !DELIMITER_P(ctmp))
				goto bad_char_syntax;
			unget_character(ctmp,port);
			return make_character(c);
		} else if (WHITESPACE_P(c) || DELIMITER_P(c)) {
			/* the normal case - a simple symbolic name */
			int i = named_character(buf+2);
			unget_character(c,port); /* the delimiter */
			if (i > 0)
				c = i;
			else
				goto bad_char_syntax;
		} else {
		  bad_char_syntax:
			error(unbound_object,"read: bad character literal: %s",buf);
		}
	}
	return make_character(c);
}

static object read_line(object port) {
	long size = 0, max = BUFFER_CAPACITY(scratch_buffer);
	char c, *buf = BUFFER_DATA(scratch_buffer);
	object result = false_object;
	PUSH_GC_PROTECT(port);
	PUSH_GC_PROTECT(result);
	while ((c = get_character(port)) != -1) {
		if (c == CHAR_RETURN) {
			char c2 = get_character(port);
			if (c2 != -1 && c2 != CHAR_NEWLINE)
				unget_character(c2,port);
			break;
		} else if (c == CHAR_NEWLINE) break;
		buf[size++] = c;
		if (size >= max) {
			max *= 2;
			BUFFER_LENGTH(scratch_buffer) = max;
			scratch_buffer = grow_buffer(scratch_buffer,max);
			buf = BUFFER_DATA(scratch_buffer);
		}
	}
	if (c == -1 && size == 0)
		result = eof_object;
	else {
		buf[size] = '\0';
		result = make_string(buf);
		if (c == -1)
			result = cons(false_object,result);
		else
			result = cons(true_object,result);
	}
	POP_GC_PROTECT(2);
	return result;
}

static long read_atom_into_buf(char *buf, object port, char first_char) {
	char c;
	long i = 0;
	object result;
	result = unbound_object;
	buf[i++] = first_char;
	while ((c = get_character(port)) != -1) {
		if (WHITESPACE_P(c)) break;
		if (DELIMITER_P(c) || COMMENT_P(c)) {
			unget_character(c,port);
			break;
		}
		if (i < MAX_ATOM)
			buf[i++] = c;
	}
	buf[i] = '\0';
	return i;
}

static object read_object(object port) {
	char c, buf[MAX_ATOM+1];
	while ((c = get_character(port)) != -1) {
		if (WHITESPACE_P(c)) {
			continue;
		} else if (COMMENT_P(c)) {
			if (EOF_P(read_comment(port))) break;
			else continue;
		} else if (c == '(') {
			return read_list(port);
		} else if (c == ')') {
			error(unbound_object,"unexpected ')'");
			return void_object; 
		} else if (c == '"') {
			return read_string(port);
		} else if (c == '\'') {
			object obj = read_object(port);
			if (!EOF_P(obj))
				return list2(intern("quote"),obj);
			break;
		} else if (c == ',') {
			char d = get_character(port);
			if (d != -1) {
				object k,obj;
				if (d == '@')
					k = intern("unquote-splicing");
				else {
					k = intern("unquote");
					unget_character(d,port);
				}
				obj = read_object(port);
				if (!EOF_P(obj))
					return list2(k,obj);
			}
			break;
		} else if (c == '`') {
			object obj = read_object(port);
			if (!EOF_P(obj))
				return list2(intern("quasiquote"),obj);
			break;
		} else if (c == '#') {
			long i;
			c = get_character(port);
			if (c == -1) break;
			if (c == '!')
				return read_comment(port);
			if (c == '(')
				return read_vector(port);
			if (c == '\\') { /* a character literal */
				return read_character(port);
			}
			i = read_atom_into_buf(buf,port,c);
			c = buf[0];
			if (i == 1) {
				if (c == 'f' || c == 'F') return false_object;
				if (c == 't' || c == 'T') return true_object;
			} else if (c == 'x' ||	c == 'X') {
				object obj = parse_number(buf+1,16,-1);
				if (NUMBER_P(obj)) return obj;
			} else if (c == 'b' ||	c == 'B') {
				object obj = parse_number(buf+1,2,-1);
				if (NUMBER_P(obj)) return obj;
			} else if (c == 'd' ||	c == 'D') {
				object obj = parse_number(buf+1,10,-1);
				if (NUMBER_P(obj)) return obj;
			} else if (c == 'o' ||	c == 'O') {
				object obj = parse_number(buf+1,8,-1);
				if (NUMBER_P(obj)) return obj;
			} else if (c == 'e' || c == 'i' ||	c == 'E' ||	 c == 'I') {
				long exactness = (c == 'e' ||  c == 'E');
				object obj;
				if (buf[1] == '#') {
					char d = buf[2];
					long radix;
					if (d == 'b') radix = 2;
					else if (d == 'x') radix = 16;
					else if (d == 'o') radix = 8;
					else if (d == 'd') radix = 10;
					else radix = -1;
					if (radix > 0) {
						obj = parse_number(buf+3,radix,exactness);
						if (NUMBER_P(obj)) return obj;
					}
				} else {
					obj =  parse_number(buf+1,10,exactness);
					if (NUMBER_P(obj)) return obj;
				}
			}
			error(unbound_object,"read: bad syntax: #%s",buf);
		} else {
			object obj;
			long i = read_atom_into_buf(buf,port,c);
			if (i == 1 && c == '.')
				return dot_object;
			obj = parse_number(buf,10,-1);
			if (NUMBER_P(obj)) return obj;
#ifndef CASE_SENSITIVE
			while (i--) {
				char c = buf[i];
				if (c >= 'A' && c <= 'Z') buf[i] = c + 'a' - 'A';
			}
#endif
			return intern(buf);
		}
	}
	return eof_object;
}

object read_object_from_port(object inport) {
	object tmp;
	TYPE_CHECK(INPUT_PORT_P(inport),1,"input-port", inport);
	tmp = read_object(inport);
	return tmp;
}


/*
 *		  Writing
 */

void write_character(object port, int c) {
	(*(PORT_PUTC_PROC(port)))(port,c);
}

void write_string(object port,char *s) {
	putc_proc_pointer p = PORT_PUTC_PROC(port);
	while (*s) (*p)(port,*s++);
}

void printv(object port, char *format, va_list args) {
	char buf[1024];
	vsprintf(buf,format,args);
	write_string(port,buf);
	(PORT_FLUSH_PROC(port))(port);
}

void print(object port, char *format, ...) {
	va_list args;
	va_start(args,format);
	printv(port,format,args);
	va_end(args);
}

void number_to_string(object n, long radix, char *buf) {
	if (FIXNUM_P(n)) {
		long val = FIXNUM_VALUE(n);
		switch (radix) {
		  case 10:
			sprintf(buf,"%ld",val);
			break;
		  case 16:
			sprintf(buf,"%lx",val);
			break;
		  case 8:
			sprintf(buf,"%lo",val);
			break;
		  default:
			error(MAKE_FIXNUM(radix),"unsupported radix");
		}
	} else {
		double d = the_double(1,n);
		if (radix != 10) /* BUG: maybe shouldn't error on this */
			error(MAKE_FIXNUM(radix),"radix must be 10 for an inexact number");
		sprintf(buf,"%.8g",d);
	}
}

static char *char_names[256];

void write_object(object port, object obj, long for_read) {
	char buf[1024];
	PUSH_GC_PROTECT(port);
	PUSH_GC_PROTECT(obj);
	if (NULL_P(obj)) {
		write_character(port,'(');
		write_character(port,')');
	} else if (PAIR_P(obj)) {
		if (eq_p(CAR(obj),quote_symbol) && length(obj) == 2) {
			write_character(port,'\'');
			write_object(port,CADR(obj),for_read);
		} else if (eq_p(CAR(obj),quasiquote_symbol) && length(obj) == 2) {
			write_character(port,'`');
			write_object(port,CADR(obj),for_read);
		} else if (eq_p(CAR(obj),unquote_symbol) && length(obj) == 2) {
			write_character(port,',');
			write_object(port,CADR(obj),for_read);
		} else if (eq_p(CAR(obj),unquote_splicing_symbol) && length(obj) == 2){
			write_string(port,",@");
			write_object(port,CADR(obj),for_read);
		} else {
			long max = MAX_INT, count = 0;
			object omax = global("system:*print-max*");
			if (FIXNUM_P(omax)) max = FIXNUM_VALUE(omax);
			write_character(port,'(');
			while (count >= 0) {
				if (count++ < max)
					write_object(port,CAR(obj),for_read);
				else {
					count = -1;
					write_string(port,"...");
				}
				obj = CDR(obj);
				if (PAIR_P(obj))
					write_character(port,' ');
				else if (!NULL_P(obj)) {
					write_string(port," . ");
					write_object(port,obj,for_read);
					break;
				} else
					break;
			}
			write_character(port,')');
		}
	} else if (BOOLEAN_P(obj)) {
		write_character(port,'#');
		write_character(port,TRUE_P(obj)? 't' : 'f');
	} else if (UNBOUND_P(obj)) {
		write_string(port,"#<unbound>");
	} else if (VOID_P(obj)) {
		write_string(port,"#<void>");
	} else if (SYMBOL_P(obj)) {
		write_string(port,SYMBOL_NAME(obj));
	} else if (STRING_P(obj)) {
		if (for_read) {
			char c, *p = STRING_VALUE(obj);
			write_character(port,'"');
			while ((c = *p++) != 0) {
				if (c == '"' || c == '\\')
					write_character(port,'\\');
				write_character(port,c);
			}
			write_character(port,'"');
		} else
			write_string(port,STRING_VALUE(obj));
	} else if (FRAME_P(obj)) {
		write_string(port,"#<call-frame>");
	} else if (BUFFER_P(obj)) {
		sprintf(buf,"#<buffer (%ld)>",BUFFER_LENGTH(obj));
		write_string(port,buf);
	} else if (SYMBOLTABLE_P(obj)) {
		write_string(port,"#<symboltable>");
	} else if (FLONUM_P(obj)) {
		if (for_read)
			sprintf(buf,"%#.6g", FLONUM_VALUE(obj));
		else
			sprintf(buf,"%.8g", FLONUM_VALUE(obj));
		write_string(port,buf);
	} else if (CHARACTER_P(obj)) {
		int c = CHARACTER_VALUE(obj);
		if (for_read) {
			char *name = char_names[(unsigned char)c];
			if (name) {
				write_string(port,name);
			} else if (c >= 0 && c <= 32) {
				write_string(port,"#\\ctrl-");
				write_character(port,c+'@');
				
			} else {
				write_string(port,"#\\");
				write_character(port,c);
			}
		} else
			write_character(port,c);
	} else if (FIXNUM_P(obj)) {
		sprintf(buf,"%ld",FIXNUM_VALUE(obj));
		write_string(port,buf);
	} else if (VECTOR_P(obj)) {
		long i, max = VECTOR_LENGTH(obj);
		object *elements = VECTOR_ELEMENTS(obj);
		if (FALSE_P(VECTOR_TAG(obj))) {
			write_string(port,"#(");
			for (i=0; i<max; i++) {
				object o = elements[i];
				if (i) write_character(port,' ');
				write_object(port,o,for_read);
			}
			write_character(port,')');
		} else {
			write_string(port,"#<");
			write_object(port,VECTOR_TAG(obj),0);
			write_string(port,">");
		}
	} else if (SIGNAL_P(obj)) {
		long i, max = SIGNAL_LENGTH(obj);
		double *elements = SIGNAL_ELEMENTS(obj);
		write_string(port,"#(");
		for (i=0; i<max; i++) {
		    double d = elements[i];
		    if (i) write_character(port,' ');
		    if (for_read)
			sprintf(buf,"%#.6g", d);
		    else
			sprintf(buf,"%.8g", d);
		    write_string(port,buf);
		}
		write_character(port,')');
	} else if (OPCODE_P(obj)) {
		sprintf(buf,"#<procedure %s>", opcode_name(obj));
		write_string(port,buf);
	} else if (PRIMOP_P(obj)) {
		sprintf(buf,"#<procedure %s>", PRIMOP_NAME(obj));
		write_string(port,buf);
	} else if (CLOSURE_P(obj)) {
		char *name = CLOSURE_NAME(obj);
		if (name) {
			sprintf(buf,"#<closure %s>",name);
		} else {
			sprintf(buf,"#<anonymous closure>");
		}
		write_string(port,buf);
	} else if (PROCEDURE_P(obj)) {
		char *name = PROC_NAME(obj);
		if (name) {
			sprintf(buf,"#<procedure %s>",name);
		} else {
			sprintf(buf,"#<anonymous procedure>");
		}
		write_string(port,buf);
	} else if (CONTINUATION_P(obj)) {
		write_string(port,"#<continuation>");
	} else if (EOF_P(obj)) {
		write_string(port,"#<eof>");
	} else if (INPUT_PORT_P(obj)) {
		if (PORT_IS_OPEN_P(obj)) {
			if (PORT_STRING_P(obj))
				sprintf(buf,"#<input-string-port>");
			else
				sprintf(buf,"#<input-port %s>", PORT_NAME(obj));
		} else
			sprintf(buf,"#<input-port CLOSED>");
		write_string(port,buf);
	} else if (WEAK_P(obj)) {
		write_string(port,"#<weak-binding>");
	} else if (OUTPUT_PORT_P(obj)) {
		if (PORT_IS_OPEN_P(obj)) {
			if (PORT_STRING_P(obj))
				sprintf(buf,"#<output-string-port>");
			else
				sprintf(buf,"#<output-port %s>", PORT_NAME(obj));
		} else
			sprintf(buf,"#<output-port CLOSED>");
		write_string(port,buf);
	} else if (FORWARDED_P(obj)) {
		write_string(port,"#<FORWARDED ");
		write_object(port,FORWARDED_POINTER(obj),for_read);
		write_string(port,">");
	} else {
		if (POINTER_P(obj))
			sprintf(buf,"#<bizarre	%ld>",POINTER_TYPE(obj));
		else
			sprintf(buf,"#<bizarre	%lx>",obj);
		write_string(port,buf);
	}
	POP_GC_PROTECT(2);
}


void write_object_to_port(object outport, object obj, long for_read) {
	TYPE_CHECK(OUTPUT_PORT_P(outport),1,"output-port", outport);
	write_object(outport,obj,for_read);
}

static void primop_newline(long argc) {
	object p = argc? *sp++ : global("system:*current-output-port*");
	TYPE_CHECK(OUTPUT_PORT_P(p),1,"output-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot write to a closed port");
	write_string(p,newline_string(p));
	*--sp = void_object;
}

static void primop_write_char(long argc) {
	object p = (argc == 2)? sp[1] : global("system:*current-output-port*");
	char c = the_char(1,sp[0]);
	TYPE_CHECK(OUTPUT_PORT_P(p),2,"output-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot write to a closed port");
	write_character(p,c);
	if (argc == 2) sp++;
	*sp = void_object;
}

static void primop_write_string(long argc) {
	object s = *sp++;
	object p = *sp++;
	char *sz;
	TYPE_CHECK(STRING_P(s),1,"string",s);
	TYPE_CHECK(OUTPUT_PORT_P(p),2,"output-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot write to a closed port");
	sz = STRING_VALUE(s);
	while (*sz) {
		write_character(p,*sz++);
	}
	*--sp = void_object;
}

static void primop_write_chars(long argc) {
	int i;
	object p = *sp++;
	TYPE_CHECK(OUTPUT_PORT_P(p),1,"output-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot write to a closed port");
	for (i=1; i<argc; i++) {
		char c = the_char(1,*sp++);
		write_character(p,c);
	}
	*--sp = void_object;
}

static void primop_write_fixnums(long argc) {
	int i;
	object p = *sp++;
	TYPE_CHECK(OUTPUT_PORT_P(p),1,"output-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot write to a closed port");
	for (i=1; i<argc; i++) {
		char c = (char)the_long(1,*sp++);
		write_character(p,c);
	}
	*--sp = void_object;
}

static void primop_write(long argc) {
	object p = (argc == 2)? sp[1] : global("system:*current-output-port*");
	TYPE_CHECK(OUTPUT_PORT_P(p),2,"output-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot write to a closed port");
	write_object(p,*sp,1);
	if (argc == 2) sp++;
	*sp = void_object;
}

static void primop_display(long argc) {
	object p = (argc == 2)? sp[1] : global("system:*current-output-port*");
	TYPE_CHECK(OUTPUT_PORT_P(p),2,"output-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot write to a closed port");
	write_object(p,sp[0],0);
	if (argc == 2) sp++;
	*sp = void_object;
}


static void primop_read(long argc) {
	object p = (argc == 1)? sp[0] : global("system:*current-input-port*");
	TYPE_CHECK(INPUT_PORT_P(p),2,"input-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot read from a closed port");
	p = read_object_from_port(p);
	if (argc == 1)
		*sp = p;
	else
		*--sp = p;
}

static void primop_read_char(long argc) {
	char c;
	object p = (argc == 1)? sp[0] : global("system:*current-input-port*");
	TYPE_CHECK(INPUT_PORT_P(p),2,"input-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot read from a closed port");
	c = get_character(p);
	p = (c == -1)? eof_object : make_character(c);
	if (argc)
		*sp = p;
	else
		*--sp = p;
}

static void primop_peek_char(long argc) {
	char c;
	object p = (argc == 1)? sp[0] : global("system:*current-input-port*");
	TYPE_CHECK(INPUT_PORT_P(p),2,"input-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot read from a closed port");
	c = get_character(p);
	unget_character(c,p);
	p = (c == -1)? eof_object : make_character(c);
	if (argc)
		*sp = p;
	else
		*--sp = p;
}

static void primop_char_ready_p(long argc) {
	object p = (argc == 1)? sp[0] : global("system:*current-input-port*");
	TYPE_CHECK(INPUT_PORT_P(p),2,"input-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot read from a closed port");
	p = ((PORT_READY_PROC(p))(p))? true_object : false_object;
	if (argc)
		*sp = p;
	else
		*--sp = p;
}

static void primop_eof_p(long argc) {
	if (!EOF_P(*sp)) *sp = false_object;
}

static void primop_read_line(long argc) {
	object p = (argc == 1)? sp[0] : global("system:*current-input-port*");
	TYPE_CHECK(INPUT_PORT_P(p),2,"input-port",p);
	if (!PORT_IS_OPEN_P(p))
		error(p,"Cannot read from a closed port");
	p = read_line(p);
	if (argc)
		*sp = p;
	else
		*--sp = p;
}

static char *string_value(object o) {
	if (SYMBOL_P(o)) return SYMBOL_NAME(o);
	if (!STRING_P(o))
		error(o,"Not a string or symbol");
	return STRING_VALUE(o);
}

int find_file(char *buf, char *name) {
	static char dir_buf[MAX_FILENAME_LENGTH];
	object dirs = global("system:*search-path*");
	if (name[0] == '~') {
		expand_path(buf,name);
		if (does_file_exist(buf)) return 1;
		strcat(buf,".scm");
		if (does_file_exist(buf)) return 1;
	} else {
		strcpy(buf,name);
		if (does_file_exist(buf)) return 1;
		strcat(buf,".scm");
		if (does_file_exist(buf)) return 1;
		if (name[0] != '/' && strncmp(name,"./",2)) {
			while (PAIR_P(dirs)) {
				char *p = NULL;
				object dir = CAR(dirs);
				dirs = CDR(dirs);
				if (STRING_P(dir))
					p = STRING_VALUE(dir);
				else if (SYMBOL_P(dir))
					p = SYMBOL_NAME(dir);
				expand_path(dir_buf,p);
				sprintf(buf,"%s/%s",dir_buf,name);
				if (does_file_exist(buf)) return 1;
				sprintf(buf,"%s/%s.scm",dir_buf,name);
				if (does_file_exist(buf)) return 1;
			}
		}
	}
	return 0;
}

static void primop_find_file(long argc) {
	char fullpath[MAX_FILENAME_LENGTH];
	char *file = string_value(sp[0]);
	if (argc == 2) {
		char tmp[MAX_FILENAME_LENGTH];
		char *dir = string_value(sp[1]);
		sp++;
		sprintf(tmp,"%s/%s",dir,file);
		if (find_file(fullpath,tmp))
			*sp = make_string(fullpath);
		else
			*sp = false_object;
	} else {
		if (find_file(fullpath,file))
			*sp = make_string(fullpath);
		else
			*sp = false_object;
	}
}

static void primop_probe_file(long argc) {
	char path[1024];
	object tmp = *sp;
	TYPE_CHECK(STRING_P(tmp),1,"string",tmp);
	expand_path(path,STRING_VALUE(tmp));
	if (!does_file_exist(path))
		*sp = false_object;
	else
		*sp = make_string(path);
}

static void primop_home_directory(long argc) {
	char buf[MAX_FILENAME_LENGTH];
	get_home_directory(buf);
	*--sp = make_string(buf);
}

static void primop_remove_file(long argc) {
	char buf[MAX_FILENAME_LENGTH];
	object tmp = *sp;
	TYPE_CHECK(STRING_P(tmp),1,"string",tmp);
	expand_path(buf,STRING_VALUE(tmp));
	remove_file(buf);
}

static void primop_command(long argc) {
	TYPE_CHECK(STRING_P(sp[0]),1,"string",sp[0]);
	if (!execute_system_command(STRING_VALUE(sp[0])))
	*sp = false_object;
}

void primop_pause(long argc) {
	long msec = the_long(1,*sp);
	msec_sleep(msec);
}

void primop_timestamp(long argc) {
	long msec = msec_since_launch();
	*--sp = MAKE_FIXNUM(msec);
}

void primop_flush_output_port(long argc) {
	object p = (argc == 1)? sp[0] : global("system:*current-output-port*");
	TYPE_CHECK(OUTPUT_PORT_P(p),1,"output-port",p);
	if (PORT_IS_OPEN_P(p))
		(PORT_FLUSH_PROC(p))(p);
	if (argc != 1) *--sp = p;
}


void init_io(void) {
	int i;
	for (i=0; i<256; i++) char_names[i] = NULL;
	char_names[CHAR_NULL] = "#\\null";
	char_names[CHAR_BACKSPACE] = "#\\backspace";
	char_names[CHAR_TAB] = "#\\tab";
	char_names[CHAR_NEWLINE] = "#\\newline";
	char_names[CHAR_RETURN] = "#\\return";
	char_names[CHAR_ESCAPE] = "#\\escape";
	char_names[CHAR_SPACE] = "#\\space";
	char_names[CHAR_RUBOUT] = "#\\rubout";
	char_names[CHAR_F1] = "#\\f1";
	char_names[CHAR_F2] = "#\\f2";
	char_names[CHAR_F3] = "#\\f3";
	char_names[CHAR_F4] = "#\\f4";
	char_names[CHAR_F5] = "#\\f5";
	char_names[CHAR_F6] = "#\\f6";
	char_names[CHAR_F7] = "#\\f7";
	char_names[CHAR_F8] = "#\\f8";
	char_names[CHAR_F9] = "#\\f9";
	char_names[CHAR_F10] = "#\\f10";
	char_names[CHAR_F11] = "#\\f11";
	char_names[CHAR_F12] = "#\\f12";
	char_names[CHAR_LEFT] = "#\\left";
	char_names[CHAR_DOWN] = "#\\down";
	char_names[CHAR_UP] = "#\\up";
	char_names[CHAR_RIGHT] = "#\\right";
	char_names[CHAR_PRINT] = "#\\print";
	char_names[CHAR_PAUSE] = "#\\pause";
	char_names[CHAR_HOME] = "#\\home";
	char_names[CHAR_END] = "#\\end";
	char_names[CHAR_PAGEUP] = "#\\pageup";
	char_names[CHAR_PAGEDOWN] = "#\\pagedown";
	char_names[CHAR_INSERT] = "#\\insert";
	char_names[CHAR_DELETE] = "#\\delete";
	scratch_buffer = make_buffer(64);
	PUSH_GC_PROTECT(scratch_buffer);
	define("system:*print-max*",MAKE_FIXNUM(1000));
	define_primop("display",primop_display,1,2);
	define_primop("write-char",primop_write_char,1,2);
	define_primop("write",primop_write,1,2);
	define_primop("newline",primop_newline,0,1);
	define_primop("read",primop_read,0,1);
	define_primop("read-char",primop_read_char,0,1);
	define_primop("peek-char", primop_peek_char,0,1);
	define_primop("char-ready?",primop_char_ready_p,0,1);
	define_primop("eof-object?",primop_eof_p,1,1);
	define_primop("system:find-file",primop_find_file,1,2);
	define_primop("system:probe-file",primop_probe_file,1,1);
	define_primop("system:read-line",primop_read_line,0,1);
	define_primop("system:home-directory",primop_home_directory,0,0);
	define_primop("system:remove-file",primop_remove_file,1,1);
	define_primop("system:command",primop_command,1,1);
	define_primop("system:pause",primop_pause,1,1);
	define_primop("system:timestamp",primop_timestamp,0,0);
	define_primop("system:flush-output-port",primop_flush_output_port,0,1);
	define_primop("system:write-string",primop_write_string,1,2);
	define_primop("system:write-chars",primop_write_chars,2,MAX_ARGC);
	define_primop("system:write-fixnums",primop_write_fixnums,2,MAX_ARGC);
	define_primop("system:port-set-edlin!",primop_port_edlin,2,2);
	
}
