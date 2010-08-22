/* LeeScheme/io.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _IO_
#define _IO_

#define COMMENT_P(c) (c == ';')
#define WHITESPACE_P(c) (c == ' ' || c == '\n' || c == '\t' || c == '\015')
#define PAREN_P(c) (c == '(' || c == ')')
#define BRACKET_P(c) (c == '[' || c == ']')
#define BRACE_P(c) (c == '{' || c == '}')
#define DELIMITER_P(c) (PAREN_P(c) || BRACKET_P(c) || BRACE_P(c))
#define ALPHA_P(c) ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
#define DIGIT_P(c) (c >= '0' && c <= '9')
#define HEXDIGIT_P(c) (DIGIT_P(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
#define MAX_ATOM 255

#define eof_object MAKE_IMMEDIATE(SPECIAL_TYPE,3)
#define EOF_P(_o) (_o == eof_object)
#define dot_object MAKE_IMMEDIATE(SPECIAL_TYPE,4)
#define DOT_P(_o) (_o == dot_object)


extern void init_io(void);

void write_character(object port, int c);
void write_string(object port, char *s);
void print(object port, char *format, ...);
void printv(object port, char *format, va_list args);

extern void write_object_to_port(object outport, object obj, long for_read);
    /* this one type-checks the port */
extern void write_object(object outport, object obj, long for_read);

extern object read_object_from_port(object inport);

extern object parse_number(char *buf, long radix, long exact);

extern void number_to_string(object n, long radix, char *buf);

extern int find_file(char *buf, char *name);

#endif


