/* LeeScheme/string.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _STRING_
#define _STRING_

#include "object.h"

typedef struct string_heap_structure {
    long header;
    long length;
    /* char elements[] */
} *string_pointer;
#define STRING_P(obj) (POINTER_TYPE_P(obj,STRING_TYPE))
#define STRING_LENGTH(obj) (((string_pointer)(obj))->length)
#define STRING_VALUE(obj) ((char *)((obj) + sizeof(struct string_heap_structure)))
#define MAX_STRING_SIZE (0x00ffffff)

#define CHARACTER_P(_o) (IMMEDIATE_TYPE_P(_o,CHARACTER_TYPE))
#define CHARACTER_VALUE(_o) ((char)IMMEDIATE_DATA(_o))


extern int strcmpci(char *s1, char *s2);
extern int strncmpci(char *s1, char *s2, long count);
extern int charcmpci(char c1, char c2);

extern char the_char(long argnum, object o);

extern void init_string(void);

extern object make_character(char value);
extern object make_string_of_size(long length,int zero);
extern object make_string(char *value);

/* ANSI extended character codes with names. */
#define CHAR_UNKNOWN (3) /* should print as a unique glyph, if possible */
#define CHAR_NULL (0)
#define CHAR_BEEP (7)
#define CHAR_BACKSPACE (8)
#define CHAR_TAB (9)
#define CHAR_NEWLINE (10)
#define CHAR_RETURN (13)
#define CHAR_ESCAPE (27)
#define CHAR_SPACE (32)
#define CHAR_RUBOUT (127)
#define CHAR_F1 (128)
#define CHAR_F2 (129)
#define CHAR_F3 (130)
#define CHAR_F4 (131)
#define CHAR_F5 (132)
#define CHAR_F6 (133)
#define CHAR_F7 (134)
#define CHAR_F8 (135)
#define CHAR_F9 (136)
#define CHAR_F10 (137)
#define CHAR_F11 (138)
#define CHAR_F12 (139)
#define CHAR_LEFT (140)
#define CHAR_UP (141)
#define CHAR_DOWN (142)
#define CHAR_RIGHT (143)
#define CHAR_PRINT (144)
#define CHAR_PAUSE (147)
#define CHAR_HOME (148)
#define CHAR_END (149)
#define CHAR_PAGEUP (150)
#define CHAR_PAGEDOWN (151)
#define CHAR_INSERT (152)
#define CHAR_DELETE (153)

#endif
