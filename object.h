/* LeeScheme/object.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _OBJECT_
#define _OBJECT_

typedef unsigned long object;

#ifndef  POINTERS

#define POINTER_P(_o) (((int)(_o) & 3) == 0)
#define FIXNUM_P(_o) (((int)(_o) & 3) == 1)
#define IMMEDIATE_P(_o) (((int)(_o) & 3) == 2)
#define UNUSED_TYPE_P(_o) (((int)(_o) & 3) == 3)

#define IMMEDIATE_DATA(_o) ((_o) >> 6)
#define IMMEDIATE_TYPE(_o) ((_o) & 0x0000003f)
#define IMMEDIATE_TYPE_P(_o,_t) (IMMEDIATE_TYPE(_o) == _t)
#define MAKE_IMMEDIATE(_t,_v) ((object)(((_v) << 6) + _t))

#else

#define POINTER_P(_o) (1)
#define IMMEDIATE_P(_o) (0)
#define FIXNUM_P(obj) (POINTER_TYPE_P(obj,FIXNUM_TYPE))

#endif

#define POINTER_LENGTH(_o) (((*((long *)(_o))) >> 8) & 0x00ffffff)
#define POINTER_TYPE(_o) ((*((long *)(_o))) & 255)
#define POINTER_TYPE_P(_o,_t) (POINTER_P(_o) && POINTER_TYPE(_o) == _t)
#define POINTER_HEADER(_o) (*((long *)(_o)))

#define FIXNUM_TYPE (1)

#define BOOLEAN_TYPE (2)
#define CHARACTER_TYPE (6)
#define OPCODE_TYPE (10)
#define SPECIAL_TYPE (18)

#define PAIR_TYPE (23)
#define SYMBOL_TYPE (24)
#define FLONUM_TYPE (25)
#define STRING_TYPE (26)
#define VECTOR_TYPE (27)
#define PORT_TYPE (28)
#define PROCEDURE_TYPE (29)
#define CLOSURE_TYPE (30)
#define FRAME_TYPE (31)
#define CONTINUATION_TYPE (32)
#define PRIMOP_TYPE (33)
#define BUFFER_TYPE (34)
#define SYMBOLTABLE_TYPE (35)
#define WEAK_TYPE (36)
#define TERMINAL_TYPE (37)
#define FORWARDED_TYPE (38)
#define BIGNUM_TYPE (39)
#define TABLE_TYPE (40)
#define SIGNAL_TYPE (41)

#define MAX_INT (0x7fffffff)
#define MIN_INT (0x80000000)

#endif

