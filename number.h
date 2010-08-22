/* LeeScheme/number.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _NUMBER_
#define _NUMBER_

#include "object.h"

#define FIXNUM_SHIFT 2
#define MAKE_FIXNUM(_v) ((object)(((long)(_v) << 2) + FIXNUM_TYPE))
#define FIXNUM_VALUE(_o) (((long)(_o)) >> FIXNUM_SHIFT)
#define MAX_FIXNUM (536870911)
#define MIN_FIXNUM (-536870911)


typedef struct flonum_heap_structure {
    object header;
    double value;
} *flonum_pointer;
#define FLONUM_P(obj) (POINTER_TYPE_P(obj,FLONUM_TYPE))
#define FLONUM_VALUE(obj) (((flonum_pointer)(obj))->value)

#define NUMBER_P(_o) (FIXNUM_P(_o) || FLONUM_P(_o))

extern long the_long(long argnum, object o);
extern double the_double(long argnum, object o);


extern void init_math(void);

extern object make_fixnum(long value);
extern object make_flonum(double value);
extern object make_number(double val);



#if 0
/* this would be nice: */
typedef struct bignum_heap_structure {
    object header;
    long length;
    /* short digits[] */
} *bignum_pointer;
#define BIGNUM_P(obj) (POINTER_TYPE_P(obj,BIGNUM_TYPE))
#define BIGNUM_LENGTH(obj) (((bignum_pointer)(obj))->length)
#define BIGNUM_DIGITS(obj) ((short *)((obj) + sizeof(struct bignum_heap_structure)))
extern object make_bignum(long value);
#endif


#endif
