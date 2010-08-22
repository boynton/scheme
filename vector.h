/* LeeScheme/vector.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _VECTOR_
#define _VECTOR_

#include "object.h"

typedef struct vector_heap_structure {
    long header;
    long length;
    object tag; /* determines what "type-of" returns */
    /* object elements[] */
} *vector_pointer;
#define VECTOR_P(obj) (POINTER_TYPE_P(obj,VECTOR_TYPE))
#define VECTOR_LENGTH(obj) (((vector_pointer)(obj))->length)
#define VECTOR_TAG(obj) (((vector_pointer)(obj))->tag)
#define VECTOR_ELEMENTS(obj) ((object *)((obj) + sizeof(struct vector_heap_structure)))
#define MAX_VECTOR_SIZE (MAX_INT)

extern void init_vector(void);
extern object make_vector(long length, object fill);
extern object vector_to_list(object v);
extern object list_to_vector(object l);

typedef struct signal_heap_structure {
    long header;
    long length;
    /* double elements[] */
} *signal_pointer;
#define SIGNAL_P(obj) (POINTER_TYPE_P(obj,SIGNAL_TYPE))
#define SIGNAL_LENGTH(obj) (((signal_pointer)(obj))->length)
#define SIGNAL_ELEMENTS(obj) ((double *)((obj) + sizeof(struct signal_heap_structure)))
#define MAX_SIGNAL_SIZE (MAX_INT)

extern object make_signal(long length, double fill);
extern object vector_to_list(object v);
extern object list_to_vector(object l);

#endif
