/* LeeScheme/table.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _TABLE_
#define _TABLE_

#include "object.h"
#include "vector.h"

typedef struct table_heap_structure {
    long header;
    long count;
    object tag; /* determines what "type-of" returns */
    object bindings; /* the type tag of the bindings determine the type. */
} *table_pointer;
#define TABLE_P(obj) (POINTER_TYPE_P(obj,TABLE_TYPE))
#define TABLE_COUNT(obj) (((table_pointer)(obj))->count)
#define TABLE_CAPACITY(obj) (VECTOR_LENGTH(((table_pointer)(obj))->bindings))
#define TABLE_TAG(obj) (((table_pointer)(obj))->tag)
#define TABLE_BINDINGS(obj) (((table_pointer)(obj))->bindings)
#define TABLE_ELEMENTS(obj) (VECTOR_ELEMENTS((((table_pointer)(obj))->bindings)))

#define MAX_TABLE_SIZE (MAX_INT>>1)

extern void init_table(void);

#endif
