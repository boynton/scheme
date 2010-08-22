/* LeeScheme/symbol.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _SYMBOL_
#define _SYMBOL_

#include "object.h"

typedef struct symbol_heap_structure {
    long header;
    object global_value;
    /* char name[] */
} *symbol_pointer;
#define SYMBOL_P(obj) (POINTER_TYPE_P(obj,SYMBOL_TYPE))
#define SYMBOL_NAME(obj) ((char *)((obj) + sizeof(struct symbol_heap_structure)))
#define SYMBOL_VALUE(obj) (((symbol_pointer)(obj))->global_value)
extern object make_symbol(char *name);

typedef struct symboltable_heap_structure {
    long header;
    long count;
    object mappings; /* a vector of symbols */
} *symboltable_pointer;
#define SYMBOLTABLE_P(obj) (POINTER_TYPE_P(obj, SYMBOLTABLE_TYPE))
#define SYMBOLTABLE_COUNT(obj) (((symboltable_pointer)(obj))->count)
#define SYMBOLTABLE_MAPPINGS(obj) (((symboltable_pointer)(obj))->mappings)
#define SYMBOLTABLE_SIZE(obj) (VECTOR_LENGTH(SYMBOLTABLE_MAPPINGS(obj)))
extern object make_symboltable(long size);
extern void symboltable_rehash(object table);
extern void symboltable_add(object table, object sym);
extern void symboltable_map(object table, void (*proc)(object val));
extern object symboltable_get(object table, char *key);

extern void init_symbol(void);

#endif
