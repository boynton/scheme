/* LeeScheme/symbol.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include <string.h>

static long strhash(char *s, long range) {
        long len = strlen(s);
	long hash = 0;
	long tmp = 0;
	while (len >= sizeof(long)) {
		hash ^= *((long *)s);
		s += sizeof(long);
		len -= sizeof(long);
	}
	while (len-- > 0)
		tmp = (tmp << 8) + *s++;
	hash ^= tmp;
	return hash % range;
}

void symboltable_rehash(object table) {
	long i, max = SYMBOLTABLE_SIZE(table);
	long new_size = max * 2;
	object new_mappings = make_vector(new_size, unbound_object);
	object *mappings = VECTOR_ELEMENTS(SYMBOLTABLE_MAPPINGS(table));
	SYMBOLTABLE_MAPPINGS(table) = new_mappings;
	SYMBOLTABLE_COUNT(table) = 0;
	for (i=0; i<max; i++) {
		object sym = mappings[i];
		if (SYMBOL_P(sym))
			symboltable_add(table,sym);
	}
}

void symboltable_add(object table, object sym) {
	char *key = SYMBOL_NAME(sym);
	long max = SYMBOLTABLE_SIZE(table);
	long i, h = strhash(key,max);
	long patience = 100;
	object *mappings = VECTOR_ELEMENTS(SYMBOLTABLE_MAPPINGS(table));
	i = h;
	while (SYMBOL_P(mappings[i])) {
		if (!strcmp(SYMBOL_NAME(mappings[i]),key))
			return;
		i++;
		if (i >= max) i = 0;
		if (i == h || !patience--) {
			symboltable_rehash(table);
			symboltable_add(table,sym);
			return;
		}
	}
	mappings[i] = sym;
	SYMBOLTABLE_COUNT(table)++;
}

void symboltable_map(object table, void (*proc)(object val)) {
	object *mappings = VECTOR_ELEMENTS(SYMBOLTABLE_MAPPINGS(table));
	long i, max = SYMBOLTABLE_SIZE(table);
	for (i=0; i<max; i++) {
		if (SYMBOL_P(mappings[i]))
			(*proc)((mappings[i]));
	}
}

object symboltable_get(object table, char *key) {
	long max = SYMBOLTABLE_SIZE(table);
	long i, h = strhash(key,max);
	object *mappings = VECTOR_ELEMENTS(SYMBOLTABLE_MAPPINGS(table));
	i = h;
	while (SYMBOL_P(mappings[i])) {
		if (!strcmp(SYMBOL_NAME(mappings[i]),key))
			return mappings[i];
		i++;
		if (i >= max) i = 0;
		if (i == h) break;
	}
	return null_object;
}

object make_symboltable(long size) {
	long i = sizeof(struct symboltable_heap_structure);
	object table = make_heap_object(SYMBOLTABLE_TYPE,i);
	object mappings = make_vector(size,unbound_object);
	SYMBOLTABLE_COUNT(table) = 0;
	SYMBOLTABLE_MAPPINGS(table) = mappings;
	return table;
}

object make_symbol(char *name) {
	long len = strlen(name);
	long size = sizeof(struct symbol_heap_structure) + len + 1;
	object s = make_heap_object(SYMBOL_TYPE,size);
	SYMBOL_VALUE(s) = unbound_object;
	strcpy(SYMBOL_NAME(s),name);
	SYMBOL_NAME(s)[len] = '\0';
	return s;
}


static void primop_string_to_symbol(long argc) {
	object o = *sp;
	TYPE_CHECK(STRING_P(o),1,"string",o);
	*sp = intern(STRING_VALUE(o));
}

static void primop_symbol_to_string(long argc) {
	object o = *sp;
	TYPE_CHECK(SYMBOL_P(o),1,"symbol",o);
	*sp = make_string(SYMBOL_NAME(o));
}

static void primop_symbol_p(long argc) {
	if (!SYMBOL_P(*sp)) *sp = false_object;
}


void init_symbol(void) {
	define_primop("symbol?",primop_symbol_p,1,1);
	define_primop("string->symbol",primop_string_to_symbol,1,1);
	define_primop("symbol->string",primop_symbol_to_string,1,1);
}
