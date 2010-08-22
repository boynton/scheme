/* LeeScheme/table.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include "table.h"

object make_table(long capacity) {
  object bindings = make_vector(capacity*2, false_object);
  long size = sizeof(struct table_heap_structure);
  object *ve, v;
  PUSH_GC_PROTECT(bindings);
  v = make_heap_object(TABLE_TYPE,size);
  TABLE_COUNT(v) = 0;
  VECTOR_TAG(bindings) = false_object;
  /* FIX ME */
  TABLE_BINDINGS(v) = bindings;
  POP_GC_PROTECT(1);
  return v;
}

static void primop_table_p(long argc) {
    if (TABLE_P(*sp) && FALSE_P(VECTOR_TAG(TABLE_BINDINGS(*sp)))) return;
    *sp = false_object;
}

static void primop_make_table(long argc) {
  long capacity = (argc == 1)? the_long(1,*sp) : 0;
  if (capacity < 1)
	capacity = 1;
  else if (capacity > MAX_TABLE_SIZE)
	error(sp[0],"too big of a capacity for a table");
  if (argc == 1)
	*sp = make_table(capacity);
  else
	*--sp = make_table(capacity);
}

static void primop_table(long argc) {
  if (argc & 1)
	error(sp[0], "table requires an even number of arguments");
  else {
    object *pBindings, tbl = make_table(argc/2);
    long i;
    pBindings = VECTOR_ELEMENTS(TABLE_BINDINGS(tbl));
    for (i=0; i<argc; i++)
        *pBindings++ = *sp++;
    *--sp = tbl;
  }
}

static void primop_table_count(long argc) {
    object o = *sp;
    TYPE_CHECK(TABLE_P(o),1,"table",o);
    *sp = MAKE_FIXNUM(TABLE_COUNT(o));
}

static void primop_set_table_tag(long argc) {
    object o = *sp++;
    TYPE_CHECK(TABLE_P(o),1,"table",o);
    TABLE_TAG(o) = *sp;
}

static void primop_table_get(long argc) {
    object tbl = *sp++;
    object key = *sp;
    object *pBindings;
    long i, imax;
    TYPE_CHECK(TABLE_P(tbl),1,"table",tbl);
    pBindings = TABLE_ELEMENTS(tbl);
    imax = TABLE_COUNT(tbl);
    while (imax-- > 0) {
      if (eq_p(*pBindings++, key)) {
	*sp = *pBindings;
	return;
      } else
	pBindings++;
    }
    *sp = false_object;
}

static void primop_table_put(long argc) {
    object tbl = *sp++;
    object key = *sp++;
    object val = *sp;
    object *pBindings;
    int i, imax;
    TYPE_CHECK(TABLE_P(tbl),1,"table",tbl);
    pBindings = TABLE_ELEMENTS(tbl);
    imax = TABLE_COUNT(tbl);
    while (imax-- > 0) {
      if (eq_p(*pBindings++, key)) {
	*pBindings = val;
	return;
      } else
	pBindings++;
    }
    *sp = false_object;
}

static void primop_table_ref(long argc) {
    object o = sp[1];
    long i = the_long(2,o);
    o = *sp++;
    TYPE_CHECK(TABLE_P(o),1,"table",o);
    if (i < 0 || i >= TABLE_COUNT(o))
        error(MAKE_FIXNUM(i),"index out of range");
    *sp = TABLE_ELEMENTS(o)[i*2+1];
}

static void primop_table_key_ref(long argc) {
    object o = sp[1];
    long i = the_long(2,o);
    o = *sp++;
    TYPE_CHECK(TABLE_P(o),1,"table",o);
    if (i < 0 || i >= TABLE_COUNT(o))
        error(MAKE_FIXNUM(i),"index out of range");
    *sp = TABLE_ELEMENTS(o)[i*2];
}

static void primop_table_set(long argc) {
    object o = sp[1];
    long i = the_long(2,o);
    o = *sp;
    sp += 2;
    TYPE_CHECK(TABLE_P(o),1,"table",o);
    if (i < 0 || i >= TABLE_COUNT(o))
        error(MAKE_FIXNUM(i),"index out of range");
    TABLE_ELEMENTS(o)[i*2+1] = *sp;
}

void init_table(void) {
    define_primop("system:table?",primop_table_p,1,1);
    define_primop("system:make-table",primop_make_table,0,1);
    define_primop("system:table",primop_table,0,MAX_ARGC);
    define_primop("system:table-count",primop_table_count,1,1);
    define_primop("system:set-table-type!",primop_set_table_tag,2,2);
    define_primop("system:table-get",primop_table_get,2,2);
    define_primop("system:table-put!",primop_table_put,3,3);
    define_primop("system:table-ref",primop_table_ref,2,2);
    define_primop("system:table-set!",primop_table_set,3,3);
}
