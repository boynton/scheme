/* LeeScheme/vector.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"

object make_vector(long length, object fill) {
	long i = length * sizeof(object);
	long size = sizeof(struct vector_heap_structure) + i;
	object *ve, v = make_heap_object(VECTOR_TYPE,size);
	VECTOR_LENGTH(v) = length;
	VECTOR_TAG(v) = false_object;
	ve = VECTOR_ELEMENTS(v);
	while (length--)
		*ve++ = fill;
	return v;
}

static void primop_vector_p(long argc) {
	if (VECTOR_P(*sp) && FALSE_P(VECTOR_TAG(*sp))) return;
	*sp = false_object;
}

static void primop_make_vector(long argc) {
	object o = *sp++;
	long size = the_long(1,o);
	if (size < 0 || size > MAX_VECTOR_SIZE)
		error(sp[0],"too big of a size for a vector");
	if (argc == 2)
		*sp = make_vector(size, *sp);
	else
		*--sp = make_vector(size, null_object);
}

static void primop_vector(long argc) {
	object *ve, v = make_vector(argc,null_object);
	long i;
	ve = VECTOR_ELEMENTS(v);
	for (i=0; i<argc; i++)
		*ve++ = *sp++;
	*--sp = v;
}

static void primop_vector_length(long argc) {
	object o = *sp;
	TYPE_CHECK(VECTOR_P(o),1,"vector",o);
	*sp = MAKE_FIXNUM(VECTOR_LENGTH(o));
}

static void primop_set_vector_tag(long argc) {
	object o = *sp++;
	TYPE_CHECK(VECTOR_P(o),1,"vector",o);
	VECTOR_TAG(o) = *sp;
}

static void primop_vector_ref(long argc) {
	object o = sp[1];
	long i = the_long(2,o);
	o = *sp++;
	TYPE_CHECK(VECTOR_P(o),1,"vector",o);
	if (i < 0 || i >= VECTOR_LENGTH(o))
		error(MAKE_FIXNUM(i),"index out of range");
	*sp = VECTOR_ELEMENTS(o)[i];
}

static void primop_vector_set(long argc) {
	object o = sp[1];
	long i = the_long(2,o);
	o = *sp;
	sp += 2;
	TYPE_CHECK(VECTOR_P(o),1,"vector",o);
	if (i < 0 || i >= VECTOR_LENGTH(o))
		error(MAKE_FIXNUM(i),"index out of range");
	VECTOR_ELEMENTS(o)[i] = *sp;
}

object list_to_vector(object l) {
	long i = length(l);
	object v, *ve;
	if (i < 0)
		error(l,"argument not a proper list");
	v = make_vector(i,null_object);
	ve = VECTOR_ELEMENTS(v);
	while (i--) {
		*ve++ = CAR(l);
		l = CDR(l);
	}
	return v;
}

static void primop_list_to_vector(long argc) {
	object l = *sp;
	TYPE_CHECK(LIST_P(l),1,"list",l);
	*sp = list_to_vector(l);
}

object vector_to_list(object v) {
	object l = null_object, *ve;
	long i = VECTOR_LENGTH(v);
	PUSH_GC_PROTECT(l);
	ve = VECTOR_ELEMENTS(v);
	while (i--)
		l = cons(ve[i],l);
	POP_GC_PROTECT(1);
	return l;
}

static void primop_vector_to_list(long argc) {
	object v = *sp;
	TYPE_CHECK(VECTOR_P(v),1,"vector",v);
	*sp = vector_to_list(v);
}



object make_signal(long length, double fill) {
	long i = length * sizeof(double);
	long size = sizeof(struct signal_heap_structure) + i;
	object v = make_heap_object(SIGNAL_TYPE,size);
	double *ve;
	SIGNAL_LENGTH(v) = length;
	ve = SIGNAL_ELEMENTS(v);
	while (length--)
		*ve++ = fill;
	return v;
}

static void primop_signal_p(long argc) {
	if (SIGNAL_P(*sp)) return;
	*sp = false_object;
}

static void primop_make_signal(long argc) {
	object o = *sp++;
	long size = the_long(1,o);
	if (size < 0 || size > MAX_VECTOR_SIZE)
		error(o,"too big of a size for a signal");
	if (argc == 2) {
	    double fill = the_double(2, *sp);
	    *sp = make_signal(size, fill);
	} else
	    *--sp = make_signal(size, 0.0);
}

static void primop_signal_length(long argc) {
	object o = *sp;
	TYPE_CHECK(SIGNAL_P(o),1,"signal",o);
	*sp = MAKE_FIXNUM(SIGNAL_LENGTH(o));
}

static void primop_signal_ref(long argc) {
	object o = sp[1];
	long i = the_long(2,o);
	o = *sp++;
	TYPE_CHECK(SIGNAL_P(o),1,"signal",o);
	if (i < 0 || i >= SIGNAL_LENGTH(o))
		error(MAKE_FIXNUM(i),"index out of range");
	*sp = make_flonum(SIGNAL_ELEMENTS(o)[i]);
}

static void primop_signal_set(long argc) {
	object o = sp[1];
	long i = the_long(2,o);
	o = *sp;
	sp += 2;
	TYPE_CHECK(SIGNAL_P(o),1,"signal",o);
	if (i < 0 || i >= SIGNAL_LENGTH(o))
		error(MAKE_FIXNUM(i),"index out of range");
	SIGNAL_ELEMENTS(o)[i] = the_double(3, *sp);
}

void init_vector(void) {
	define_primop("vector?",primop_vector_p,1,1);
	define_primop("make-vector",primop_make_vector,1,2);
	define_primop("vector",primop_vector,0,MAX_ARGC);
	define_primop("vector-length",primop_vector_length,1,1);
	define_primop("system:set-vector-type!",primop_set_vector_tag,2,2);
	define_primop("vector-ref",primop_vector_ref,2,2);
	define_primop("vector-set!",primop_vector_set,3,3);
	define_primop("list->vector",primop_list_to_vector,1,1);
	define_primop("vector->list",primop_vector_to_list,1,1);

	define_primop("signal?",primop_signal_p,1,1);
	define_primop("make-signal",primop_make_signal,1,2);
	define_primop("signal-length",primop_signal_length,1,1);
	define_primop("signal-ref",primop_signal_ref,2,2);
	define_primop("signal-set!",primop_signal_set,3,3);
}
