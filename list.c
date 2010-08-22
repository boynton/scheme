/* LeeScheme/list.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"

object cons(object car, object cdr) {
	object p;
	gc_tmp1 = car;
	gc_tmp2 = cdr;
	p = make_heap_object(PAIR_TYPE,sizeof(struct pair_heap_structure));
	CAR(p) = gc_tmp1;
	CDR(p) = gc_tmp2;
	return p;
}

object make_list(long size, object val) {
	gc_tmp4 = null_object;
	gc_tmp3 = val;
	while (size--)
		gc_tmp4 = cons(gc_tmp3,gc_tmp4);
	return gc_tmp4;
}

object list1(object car) {
	return cons(car,null_object);
}

object cons2(object car1, object car2, object cdr) {
	gc_tmp3 = car1;
	return cons(car1,cons(car2,cdr));
}

object list2(object car1, object car2) {
	return cons2(car1,car2,null_object);
}

object cons3(object car1, object car2, object car3, object cdr) {
	gc_tmp4 = car1;
	return cons(car1,cons2(car2,car3,cdr));
}

object list3(object car1, object car2, object car3) {
	return cons3(car1,car2,car3,null_object);
}

object list4(object car1,object car2,object car3,object car4) {
	gc_tmp5 = car1;
	return cons(car1,list3(car2,car3,car4));
}

long length(object l) {
	long i = 0;
	object start = l;
	if (NULL_P(l)) return i;
	else while (PAIR_P(l)) {
		i++;
		l = CDR(l);
		if (eq_p(l,start)) return -1;
	}
	return NULL_P(l)? i : -1;
}

object memq(object o, object l) {
	if (NULL_P(l)) return false_object;
	else while (PAIR_P(l)) {
		if (eq_p(o,CAR(l))) return l;
		l = CDR(l);
	}
	return false_object;
}

object assq(object key, object alist) {
	if (NULL_P(alist))
		return false_object;
	else while (PAIR_P(alist)) {
		object tmp = CAR(alist);
		if (PAIR_P(tmp) && eq_p(key,CAR(tmp)))
			return tmp;
		alist = CDR(alist);
	}
	return false_object;
}

object nreverse(object l) {
	object result = null_object, tmp = null_object;
	while (!NULL_P(l)) {
		tmp = l;
		l = CDR(l);
		CDR(tmp) = result;
		result = tmp;
	}
	return result;
}


/* Runtime library */

static void primop_null_p(long argc) {
	if (!NULL_P(*sp)) *sp = false_object;
}

static void primop_pair_p(long argc) {
	if (!PAIR_P(*sp)) *sp = false_object;
}

static void primop_list_p(long argc) {
	object o = *sp;
	if (!NULL_P(o)) {
		long i = length(o);
		if (i < 0)
			*sp = false_object;
	}
}

static void primop_set_car(long argc) {
	object p = *sp++;
	TYPE_CHECK(PAIR_P(p),1,"pair",p);
	CAR(p) = *sp;
}

static void primop_set_cdr(long argc) {
	object p = *sp++;
	TYPE_CHECK(PAIR_P(p),1,"pair",p);
	CDR(p) = *sp;
}

static void primop_list_tail(long argc) {
	object l = *sp++;
	object c = *sp;
	long count;
	TYPE_CHECK(LIST_P(l),1,"list",l);
	count = the_long(2,c);
	while (count-- > 0) {
		if (!PAIR_P(l)) break;
		l = CDR(l);
	}
	*sp = l;
}

static void primop_list_ref(long argc) {
	object l = *sp++;
	object c = *sp;
	long count;
	TYPE_CHECK(LIST_P(l),1,"list",l);
	count = the_long(2,c);
	while (count-- > 0) {
		if (!PAIR_P(l)) break;
		l = CDR(l);
	}
	if (!PAIR_P(l))
		error(c,"index out of range");
	*sp = CAR(l);
}


static void primop_memq(long argc) {
	object o = *sp++;
	object l = *sp;
	while (!NULL_P(l)) {
		TYPE_CHECK(PAIR_P(l),2,"list",l);
		if (eq_p(CAR(l),o)) {
			*sp = l;
			return;
		}
		l = CDR(l);
	}
	*sp = false_object;
}

static void primop_memv(long argc) {
	object o = *sp++;
	object l = *sp;
	while (!NULL_P(l)) {
		TYPE_CHECK(PAIR_P(l),2,"list",l);
		if (eqv_p(CAR(l),o)) {
			*sp = l;
			return;
		}
		l = CDR(l);
	}
	*sp = false_object;
}

static void primop_member(long argc) {
	object o = *sp++;
	object l = *sp;
	while (!NULL_P(l)) {
		TYPE_CHECK(PAIR_P(l),2,"list",l);
		if (equal_p(CAR(l),o)) {
			*sp = l;
			return;
		}
		l = CDR(l);
	}
	*sp = false_object;
}

static void primop_assq(long argc) {
	object obj = *sp++;
	object f, alist = *sp;
	if (!NULL_P(alist)) {
		TYPE_CHECK(PAIR_P(alist),2,"list",alist);
		while (PAIR_P(alist)) {
			f = CAR(alist);
			if (PAIR_P(f) && eq_p(CAR(f),obj)) {
				*sp = f;
				return;
			}
			alist = CDR(alist);
		}
	}
	*sp = false_object;
}

static void primop_assv(long argc) {
	object obj = *sp++;
	object f, alist = *sp;
	if (!NULL_P(alist)) {
		TYPE_CHECK(PAIR_P(alist),2,"list",alist);
		while (PAIR_P(alist)) {
			f = CAR(alist);
			if (PAIR_P(f) && eqv_p(CAR(f),obj)) {
				*sp = f;
				return;
			}
			alist = CDR(alist);
		}
	}
	*sp = false_object;
}

static void primop_assoc(long argc) {
	object obj = *sp++;
	object f, alist = *sp;
	if (!NULL_P(alist)) {
		TYPE_CHECK(PAIR_P(alist),2,"list",alist);
		while (PAIR_P(alist)) {
			f = CAR(alist);
			if (PAIR_P(f) && equal_p(CAR(f),obj)) {
				*sp = f;
				return;
			}
			alist = CDR(alist);
		}
	}
	*sp = false_object;
}

static void primop_length(long argc) {
	long i = length(*sp);
	if (i < 0)
		TYPE_CHECK(0,1,"list",*sp);
	*sp = MAKE_FIXNUM(i);
}

static void primop_append(long argc) {
	object result = null_object, tail = null_object, tmp = null_object;
	long i = 0, max = argc-1;
	PUSH_GC_PROTECT(result);
	PUSH_GC_PROTECT(tail);
	PUSH_GC_PROTECT(tmp);
	while (i < argc) {
		tmp = sp[i];
		if (!NULL_P(tmp)) {
			if (i == max && !PAIR_P(tmp)) {
				if (NULL_P(result))
					result = tmp;
				else
					CDR(tail) = tmp;
			} else {
				TYPE_CHECK(PAIR_P(tmp),i+1,"list",tmp);
				gc_tmp5 = cons(CAR(tmp),null_object);
				if (NULL_P(result)) {
					tail = result = gc_tmp5;
				} else {
					CDR(tail) = gc_tmp5;
					tail = CDR(tail);
				}
				tmp = CDR(tmp);
				while (PAIR_P(tmp)) {
					gc_tmp5 = cons(CAR(tmp),null_object);
					CDR(tail) = gc_tmp5;
					tail = CDR(tail);
					tmp = CDR(tmp);
				}
				if (i != max) {
					TYPE_CHECK(NULL_P(tmp),i+1,"list",tmp);
				} else
					CDR(tail) = tmp;
			}
		}
		i++;
	}
	POP_GC_PROTECT(3);
	sp += argc;
	*--sp = result;
}

static void primop_reverse(long argc) {
	object l = *sp;
	object result = null_object;
	PUSH_GC_PROTECT(result);
	PUSH_GC_PROTECT(l);
	while (!NULL_P(l)) {
		if (!PAIR_P(l))
			error(l,"argument not a list");
		result = cons(CAR(l),result);
		l = CDR(l);
	}
	POP_GC_PROTECT(2);
	*sp = result;
}

static void primop_cons(long argc) {
	object tmp = *sp++;
	*sp = cons(tmp,*sp);
}

static void primop_car(long argc) {
	object p = *sp;
	TYPE_CHECK(PAIR_P(p),1,"pair",p);
	*sp = CAR(p);
}

static void primop_cdr(long argc) {
	object p = *sp;
	TYPE_CHECK(PAIR_P(p),1,"pair",p);
	*sp = CDR(p);
}

void init_list(void) {
	define_primop_inline("null?", primop_null_p,1,OPCODE_NULL_P);
	define_primop_inline("car", primop_car,1,OPCODE_CAR);
	define_primop_inline("cdr", primop_cdr,1,OPCODE_CDR);
	define_primop("cons", primop_cons,2,2);
	define_primop("pair?", primop_pair_p,1,1);
	define_primop("list?", primop_list_p,1,1);
	define_primop("length", primop_length,1,1);
	define_primop("append", primop_append,0,MAX_ARGC);
	define_primop("reverse", primop_reverse,1,1);
	define_primop("set-car!", primop_set_car,2,2);
	define_primop("set-cdr!", primop_set_cdr,2,2);
	define_primop("list-ref", primop_list_ref,2,2);
	define_primop("list-tail", primop_list_tail,2,2);
	define_primop("memq", primop_memq,2,2);
	define_primop("memv", primop_memv,2,2);
	define_primop("member", primop_member,2,2);
	define_primop("assq", primop_assq,2,2);
	define_primop("assv", primop_assv,2,2);
	define_primop("assoc", primop_assoc,2,2);
}

