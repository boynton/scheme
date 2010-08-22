/* LeeScheme/logical.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"

int eqv_p(object o1, object o2) {
	if (eq_p(o1,o2)) return 1;
	if (NUMBER_P(o1) && NUMBER_P(o2))
		return (the_double(1,o1) == the_double(2,o2))? 1 : 0;
	return 0;
}

int equal_p(object o1, object o2) {
	if (eqv_p(o1,o2)) return 1;
	if (PAIR_P(o1)) {
		return PAIR_P(o2)&&equal_p(CAR(o1),CAR(o2))&&equal_p(CDR(o1),CDR(o2));
	} else if (VECTOR_P(o1)) {
		if (VECTOR_P(o2)) {
			long max = VECTOR_LENGTH(o1);
			if (max == VECTOR_LENGTH(o2)) {
				object *e1 = VECTOR_ELEMENTS(o1), *e2 = VECTOR_ELEMENTS(o2);
				long i;
				for (i=0; i<max; i++)
					if (!equal_p(e1[i],e2[i]))
						return 0;
				return 1;
			}
		}
	} else if (STRING_P(o1)) {
		if (STRING_P(o2)) {
			long max = STRING_LENGTH(o1);
			if (max == STRING_LENGTH(o2)) {
				char *p1 = STRING_VALUE(o1);
				char *p2 = STRING_VALUE(o2);
				while (*p1 && *p2) {
					if (*p1++ != *p2++) return 0;
				}
				return (*p1 == *p2);
			}
		}
	}
	return 0;
}

static void primop_eq_p(long argc) {
	object tmp = *sp++;
	*sp = (eq_p(tmp,*sp))? true_object : false_object;
}

static void primop_eqv_p(long argc) {
	object tmp = *sp++;
	*sp = (eqv_p(tmp,*sp))? true_object : false_object;
}

static void primop_equal_p(long argc) {
	object tmp = *sp++;
	*sp = (equal_p(tmp,*sp))? true_object : false_object;
}

static void primop_not(long argc) {
	if (FALSE_P(*sp))
		*sp = true_object;
	else
		*sp = false_object;
}

static void primop_boolean_p(long argc) {
	*sp = (BOOLEAN_P(*sp))? true_object : false_object;
}

void init_logical(void) {
	define_primop("eq?",primop_eq_p,2,2);
	define_primop("eqv?",primop_eqv_p,2,2);
	define_primop("equal?", primop_equal_p,2,2);
	define_primop_inline("not", primop_not,1,OPCODE_NOT);
	/*	define_primop("not", primop_not,1,1);*/
	define_primop("boolean?",primop_boolean_p,1,1);
}

