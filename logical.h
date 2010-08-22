/* LeeScheme/logical.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _LOGICAL_
#define _LOGICAL_

#include "object.h"

#define BOOLEAN_P(_o) (IMMEDIATE_TYPE_P(_o,BOOLEAN_TYPE))
#ifdef POINTERS
extern object false_object, true_object;
#else
#define false_object MAKE_IMMEDIATE(BOOLEAN_TYPE,0)
#define true_object MAKE_IMMEDIATE(BOOLEAN_TYPE,1)
#endif
#define FALSE_P(_o) (_o == false_object)
#define TRUE_P(_o) (_o != false_object)

#define eq_p(o1,o2) (o1 == o2)
extern int eqv_p(object o1, object o2);
extern int equal_p(object o1, object o2);

extern void init_logical(void);

#endif
