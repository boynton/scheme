/* LeeScheme/list.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _LIST_
#define _LIST_

#include "object.h"

#ifdef POINTERS
extern object null_object;
#else
#define null_object MAKE_IMMEDIATE(SPECIAL_TYPE,1)
#endif
#define NULL_P(_o) (_o == null_object)

typedef struct pair_heap_structure {
    long header;
    object car;
    object cdr;
} *pair_pointer;
#define PAIR_P(obj) (POINTER_TYPE_P(obj,PAIR_TYPE))
#define CAR(obj) (((pair_pointer)(obj))->car)
#define CDR(obj) (((pair_pointer)(obj))->cdr)
#define CAAR(obj) (CAR(CAR(obj)))
#define CADR(obj) (CAR(CDR(obj)))
#define CDAR(obj) (CDR(CAR(obj)))
#define CDDR(obj) (CDR(CDR(obj)))
#define CAADR(o) (CAR(CADR(o)))
#define CADDR(obj) (CAR(CDR(CDR(obj))))
#define CADAR(obj) (CAR(CDR(CAR(obj))))
#define CDADR(o) (CDR(CADR(o)))
#define CDDDR(obj) (CDR(CDR(CDR(obj))))

#define LIST_P(_o) (NULL_P(_o) || PAIR_P(_o))

extern void init_list(void);

extern object cons(object car, object cdr);
extern object cons2(object car1, object car2, object cdr);
extern object cons3(object car1, object car2, object car3, object cdr);
extern object make_list(long size, object val);
extern object list1(object car1);
extern object list2(object car1, object car2);
extern object list3(object car1, object car2, object car3);
extern object list4(object car1,object car2,object car3,object car4);
extern long length(object l);
extern object nreverse(object l);
extern object memq(object o, object l);
extern object assq(object key, object alist);

#endif
