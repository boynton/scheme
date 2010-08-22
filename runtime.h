/* LeeScheme/runtime.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _RUNTIME_
#define _RUNTIME_

extern object environment;
extern object constant_vector, *constants;

typedef struct frame_heap_structure {
    long header;
    object previous;
    long *pc;
    object env;
    /* object elements[] */
} *frame_pointer;
#define FRAME_P(o) (POINTER_TYPE_P(o,FRAME_TYPE))
#define FRAME_PC(o) (((frame_pointer)(o))->pc)
#define FRAME_ENV(o) (((frame_pointer)(o))->env)
#define FRAME_PREVIOUS(o) (((frame_pointer)(o))->previous)
#define FRAME_ELEMENTS(o) ((object *)((o) + sizeof(struct frame_heap_structure)))

typedef struct weak_heap_structure {
    long header;
    long bound;
    object value;
} *weak_pointer;
#define WEAK_P(o) (POINTER_TYPE_P(o,WEAK_TYPE))
#define WEAK_BOUND(o) (((weak_pointer)(o))->bound)
#define WEAK_VALUE(o) (((weak_pointer)(o))->value)
extern object weak_binding(object value);

typedef struct buffer_heap_structure {
    long header;
    long length;
    /* data */
} *buffer_pointer;
#define BUFFER_P(o) (POINTER_TYPE_P(o,BUFFER_TYPE))
#define BUFFER_CAPACITY(o) (POINTER_LENGTH(o)-sizeof(struct buffer_heap_structure))
#define BUFFER_LENGTH(o) (((buffer_pointer)(o))->length)
#define BUFFER_DATA(o) ((char *)((o) + sizeof(struct buffer_heap_structure)))
extern object make_buffer(long length);
extern object grow_buffer(object old, long new_length);

#ifdef POINTERS
extern object void_object;
extern object unbound_object;
#else
#define void_object (MAKE_IMMEDIATE(SPECIAL_TYPE,0))
#define unbound_object (MAKE_IMMEDIATE(SPECIAL_TYPE,2))
#endif
#define VOID_P(o) ((o) == void_object)
#define UNBOUND_P(o) ((o) == unbound_object)

extern object intern(char *name);
extern object define(char *name, object value);
extern object global(char *name);

#ifndef NO_TYPE_CHECK
#define TYPE_CHECK(_pred,_argnum,_type,_arg) { \
    if (!(_pred)) \
        error(_arg,"argument %d is not a %s",_argnum,_type);\
}
#else
#define TYPE_CHECK(_pred,_argnum,_type,_arg) ((void)0)
#endif

extern void init_runtime(void);
extern void fix_runtime_pointers(void);

extern object execute(object the_code);

extern void restart(object tag);
extern void interrupt(void);
extern void warning(object obj, char *format, ...);
extern void error(object obj, char *format, ...);
extern void fatal_error(char *format, ...);


#endif
