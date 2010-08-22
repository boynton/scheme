/* LeeScheme/proc.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _PROCEDURE_
#define _PROCEDURE_

#define MAX_ARGC MAX_INT

typedef struct proc_heap_structure {
    long header;
    object module;
    long argc;
    long *ops;
} *proc_pointer;
#define PROCEDURE_P(o) (POINTER_TYPE_P(o,PROCEDURE_TYPE))
#define PROC_OPS(o) (((proc_pointer)(o))->ops)
#define PROC_ARGC(o) (((proc_pointer)(o))->argc)
#define PROC_MODULE(o) (((proc_pointer)(o))->module)
#define PROC_NAME(o) (SYMBOL_P(PROC_MODULE(o))? \
		      SYMBOL_NAME(PROC_MODULE(o)) \
		      : (char *)0)

typedef struct closure_heap_structure {
    long header;
    object environment;
    object procedure;
} *closure_pointer;
#define CLOSURE_P(o) (POINTER_TYPE_P(o,CLOSURE_TYPE))
#define CLOSURE_PROC(o) (((closure_pointer)(o))->procedure)
#define CLOSURE_ENV(o) (((closure_pointer)(o))->environment)
#define CLOSURE_MODULE(o) (PROC_MODULE(CLOSURE_PROC(o)))
#define CLOSURE_NAME(o) (PROC_NAME(CLOSURE_PROC(o)))


typedef struct continuation_heap_structure {
    long header;
    object frame;
    long *pc;
    long stack_size;
    /* object stack[] */
} *continuation_pointer;
#define CONTINUATION_P(o) (POINTER_TYPE_P(o,CONTINUATION_TYPE))
#define CONTINUATION_FRAME(o) (((continuation_pointer)(o))->frame)
#define CONTINUATION_PC(o) (((continuation_pointer)(o))->pc)
#define CONTINUATION_STACKSIZE(o) (((continuation_pointer)(o))->stack_size)
#define CONTINUATION_STACK(o) \
    ((object *)((o) + sizeof(struct continuation_heap_structure)))

#define OPCODE_P(_o) (IMMEDIATE_TYPE_P(_o, OPCODE_TYPE))
typedef enum {
    OPCODE_LOCAL00,	/* 0 */
    OPCODE_LOCAL01,
    OPCODE_LOCAL0,
    OPCODE_LOCAL10,
    OPCODE_LOCAL1,
    OPCODE_LOCAL2,	/* 5 */
    OPCODE_LOCAL3,
    OPCODE_JUMPFALSE,
    OPCODE_JUMP,
    OPCODE_PRIMOP,
    OPCODE_TAILCALL,	/* 10 */
    OPCODE_CALL,
    OPCODE_RETURN,
    OPCODE_CLOSURE,
    OPCODE_GLOBAL,
    OPCODE_LITERAL,	/* 15 */
    OPCODE_CONSTANT,
    OPCODE_POP,
    OPCODE_LOCAL,
    OPCODE_DEF_GLOBAL,
    OPCODE_SET_GLOBAL,	/* 20 */
    OPCODE_SET_LOCAL,
    OPCODE_NULL_P,
    OPCODE_CAR,
    OPCODE_CDR,
    OPCODE_NOT, 	/* 25 */
    OPCODE_CALLCC,
    OPCODE_APPLY
} opcode;
#define OPCODE_MAX OPCODE_APPLY
#define OPCODE_VALUE(o) ((opcode)IMMEDIATE_DATA(o))
#define MAKE_OPCODE(o) (MAKE_IMMEDIATE(OPCODE_TYPE,o))
extern char *opcode_name(object o);

typedef void (*primop_proc)(long argc);

typedef struct primop_heap_structure {
    long header;
    primop_proc proc;
    long argc_min;
    long argc_max;
    long inline_op;
    char * name;
} *primop_pointer;
#define PRIMOP_P(o) (POINTER_TYPE_P(o,PRIMOP_TYPE))
#define PRIMOP_PROC(o) (((primop_pointer)(o))->proc)
#define PRIMOP_ARGC_MIN(o) (((primop_pointer)(o))->argc_min)
#define PRIMOP_ARGC_MAX(o) (((primop_pointer)(o))->argc_max)
#define PRIMOP_INLINE_OP(o) (((primop_pointer)(o))->inline_op)
#define PRIMOP_NAME(o) (((primop_pointer)(o))->name)

extern void define_primop(char *name, primop_proc proc, long argc1, long argc2);
extern void define_primop_inline(char *name,primop_proc proc,long argc,long op);

extern object make_continuation(object the_frame, long *pc, object *sp);
extern object make_procedure(object opbuffer, object name, long argc);
extern object make_closure(object code, object env);

extern void init_procedure(void);

#endif
