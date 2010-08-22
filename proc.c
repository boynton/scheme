/* LeeScheme/proc.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include <string.h> /* memcpy */

void define_primop(char *name, primop_proc proc,long argc0, long argcN) {
	object result;
	result = make_heap_object(PRIMOP_TYPE,
							  sizeof(struct primop_heap_structure));
	PRIMOP_PROC(result) = proc;
	PRIMOP_ARGC_MIN(result) = argc0;
	PRIMOP_ARGC_MAX(result) = argcN;
	PRIMOP_INLINE_OP(result) = -1;
	PRIMOP_NAME(result) = name;
	define(name,result);
}

void define_primop_inline(char *name, primop_proc proc,long argc, long op) {
	object result;
	result = make_heap_object(PRIMOP_TYPE,
							  sizeof(struct primop_heap_structure));
	PRIMOP_PROC(result) = proc;
	PRIMOP_ARGC_MIN(result) = argc;
	PRIMOP_ARGC_MAX(result) = argc;
	PRIMOP_NAME(result) = name;
	PRIMOP_INLINE_OP(result) = op;
	define(name,result);
}

char *opcode_name(object op) {
	switch (OPCODE_VALUE(op)) {
	  case OPCODE_LOCAL: return "local";
	  case OPCODE_LOCAL00: return "local00";
	  case OPCODE_LOCAL01: return "local01";
	  case OPCODE_LOCAL0: return "local0";
	  case OPCODE_LOCAL10: return "local10";
	  case OPCODE_LOCAL1: return "local1";
	  case OPCODE_LOCAL2: return "local2";
	  case OPCODE_LOCAL3: return "local3";
	  case OPCODE_PRIMOP: return "primop";
	  case OPCODE_POP: return "pop";
	  case OPCODE_LITERAL: return "literal";
	  case OPCODE_CONSTANT: return "constant";
	  case OPCODE_CALL: return "call";
	  case OPCODE_RETURN: return "return";
	  case OPCODE_TAILCALL: return "tailcall";
	  case OPCODE_CLOSURE: return "closure";
	  case OPCODE_GLOBAL: return "global";
	  case OPCODE_CALLCC: return "call/cc";
	  case OPCODE_APPLY: return "apply";
	  case OPCODE_SET_LOCAL: return "set-local";
	  case OPCODE_SET_GLOBAL: return "set-global";
	  case OPCODE_DEF_GLOBAL: return "def-global";
	  case OPCODE_JUMPFALSE: return "jump-false";
	  case OPCODE_JUMP: return "jump";
	  case OPCODE_NULL_P: return "null?";
	  case OPCODE_CAR: return "car";
	  case OPCODE_CDR: return "cdr";
	  case OPCODE_NOT: return "not";
	  default: return "Bizarre opcode";
	}
}


object make_procedure(object opbuffer, object name, long argc) {
	long op_size = BUFFER_LENGTH(opbuffer);
	object result;
	gc_tmp2 = name;
	gc_tmp3 = opbuffer;
	result = make_heap_object(PROCEDURE_TYPE,
							  sizeof(struct proc_heap_structure));
	PROC_OPS(result) = (long *)allocate_code_space(op_size);
	memcpy(PROC_OPS(result),BUFFER_DATA(gc_tmp3),op_size);
	PROC_MODULE(result) = gc_tmp2;
	PROC_ARGC(result) = argc;
	return result;
}

object make_continuation(object the_frame, long *pc, object *sp) {
	object result;
	long size = stack_top - sp;
	long i = size * sizeof(object);
	gc_tmp1 = the_frame;
	result = make_heap_object(CONTINUATION_TYPE,
							  sizeof(struct continuation_heap_structure)+i);
	CONTINUATION_FRAME(result) = gc_tmp1;
	CONTINUATION_PC(result) = pc;
	if ((CONTINUATION_STACKSIZE(result) = size) != 0) {
		memcpy(CONTINUATION_STACK(result),sp,i);
	}
	return result;
}

object make_closure(object proc, object env) {
	object result;
	gc_tmp1 = proc;
	gc_tmp2 = env;
	result = make_heap_object(CLOSURE_TYPE,
							  sizeof(struct closure_heap_structure));
	CLOSURE_ENV(result) = gc_tmp2;
	CLOSURE_PROC(result) = gc_tmp1;
	return result;
}

/* Runtime library */

static void primop_procedure_p(long argc) {
	object p = *sp;
	if (!(OPCODE_P(p) || PRIMOP_P(p) || PROCEDURE_P(p) ||
		  CLOSURE_P(p) || CONTINUATION_P(p)))
		*sp = false_object;
}


void init_procedure(void) {
	define_primop("procedure?", primop_procedure_p,1,1);
}

