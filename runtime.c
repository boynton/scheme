/* LeeScheme/runtime.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include <stdarg.h>
#include <string.h>
#include <setjmp.h>
#include <stdio.h>

object environment, constant_vector, *constants;
static object restart_continuation;
static object restart_value;

object *reestablish_stack(long i, object *the_sp, object cont) {
	object arg, *p, *q;
	if (i != 1)
		error(MAKE_FIXNUM(i),"wrong number of arguments to call/cc");
	arg = *the_sp;
	i = CONTINUATION_STACKSIZE(cont);
	p = the_sp = stack_top - i;
	q = CONTINUATION_STACK(cont);
	while (i--)
		*p++ = *q++;
	*--the_sp = arg;
	return the_sp;
}

#define PROC_ERROR_NAME(p) \
(SYMBOL_P(PROC_MODULE(p))? SYMBOL_NAME(PROC_MODULE(p)) : "anonymous procedure")

object link_frame(object the_env, long argc, object *values, long *pc, object previous) {
    long i = argc * sizeof(object);
    long j = sizeof(struct frame_heap_structure) + i;
    object *penv, result = (object)heap_pointer;
    j = ALIGN_LONG_POINTER(j);
    heap_pointer += j;
    if (heap_pointer >= heap_end) {
	gc_tmp1 = previous;
	gc_tmp3 = the_env;
	garbage_collect(j);
	previous = gc_tmp1;
	the_env = gc_tmp3;
	result = (object)heap_pointer;
	heap_pointer += j;
    }
    POINTER_HEADER(result) = (j << 8) + FRAME_TYPE;
    FRAME_PREVIOUS(result) = previous;
    FRAME_PC(result) = pc;
    FRAME_ENV(result) = the_env;
    penv = FRAME_ELEMENTS(result);
    while (argc--) *penv++ = *values++;
    FILL_TO_ALIGN(penv,unbound_object);
    return result;	
}

object make_frame(object the_code, object the_env, long argc, object *values, long *pc, object previous) {
    long expected_argc = PROC_ARGC(the_code);
    object result;
    if (expected_argc >= 0) {
	object *penv;
	long i = expected_argc * sizeof(object);
	long j = sizeof(struct frame_heap_structure) + i;
	if (expected_argc != argc)
	    error(MAKE_FIXNUM(argc), "wrong number of arguments to %s",
		  PROC_ERROR_NAME(the_code));
	result = (object)heap_pointer;
	j = ALIGN_LONG_POINTER(j);
	heap_pointer += j;
	if (heap_pointer >= heap_end) {
	    gc_tmp1 = previous;
	    gc_tmp3 = the_env;
	    garbage_collect(j);
	    previous = gc_tmp1;
	    the_env = gc_tmp3;
	    result = (object)heap_pointer;
	    heap_pointer += j;
	}
	POINTER_HEADER(result) = (j << 8) + FRAME_TYPE;
	FRAME_PREVIOUS(result) = previous;
	FRAME_PC(result) = pc;
	FRAME_ENV(result) = the_env;
	penv = FRAME_ELEMENTS(result);
	while (argc--) *penv++ = *values++;
	FILL_TO_ALIGN(penv,unbound_object);
    } else {
	long required_argc = -expected_argc - 1;
	long i = (required_argc+1) * sizeof(object);
	long j = sizeof(struct frame_heap_structure) + i;
	object *penv, rest = null_object;
	if (argc < required_argc)
	    error(MAKE_FIXNUM(argc), "too few arguments to %s",
		  PROC_ERROR_NAME(the_code));
	PUSH_GC_PROTECT(previous);
	PUSH_GC_PROTECT(the_env);
	PUSH_GC_PROTECT(rest);
	while (argc-- > required_argc)
	    rest = cons(values[argc],rest);
	result = make_heap_object(FRAME_TYPE,j);
	FRAME_PREVIOUS(result) = previous;
	FRAME_PC(result) = pc;
	FRAME_ENV(result) = the_env;
	penv = FRAME_ELEMENTS(result);
	while (required_argc--)
	    *penv++ = *values++;
	*penv++ = rest;
	FILL_TO_ALIGN(penv,unbound_object);
	POP_GC_PROTECT(3);
    }
    return result;
}

long explode_apply_args(object *the_sp, long argc) {
	long i, j = argc-1;
	long len;
	gc_tmp3 = the_sp[j];
	/* the last arg must be a list */
	if (!NULL_P(gc_tmp3) && !PAIR_P(gc_tmp3))
		error(gc_tmp3,"Last argument of apply not a list");
	for (i=j-1; i >= 0; i--)
		gc_tmp3 = cons(the_sp[i],gc_tmp3);
	len = length(gc_tmp3);
	the_sp = the_sp + j - len + 1;
	for (i=0; i < len; i++) {
		*the_sp++ = CAR(gc_tmp3);
		gc_tmp3 = CDR(gc_tmp3);
	}
	return len - argc;
}

long *funcall(object proc, long argc, long *pc) {
  funcall_again:
	if (POINTER_P(proc)) {
		unsigned char type = POINTER_TYPE(proc);
		if (type == PRIMOP_TYPE) {
			if (PRIMOP_ARGC_MIN(proc) <= argc && PRIMOP_ARGC_MAX(proc) >= argc)
				(PRIMOP_PROC(proc))(argc);
			else
				error(MAKE_FIXNUM(argc),"wrong number of arguments to '%s'",
					  PRIMOP_NAME(proc));
			return pc;
		} else if (type == PROCEDURE_TYPE) {
			gc_tmp5 = proc;
			environment = make_frame(proc, null_object, argc, sp,
									 pc, environment);
			sp += argc;
			return PROC_OPS(gc_tmp5);
		} else if (type == CLOSURE_TYPE) {
			gc_tmp5 = proc;
			environment = make_frame(CLOSURE_PROC(proc), CLOSURE_ENV(proc),
									 argc, sp, pc, environment);
			sp += argc;
			return PROC_OPS(CLOSURE_PROC(gc_tmp5));
		} else if (type == CONTINUATION_TYPE) {
			gc_tmp5 = proc;
			sp = reestablish_stack(argc,sp,gc_tmp5);
			environment = CONTINUATION_FRAME(gc_tmp5);
			return CONTINUATION_PC(gc_tmp5);
		} else {
			goto not_a_proc_error;
		}
	} else if (OPCODE_P(proc)) {
		opcode op = OPCODE_VALUE(proc);
		if (op == OPCODE_CALLCC) {
			if (argc != 1) goto argc_error;
			gc_tmp5 = *sp++;
			*--sp = make_continuation(environment,pc,sp);
			proc = gc_tmp5;
			goto funcall_again;
		} else if (op == OPCODE_APPLY) {
			long j = argc-1;
			if (j < 1) goto argc_error;
			gc_tmp4 = *sp++;
			argc = explode_apply_args(sp,j);
			sp -= argc;
			argc += j;
			proc = gc_tmp4;
			goto funcall_again;
		} else if (op == OPCODE_NOT) {
		    if (FALSE_P(*sp))
			*sp = true_object;
		    else
			*sp = false_object;
		}
	} else
		goto not_a_proc_error;
	return pc;
  not_a_proc_error:
	error(proc,"not a procedure");
  argc_error:
	error(MAKE_FIXNUM(argc),"wrong number of arguments");
	return 0; /* never reached */
}

long *tailcall(object tmp, long i) {
	long *pc;
  tail_funcall:
	if (POINTER_P(tmp)) {
		unsigned char type = POINTER_TYPE(tmp);
		if (type == PRIMOP_TYPE) {
			if (PRIMOP_ARGC_MIN(tmp) <= i && PRIMOP_ARGC_MAX(tmp) >= i)
				(PRIMOP_PROC(tmp))(i);
			else
				error(MAKE_FIXNUM(i), "wrong number of arguments to '%s'",
					  PRIMOP_NAME(tmp));
			pc = FRAME_PC(environment);
			environment = FRAME_PREVIOUS(environment);
			return pc;
		} else if (type == CLOSURE_TYPE) {
			object proc = CLOSURE_PROC(tmp);
			environment = make_frame(proc,CLOSURE_ENV(tmp), i, sp,
									 FRAME_PC(environment),
									 FRAME_PREVIOUS(environment));
			sp += i;
			return PROC_OPS(proc);
		} else if (type == PROCEDURE_TYPE) {
			environment = make_frame(tmp, null_object, i, sp,
									 FRAME_PC(environment),
									 FRAME_PREVIOUS(environment));
			sp += i;
			return PROC_OPS(tmp);
		} else if (type == CONTINUATION_TYPE) {
			gc_tmp5 = tmp;
			sp = reestablish_stack(i,sp,tmp);
			environment = CONTINUATION_FRAME(gc_tmp5);
			return CONTINUATION_PC(gc_tmp5);
		} else {
			goto not_a_proc_error;
		}
	} else if (OPCODE_P(tmp)) {
		opcode op = OPCODE_VALUE(tmp);
		if (op == OPCODE_CALLCC) {
			if (i != 1) goto argc_error;
			tmp = *sp++;
			pc = FRAME_PC(environment);
			environment = FRAME_PREVIOUS(environment);
			gc_tmp5 = tmp;
			*--sp = make_continuation(environment,pc,sp);
			tmp = gc_tmp5;
			goto tail_funcall;
		} else if (op == OPCODE_APPLY) {
			long j = i-1;
			if (j < 1) goto argc_error;
			gc_tmp4 = *sp++;
			i = explode_apply_args(sp,j);
			sp -= i;
			i += j;
			tmp = gc_tmp4;
			goto tail_funcall;
		} else if (op == OPCODE_NOT) {
		    if (FALSE_P(*sp))
			*sp = true_object;
		    else
			*sp = false_object;
		    goto tail_funcall;
		} else
			goto not_a_proc_error;
	} else
		goto not_a_proc_error;
	return 0;
  not_a_proc_error:
	error(tmp,"not a procedure");
  argc_error:
	error(MAKE_FIXNUM(i),"wrong number of arguments");
	return 0; /* never reached */
}

#ifdef INSTRUMENT_OPS
int opcount[100];
#endif

void run(long *initial_ops) {
    long i=0;
    long *pc = initial_ops;
    object tmp = void_object;

    PUSH_GC_PROTECT(tmp);
    
    while (1) {
#ifdef INSTRUMENT_OPS
	opcount[(opcode)*pc]++;
#endif
	switch ((opcode)*pc) {

	case OPCODE_LOCAL00:
	    *--sp = FRAME_ELEMENTS(environment)[0];
	    pc += 1;
	    break;
	
	case OPCODE_PRIMOP:
	    (*((primop_proc)(pc[1])))(pc[2]);
	    pc += 3;
	    break;

	case OPCODE_LOCAL01:
	    *--sp = FRAME_ELEMENTS(environment)[1];
	    pc += 1;
	    break;

	case OPCODE_LOCAL0:
	    *--sp = FRAME_ELEMENTS(environment)[pc[1]];
	    pc += 2;
	    break;

	case OPCODE_JUMPFALSE:
	    if (eq_p(*sp++,false_object)) {
		pc = (long *)((char *)pc + pc[1]);
		break;
	    } else {
		pc += 2;
		break;
	    }
	    
	case OPCODE_LOCAL10:
	    *--sp = FRAME_ELEMENTS(FRAME_ENV(environment))[0];
	    pc += 1;
	    break;

	case OPCODE_LOCAL1:
	    *--sp = FRAME_ELEMENTS(FRAME_ENV(environment))[pc[1]];
	    pc += 2;
	    break;

	case OPCODE_LOCAL2:
	    *--sp = FRAME_ELEMENTS(FRAME_ENV(FRAME_ENV(environment)))[pc[1]];
	    pc += 2;
	    break;

	case OPCODE_LOCAL3:
	    *--sp = FRAME_ELEMENTS(FRAME_ENV(FRAME_ENV(FRAME_ENV(environment))))[pc[1]];
	    pc += 2;
	    break;

	case OPCODE_JUMP:
	    pc = (long *)((char *)pc + pc[1]);
	    break;

	case OPCODE_LITERAL:
	    *--sp = pc[1];
	    pc += 2;
	    break;

	case OPCODE_TAILCALL:
#ifndef NO_INTERRUPT_CHECK
	    check_interrupts();
#endif
	    i = pc[1];
	    tmp = *sp++;
	    if (CLOSURE_P(tmp) && PROC_ARGC(CLOSURE_PROC(tmp)) == i) {
		environment = link_frame(CLOSURE_ENV(tmp), i, sp,
					 FRAME_PC(environment),
					 FRAME_PREVIOUS(environment));
		sp += i;
		pc = PROC_OPS(CLOSURE_PROC(tmp));
		break;
	    } else {
		pc = tailcall(tmp,i);
		break;
	    }
	    
	case OPCODE_CALL:
	    /* should check stack here */
	    i = pc[1];
	    pc += 2;
	    tmp = *sp++;
	    if (CLOSURE_P(tmp) && PROC_ARGC(CLOSURE_PROC(tmp)) == i) {
		environment = link_frame(CLOSURE_ENV(tmp), i, sp, pc, environment);
		sp += i;
		pc = PROC_OPS(CLOSURE_PROC(tmp));
		break;
	    } else {
		pc = funcall(tmp,i,pc);
		break;
	    }
	    
	case OPCODE_RETURN:
	    pc = FRAME_PC(environment);
	    environment = FRAME_PREVIOUS(environment);
	    break;
	    
	case OPCODE_CLOSURE:
	    *--sp = make_closure(constants[pc[1]],environment);
	    pc += 2;
	    break;

	case OPCODE_GLOBAL:
	    *--sp = SYMBOL_VALUE(constants[pc[1]]);
	    if (UNBOUND_P(*sp)) goto unbound_error;
	    pc += 2;
	    break;

	case OPCODE_CONSTANT:
	    *--sp = constants[pc[1]];
	    pc += 2;
	    break;

	case OPCODE_POP:
	    sp++;
	    pc++;
	    break;

	case OPCODE_LOCAL:
	    tmp = environment;
	    i = pc[1];
	    while (i) {
		tmp = FRAME_ENV(tmp);
		i--;
	    }
	    *--sp = FRAME_ELEMENTS(tmp)[pc[2]];
	    pc += 3;
	    break;

	case OPCODE_SET_LOCAL:
	    tmp = environment;
	    i = pc[1];
	    while (i) {
		tmp = FRAME_ENV(tmp);
		i--;
	    }
	    FRAME_ELEMENTS(tmp)[pc[2]] = *sp;
	    pc += 3;
	    break;

	case OPCODE_DEF_GLOBAL:
	    tmp = constants[pc[1]];
	    SYMBOL_VALUE(tmp) = *sp;
	    pc += 2;
	    break;

	case OPCODE_SET_GLOBAL:
	    tmp = constants[pc[1]];
	    if (UNBOUND_P(SYMBOL_VALUE(tmp))) goto unbound_error;
	    SYMBOL_VALUE(tmp) = *sp;
	    pc += 2;
	    break;

	case OPCODE_NULL_P:
	    if (*sp != null_object)
		*sp = false_object;
	    pc++;
	    break;

	case OPCODE_CAR:
#ifndef NO_TYPE_CHECK
	    TYPE_CHECK(PAIR_P((*sp)),1,"pair",*sp);
#endif
	    *sp = CAR(*sp);
	    pc++;
	    break;

	case OPCODE_CDR:
#ifndef NO_TYPE_CHECK
	    TYPE_CHECK(PAIR_P((*sp)),1,"pair",*sp);
#endif
	    *sp = CDR(*sp);
	    pc++;
	    break;

	case OPCODE_NOT:
	    if (FALSE_P(*sp))
		*sp = true_object;
	    else
		*sp = false_object;
	    pc++;
	    break;
	    
	case OPCODE_CALLCC:
	    gc_tmp5 = *sp++;
	    *--sp = make_continuation(environment,pc+1,sp);
	    pc = funcall(gc_tmp5,1,pc+1);
	    break;

	case OPCODE_APPLY:
	    gc_tmp4 = *sp++; /* the fun */
	    i = pc[1] - 1; /* argc */
	    i = explode_apply_args(sp,i);
	    sp -= i;
	    i += pc[1] - 1;
	    pc = funcall(gc_tmp4,i,pc+2);
	    break;
	}
    }

  unbound_error:
	error(constants[pc[1]],"unbound variable");
	POP_GC_PROTECT(1);
}

/* The global symbol table */

object symboltable;

object intern(char *name) {
	object sym = symboltable_get(symboltable,name);
	if (NULL_P(sym)) {
		sym = make_symbol(name);
		symboltable_add(symboltable,sym);
	}
	return sym;
}

object define(char *name, object value) {
	object sym = intern(name);
	SYMBOL_VALUE(sym) = value;
	return sym;
}

object global(char *name) {
	return SYMBOL_VALUE(intern(name));
}


/* runtime support */

void fatal_error(char *format, ...) {
	va_list args;
	va_start(args,format);
	print(curout_port(),"%s*** Fatal Error: ",newline_string(curout_port()));
	printv(curout_port(),format,args);
	print(curout_port(),newline_string(curout_port()));
	va_end(args);
	quit(-1);
}

static jmp_buf restart_jmp_buf;

void restart(object tag) {
	object r = global("system:*restart*");
	if (!UNBOUND_P(r) && CONTINUATION_P(r)) {
		restart_continuation = r;
		restart_value = tag;
		compiler_module_name = void_object;
		longjmp(restart_jmp_buf,1);
	} else
		quit(-1);
}

void interrupt(void) {
	object sym = intern("interrupt");
	object v = global("system:*error-info*");
	if (VECTOR_P(v)) {
		int i;
		VECTOR_ELEMENTS(v)[0] = sym;
		for (i=1; i<5; i++)
			VECTOR_ELEMENTS(v)[i] = false_object;
	}
	restart(sym);
}

object string_port_to_string(object p) {
	int max = PORT_POSITION(p);
	char *src, *dst;
	object result;
	PUSH_GC_PROTECT(p);
	result = make_string_of_size(max+1,0);
	src = STRING_VALUE(PORT_BUFFER(p));
	dst = STRING_VALUE(result);
	while (max--) *dst++ = *src++;
	*dst = '\0';
	POP_GC_PROTECT(1);
	return result;
}

object error_info_vector;

void error(object obj, char *format, ...) {
	object sym = intern("error");
	object v = global("system:*error-info*");
	object tmp = false_object;
	char *routine;
	va_list args;
	va_start(args,format);
	VECTOR_ELEMENTS(v)[0] = sym;
	if (UNBOUND_P(obj))
		VECTOR_ELEMENTS(v)[2] = void_object;
	else
		VECTOR_ELEMENTS(v)[2] = obj;
	PUSH_GC_PROTECT(sym);
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(v);
	tmp = open_output_string(make_string_of_size(256,0));
	printv(tmp,format,args);
	VECTOR_ELEMENTS(v)[1] =	 string_port_to_string(tmp);
	routine = "";
	if (SYMBOL_P(compiler_module_name)) {
		VECTOR_ELEMENTS(v)[3] = intern("compiling");
		VECTOR_ELEMENTS(v)[4] = compiler_module_name;
	} else if (routine && *routine) {
		VECTOR_ELEMENTS(v)[3] = intern("executing");
		VECTOR_ELEMENTS(v)[4] = intern(routine);
	} else {
		VECTOR_ELEMENTS(v)[3] = false_object;
		VECTOR_ELEMENTS(v)[4] = false_object;
	}
	POP_GC_PROTECT(3);
	va_end(args);
	restart(sym);
}

void fix_runtime_pointers(void) {
	/* called by gc in finalizatino pass */
	constants = VECTOR_ELEMENTS(constant_vector);
}


object execute(object the_code) {
	long *pc;
	PUSH_GC_PROTECT(the_code);
	gc_save_root_enumeration();
	if (setjmp(restart_jmp_buf)) {
		long i = CONTINUATION_STACKSIZE(restart_continuation);
		gc_restore_root_enumeration();
		environment = CONTINUATION_FRAME(restart_continuation);
		pc = CONTINUATION_PC(restart_continuation);
		sp = stack_top-i;
		memcpy(sp,CONTINUATION_STACK(restart_continuation),i);
		*--sp = restart_value;
	} else {
		pc = PROC_OPS(the_code);
		sp = stack_top;
		environment = make_frame(the_code,null_object, 0,
								 NULL,NULL,null_object);
	}
	run(pc);
	POP_GC_PROTECT(1);
	return void_object;
}

/* Primitives */

static void primop_weak_binding(long argc) {
   *sp = weak_binding(*sp);
}

static void primop_weak_binding_p(long argc) {
	if (!WEAK_P(*sp)) *sp = false_object;
}

static void primop_weak_pred(long argc) {
	object w = *sp;
	TYPE_CHECK(WEAK_P(w),1,"weak-binding",w);
	if (!WEAK_BOUND(w))
		*sp = false_object;
}

static void primop_weak_value(long argc) {
	object w = *sp;
	TYPE_CHECK(WEAK_P(w),1,"weak-binding",w);
	*sp = WEAK_VALUE(w);
}

static void primop_exit(long argc) {
	long exit_code = 0;
	if (argc)
		exit_code = (long)the_long(1,*sp++);
	quit(exit_code);
}

static void primop_restart(long argc) {
	object tag = argc? *sp++ : intern("error");
	restart(tag);
}

static void primop_gc(long argc) {
	garbage_collect(0);
	*--sp = void_object;
}

static void primop_enable_ints(long argc) {
	enable_interrupts(1);
	*--sp = void_object;
}

static void primop_disable_ints(long argc) {
	enable_interrupts(0);
	*--sp = void_object;
}

static void primop_unbound_p(long argc) {
	*sp = UNBOUND_P(*sp)? true_object : false_object;
}

static void primop_set_global(long argc) {
	object tmp = *sp++;
	TYPE_CHECK(SYMBOL_P(tmp),1,"symbol",tmp);
	SYMBOL_VALUE(tmp) = *sp;
}

static object primop_globals_tmp;

static void primop_globals_proc(object sym) {
	if (!UNBOUND_P(SYMBOL_VALUE(sym)))
		primop_globals_tmp = cons(sym,primop_globals_tmp);
}

static void primop_globals(long argc) {
	primop_globals_tmp = null_object;
	PUSH_GC_PROTECT(primop_globals_tmp);
	symboltable_map(symboltable,&primop_globals_proc);
	POP_GC_PROTECT(1);
	*--sp = primop_globals_tmp;
}

static void primop_type_of(long argc) {
	object o = *sp;
	if (POINTER_P(o)) {
		switch (POINTER_TYPE(o)) {
		  case PAIR_TYPE:
			*sp = intern("pair");
			break;
		  case SYMBOL_TYPE:
			*sp = intern("symbol");
			break;
		  case FLONUM_TYPE:
			*sp = intern("real");
			break;
		  case BIGNUM_TYPE:
			*sp = intern("integer");
			break;
		  case STRING_TYPE:
			*sp = intern("string");
			break;
		  case VECTOR_TYPE:
			*sp = VECTOR_TAG(o);
			if (FALSE_P(*sp))
				*sp = intern("vector");
			break;
		  case SIGNAL_TYPE:
			*sp =intern("signal");
			break;
		  case PORT_TYPE:
			*sp =intern("port");
			break;
		  case PROCEDURE_TYPE:
			*sp = intern("procedure");
			break;
		  case CLOSURE_TYPE:
			*sp = intern("closure");
			break;
		  case FRAME_TYPE:
			*sp = intern("call-frame");
			break;
		  case CONTINUATION_TYPE:
			*sp = intern("continuation");
			break;
		  case BUFFER_TYPE:
			*sp = intern("buffer");
			break;
		  case SYMBOLTABLE_TYPE:
			*sp = intern("symboltable");
			break;
		  case WEAK_TYPE:
			*sp = intern("weak-binding");
			break;
		  case TERMINAL_TYPE:
			*sp = intern("terminal");
			break;
		  case FORWARDED_TYPE:
			*sp = intern("forwarded");
			break;
		  default:
			*sp = false_object;
			break;
		}
	} else if (IMMEDIATE_P(o)) {
		switch (IMMEDIATE_TYPE(o)) {
		  case BOOLEAN_TYPE:
			*sp = intern("boolean");
			break;
		  case CHARACTER_TYPE:
			*sp = intern("character");
			break;
		  case OPCODE_TYPE:
			*sp = intern("opcode");
			break;
		  case PRIMOP_TYPE:
			*sp = intern("primitive");
			break;
		  case SPECIAL_TYPE:
			*sp = intern("special");
			break;
		  default:
			*sp = false_object;
			break;
		}
	} else if (FIXNUM_P(o))
		*sp = intern("integer");
	else
		*sp = false_object;
}

void add_feature(char *name) {
	object sym = intern(name);
	object features = global("system:*features*");
	if (FALSE_P(memq(sym,features)))
		define("system:*features*",cons(sym,features));
}

static void primop_map_args(long argc) {
	long i, j, count = length(*sp);
	object result = null_object;
	object arglist = null_object;
	object tail = null_object;
	object tmp = null_object;
	object resulttail = null_object;
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(arglist);
	PUSH_GC_PROTECT(tail);
	PUSH_GC_PROTECT(result);
	PUSH_GC_PROTECT(resulttail);
	if (count < 0)
		TYPE_CHECK(0,1,"list",sp[0]);
	for (i=0; i<count; i++) {
		tmp = *sp;
		*sp = CDR(tmp);
		tail = arglist = list1(CAR(tmp));
		for (j=1;j<argc;j++) {
			tmp = sp[j];
			sp[j] = CDR(tmp);
			tmp = list1(CAR(tmp));
			CDR(tail) = tmp;
			tail = tmp;
		}
		tmp = list1(arglist);
		if (NULL_P(result))
			result = resulttail = tmp;
		else {
			CDR(resulttail) = tmp;
			resulttail = tmp;
		}
	}
	POP_GC_PROTECT(5);
	sp += argc - 1;
	*sp = result;
}

static void primop_unbound(long argc) {
	*--sp = unbound_object;
}

void init_runtime(void) {
	environment = null_object;
	PUSH_GC_PROTECT(environment);
	constant_vector = make_vector(100,unbound_object);
	PUSH_GC_PROTECT(constant_vector);
	constants = VECTOR_ELEMENTS(constant_vector);
	symboltable = make_symboltable(1000);
	PUSH_GC_PROTECT(symboltable);
	restart_continuation = unbound_object;
	PUSH_GC_PROTECT(restart_continuation);
	restart_value = unbound_object;
	PUSH_GC_PROTECT(restart_value);
	define("system:*error-info*",make_vector(5,false_object));
	define("system:*void*", void_object);
	define_primop("system:unbound-object",primop_unbound,0,0);
	define("call-with-current-continuation",MAKE_OPCODE(OPCODE_CALLCC));
	define("apply", MAKE_OPCODE(OPCODE_APPLY));
	define_primop("system:weak-binding",primop_weak_binding,1,1);
	define_primop("system:weak-binding?",primop_weak_binding_p,1,1);
	define_primop("system:weak-predicate",primop_weak_pred,1,1);
	define_primop("system:weak-value",primop_weak_value,1,1);
	define_primop("system:exit",primop_exit,0,1);
	define_primop("system:restart",primop_restart,0,1);
	define_primop("system:gc",primop_gc,0,0);
	define_primop("system:enable-interrupts",primop_enable_ints,0,0);
	define_primop("system:disable-interrupts",primop_disable_ints,0,0);
	define_primop("system:unbound?",primop_unbound_p,1,1);
	define_primop("system:set-global!",primop_set_global,2,2);
	define_primop("system:globals",primop_globals,0,0);
	define_primop("system:crack-map-args",primop_map_args,1,MAX_ARGC);
	define_primop("system:type-of",primop_type_of,1,1);
	define("system:*features*",null_object);
	add_feature("system");
}



