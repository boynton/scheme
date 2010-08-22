/* LeeScheme/compiler.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include <stdio.h>

object quote_symbol, begin_symbol, if_symbol;
object set_symbol, lambda_symbol, define_symbol;

object compiler_module_name;
static object compiler_ops;

int can_bind_global_procs = 0;
int disable_type_checks = 0;


static void encode(long data) {
	long bufsize = BUFFER_LENGTH(compiler_ops);
	long *p;
	if (bufsize >= (long)BUFFER_CAPACITY(compiler_ops))
		compiler_ops = grow_buffer(compiler_ops, bufsize * 2);
	p = (long *)((char *)BUFFER_DATA(compiler_ops) + bufsize);
	*p = data;
	BUFFER_LENGTH(compiler_ops) = bufsize + sizeof(long);
}

static int calculate_location(object sym, object env, long *ip, long *jp) {
	long i = 0;
	while (!NULL_P(env)) {
		long j = 0;
		object e = CAR(env);
		while (PAIR_P(e)) {
			if (eq_p(CAR(e),sym)) {
				*ip = i;
				*jp = j;
				return 1;
			}
			j++;
			e = CDR(e);
		}
		if (SYMBOL_P(e) && eq_p(e,sym)) {
			*ip = i;
			*jp = j;
			return 1;
		}
		i++;
		env = CDR(env);
	}
	return 0;
}

static long calculate_argc(object arglist) {
	long i = 0;
	if (NULL_P(arglist))
		return 0;
	else while (PAIR_P(arglist)) {
		i++;
		arglist = CDR(arglist);
	}
	if (NULL_P(arglist))
		return i;
	return -i-1;
}

static long constant_index(object c) {
	long i;
	object tmp, *cp = constants, *cp2;
	long count = VECTOR_LENGTH(constant_vector);
	for (i=0; i<count; i++) {
		tmp = *cp++;
		if (eq_p(tmp,c)) return i;
		if (UNBOUND_P(tmp)) goto return_result;
	}
	PUSH_GC_PROTECT(c);
	tmp = make_vector(count*2,unbound_object);
	cp2 = constants;
	constants = cp = VECTOR_ELEMENTS(tmp);
	for (i=0; i<count; i++)
		*cp++ = *cp2++;
	constant_vector = tmp;
	POP_GC_PROTECT(1);
  return_result:
	constants[i] = c;
	return i;
}

void compile_expr(object expr, object env, int is_tail, int ignore_result);

static void encode_op(long op) {
	encode(op);
}

static void compile_object(object the_object) {
	if (IMMEDIATE_P(the_object) || FIXNUM_P(the_object)) {
		encode_op(OPCODE_LITERAL);
		encode(the_object);
	} else {
		long i = constant_index(the_object);
		encode_op(OPCODE_CONSTANT);
		encode(i);
	}
}

static void compile_local_ref(long i, long j) {
	if (i <= 3) {
		switch (i) {
		  case 0: 
			switch (j) {
			  case 0:
				encode_op(OPCODE_LOCAL00); return;
			  case 1:
				encode_op(OPCODE_LOCAL01); return;
			  default:
				encode_op(OPCODE_LOCAL0); break;
			}
			break;
		  case 1:
			if (j == 0) {
				encode_op(OPCODE_LOCAL10);
				return;
			}
			encode_op(OPCODE_LOCAL1); break;
		  case 2: encode_op(OPCODE_LOCAL2); break;
		  case 3: encode_op(OPCODE_LOCAL3); break;
		}
		encode(j);
	} else {
		encode_op(OPCODE_LOCAL);
		encode(i);
		encode(j);
	}
}

static void compile_local_set(long i, long j) {
	encode_op(OPCODE_SET_LOCAL);
	encode(i);
	encode(j);
}

static void compile_sequence(object exprs, object env, int is_tail, int ignore_result) {
	if (PAIR_P(exprs)) {
		PUSH_GC_PROTECT(exprs);
		PUSH_GC_PROTECT(env);
		while (!NULL_P(CDR(exprs))) {
			compile_expr(CAR(exprs),env,0,1);
			exprs = CDR(exprs);
		}
		compile_expr(CAR(exprs),env,is_tail,ignore_result);
		POP_GC_PROTECT(2);
	}
}

static void compile_lambda(object arglist, object body, object env,
						   int is_tail, int ignore_result) {
	long new_argc = calculate_argc(arglist);
	object old_ops = compiler_ops;
	object new_proc = null_object;
	object new_env = null_object;
	object tmp = arglist;
	while (PAIR_P(tmp)) {
		if (!SYMBOL_P(CAR(tmp)))
			error(CAR(tmp),"formal argument must be a symbol");
		tmp = CDR(tmp);
	}
	if (!NULL_P(tmp) && !SYMBOL_P(tmp))
		error(CAR(tmp),"formal argument list ill constructed");
	PUSH_GC_PROTECT(arglist);
	PUSH_GC_PROTECT(body);
	PUSH_GC_PROTECT(env);
	PUSH_GC_PROTECT(new_proc);
	PUSH_GC_PROTECT(old_ops);
	PUSH_GC_PROTECT(new_env);
	compiler_ops = make_buffer(100);
	new_env = cons(arglist,env);
	compile_sequence(body,new_env,1,0);
	new_proc = make_procedure(compiler_ops, compiler_module_name, new_argc);
	compiler_ops = old_ops;
	if (!ignore_result) {
		long i = constant_index(new_proc);
		encode_op(OPCODE_CLOSURE);
		encode(i);
		if (is_tail) encode_op(OPCODE_RETURN);
	}
	POP_GC_PROTECT(6);
}

static void compile_args(object args, object env) {
	if (PAIR_P(args)) {
		PUSH_GC_PROTECT(args);
		PUSH_GC_PROTECT(env);
		compile_args(CDR(args),env);
		compile_expr(CAR(args),env,0,0);
		POP_GC_PROTECT(2);
	}
}

static void compile_funcall(object fun, object args, object env,
							int is_tail, int ignore_result) {
	long argc = length(args);
	long i, j;
	if (argc < 0) {
		object tmp = cons(fun,args);
		error(tmp,"Syntax error for function call");
	}
	if (can_bind_global_procs && SYMBOL_P(fun) &&
		!calculate_location(fun,env,&i,&j)) {
		object tmp = SYMBOL_VALUE(fun);
		if (PRIMOP_P(tmp)) {
			int argc_min = PRIMOP_ARGC_MIN(tmp);
			int argc_max = PRIMOP_ARGC_MAX(tmp);
			if (argc >= argc_min && argc <= argc_max) {
				long inline_opcode = PRIMOP_INLINE_OP(tmp);
				compile_args(args,env);
				/* if (disable_type_checks && inline_opcode >= 0) { */
				if (inline_opcode >= 0) {
					encode_op(inline_opcode);
				} else {
					encode_op(OPCODE_PRIMOP);
					encode((long)PRIMOP_PROC(tmp));
					encode(argc);
				}
			} else {
				object tmp2 = MAKE_FIXNUM(argc);
				error(tmp2,"Wrong number of arguments to '%s'", PRIMOP_NAME(tmp));
			}
			if (ignore_result)
				encode_op(OPCODE_POP);
			else if (is_tail)
				encode_op(OPCODE_RETURN);
			return;
		}
	}
	PUSH_GC_PROTECT(fun);
	PUSH_GC_PROTECT(env);
	compile_args(args,env);
	compile_expr(fun,env,0,0);
	if (is_tail) {
		encode_op(OPCODE_TAILCALL);
		encode(argc);
	} else {
		encode_op(OPCODE_CALL);
		encode(argc);
		if (ignore_result)
			encode_op(OPCODE_POP);
	}
	POP_GC_PROTECT(2);
}

static void compile_ifelse(object pred, object consequent, object antecedent,
						   object env, int is_tail, int ignore_result) {
	long loc1, loc2=0, *ploc;
	long i;
	if (PAIR_P(antecedent))
		antecedent = CAR(antecedent);
	else
		antecedent = void_object;
	PUSH_GC_PROTECT(pred);
	PUSH_GC_PROTECT(consequent);
	PUSH_GC_PROTECT(antecedent);
	PUSH_GC_PROTECT(env);
	compile_expr(pred,env,0,0);
	encode_op(OPCODE_JUMPFALSE);
	loc1 = BUFFER_LENGTH(compiler_ops);
	encode(0);
	compile_expr(consequent,env,is_tail,ignore_result);
	if (!is_tail) {
		encode_op(OPCODE_JUMP);
		loc2 = BUFFER_LENGTH(compiler_ops);
		encode(0);
	}
	ploc = (long *)((char *)BUFFER_DATA(compiler_ops) + loc1);
	i = ((BUFFER_LENGTH(compiler_ops) - loc1 - 1) / sizeof(long)) + 2;
	*ploc = i * sizeof(long);
	compile_expr(antecedent,env,is_tail,ignore_result);
	if (!is_tail) {
		ploc = (long *)((char *)BUFFER_DATA(compiler_ops) + loc2);
		i = ((BUFFER_LENGTH(compiler_ops) - loc2 - 1) / sizeof(long)) + 2;
		*ploc = i * sizeof(long);
	}
	POP_GC_PROTECT(4);
}

void compile_expr(object expr, object env, int is_tail, int ignore_result) {
	if (SYMBOL_P(expr)) {
		if (!ignore_result) {
			long i, j;
			if (calculate_location(expr,env,&i,&j)) {
				compile_local_ref(i,j);
			} else {
				long i = constant_index(expr);
				encode_op(OPCODE_GLOBAL);
				encode(i);
			}
			if (is_tail) encode_op(OPCODE_RETURN);
		}
	} else if (PAIR_P(expr)) {
		object fun = CAR(expr);
		if (eq_p(fun,quote_symbol)) {
			if (length(expr) == 2) {
				if (!ignore_result) {
					compile_object(CADR(expr));
					if (is_tail) encode_op(OPCODE_RETURN);
				}
			} else
				error(expr,"Syntax error");
		} else if (eq_p(fun,begin_symbol)) {
			compile_sequence(CDR(expr),env,is_tail,ignore_result);
		} else if (eq_p(fun,if_symbol)) {
			if (length(expr) == 3 || length(expr) == 4)
				compile_ifelse(CADR(expr),CADDR(expr),CDDDR(expr),env,
							   is_tail, ignore_result);
			else
				error(expr,"Syntax error");
		} else if (eq_p(fun,define_symbol)) {
			if (length(expr) == 3) {
				long i = constant_index(CADR(expr));
				object old_module_name = compiler_module_name;
				compiler_module_name = CADR(expr);
				PUSH_GC_PROTECT(old_module_name);
				if (TRUE_P(global("system:*show-compiled-definitions*")))
					print(curout_port(),"  %s%s",
						  SYMBOL_NAME(compiler_module_name),
						  newline_string(curout_port()));
				compile_expr(CADDR(expr),env,0,0);
				POP_GC_PROTECT(1);
				compiler_module_name = old_module_name;
				encode_op(OPCODE_DEF_GLOBAL);
				encode(i);
				if (ignore_result)
					encode_op(OPCODE_POP);
				else if (is_tail)
					encode_op(OPCODE_RETURN);
			} else
				error(expr,"Syntax error");
		} else if (eq_p(fun,set_symbol)) {
			if (length(expr) == 3) {
				long i, j;
				PUSH_GC_PROTECT(env);
				PUSH_GC_PROTECT(expr);
				compile_expr(CADDR(expr),env,0,0);
				POP_GC_PROTECT(2);
				if (calculate_location(CADR(expr),env,&i,&j)) {
					compile_local_set(i,j);
				} else {
					i = constant_index(CADR(expr));
					encode_op(OPCODE_SET_GLOBAL);
					encode(i);
				}
				if (ignore_result)
					encode_op(OPCODE_POP);
				else if (is_tail)
					encode_op(OPCODE_RETURN);
			} else
				error(expr,"Syntax error");
		} else if (eq_p(fun,lambda_symbol)) {
			if (length(expr) >= 3) {
				object arglist = CADR(expr);
				object body = CDDR(expr);
				compile_lambda(arglist,body,env,is_tail,ignore_result);
			} else
				error(expr,"Syntax error");
		} else {  /* a function call */
			compile_funcall(fun,CDR(expr),env,is_tail,ignore_result);
		}
	} else { /*	 a literal */
		if (!ignore_result) {
			compile_object(expr);
			if (is_tail) encode_op(OPCODE_RETURN);
		}
	}
}

object compile(object expr) {
	object tmp = null_object;
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(tmp);
	compiler_module_name = void_object;
	compiler_ops = make_buffer(100);
	compile_expr(expr,null_object,0,0);
	encode_op(OPCODE_RETURN);
	tmp = make_procedure(compiler_ops,void_object,0);
	compiler_module_name = void_object;
	compiler_ops = void_object;
	POP_GC_PROTECT(2);
	return tmp;
}

static void primop_compile(long argc) {
	*sp = compile(*sp);
}

static void primop_declare(long argc) {
	long i;
	for (i=0; i<argc; i++) {
		object val = true_object,flag = sp[i];
		if (!SYMBOL_P(flag)) {
			if (PAIR_P(flag)) {
				if (length(flag) >= 2)
					val = CADR(flag);
				flag = CAR(flag);
			} else
				error(flag,"Cannot declare this");
		}
		if (eq_p(flag,intern("bind-global-procs")))
			can_bind_global_procs = FALSE_P(val)? 0 : 1;
		else if (eq_p(flag,intern("disable-type-checks")))
			disable_type_checks = FALSE_P(val)? 0 : 1;
		else if (eq_p(flag,intern("default"))) {
			/* reset all options */
			can_bind_global_procs = 0;
			disable_type_checks = 0;
		} else
			error(flag,"unknown option");
	}
	sp += argc;
	*--sp = void_object;
}

void init_compiler(void) {
	define("system:*show-compiled-definitions*",false_object);
	quote_symbol = intern("quote");
	PUSH_GC_PROTECT(quote_symbol);
	begin_symbol = intern("begin");
	PUSH_GC_PROTECT(begin_symbol);
	if_symbol = intern("if");
	PUSH_GC_PROTECT(if_symbol);
	set_symbol = intern("set!");
	PUSH_GC_PROTECT(set_symbol);
	lambda_symbol = intern("lambda");
	PUSH_GC_PROTECT(lambda_symbol);
	define_symbol = intern("define");
	PUSH_GC_PROTECT(define_symbol);
	compiler_ops = void_object;
	PUSH_GC_PROTECT(compiler_ops);
	compiler_module_name = void_object;
	PUSH_GC_PROTECT(compiler_module_name);
	define_primop("system:compile",primop_compile,1,1);
	define_primop("system:declare!",primop_declare,1,MAX_ARGC);
}

