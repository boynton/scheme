/* LeeScheme/macros.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

/* Primitive macros for bootstrap purposes. */

#include "scheme.h"

typedef object (*transformer_proc)(object expr);

static object macro_alist;

typedef struct {
	char *name;
	transformer_proc transformer;
} macro_descriptor;

#define MAX_MACRO (100)
static macro_descriptor macros[MAX_MACRO];

static long macro_count = 0;

void define_macro(char *name, transformer_proc expander) {
	long i;
	object sym = intern(name);
	object tmp = assq(sym,macro_alist);
	if (PAIR_P(tmp)) {
		i = FIXNUM_VALUE(CDR(tmp));
		macros[i].transformer = expander;
	} else {
		if (macro_count >= MAX_MACRO)
			fatal_error("macro table overflow: %d",macro_count);
		i = macro_count++;
		macros[i].name = name;
		macros[i].transformer = expander;
		PUSH_GC_PROTECT(sym);
		sym = cons(sym,MAKE_FIXNUM(i));
		macro_alist = cons(sym,macro_alist);
		POP_GC_PROTECT(1);
	}
}

transformer_proc macro_transformer(object sym) {
	object tmp = assq(sym,macro_alist);
	if (PAIR_P(tmp)) {
		long i = FIXNUM_VALUE(CDR(tmp));
		return macros[i].transformer;
	}
	return (transformer_proc)0;
}

object macro_expand(object source) {
	object result;
	if (PAIR_P(source)) {
		object head = null_object, tail = null_object, tmp = CAR(source);
		PUSH_GC_PROTECT(source);
		PUSH_GC_PROTECT(head);
		PUSH_GC_PROTECT(tail);
		PUSH_GC_PROTECT(tmp);
		if (SYMBOL_P(tmp)) {
			transformer_proc expander = macro_transformer(tmp);
			if (expander) {
				result = (*expander)(source);
				POP_GC_PROTECT(4);
				goto done;
			} else {
				head = cons(tmp,null_object);
			}
		} else {
			tmp = macro_expand(tmp);
			head = cons(tmp,null_object);
		}
		tail = head;
		source = CDR(source);
		while (PAIR_P(source)) {
			tmp = macro_expand(CAR(source));
			tmp = cons(tmp,null_object);
			CDR(tail) = tmp;
			tail = tmp;
			source = CDR(source);
		}
		if (!NULL_P(source))
			CDR(tail) = source;
		POP_GC_PROTECT(4);
		result = head;
	} else
		result = source;
  done:
	return result;
}


/* The standard r4rs macros */

object system_loop_symbol, system_tmp_symbol;
object let_symbol, letrec_symbol, letstar_symbol;
object case_symbol, or_symbol, and_symbol, do_symbol;
object cond_symbol, else_symbol, memv_symbol, eqgt_symbol;
object quasiquote_symbol, unquote_symbol, unquote_splicing_symbol;
object delay_symbol, make_promise_symbol;
object cons_symbol, append_symbol, list_to_vector_symbol;

static object expand_sequence(object body) {
	object tmp = null_object;
	object head = null_object;
	object tail = head;
	object p = body;
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(head);
	PUSH_GC_PROTECT(tail);
	PUSH_GC_PROTECT(p);
	while (PAIR_P(p)) {
		tmp = macro_expand(CAR(p));
		tmp = cons(tmp,null_object);
		if (NULL_P(head))
			head = tmp;
		else
			CDR(tail) = tmp;
		tail = tmp;
		p = CDR(p);
	}
	POP_GC_PROTECT(4);
	return head;
}

static object expand_lambda(object expr) {
	object tmp;
	long i = length(expr);
	if (i < 3)
		error(expr,"bad syntax for lambda");
	tmp = CDDR(expr);
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(tmp);
	if (i > 3) {
		if (PAIR_P(CAR(tmp)) && eq_p(CAAR(tmp),define_symbol)) {
			object bindings = null_object;
			PUSH_GC_PROTECT(bindings);
			do {
				object def = CDR(macro_expand(CAR(tmp)));
				bindings = cons(def, bindings);
				tmp = CDR(tmp);
			} while (PAIR_P(CAR(tmp)) && eq_p(CAAR(tmp),define_symbol));
			tmp = cons2(letrec_symbol,bindings,tmp);
			tmp = macro_expand(tmp);
			tmp = list3(CAR(expr),CADR(expr),tmp);
			POP_GC_PROTECT(3);
			return tmp;
		}
	}
	tmp = expand_sequence(tmp);
	tmp = cons2(CAR(expr),CADR(expr),tmp);
	POP_GC_PROTECT(2);
	return tmp;
}

static object expand_define(object expr) {
	long i = length(expr);
	object tmp = null_object;
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(tmp);
	if (i < 2) error(expr,"bad syntax for define");
	if (PAIR_P(CADR(expr))) {
		if (i < 3)
			error(expr,"bad syntax for define");
		tmp = cons2(lambda_symbol, CDADR(expr), CDDR(expr));
		tmp = macro_expand(tmp);
		POP_GC_PROTECT(2);
		return list3(CAR(expr), CAADR(expr), tmp);
	} else if (!SYMBOL_P(CADR(expr)))
		error(CADR(expr),"argument 1 of define must be a symbol");
	else if (i != 3)
		error(expr,"bad syntax for define");
	tmp = macro_expand(CADDR(expr));
	tmp = list3(CAR(expr),CADR(expr),tmp);
	POP_GC_PROTECT(2);
	return tmp;
}

static object expand_set(object expr) {
	long i = length(expr);
	object tmp = null_object;
	if (i != 3 || !SYMBOL_P(CADR(expr)))
		error(expr,"bad syntax for set!");
	PUSH_GC_PROTECT(expr);
	tmp = macro_expand(CADDR(expr));
	tmp = list3(CAR(expr),CADR(expr),tmp);
	POP_GC_PROTECT(1);
	return tmp;
}

static object expand_quote(object expr) {
	if (length(expr) != 2)
		error(expr,"bad syntax for quote");
	return expr;
}

static object expand_if(object expr) {
	long i = length(expr);
	object tmp = null_object;
	if (i < 3 || i > 4)
		error(expr,"bad syntax for if");
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(expr);
	if (i == 4) {
		tmp = expand_sequence(CDR(expr));
		tmp = cons(CAR(expr),tmp);
	} else {
		tmp = list3(CADR(expr),CADDR(expr),void_object);
		tmp = expand_sequence(tmp);
		tmp = cons(CAR(expr),tmp);
	}
	POP_GC_PROTECT(2);
	return tmp;
}

static object expand_begin(object expr) {
	long i = length(expr);
	if (i < 2)
		error(expr,"bad syntax for begin");
	if (i == 2)
		return macro_expand(CADR(expr));
	else {
		object tmp = null_object;
		PUSH_GC_PROTECT(tmp);
		PUSH_GC_PROTECT(expr);
		tmp = expand_sequence(CDR(expr));
		tmp = cons(CAR(expr),tmp);
		POP_GC_PROTECT(2);
		return tmp;
	}
}

static int crack_letrec_bindings(object bindings, object *names,
								 object *set_exprs, object tail) {
	object tmp = null_object;
	object result = null_object;
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(bindings);
	PUSH_GC_PROTECT(tail);
	while (!NULL_P(bindings)) {
		if (!PAIR_P(bindings)) goto return_zero;
		tmp = CAR(bindings);
		if (!PAIR_P(tmp)) goto return_zero;
		if (!SYMBOL_P(CAR(tmp))) goto return_zero;
		if (!PAIR_P(CDR(tmp))) goto return_zero;
		result = cons(CAR(tmp), result);
		tail = cons(cons(set_symbol,CAR(bindings)), tail);
		bindings = CDR(bindings);
	}
	result = nreverse(result);
	*names = result;
	*set_exprs = tail; /* set_exprs */
	POP_GC_PROTECT(3);
	return 1;
  return_zero:
	POP_GC_PROTECT(3);
	return 0;
}

static object expand_letrec(object expr) {
	long i = length(expr);
	object names = null_object, values = null_object, tmp = null_object;
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(names);
	PUSH_GC_PROTECT(values);
	PUSH_GC_PROTECT(tmp);
	tmp = cons2(lambda_symbol,null_object,CDDR(expr));
	tmp = macro_expand(tmp);
	if (i < 3 || !crack_letrec_bindings(CADR(expr),&names,&values,CDDR(tmp)))
		error(expr,"bad syntax for letrec");
	tmp = cons2(lambda_symbol,names,values);
	tmp = macro_expand(tmp);
	names = make_list(length(names),void_object);
	tmp = cons(tmp,names);
	POP_GC_PROTECT(4);
	return tmp;
}

static int crack_let_bindings(object bindings, object *names, object *values) {
	object tmp = null_object;
	object name_result = null_object;
	object value_result = null_object;
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(name_result);
	PUSH_GC_PROTECT(value_result);
	PUSH_GC_PROTECT(bindings);
	while (!NULL_P(bindings)) {
		if (!PAIR_P(bindings)) goto fail;
		tmp = CAR(bindings);
		if (!PAIR_P(tmp)) goto fail;
		if (!SYMBOL_P(CAR(tmp))) goto fail;
		if (!PAIR_P(CDR(tmp))) goto fail;
		name_result = cons(CAR(tmp), name_result);
		value_result = cons(CADAR(bindings), value_result);
		bindings = CDR(bindings);
	}
	name_result = nreverse(name_result);
	value_result = expand_sequence(value_result);
	*values = nreverse(value_result);
	*names = name_result;
	POP_GC_PROTECT(4);
	return 1;
  fail:
	POP_GC_PROTECT(4);
	return 0;
}

static object expand_named_let(object expr) {
	object name = CADR(expr);
	object names = null_object, values = null_object, tmp = null_object;
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(name);
	PUSH_GC_PROTECT(names);
	PUSH_GC_PROTECT(values);
	PUSH_GC_PROTECT(tmp);
	if (!crack_let_bindings(CADDR(expr),&names,&values))
	error(expr,"bad syntax for let");
	tmp = cons2(lambda_symbol,names,CDDDR(expr));
	tmp = list2(name,tmp);
	tmp = list1(tmp);
	name = cons(name,values);
	tmp = list3(letrec_symbol,tmp,name);
	tmp = macro_expand(tmp);
	POP_GC_PROTECT(5);
	return tmp;
}

static object expand_let(object expr) {
	long i = length(expr);
	object names = null_object, values = null_object, tmp = null_object;
	if (i < 3)
		error(expr,"bad syntax for let");
	if (SYMBOL_P(CADR(expr)))
		return expand_named_let(expr);
	PUSH_GC_PROTECT(names);
	PUSH_GC_PROTECT(values);
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(expr);
	if (!crack_let_bindings(CADR(expr),&names,&values))
		error(expr,"bad syntax for let");
	tmp = cons2(lambda_symbol,names,CDDR(expr));
	tmp = macro_expand(tmp);
	POP_GC_PROTECT(4);
	return cons(tmp,values);
}

static object expand_let_star(object expr) {
	long i = length(expr);
	object binding = null_object, bindings = null_object, tmp = null_object;
	if (i < 3)
		error(expr,"bad syntax for let*");
	bindings = CADR(expr);
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(binding);
	PUSH_GC_PROTECT(bindings);
	while (PAIR_P(bindings)) {
		binding = CAR(bindings);
		if (length(binding) != 2 || !SYMBOL_P(CAR(binding)))
			error(expr,"bad syntax for let*");
		tmp = cons(binding,tmp);
		bindings = CDR(bindings);
	}
	if (!NULL_P(bindings))
		error(expr,"bad syntax for let*");
	bindings = tmp;
	if (PAIR_P(bindings)) {
		binding = CAR(bindings);
		tmp = list1(CAR(binding));
		tmp = cons2(lambda_symbol,tmp,CDDR(expr));
		tmp = cons(tmp,CDR(binding));
		bindings = CDR(bindings);
	} else {
		tmp = cons2(lambda_symbol,null_object, CDDR(expr));
		tmp = list1(tmp);
	}
	while (PAIR_P(bindings)) {
		binding = CAR(bindings);
		gc_tmp5 = list1(CAR(binding));
		tmp = list3(lambda_symbol,gc_tmp5,tmp);
		tmp = cons(tmp,CDR(binding));
		bindings = CDR(bindings);
	}
	tmp = macro_expand(tmp);
	POP_GC_PROTECT(4);
	return tmp;
}

static int crack_do_bindings(object bindings, object *names,
							 object *inits, object *incrs) {
	object tmp = null_object;
	object n = null_object, ini = null_object, inc = null_object;
	PUSH_GC_PROTECT(n);
	PUSH_GC_PROTECT(ini);
	PUSH_GC_PROTECT(inc);
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(bindings);
	while (!NULL_P(bindings)) {
		if (!PAIR_P(bindings)) return 0;
		tmp = CAR(bindings);
		if (!PAIR_P(tmp)) return 0;
		if (!SYMBOL_P(CAR(tmp))) return 0;
		if (!PAIR_P(CDR(tmp))) return 0;
		n = cons(CAR(tmp),n);
		ini = cons(CADR(tmp), ini);
		if (PAIR_P(CDDR(tmp)))
			inc = cons(CADDR(tmp), inc);
		else
			inc = cons(CAR(tmp), inc);
		bindings = CDR(bindings);
	}
	ini = expand_sequence(ini);
	inc = expand_sequence(inc);
	*inits = ini;
	*incrs = inc;
	*names = n;
	POP_GC_PROTECT(5);
	return 1;
}

static object expand_do(object expr) {
	object loop_sym = system_loop_symbol;
	object names = null_object, inits = null_object, incrs = null_object;
	object tmp2 = null_object;
	object tmp=null_object, exit_pred=null_object, exit_exprs=null_object;
	long i = length(expr);
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(tmp2);
	PUSH_GC_PROTECT(names);
	PUSH_GC_PROTECT(inits);
	PUSH_GC_PROTECT(incrs);
	PUSH_GC_PROTECT(loop_sym);
	PUSH_GC_PROTECT(exit_pred);
	PUSH_GC_PROTECT(exit_exprs);
	if (i < 3 || !crack_do_bindings(CADR(expr),&names,&inits,&incrs))
		error(expr,"bad syntax for do");
	tmp = expand_sequence(CADDR(expr));
	exit_pred = CAR(tmp);
	if (NULL_P(CDR(tmp)))
		exit_exprs = void_object;
	else if (PAIR_P(CDDR(tmp)))
		exit_exprs = cons(begin_symbol,CDR(tmp));
	else 
		exit_exprs = CADR(tmp);
	if (PAIR_P(CDDDR(expr))) {
		tmp = expand_sequence(CDDDR(expr));
		tmp = cons(begin_symbol,tmp);
		tmp2 = cons(loop_sym,incrs);
		tmp2 = list1(tmp2);
		tmp = cons2(begin_symbol,tmp,tmp2);
	} else
		tmp = cons(loop_sym,incrs);
	tmp = list1(tmp);
	tmp = cons3(if_symbol,exit_pred,exit_exprs,tmp);
	tmp = list3(lambda_symbol,names,tmp);
	tmp = list2(loop_sym,tmp);
	tmp = list1(tmp);
	tmp2 = cons(loop_sym,inits);
	tmp = list3(letrec_symbol,tmp,tmp2);
	tmp = macro_expand(tmp);
	POP_GC_PROTECT(9);
	return tmp;
}

static object expand_and(object expr) {
	long i = length(expr);
	if (i < 1)
		error(expr,"bad syntax for and");
	if (i == 1)
		return true_object;
	else if (i == 2)
		return macro_expand(CADR(expr));
	else if (i == 3) {
		object tmp = null_object, tmp2 = null_object;
		PUSH_GC_PROTECT(expr);
		PUSH_GC_PROTECT(tmp);
		PUSH_GC_PROTECT(tmp2);
		tmp = macro_expand(CADR(expr));
		tmp2 = macro_expand(CADDR(expr));
		tmp = list4(if_symbol,tmp,tmp2,false_object);
		POP_GC_PROTECT(3);
		return tmp;
	} else {
		object tmp=null_object, result = null_object, clause = CDR(expr);
		object tmp2=null_object;
		PUSH_GC_PROTECT(expr);
		PUSH_GC_PROTECT(tmp);
		PUSH_GC_PROTECT(tmp2);
		PUSH_GC_PROTECT(result);
		PUSH_GC_PROTECT(clause);
		while (i-- > 3) {
			tmp2 = macro_expand(CAR(clause));
			tmp = cons(tmp2,tmp);
			clause = CDR(clause);
		}
		result = list4(if_symbol,CAR(clause),CADR(clause), false_object);
		while (!NULL_P(tmp)) {
			result = list4(if_symbol,CAR(tmp),result, false_object);
			tmp = CDR(tmp);
		}
		result = macro_expand(result);
		POP_GC_PROTECT(5);
		return result;
	}
}

static object expand_or(object expr) {
	long i = length(expr);
	if (i < 1)
		error(expr,"bad syntax for or");
	if (i == 1)
		return false_object;
	else if (i == 2)
		return macro_expand(CADR(expr));
	else if (i == 3) {
		object tmp=null_object, tmp2=null_object;
		PUSH_GC_PROTECT(tmp);
		PUSH_GC_PROTECT(tmp2);
		PUSH_GC_PROTECT(expr);
		tmp = list4(if_symbol,system_tmp_symbol,system_tmp_symbol,CADDR(expr));
		tmp2 = list1(system_tmp_symbol);
		tmp = list3(lambda_symbol,tmp2,tmp);
		tmp = list2(tmp,CADR(expr));
		tmp = macro_expand(tmp);
		POP_GC_PROTECT(3);
		return tmp;
	} else {
		object tmp=null_object, result = null_object, clause = CDR(expr);
		object tmp2=null_object;
		PUSH_GC_PROTECT(expr);
		PUSH_GC_PROTECT(tmp);
		PUSH_GC_PROTECT(clause);
		PUSH_GC_PROTECT(result);
		PUSH_GC_PROTECT(tmp2);
		while (i-- > 3) {
			tmp = cons(CAR(clause),tmp);
			clause = CDR(clause);
		}
		result = list4(if_symbol, system_tmp_symbol, system_tmp_symbol,
					   CADR(clause));
		tmp2 = list1(system_tmp_symbol);
		tmp2 = list3(lambda_symbol, tmp2,result);
		result = list2(tmp2, CAR(clause));
		while (!NULL_P(tmp)) {
			tmp2 = list4(if_symbol,system_tmp_symbol,system_tmp_symbol,result);
			result = list1(system_tmp_symbol);
			tmp2 = list3(lambda_symbol,result,tmp2);
			result = list2(tmp2, CAR(tmp));
			tmp = CDR(tmp);
		}
		result = macro_expand(result);
		POP_GC_PROTECT(5);
		return result;
	}
}

static object next_cond_clause(object expr, object clauses, long count) {
	object tmp, tmp2 = null_object;
	object clause = CAR(clauses);
	clauses = CDR(clauses);
	if (!PAIR_P(clause))
		error(clause,"bad syntax for cond (ill formed clause)");
	tmp = CAR(clauses);
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(clauses);
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(tmp2);
	PUSH_GC_PROTECT(clause);
	
	if (count == 2) { /* last two clauses */
		if (!PAIR_P(tmp))
			error(tmp,"bad syntax for cond (ill formed clause)");
		if (eq_p(CAR(tmp),else_symbol)) {
			tmp = cons(begin_symbol,CDR(tmp));
			if (PAIR_P(CDR(clause)) && eq_p(CADR(clause),eqgt_symbol)) {
				if (length(clause) != 3)
					error(clause,"bad syntax for cond (ill formed clause)");
				tmp2 = list2(CADDR(clause), system_tmp_symbol);
				tmp = list4(if_symbol,system_tmp_symbol,tmp2,tmp);
				tmp2 = list2(system_tmp_symbol,CAR(clause));
				tmp2 = list1(tmp2);
				tmp = list3(let_symbol,tmp2,tmp);
			} else {
				tmp2 = cons(begin_symbol,CDR(clause));
				tmp = list4(if_symbol,CAR(clause),tmp2,tmp);
			}
		} else {
			if (PAIR_P(CDR(tmp)) && eq_p(CADR(tmp),eqgt_symbol)) {
				if (length(tmp) != 3)
					error(tmp,"bad syntax for cond (ill formed clause)");
				tmp2 = list2(CADDR(tmp), system_tmp_symbol);
				tmp = list4(if_symbol,system_tmp_symbol,tmp2,tmp);
				tmp2 = list2(system_tmp_symbol,CAR(tmp));
				tmp2 = list1(tmp2);
				tmp = list3(let_symbol,tmp2,tmp);
			} else {
				tmp2 = cons(begin_symbol,CDR(tmp));
				tmp = list3(if_symbol,CAR(tmp),tmp2);
			}
			if (PAIR_P(CDR(clause)) && eq_p(CADR(clause),eqgt_symbol)) {
				if (length(clause) != 3)
					error(clause,"bad syntax for cond (ill formed clause)");
				tmp2 = list2(CADDR(clause),system_tmp_symbol);
				tmp = list4(if_symbol,system_tmp_symbol,tmp2,tmp);
				tmp2 = list2(system_tmp_symbol,CAR(clause));
				tmp2 = list1(tmp2);
				tmp = list3(let_symbol,tmp2,tmp);
			} else {
				tmp2 = cons(begin_symbol,CDR(clause));
				tmp = list4(if_symbol,CAR(clause),tmp2,tmp);
			}
		}
	} else {
		tmp = next_cond_clause(expr,clauses,count-1);
		if (PAIR_P(CDR(clause)) && eq_p(CADR(clause),eqgt_symbol)) {
			if (length(clause) != 3)
				error(clause,"bad syntax for cond (ill formed clause)");
			tmp2 = list2(CADDR(clause),system_tmp_symbol);
			tmp = list4(if_symbol,system_tmp_symbol,tmp2,tmp);
			tmp2 = list2(system_tmp_symbol,CAR(clause));
			tmp2 = list1(tmp2);
			tmp = list3(let_symbol,tmp2,tmp);
		} else {
			tmp2 = cons(begin_symbol,CDR(clause));
			tmp = list4(if_symbol,CAR(clause),tmp2,tmp);
		}
	}
	tmp = macro_expand(tmp);
	POP_GC_PROTECT(5);
	return tmp;
}

static object expand_cond(object expr) {
	long i = length(expr);
	if (i < 2) {
		error(expr,"bad syntax for cond (no clauses)");
		return void_object;
	} else if (i == 2) {
		object tmp = CADR(expr);
		PUSH_GC_PROTECT(expr);
		PUSH_GC_PROTECT(tmp);
		if (eq_p(CAR(tmp),else_symbol)) {
			tmp = cons(begin_symbol,CDR(tmp));
		} else {
			expr = cons(begin_symbol, CDR(tmp));
			tmp = list3(if_symbol, CAR(tmp), expr);
		}
		tmp = macro_expand(tmp);
		POP_GC_PROTECT(2);
		return tmp;
	} else {
		object tmp;
		PUSH_GC_PROTECT(expr);
		tmp = next_cond_clause(expr,CDR(expr),i-1);
		POP_GC_PROTECT(1);
		return tmp;
	}
}

static object next_case_clause(object expr, object clauses,
							   long i, object key) {
	object clause = CAR(clauses);
	object tmp = CAR(clause), tmp2 = null_object;
	if (!PAIR_P(clause))
		error(clause,"bad syntax for case (ill formed clause)");
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(tmp2);
	PUSH_GC_PROTECT(clause);
	PUSH_GC_PROTECT(clauses);
	PUSH_GC_PROTECT(key);
	if (i == 1) {
		if (eq_p(tmp,else_symbol))
			tmp = cons(begin_symbol,CDR(clause));
		else if (PAIR_P(tmp)) {
			tmp = list2(quote_symbol,tmp);
			tmp = list3(memv_symbol,key,tmp);
			tmp2 = cons(begin_symbol,CDR(clause));
			tmp = list3(if_symbol,tmp,tmp2);
		} else {
			error(tmp,"bad syntax for case (ill formed clause)");
		} 
	} else {
		if (!PAIR_P(tmp))
			error(tmp,"bad syntax for case (ill formed clause)");
		tmp = list2(quote_symbol,tmp);
		tmp = list3(memv_symbol,key,tmp);
		tmp2 = next_case_clause(expr,CDR(clauses),i-1,key);
		key = cons(begin_symbol,CDR(clause));
		tmp = list4(if_symbol,tmp,key,tmp2);
	}
	tmp = macro_expand(tmp);
	POP_GC_PROTECT(6);
	return tmp;
}

static object expand_case(object expr) {
	object tmp = void_object;
	object key = system_tmp_symbol;
	long i = length(expr);
	if (i < 3)
		error(expr,"bad syntax for case (no clauses)");
	PUSH_GC_PROTECT(expr);
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(key);
	tmp = next_case_clause(expr,CDDR(expr),i-2,key);
	key = list1(key);
	tmp = list3(lambda_symbol,key,tmp);
	tmp = list2(tmp,CADR(expr));
	tmp = macro_expand(tmp);
	POP_GC_PROTECT(3);
	return tmp;
}

static object quasiquoted_list(object template, long depth) {
	object result = null_object, tmp = null_object, tmp2 = null_object;
	object tail = null_object;
	PUSH_GC_PROTECT(result);
	PUSH_GC_PROTECT(tmp);
	PUSH_GC_PROTECT(tmp2);
	PUSH_GC_PROTECT(template);
	PUSH_GC_PROTECT(tail);
	while (!NULL_P(template)) {
		if (PAIR_P(template)) {
			tmp = CAR(template);
			if (PAIR_P(tmp)) {
				tmp2 = CAR(tmp);
				if (eq_p(tmp2,unquote_symbol)) {
					if (depth <= 1)
						tmp = macro_expand(CADR(tmp));
					else {
						if (PAIR_P(CADR(tmp)))
							tmp = quasiquoted_list(CADR(tmp),depth-1);
						else
							tmp = CADR(tmp);
						tmp = list3(cons_symbol,tmp,null_object);
						tmp2 = list2(quote_symbol,unquote_symbol);
						tmp = list3(cons_symbol,tmp2,tmp);
					}
					goto append_item;
				} else if (eq_p(tmp2,unquote_splicing_symbol)) {
					if (depth <= 1) {
						tmp = macro_expand(CADR(tmp));
						tmp = list2(intern("append"),tmp);
						if (NULL_P(tail)) {
							result = tail = tmp;
							tail = CDR(tmp);
						} else {
							tmp = cons(tmp,null_object);
							CDR(tail) = tmp;
							tail = CDAR(tmp);
						}
					} else {
						if (PAIR_P(CADR(tmp)))
							tmp = quasiquoted_list(CADR(tmp),depth-1);
						else
							tmp = CADR(tmp);
						tmp = list3(cons_symbol,tmp,null_object);
						tmp2 = list2(quote_symbol,unquote_splicing_symbol);
						tmp = list3(cons_symbol,tmp2,tmp);
					}
				} else if (eq_p(tmp2,quasiquote_symbol)) {
					tmp = quasiquoted_list(CADR(tmp),depth+1);
					if (depth >= 1) {
						tmp2 = list2(quote_symbol,quasiquote_symbol);
						tmp = list3(cons_symbol,tmp,null_object);
						tmp2 = list3(cons_symbol,tmp2,tmp);
						tmp = list3(cons_symbol,tmp2,null_object);
						if (NULL_P(tail)) {
							result = tmp;
							tail = tmp2;
						} else {
							tmp = cons(tmp,null_object);
							CDR(tail) = tmp;
							tail = CDAR(tmp);
						}
					} else {
						goto append_item;
					}
				} else {
					tmp = quasiquoted_list(tmp,depth);
					goto append_item;
				}
			} else if (eq_p(tmp,unquote_symbol)) {
				if (depth <= 1) {
					tmp = macro_expand(CADR(template));
					if (NULL_P(tail)) {
						result = tmp;
					} else {
						tmp = cons(tmp,null_object);
						CDR(tail) = tmp;
						tail = CDAR(tmp);
					}
					goto return_result;
				} else {
					if (PAIR_P(CADR(template)))
						tmp = quasiquoted_list(CADR(template),depth-1);
					else
						tmp = CADR(template);
					tmp = list3(cons_symbol,tmp,null_object);
					tmp2 = list2(quote_symbol,unquote_symbol);
					tmp = list3(cons_symbol,tmp2,tmp);
					template = CDR(template);
					goto append_item;
				}
			} else if (eq_p(tmp,unquote_splicing_symbol)) {
				result = list2(intern("append"),result);
				tail = CDR(result);
				tmp = macro_expand(CADR(template));
				tmp = cons(tmp,null_object);
				CDR(tail) = tmp;
				tail = CDR(tail);
				if (PAIR_P(CDR(template))) {
					tmp = list1(intern("list"));
					tmp = cons(tmp,null_object);
					CDR(tail) = tmp;
					tail = CDAR(tmp);
				}
			} else if (eq_p(tmp2,quasiquote_symbol)) {
				tmp = list2(quote_symbol,intern("BOGUS2"));
				goto append_item;
			} else {
				tmp = list2(quote_symbol,tmp);
			  append_item:
				tmp = list2(cons_symbol,tmp);
				if (NULL_P(tail)) {
					result = tail = tmp;
					tail = CDR(tmp);
				} else {
					tmp = cons(tmp,null_object);
					CDR(tail) = tmp;
					tail = CDAR(tmp);
				}
			}
			template = CDR(template);
		} else {
			if (PAIR_P(tail)) {
				tmp = list2(quote_symbol, template);
				tmp = cons(tmp,null_object);
				CDR(tail) = tmp;
			} else
				result = template;
			goto return_result;
		}
	}
	if (PAIR_P(tail)) {
		tmp = cons(null_object,null_object);
		CDR(tail) = tmp;
	}
  return_result:
	POP_GC_PROTECT(5);
	return result;
}

static object expand_quasiquote(object expr) {
	object result = expr, tmp = null_object;
	PUSH_GC_PROTECT(result);
	PUSH_GC_PROTECT(tmp);
	if (length(expr) != 2)
		error(expr,"bad syntax for quasiquote");
	if (PAIR_P(CADR(expr))) {
		result = quasiquoted_list(CADR(expr),1);
	} else if (VECTOR_P(CADR(expr))) {
		result = vector_to_list(CADR(expr));
		result = quasiquoted_list(result,1);
		result = list2(intern("list->vector"),result);
	} else
		result = list2(quote_symbol,CADR(expr));
	POP_GC_PROTECT(2);
	return result;
}

static object expand_delay(object expr) {
	object tmp = null_object;
	PUSH_GC_PROTECT(tmp);
	if (length(expr) != 2)
		error(expr,"bad syntax for delay");
	tmp = list3(lambda_symbol,null_object,CADR(expr));
	tmp = list2(make_promise_symbol,tmp);
	tmp = macro_expand(tmp);
	POP_GC_PROTECT(1);
	return tmp;
}

/* Runtime library */

static void primop_macro_p(long argc) {
	if (!macro_transformer(*sp)) *sp = false_object;
}

static void primop_macroexpand(long argc) {
	*sp = macro_expand(*sp);
}

void init_macros(void) {
	macro_alist = null_object;
	PUSH_GC_PROTECT(macro_alist);
	define_macro("lambda", expand_lambda);
	define_macro("quote", expand_quote);
	define_macro("if", expand_if);
	define_macro("begin", expand_begin);
	define_macro("define", expand_define);
	define_macro("set!", expand_set);
	letrec_symbol = intern("letrec");
	PUSH_GC_PROTECT(letrec_symbol);
	define_macro("letrec", expand_letrec);
	let_symbol = intern("let");
	PUSH_GC_PROTECT(let_symbol);
	define_macro("let", expand_let);
	do_symbol = intern("do");
	PUSH_GC_PROTECT(do_symbol);
	define_macro("do", expand_do);
	letstar_symbol = intern("let*");
	PUSH_GC_PROTECT(letstar_symbol);
	define_macro("let*", expand_let_star);
	and_symbol = intern("and");
	PUSH_GC_PROTECT(and_symbol);
	define_macro("and", expand_and);
	or_symbol = intern("or");
	PUSH_GC_PROTECT(or_symbol);
	define_macro("or", expand_or);
	cond_symbol = intern("cond");
	PUSH_GC_PROTECT(cond_symbol);
	define_macro("cond", expand_cond);
	else_symbol = intern("else");
	PUSH_GC_PROTECT(else_symbol);
	eqgt_symbol = intern("=>");
	PUSH_GC_PROTECT(eqgt_symbol);
	case_symbol = intern("case");
	PUSH_GC_PROTECT(case_symbol);
	define_macro("case", expand_case);
	memv_symbol = intern("memv");
	PUSH_GC_PROTECT(memv_symbol);
	delay_symbol = intern("delay");
	PUSH_GC_PROTECT(delay_symbol);
	make_promise_symbol = intern("system:make-promise");
	PUSH_GC_PROTECT(make_promise_symbol);
	define_macro("delay", expand_delay);
	quasiquote_symbol = intern("quasiquote");
	PUSH_GC_PROTECT(quasiquote_symbol);
	define_macro("quasiquote", expand_quasiquote);
	unquote_symbol = intern("unquote");
	PUSH_GC_PROTECT(unquote_symbol);
	unquote_splicing_symbol = intern("unquote-splicing");
	PUSH_GC_PROTECT(unquote_splicing_symbol);
	append_symbol = intern("append");
	PUSH_GC_PROTECT(append_symbol);
	cons_symbol = intern("cons");
	PUSH_GC_PROTECT(cons_symbol);
	list_to_vector_symbol = intern("list->vector");
	PUSH_GC_PROTECT(list_to_vector_symbol);
	system_loop_symbol = intern("system:loop");
	PUSH_GC_PROTECT(system_loop_symbol);
	system_tmp_symbol = intern("system:tmp");
	PUSH_GC_PROTECT(system_tmp_symbol);
	define_primop("system:macro?",primop_macro_p,1,1);
	define_primop("system:macroexpand",primop_macroexpand,1,1);
}
