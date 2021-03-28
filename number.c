/* LeeScheme/math.c - Copyright (C) Lee Richard Boynton, 1993-2001. */

#include "scheme.h"

#include <math.h>

#ifdef APPLE
#define drem remainder
#endif

#ifdef LINUX
extern double drem (double x, double y);
#endif

#ifdef NeXT
extern double drem(double x, double y);
#endif /* NeXT */

#ifdef SOLARIS
#define drem remainder
#endif

#ifdef MSDOS
#include <fltpnt.h>
#define drem remainder
#endif /* MSDOS */

#ifdef WIN32
double drem(double x, double y) {
	error(unbound_object,"number.c: drem not available in this implementation");
	return x;
}
#endif /* WIN32 */

#define THE_LONG(argnum,n) (FIXNUM_P(n)? FIXNUM_VALUE(n) : the_long(argnum,n))

object make_fixnum(long value) {
	return MAKE_FIXNUM(value);
}

object make_flonum(double value) {
	object r = make_heap_object(FLONUM_TYPE,
								sizeof(struct flonum_heap_structure));
	FLONUM_VALUE(r) = value;
	return r;
}

object make_number(double val) {
	long i = (long)val;
	if (i >= MIN_FIXNUM && i <= MAX_FIXNUM && (double)i == val)
		return MAKE_FIXNUM(i);
	else
		return make_flonum(val);
}

double epsilon;

long the_long(long argnum, object o) {
	if (FIXNUM_P(o))
		return FIXNUM_VALUE(o);
	else if (FLONUM_P(o)) {
		double d = FLONUM_VALUE(o);
		long  di = (long)d;
		double dd = d - di;
		if (dd < epsilon && dd > -epsilon && d <= MAX_INT && d >= MIN_INT)
			return (long)d;
		else
			error(o,"number is not an integer");
	}
	error(o,"argument %d is not an integer",argnum);
	return 0;
}

double the_double(long argnum, object o) {
	if (FIXNUM_P(o))
		return (double)FIXNUM_VALUE(o);
	else if (FLONUM_P(o))
		return FLONUM_VALUE(o);
	error(o,"argument %d is not a number",argnum);
	return 0;
}

char the_char(long argnum, object o) {
	if (CHARACTER_P(o))
		return CHARACTER_VALUE(o);
	error(o,"argument %d is not a character",argnum);
	return '\0';
}

static void primop_number_p(long arg) {
	if (!NUMBER_P(*sp)) *sp = false_object;
}

static void primop_complex_p(long arg) {
	if (!NUMBER_P(*sp)) *sp = false_object;
}

static void primop_real_p(long arg) {
	if (!NUMBER_P(*sp)) *sp = false_object;
}

static void primop_rational_p(long arg) {
	if (!FIXNUM_P(*sp)) *sp = false_object;
}

static void primop_integer_p(long arg) {
	if (!FIXNUM_P(*sp)) *sp = false_object;
}

static void primop_exact_p(long arg) {
	if (!FIXNUM_P(*sp)) *sp = false_object;
}

static void primop_inexact_p(long arg) {
	if (!FLONUM_P(*sp)) *sp = false_object;
}

static void primop_zero_p(long arg) {
	object n = *sp;
	if (FIXNUM_P(n)) {
		if (FIXNUM_VALUE(n) != 0) *sp = false_object;
	} else if (the_double(1,n) != 0)
		*sp = false_object;
}

static void primop_positive_p(long arg) {
	object n = *sp;
	if (FIXNUM_P(n)) {
		if (FIXNUM_VALUE(n) <= 0) *sp = false_object;
	} else if (the_double(1,n) <= 0)
		*sp = false_object;
}

static void primop_negative_p(long arg) {
	object n = *sp;
	if (FIXNUM_P(n)) {
		if (FIXNUM_VALUE(n) >= 0) *sp = false_object;
	} else if (the_double(1,n) >= 0)
		*sp = false_object;
}

static void primop_odd_p(long arg) {
	long n = THE_LONG(1,*sp);
	if (!(n & 1))
		*sp = false_object;
}

static void primop_even_p(long arg) {
	long n = THE_LONG(1,*sp);
	if (n & 1)
		*sp = false_object;
}

static void primop_min_p(long argc) {
	long i;
	double the_min = the_double(1,sp[0]);
	for(i=1; i<argc; i++) {
		double d = the_double(i+1,sp[i]);
		if (d < the_min) the_min = d;
	}
	sp += argc;
	*--sp = make_number(the_min);
}

static void primop_max_p(long argc) {
	long i;
	double the_max = the_double(1,sp[0]);
	for(i=1; i<argc; i++) {
		double d = the_double(i+1,sp[i]);
		if (d > the_max) the_max = d;
	}
	sp += argc;
	*--sp = make_number(the_max);
}

/* double epsilon = 0.0000005; */

static void primop_num_eq(long argc) {
	object n1, n2;
	double d1, d2, d;
	long argnum=1;
	if (!argc) {
		*--sp = true_object;
		return;
	}
	n1 = sp[0];
	if (FIXNUM_P(n1)) {
		while (argnum < argc) {
			n2 = sp[argnum++];
			if (eq_p(n1,n2)) continue;
			if (FIXNUM_P(n2)) goto fail;
			if (FLONUM_P(n2)) {
				d1 = FLONUM_VALUE(n2);
				d = d1 - (double)FIXNUM_VALUE(n1);
				if (d > epsilon || d < -epsilon) goto fail;
				goto real_eq;
			}
			error(n2,"argument %d is not a number",argnum);
		}
		goto succeed;
	} else if (!FLONUM_P(n1))
		error(n1,"argument %d is not a number",argnum);
	d1 = FLONUM_VALUE(n1);
  real_eq:
	while (argnum < argc) {
		n2 = sp[argnum++];
		d2 = the_double(argnum,n2);
		d = d2 - d1;
		if (d > epsilon || d < -epsilon) goto fail;
	}
  succeed:
	sp += argc;
	*--sp = true_object;
	return;
  fail:
	sp += argc;
	*--sp = false_object;
}

static void primop_num_ge(long argc) {
	if (argc == 2 && FIXNUM_P(sp[0]) && FIXNUM_P(sp[1])) {
		if (FIXNUM_VALUE(sp[0]) >= FIXNUM_VALUE(sp[1]))
			goto succeed;
		else
			goto fail;
	} else {
		object n1, n2;
		double d1, d2;
		long argnum=1;
		if (!argc) goto succeed;
		n1 = sp[0];
		if (FIXNUM_P(n1)) {
			long val = FIXNUM_VALUE(n1);
			while (argnum < argc) {
				n2 = sp[argnum++];
				if (FIXNUM_P(n2)) {
					long val2 = FIXNUM_VALUE(n2);
					if (val >= val2)
						val = val2;
					else
						goto fail;
				} else if (FLONUM_P(n2)) {
					d1 = FLONUM_VALUE(n2);
					if ((double)FIXNUM_VALUE(n1) < d1 + epsilon)
						goto fail;
					goto real_ge;
				} else 
					error(n2,"argument %d is not a number",argnum+1);
			}
			goto succeed;
		} else if (!FLONUM_P(n1))
			error(n1,"argument %d is not a number",argnum+1);
		d1 = FLONUM_VALUE(n1);
	  real_ge:
		while (argnum < argc) {
			n2 = sp[argnum++];
			d2 = the_double(argnum,n2) + epsilon;
			if (d1 < d2)
				goto fail;
		}
	  succeed:
		sp += argc;
		*--sp = true_object;
		return;
	  fail:
		sp += argc;
		*--sp = false_object;
	}
}

static void primop_num_gt(long argc) {
	if (argc == 2 && FIXNUM_P(sp[0]) && FIXNUM_P(sp[1])) {
		if (FIXNUM_VALUE(sp[0]) > FIXNUM_VALUE(sp[1]))
			goto succeed;
		else
			goto fail;
	} else {
		object n1, n2;
		double d1, d2;
		long argnum=1;
		if (!argc) goto succeed;
		n1 = sp[0];
		if (FIXNUM_P(n1)) {
			long val = FIXNUM_VALUE(n1);
			while (argnum < argc) {
				n2 = sp[argnum++];
				if (FIXNUM_P(n2)) {
					long val2 = FIXNUM_VALUE(n2);
					if (val > val2)
						val = val2;
					else
						goto fail;
				} else if (FLONUM_P(n2)) {
					d1 = FLONUM_VALUE(n2);
					if ((double)FIXNUM_VALUE(n1) <= d1 - epsilon)
						goto fail;
					goto real_gt;
				} else
					error(n2,"argument %d is not a number",argnum+1);
			}
			goto succeed;
		} else if (!FLONUM_P(n1))
			error(n1,"argument %d is not a number",argnum+1);
		d1 = FLONUM_VALUE(n1);
	  real_gt:
		while (argnum < argc) {
			n2 = sp[argnum++];
			d2 = the_double(argnum,n2) - epsilon;
			if (d1 <= d2)
				goto fail;
		}
	  succeed:
		sp += argc;
		*--sp = true_object;
		return;
	  fail:
		sp += argc;
		*--sp = false_object;
	}
}

static void primop_num_le(long argc) {
	if (argc == 2 && FIXNUM_P(sp[0]) && FIXNUM_P(sp[1])) {
		if (FIXNUM_VALUE(sp[0]) <= FIXNUM_VALUE(sp[1]))
			goto succeed;
		else
			goto fail;
	} else {
		object n1, n2;
		double d1, d2;
		long argnum=1;
		if (!argc) goto succeed;
		n1 = sp[0];
		if (FIXNUM_P(n1)) {
			long val = FIXNUM_VALUE(n1);
			while (argnum < argc) {
				n2 = sp[argnum++];
				if (FIXNUM_P(n2)) {
					long val2 = FIXNUM_VALUE(n2);
					if (val <= val2)
						val = val2;
					else
						goto fail;
				} else if (FLONUM_P(n2)) {
					d1 = FLONUM_VALUE(n2);
					if ((double)FIXNUM_VALUE(n1) > d1 + epsilon)
						goto fail;
					goto real_le;
				} else
					error(n2,"argument %d is not a number",argnum+1);
			}
			goto succeed;
		} else if (!FLONUM_P(n1))
			error(n1,"argument %d is not a number",argnum+1);
		d1 = FLONUM_VALUE(n1);
	  real_le:
		while (argnum < argc) {
			n2 = sp[argnum++];
			d2 = the_double(argnum,n2) + epsilon;
			if (d1 > d2)
				goto fail;
		}
	  succeed:
		sp += argc;
		*--sp = true_object;
		return;
	  fail:
		sp += argc;
		*--sp = false_object;
	}
}

static void primop_num_lt(long argc) {
	if (argc == 2 && FIXNUM_P(sp[0]) && FIXNUM_P(sp[1])) {
		if (FIXNUM_VALUE(sp[0]) < FIXNUM_VALUE(sp[1]))
			goto succeed;
		else
			goto fail;
	} else {
		object n1, n2;
		double d1, d2;
		long argnum=1;
		if (!argc) goto succeed;
		n1 = sp[0];
		if (FIXNUM_P(n1)) {
			long val = FIXNUM_VALUE(n1);
			while (argnum < argc) {
				n2 = sp[argnum++];
				if (FIXNUM_P(n2)) {
					long val2 = FIXNUM_VALUE(n2);
					if (val < val2)
						val = val2;
					else
						goto fail;
				} else if (FLONUM_P(n2)) {
					d1 = FLONUM_VALUE(n2);
					if ((double)FIXNUM_VALUE(n1) >= d1 - epsilon)
						goto fail;
					goto real_lt;
				} else
					error(n2,"argument %d is not a number",argnum+1);
			}
			goto succeed;
		} else if (!FLONUM_P(n1))
			error(n1,"argument %d is not a number",argnum+1);
		d1 = FLONUM_VALUE(n1);
	  real_lt:
		while (argnum < argc) {
			n2 = sp[argnum++];
			d2 = the_double(argnum,n2) - epsilon;
			if (d1 >= d2)
				goto fail;
		}
	  succeed:
		sp += argc;
		*--sp = true_object;
		return;
	  fail:
		sp += argc;
		*--sp = false_object;
	}
}

static void primop_abs(long arg) {
	object n = sp[0];
	if (FIXNUM_P(n)) {
		long i = FIXNUM_VALUE(n);
		if (i < 0) *sp = MAKE_FIXNUM(-i);
	} else if (FLONUM_P(n)) {
		double d = FLONUM_VALUE(n);
		if (d < 0) *sp = make_flonum(-d);
	} else {
		error(n,"argument 1 is not a number");
	}
}

static void primop_add(long argc) {
	if (argc == 2 && FIXNUM_P(sp[0]) && FIXNUM_P(sp[1])) {
		long result = FIXNUM_VALUE(sp[0]) + FIXNUM_VALUE(sp[1]);
		if (result <= MAX_FIXNUM && result >= MIN_FIXNUM)
			*++sp = MAKE_FIXNUM(result);
		else
			*++sp = make_flonum((double)result);
	} else {
		long isum = 0;
		long argnum = 0;
		double dsum;
		while (argnum < argc) {
			object n = sp[argnum++];
			if (FIXNUM_P(n)) {
				isum += FIXNUM_VALUE(n);
				if (isum <= MAX_FIXNUM && isum >= MIN_FIXNUM)
					continue;
				dsum = (double)isum;
			} else
				dsum = (double)isum + the_double(argnum,n);
			goto double_sum;
		}
		sp += argc;
		*--sp = MAKE_FIXNUM(isum);
		return;
	  double_sum:
		while (argnum < argc) {
			object n = sp[argnum++];
			dsum += the_double(argnum,n);
		}
		sp += argc;
		*--sp = make_flonum(dsum);
	}
}

static void primop_mul(long argc) {
	long iprod = 1;
	int exact = 1;
	long argnum = 0;
	double dprod;
	while (argnum < argc) {
		object n = sp[argnum++];
		if (FIXNUM_P(n)) {
			long tmp = FIXNUM_VALUE(n);
			if (tmp <= 32767 && tmp >= -32767 &&
							iprod <= 32767 && iprod >= -32767) {
				iprod *= tmp;
				continue;
			} else
				dprod = (double)iprod * (double)tmp;
		} else {
			if (!FLONUM_P(n))
				error(n,"argument %d is not a number",argnum);
			exact = 0;
			dprod = (double)iprod * FLONUM_VALUE(n);
		}
		goto double_prod;
	}
	sp += argc;
	*--sp = MAKE_FIXNUM(iprod);
	return;
  double_prod:
	while (argnum < argc) {
		object n = sp[argnum++];
		if (FIXNUM_P(n))
			dprod *= (double)(FIXNUM_VALUE(n));
		else if (FLONUM_P(n)) {
			exact = 0;
			dprod *= FLONUM_VALUE(n);
		} else
			error(n,"argument %d is not a number",argnum);
	}
	sp += argc;
	if (exact && dprod <= MAX_FIXNUM && dprod >= MIN_FIXNUM)
		*--sp = MAKE_FIXNUM((long)dprod);
	else
		*--sp = make_flonum(dprod);
}

static void primop_sub(long argc) {
	if (argc == 2 && FIXNUM_P(sp[0]) && FIXNUM_P(sp[1])) {
		long result = FIXNUM_VALUE(sp[0]) - FIXNUM_VALUE(sp[1]);
		if (result <= MAX_FIXNUM && result >= MIN_FIXNUM)
			*++sp = MAKE_FIXNUM(result);
		else
			*++sp = make_flonum((double)result);
	} else if (argc == 1) {
		object n = sp[0];
		if (FIXNUM_P(n)) {
			*sp = MAKE_FIXNUM(-FIXNUM_VALUE(n));
			return;
		} else if (!FLONUM_P(n))
			error(n,"argument 1 is not a number");
		*sp = make_flonum(-FLONUM_VALUE(n));
		return;
	} else if (argc > 1) {
		object n = sp[0];
		long idiff;
		double ddiff;
		long argnum = 1;
		if (FIXNUM_P(n)) {
			idiff = FIXNUM_VALUE(n);
			while (argnum < argc) {
				n = sp[argnum++];
				if (FIXNUM_P(n)) {
					idiff -= FIXNUM_VALUE(n);
					if (idiff <= MAX_FIXNUM && idiff >= MIN_FIXNUM) {
						continue;
					}
					ddiff = (double)idiff;
				} else {
					ddiff = (double)idiff - the_double(argnum,n);
				}
				goto double_diff;
			}
			sp += argc;
			*--sp = MAKE_FIXNUM(idiff);
		} else {
			ddiff = the_double(1,n);
		  double_diff:
			while (argnum < argc) {
				n = sp[argnum++];
				ddiff -= the_double(argnum,n);
			}
			sp += argc;
			*--sp = make_number(ddiff);
		}
	} else {
		error(MAKE_FIXNUM(argc),"wrong number of arguments");
	}
}

static void primop_div(long argc) {
	object n;
	if (!argc) error(MAKE_FIXNUM(argc),"wrong number of arguments");
	n = sp[0];
	if (argc == 1) {
		double d = the_double(1,n);
		if (d == 0)
			error(unbound_object,"divide by zero");
		d = 1/d;
		*sp = make_flonum(d);
	} else {
		long argnum = 1;
		double d, quo = the_double(1,n);
		if (quo != 0) {
			while (argnum < argc) {
				n = sp[argnum++];
				d = the_double(argnum,n);
				if (d == 0)
					error(unbound_object,"divide by zero");
				quo /= d;
			}
		}
		sp += argc;
		*--sp = make_number(quo);
	}
}

static void primop_quotient(long arg) {
	object n1 = *sp++;
	object n2 = *sp;
	if (FIXNUM_P(n1) && FIXNUM_P(n2)) {
		long result, tmp = FIXNUM_VALUE(n2);
		if (tmp == 0)
			error(unbound_object,"divide by zero");
		result = FIXNUM_VALUE(n1) / tmp;
		*sp = MAKE_FIXNUM(result);
	} else {
		double d1 = the_double(1,n1);
		double d2 = the_double(2,n2);
		if (d1 <= MAX_FIXNUM && d1 >= MIN_FIXNUM && 
			d2 <= MAX_FIXNUM && d2 >= MIN_FIXNUM) {
			long result, i2 = (long)d2;
			if (i2 == 0)
				error(unbound_object,"divide by zero");
			result = (long)d1 / i2;
			*sp = MAKE_FIXNUM(result);
		} else {
			if (d2 == 0.0)
				error(unbound_object,"divide by zero");
			d1 /= d2;
			d1 = (d1 < 0)? ceil(d1) : floor(d1);
			*sp = make_flonum(d1);
		}
	}
}

static void primop_remainder(long arg) {
	object n1 = *sp++;
	object n2 = *sp;
	if (FIXNUM_P(n1) && FIXNUM_P(n2)) {
		long result, tmp = FIXNUM_VALUE(n2);
		if (tmp == 0)
			error(unbound_object,"divide by zero");
		result = FIXNUM_VALUE(n1) % tmp;
		*sp = MAKE_FIXNUM(result);
	} else {
		double d1 = the_double(1,n1);
		double d2 = the_double(2,n2);
		if (d1 <= MAX_FIXNUM && d1 >= MIN_FIXNUM && 
			d2 <= MAX_FIXNUM && d2 >= MIN_FIXNUM) {
			long result, i2 = (long)d2;
			if (i2 == 0)
				error(unbound_object,"divide by zero");
			result = (long)d1 % i2;
			*sp = MAKE_FIXNUM(result);
		} else {
			if (d2 == 0.0)
				error(unbound_object,"divide by zero");
			d1 = drem(d1,d2);
			*sp = make_flonum(d1);
		}
	}
}

static void primop_modulo(long arg) {
	object n1 = *sp++;
	object n2 = *sp;
	if (FIXNUM_P(n1) && FIXNUM_P(n2)) {
		long i1 = FIXNUM_VALUE(n1), i2 = FIXNUM_VALUE(n2);
		long result;
		if (i2 == 0)
			error(unbound_object,"divide by zero");
		result = i1 % i2;
		if (i2 > 0) {
			if (result < 0) result += i2;
		} else if (i2 < 0) {
			if (result > 0) result += i2;
		}
		*sp = make_fixnum(result);
	} else {
		double d1 = the_double(1,n1);
		double d2 = the_double(2,n2);
		if (d1 <= MAX_FIXNUM && d1 >= MIN_FIXNUM 
			&& d2 <= MAX_FIXNUM && d2 >= MIN_FIXNUM) {
			long result, i1 = (long)d1, i2 = (long)d2;
			if (i2 == 0)
				error(unbound_object,"divide by zero");
			result = i1 % i2;
			if (i2 > 0) {
				if (result < 0) result += i2;
			} else if (i2 < 0) {
				if (result > 0) result += i2;
			}
			*sp = MAKE_FIXNUM(result);
		} else {
			double result;
			if (d2 == 0.0)
				error(unbound_object,"divide by zero");
			result = drem(d1,d2);
			if (d2 > 0) {
				if (result < 0) result += d2;
				else
					if (result > 0) result += d2;
			}
			*sp = make_flonum(result);
		}
	}
}

static long gcd(long u, long v) {
	while (v) {
		long r = u % v;
		u = v;
		v = r;
	}
	return u;
}

static long lcm(long u, long v) {
	long j, i = gcd(u,v);
	if (!i) return i;
	j = u * v;
	if (j < 0) j = -j;
	return j / i;
}

#define ABS(x) ((x >= 0)? x : -x)

static void primop_gcd(long argc) {
	/* BUG: inexact operands are not contagious */
	long i, n, r = 0;
	if (argc == 1) {
		r = ABS(THE_LONG(1,sp[0]));
		*sp = MAKE_FIXNUM(r);
	} else if (argc > 1) {
		i = 1;
		r = ABS(THE_LONG(1,sp[0]));
		while (r != 1 && i < argc) {
			n = ABS(THE_LONG(i+1,sp[i]));
			r = gcd(n,r);
			i++;
		}
		sp += argc;
		*--sp = MAKE_FIXNUM(r);
	} else
		*--sp = MAKE_FIXNUM(0);
}

static void primop_lcm(long argc) {
	if (!argc)
		*--sp = MAKE_FIXNUM(1);
	else {
		long i;
		long r = THE_LONG(1,sp[0]);
		for (i=1; i<argc; i++) {
			long n = THE_LONG(i+1,sp[i]);
			r = lcm(r,n);
		}
		sp += argc;
		*--sp = MAKE_FIXNUM(ABS(r));
	}
}

static void primop_floor(long arg) {
	double tmp;
	if (FIXNUM_P(sp[0])) return;
	TYPE_CHECK(FLONUM_P(sp[0]),1,"number",sp[0]);
	tmp = floor(FLONUM_VALUE(sp[0]));
	*sp = make_number(tmp);
}

static void primop_ceiling(long arg) {
	double tmp;
	if (FIXNUM_P(sp[0])) return;
	TYPE_CHECK(FLONUM_P(sp[0]),1,"number",sp[0]);
	tmp = ceil(FLONUM_VALUE(sp[0]));
	*sp = make_number(tmp);
}

static void primop_truncate(long arg) {
	double tmp;
	if (FIXNUM_P(sp[0])) return;
	TYPE_CHECK(FLONUM_P(sp[0]),1,"number",sp[0]);
	tmp = FLONUM_VALUE(sp[0]);
	if (tmp < 0)
		tmp = ceil(tmp);
	else
		tmp = floor(tmp);
	*sp = make_number(tmp);
}

static void primop_round(long arg) {
	double tmp;
	if (FIXNUM_P(sp[0])) return;
	TYPE_CHECK(FLONUM_P(sp[0]),1,"number",sp[0]);
	tmp = FLONUM_VALUE(sp[0]);
	if (tmp < 0)
		tmp = ceil(tmp-0.5);
	else
		tmp = floor(tmp+0.5);
	*sp = make_number(tmp);
}

static void primop_exp(long arg) {
	double tmp = the_double(1,sp[0]);
	tmp = exp(tmp);
	*sp = make_flonum(tmp);
}

static void primop_log(long arg) {
	double tmp = the_double(1,sp[0]);
	tmp = log(tmp);
	*sp = make_flonum(tmp);
}

static void primop_sin(long arg) {
	double tmp = the_double(1,sp[0]);
	tmp = sin(tmp);
	*sp = make_flonum(tmp);
}

static void primop_cos(long arg) {
	double tmp = the_double(1,sp[0]);
	tmp = cos(tmp);
	*sp = make_flonum(tmp);
}

static void primop_tan(long arg) {
	double tmp = the_double(1,sp[0]);
	tmp = tan(tmp);
	*sp = make_flonum(tmp);
}

static void primop_asin(long arg) {
	double tmp = the_double(1,sp[0]);
	tmp = asin(tmp);
	*sp = make_flonum(tmp);
}

static void primop_acos(long arg) {
	double tmp = the_double(1,sp[0]);
	tmp = acos(tmp);
	*sp = make_flonum(tmp);
}

static void primop_atan(long argc) {
	double tmp = the_double(1,sp[0]);
	if (argc == 2) { 
		tmp = atan2(tmp,the_double(2,sp[1]));
		sp++;
	} else
		tmp = atan(tmp);
	*sp = make_flonum(tmp);
}

static void primop_sqrt(long arg) {
	double tmp = the_double(1,sp[0]);
	tmp = sqrt(tmp);
	*sp = make_flonum(tmp);
}

static void primop_expt(long arg) {
	double tmp = the_double(1,sp[0]);
	tmp = pow(tmp,the_double(2,sp[1]));
	*sp = make_flonum(tmp);
}

static void primop_exact_to_inexact(long arg) {
	double n = the_double(1,sp[0]);
	*sp = make_flonum(n);
}

static void primop_inexact_to_exact(long arg) {
	long n = THE_LONG(1,sp[0]);
	*sp = MAKE_FIXNUM(n);
}	 

static void primop_number_to_string(long argc) {
	object n = sp[0];
	char buf[MAX_ATOM];
	long radix = (argc == 2)? THE_LONG(2,sp[1]) : 10;
	TYPE_CHECK(NUMBER_P(n),0,"number",n);
	number_to_string(n,radix,buf);
	*sp = make_string(buf);
}

static void primop_string_to_number(long argc) {
	object s = sp[0], n;
	long radix = (argc == 2)? THE_LONG(2,sp[1]) : 10;
	TYPE_CHECK(STRING_P(s),0,"string",s);
	n = parse_number(STRING_VALUE(s),radix,0);
	if (VOID_P(n))
		*sp = false_object;
	else
		*sp = n;
}

void init_math(void) {
	define_primop("number?",primop_number_p,1,1);
	define_primop("complex?",primop_complex_p,1,1);
	define_primop("real?",primop_real_p,1,1);
	define_primop("rational?",primop_rational_p,1,1);
	define_primop("integer?",primop_integer_p,1,1);
	define_primop("exact?",primop_exact_p,1,1);
	define_primop("inexact?",primop_inexact_p,1,1);
	define_primop("zero?",primop_zero_p,1,1);
	define_primop("negative?",primop_negative_p,1,1);
	define_primop("positive?",primop_positive_p,1,1);
	define_primop("odd?",primop_odd_p,1,1);
	define_primop("even?",primop_even_p,1,1);
	define_primop("min",primop_min_p, 1,MAX_ARGC);
	define_primop("max",primop_max_p, 1,MAX_ARGC);
	define_primop("=",primop_num_eq, 0,MAX_ARGC);
	define_primop(">=",primop_num_ge, 0,MAX_ARGC);
	define_primop(">",primop_num_gt, 0,MAX_ARGC);
	define_primop("<=",primop_num_le, 0,MAX_ARGC);
	define_primop("<",primop_num_lt, 0,MAX_ARGC);
	define_primop("abs",primop_abs,1,1);
	define_primop("+",primop_add, 0,MAX_ARGC);
	define_primop("*",primop_mul, 0,MAX_ARGC);
	define_primop("-",primop_sub, 1,MAX_ARGC);
	define_primop("/",primop_div, 1,MAX_ARGC);
	define_primop("quotient",primop_quotient,2,2);
	define_primop("remainder",primop_remainder,2,2);
	define_primop("modulo",primop_modulo,2,2);
	define_primop("gcd",primop_gcd, 0,MAX_ARGC);
	define_primop("lcm",primop_lcm, 0,MAX_ARGC);
	define_primop("floor",primop_floor,1,1);
	define_primop("ceiling",primop_ceiling,1,1);
	define_primop("truncate",primop_truncate,1,1);
	define_primop("round",primop_round,1,1);
	define_primop("exp",primop_exp,1,1);
	define_primop("log",primop_log,1,1);
	define_primop("sin",primop_sin,1,1);
	define_primop("cos",primop_cos,1,1);
	define_primop("tan",primop_tan,1,1);
	define_primop("asin",primop_asin,1,1);
	define_primop("acos",primop_acos,1,1);
	define_primop("atan",primop_atan,1,2);
	define_primop("sqrt",primop_sqrt,1,1);
	define_primop("expt",primop_expt,2,2);
	define_primop("exact->inexact",primop_exact_to_inexact,1,1);
	define_primop("inexact->exact",primop_inexact_to_exact,1,1);
	define_primop("number->string",primop_number_to_string,1,2);
	define_primop("string->number",primop_string_to_number,1,2);
}



