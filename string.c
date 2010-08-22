/* LeeScheme/string.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include <string.h>

object make_string_of_size(long length, int zero) {
	long i;
	object s = make_heap_object(STRING_TYPE,
								sizeof(struct string_heap_structure) +
								length + 1);
	STRING_LENGTH(s) = length;
	if (zero)
		for (i=0; i<=length; i++) STRING_VALUE(s)[i] = '\0';
	else
		STRING_VALUE(s)[length] = '\0';
	return s;
}

object make_string(char *value) {
	long len = strlen(value);
	object s = make_string_of_size(len,0);
	strcpy(STRING_VALUE(s), value);
	return s;
}

object make_character(char value) {
	return MAKE_IMMEDIATE(CHARACTER_TYPE,value);
}

/* The following is totally bogus. It probably doesn't handle YOUR
	character set. */
static char charmap[] = {
	'\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
	'\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
	'\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
	'\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
	'\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
	'\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
	'\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
	'\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
	'\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
	'\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
	'\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
	'\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
	'\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
	'\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
	'\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
	'\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
	'\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
	'\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
	'\300', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
	'\370', '\371', '\372', '\333', '\334', '\335', '\336', '\337',
	'\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
	'\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};

int charcmpci(char c1, char c2) {
	char *cm = charmap;
	return(cm[(unsigned)c1] - cm[(unsigned)c2]);
}

int strcmpci(char *s1, char *s2) {
	char *cm = charmap;
	while (cm[(unsigned)*s1] == cm[(unsigned)*s2++])
		if (*s1++ == '\0')
			return(0);
	return(cm[(unsigned)*s1] - cm[(unsigned)*--s2]);
}

int strncmpci(char *s1, char *s2, long count) {
	char *cm = charmap;
	long i = 0;
	while (cm[(unsigned)*s1] == cm[(unsigned)*s2++]) {
		if (*s1++ == '\0' || ++i == count)
			return(0);
	}
	return(cm[(unsigned)*s1] - cm[(unsigned)*--s2]);
}

static void primop_string_p(long argc) {
	if (!STRING_P(*sp)) *sp = false_object;
}

static void primop_make_string(long argc) {
	object s, o = sp[0];
	long size = the_long(1,o);
	if (size < 0 || size > MAX_STRING_SIZE)
		error(sp[0],"too big of a size for a string");
	s = make_string_of_size(size,argc!=2);
	if (argc == 2) {
		char *p = STRING_VALUE(s), fill;
		TYPE_CHECK(CHARACTER_P(sp[1]),1,"character",sp[1]);
		fill = CHARACTER_VALUE(sp[1]);
		while (size--)
			*p++ = fill;
		*p = '\0';
	}
	sp += argc;
	*--sp = s;
}

static void primop_string_length(long argc) {
	object o = *sp;
	TYPE_CHECK(STRING_P(o),1,"string",o);
	*sp = MAKE_FIXNUM(STRING_LENGTH(o));
}

static void primop_string(long argc) {
	object s = make_string_of_size(argc,0);
	char *p = STRING_VALUE(s);
	long i;
	for (i=0; i<argc; i++) {
		object co = *sp++;
		*p++ = the_char(i+1,co);
	}
	*p++ = '\0';
	*--sp = s;
}

static void primop_substring(long argc) {
	object s = sp[0], result;
	long start = the_long(2,sp[1]), end = the_long(3,sp[2]);
	long i, len;
	char *rp, *p;
	TYPE_CHECK(STRING_P(s),1,"string",s);
	len = STRING_LENGTH(s);
	if (start < 0 || start > len)
		error(sp[1],"starting index of range");
	if (end < start || end > len)
		error(sp[1],"ending index of range");
	result = make_string_of_size(end-start,0);
	p = STRING_VALUE(sp[0]);
	rp = STRING_VALUE(result);
	p += start;
	for (i=start; i < end; i++) {
		*rp++ = *p++;
	}
	*rp = '\0';
	sp += 2;
	*sp = result;
}

static void primop_substring_ix(long argc) {
	object s1 = *sp++;
	object s2 = *sp;
	char *s;
	TYPE_CHECK(STRING_P(s1),1,"string",s1);
	TYPE_CHECK(STRING_P(s2),1,"string",s2);
	s = strstr(STRING_VALUE(s1), STRING_VALUE(s2));
	if (s)
		*sp = MAKE_FIXNUM(s - STRING_VALUE(s1));
	else
		*sp = false_object;
}

static void primop_string_append(long argc) {
	long i, len = 0;
	object s, result;
	char *rp;
	for (i=0; i<argc; i++) {
		s = sp[i];
		TYPE_CHECK(STRING_P(s),i+1,"string",s);
		len += STRING_LENGTH(s);
	}
	result = make_string_of_size(len,0);
	rp = STRING_VALUE(result);
	for (i=0; i<argc; i++) {
		char *p = STRING_VALUE(sp[i]);
		while (*p) *rp++ = *p++;
	}
	*rp = '\0';
	sp += argc;
	*--sp = result;
}


static void primop_string_to_list(long argc) {
	long i;
	object result = null_object;
	PUSH_GC_PROTECT(result);
	TYPE_CHECK(STRING_P(sp[0]),1,"string", sp[0]);
	i = STRING_LENGTH(sp[0]);
	while (i--) {
		char c = STRING_VALUE(sp[0])[i];
		result = cons(make_character(c),result);
	}
	POP_GC_PROTECT(1);
	*sp = result;
}

static void primop_list_to_string(long argc) {
	object l = sp[0];
	long i, max = 0;
	object s;
	char *p;
	while (PAIR_P(l)) {
		object c = CAR(l);
		if (!CHARACTER_P(c))
			error(sp[0],"list contains a non-character");
		max++;
		l = CDR(l);
	}
	if (!NULL_P(l))
		error(sp[0],"not a proper list");
	s = make_string_of_size(max,0);
	p = STRING_VALUE(s);
	l = sp[0];
	for (i=0; i<max; i++) {
		*p++ = CHARACTER_VALUE(CAR(l));
		l = CDR(l);
	}
	*p = '\0';
	*sp = s;
}


static void primop_string_ref(long argc) {
	object s = *sp++;
	long i = the_long(2,*sp);
	TYPE_CHECK(STRING_P(s),1,"string",s);
	if (i >= STRING_LENGTH(s))
		error(*sp,"index out of range");
	*sp = make_character(STRING_VALUE(s)[i]);
}

static void primop_string_set(long argc) {
	object s = *sp;
	long i = the_long(2,sp[1]);
	char c = the_char(3,sp[2]);
	TYPE_CHECK(STRING_P(s),1,"string",s);
	if (i >= STRING_LENGTH(s))
		error(sp[1],"index out of range");
	STRING_VALUE(s)[i] = c;
	sp += 2;
}

static long string_compare(object s1, object s2) {
	TYPE_CHECK(STRING_P(s1),1,"string",s1);
	TYPE_CHECK(STRING_P(s2),2,"string",s2);
	return strcmp(STRING_VALUE(s1), STRING_VALUE(s2));
}

static void primop_string_eq(long argc) {
	object s1 = *sp++;
	*sp = string_compare(s1, *sp)? false_object : true_object;
}

static void primop_string_gt(long argc) {
	object s1 = *sp++;
	*sp = (string_compare(s1,*sp) > 0)? true_object : false_object;
}

static void primop_string_ge(long argc) {
	object s1 = *sp++;
	*sp = (string_compare(s1,*sp) >= 0)? true_object : false_object;
}

static void primop_string_lt(long argc) {
	object s1 = *sp++;
	*sp = (string_compare(s1,*sp) < 0)? true_object : false_object;
}

static void primop_string_le(long argc) {
	object s1 = *sp++;
	*sp = (string_compare(s1,*sp) <= 0)? true_object : false_object;
}

static long string_ci_compare(object s1, object s2) {
	TYPE_CHECK(STRING_P(s1),1,"string",s1);
	TYPE_CHECK(STRING_P(s2),2,"string",s2);
	return strcmpci(STRING_VALUE(s1), STRING_VALUE(s2));
}

static void primop_string_ci_eq(long argc) {
	object s1 = *sp++;
	*sp = string_ci_compare(s1, *sp)? false_object : true_object;
}

static void primop_string_ci_gt(long argc) {
	object s1 = *sp++;
	*sp = (string_ci_compare(s1,*sp) > 0)? true_object : false_object;
}

static void primop_string_ci_ge(long argc) {
	object s1 = *sp++;
	*sp = (string_ci_compare(s1,*sp) >= 0)? true_object : false_object;
}

static void primop_string_ci_lt(long argc) {
	object s1 = *sp++;
	*sp = (string_ci_compare(s1,*sp) < 0)? true_object : false_object;
}

static void primop_string_ci_le(long argc) {
	object s1 = *sp++;
	*sp = (string_ci_compare(s1,*sp) <= 0)? true_object : false_object;
}

static void primop_character_p(long argc) {
	if (!CHARACTER_P(*sp)) *sp = false_object;
}

static void primop_char_eq_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (c1 == c2)? true_object : false_object;
}

static void primop_char_lt_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (c1 < c2)? true_object : false_object;
}

static void primop_char_gt_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (c1 > c2)? true_object : false_object;
}

static void primop_char_le_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (c1 <= c2)? true_object : false_object;
}

static void primop_char_ge_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (c1 >= c2)? true_object : false_object;
}

#define LOWER_CASE(c) ((c >= 'A' && c <= 'Z')? (c + 'a' - 'A') : c)
/* FIX ME! use charmap instead? */

static void primop_char_ci_eq_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (LOWER_CASE(c1) == LOWER_CASE(c2))? true_object : false_object;
}

static void primop_char_ci_lt_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (LOWER_CASE(c1) < LOWER_CASE(c2))? true_object : false_object;
}

static void primop_char_ci_gt_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (LOWER_CASE(c1) > LOWER_CASE(c2))? true_object : false_object;
}

static void primop_char_ci_le_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (LOWER_CASE(c1) <= LOWER_CASE(c2))? true_object : false_object;
}

static void primop_char_ci_ge_p(long argc) {
	char c1 = the_char(1,sp[0]), c2 = the_char(2,sp[1]);
	*++sp = (LOWER_CASE(c1) >= LOWER_CASE(c2))? true_object : false_object;
}

static void primop_char_alphabetic_p(long argc) {
	char c = the_char(1,sp[0]);
	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
		*sp = true_object;
	else
		*sp = false_object;
}

static void primop_char_numeric_p(long argc) {
	char c = the_char(1,sp[0]);
	*sp = DIGIT_P(c)? true_object : false_object;
}

static void primop_char_whitespace_p(long argc) {
	char c = the_char(1,sp[0]);
	*sp = WHITESPACE_P(c)? true_object : false_object;
}

static void primop_char_upper_case_p(long argc) {
	char c = the_char(1,sp[0]);
	*sp = (c >= 'A' && c <= 'Z')? true_object : false_object;
}

static void primop_char_lower_case_p(long argc) {
	char c = the_char(1,sp[0]);
	*sp = (c >= 'a' && c <= 'z')? true_object : false_object;
}

static void primop_char_to_integer(long argc) {
	unsigned char c = the_char(1,sp[0]);
	*sp = MAKE_FIXNUM(c);
}

static void primop_integer_to_char(long argc) {
	long i = the_long(1,sp[0]);
	if (i > 255 || i < 0)
		error(sp[0],"out of range");
	*sp = make_character((char)i);
}

static void primop_char_upcase(long argc) {
	char c = the_char(1,sp[0]);
	if (c >= 'a' && c <= 'z')
		*sp = make_character((char)(c-'a'+'A'));
}

static void primop_char_downcase(long argc) {
	char c = the_char(1,sp[0]);
	if (c >= 'A' && c <= 'Z')
		*sp = make_character((char)(c-'A'+'a'));
}

void init_string(void) {
	define_primop("string?",primop_string_p,1,1);
	define_primop("make-string",primop_make_string,1,2);
	define_primop("string", primop_string, 0, MAX_ARGC);
	define_primop("string-length", primop_string_length,1,1);
	define_primop("string=?", primop_string_eq,2,2);
	define_primop("string<?", primop_string_lt,2,2);
	define_primop("string<=?", primop_string_le,2,2);
	define_primop("string>?", primop_string_gt,2,2);
	define_primop("string>=?", primop_string_ge,2,2);
	define_primop("string-ci=?", primop_string_ci_eq,2,2);
	define_primop("string-ci<?", primop_string_ci_lt,2,2);
	define_primop("string-ci<=?", primop_string_ci_le,2,2);
	define_primop("string-ci>?", primop_string_ci_gt,2,2);
	define_primop("string-ci>=?", primop_string_ci_ge,2,2);
	define_primop("string-ref", primop_string_ref,2,2);
	define_primop("string-set!", primop_string_set,3,3);
	define_primop("substring", primop_substring,3,3);
	define_primop("system:substring-index", primop_substring_ix,2,2);
	define_primop("string-append", primop_string_append, 0, MAX_ARGC);
	define_primop("string->list", primop_string_to_list,1,1);
	define_primop("list->string", primop_list_to_string,1,1);
	define_primop("char?", primop_character_p,1,1);
	define_primop("char=?",primop_char_eq_p,2,2);
	define_primop("char<?",primop_char_lt_p,2,2);
	define_primop("char>?",primop_char_gt_p,2,2);
	define_primop("char<=?",primop_char_le_p,2,2);
	define_primop("char>=?",primop_char_ge_p,2,2);
	define_primop("char-ci=?",primop_char_ci_eq_p,2,2);
	define_primop("char-ci<?",primop_char_ci_lt_p,2,2);
	define_primop("char-ci>?",primop_char_ci_gt_p,2,2);
	define_primop("char-ci<=?",primop_char_ci_le_p,2,2);
	define_primop("char-ci>=?",primop_char_ci_ge_p,2,2);
	define_primop("char-alphabetic?", primop_char_alphabetic_p,1,1);
	define_primop("char-numeric?",primop_char_numeric_p,1,1);
	define_primop("char-whitespace?",primop_char_whitespace_p,1,1);
	define_primop("char-upper-case?",primop_char_upper_case_p,1,1);
	define_primop("char-lower-case?",primop_char_lower_case_p,1,1);
	define_primop("char-upcase", primop_char_upcase,1,1);
	define_primop("char-downcase", primop_char_downcase,1,1);
	define_primop("char->integer",primop_char_to_integer,1,1);
	define_primop("integer->char",primop_integer_to_char,1,1);
}
