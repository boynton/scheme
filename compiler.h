/* LeeScheme/compiler.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _COMPILER_
#define _COMPILER_

/* recognized syntactic constructs */
extern object quote_symbol, begin_symbol, if_symbol;
extern object set_symbol, lambda_symbol, define_symbol;

extern object compiler_module_name;

extern object compile(object expr);

extern void init_compiler(void);

#endif
