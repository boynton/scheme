/* LeeScheme/macros.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _MACROS_
#define _MACROS_

extern object quasiquote_symbol, unquote_symbol, unquote_splicing_symbol;

extern object macro_expand(object expr);

extern void init_macros(void);

#endif
