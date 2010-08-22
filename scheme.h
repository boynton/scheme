/* LeeScheme/scheme.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _SCHEME_
#define _SCHEME_

#define VERSION_STRING "LeeScheme-5.9"
#define COPYRIGHT_STRING "Copyright (C) 1993-2000 Lee Richard Boynton. All rights reserved."

#include <stdarg.h>
#include "object.h"
#include "sys.h"
#include "runtime.h"
#include "heap.h"
#include "symbol.h"
#include "port.h"
#include "io.h"
#include "proc.h"
#include "logical.h"
#include "number.h"
#include "string.h"
#include "list.h"
#include "vector.h"
#include "compiler.h"
#include "macros.h"
#include "table.h"

extern void init_scheme(long heap_kbytes,long stack_kbytes,long code_kbytes);
extern object execute_file(char *filename);

#endif






