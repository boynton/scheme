/* LeeScheme/heap.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _HEAP_
#define _HEAP_

#include "object.h"

extern void garbage_collect(long min_bytes);

#ifdef SPARC
#define IS_ALIGNED(p) ((((unsigned)(p)) & 7) == 0)
#define ALIGN_BYTE_POINTER(n) ((n + 0x00000007) & 0xfffffff8)
#define ALIGN_LONG_POINTER(n) ALIGN_BYTE_POINTER(n)
#define FILL_TO_ALIGN(p,v) if (!IS_ALIGNED(p)) *p = unbound_object;
#else
#ifdef X86_64
#define IS_ALIGNED(p) ((((unsigned long)(p)) & 7) == 0)
#define ALIGN_BYTE_POINTER(n) ((n + 0x0000000000000007) & 0xfffffffffffffff8)
#define ALIGN_LONG_POINTER(n) (n)
#define FILL_TO_ALIGN(p,v)
#else
#ifdef AMD64
#define IS_ALIGNED(p) ((((unsigned long)(p)) & 7) == 0)
#define ALIGN_BYTE_POINTER(n) ((n + 0x0000000000000007) & 0xfffffffffffffff8)
#define ALIGN_LONG_POINTER(n) (n)
#define FILL_TO_ALIGN(p,v)
#else
#define IS_ALIGNED(p) ((((unsigned)(p)) & 3) == 0)
#define ALIGN_BYTE_POINTER(n) ((n + 0x00000003) & 0xfffffffc)
#define ALIGN_LONG_POINTER(n) (n)
#define FILL_TO_ALIGN(p,v)
#endif
#endif
#endif

extern object make_heap_object(long type, long size);
extern void garbage_collect(long min_bytes);

extern void *allocate_code_space(long byte_count);

typedef struct forwarded_heap_structure {
    long header;
    object pointer;
} *forwarded_pointer;
#define FORWARDED_P(obj) (POINTER_TYPE_P(obj,FORWARDED_TYPE))
#define FORWARDED_POINTER(obj) (((forwarded_pointer)(obj))->pointer)

extern void init_heap(long heap_kbytes, long stack_kbytes, long code_kbytes);

extern char *heap, *heap_pointer, *heap_end, *max_memory;
extern object stack[];
extern object *stack_top, *stack_bottom;
extern object *sp;

#define CHECK_STACK() {\
    if ((sp) > stack_top) error(unbound_object,"stack underflow"); \
    else if ((sp) < stack_bottom) error(unbound_object,"stack overflow"); \
}

extern object gc_tmp1, gc_tmp2, gc_tmp3, gc_tmp4, gc_tmp5;

extern object **gc_root_stack_pointer;
#define PUSH_GC_PROTECT(var) (*gc_root_stack_pointer++ = &(var))
#define POP_GC_PROTECT(c) (gc_root_stack_pointer -= (c))

extern void gc_save_root_enumeration(void);
extern void gc_restore_root_enumeration(void);

extern void (*will_gc_hook)(void);
extern void (*did_gc_hook)(void);

#endif

