/* LeeScheme/heap.c - Copyright (C) Lee Richard Boynton, 1993-2000. */

#include "scheme.h"
#include <string.h>
#include <stdio.h>

/* The global heap */

char *min_memory, *max_memory;
char *heap, *heap_pointer, *heap_end, *max_heap, *code_pointer;
long heap_size;

void (*will_gc_hook)(void);
void (*did_gc_hook)(void);

/*
 *		  Object allocation and collection
 */

long stack_size;
object *stack_buf, *stack_bottom, *stack_top;
object *sp;

#define GC_ROOT_STACK_MAX (500)
object gc_root_stack_buffer;
object **gc_root_stack_begin,**gc_root_stack_pointer, **gc_root_stack_end;
static long gc_root_stack_count;

object gc_tmp1;
object gc_tmp2;
object gc_tmp3;
object gc_tmp4;
object gc_tmp5;

void gc_save_root_enumeration(void) {
    gc_root_stack_count = gc_root_stack_pointer - gc_root_stack_begin;
}

void gc_restore_root_enumeration(void) {
    gc_root_stack_pointer = gc_root_stack_begin + gc_root_stack_count;
}

void migrate_object(object obj) {
    object new;
    object *p;
    long i, max;
 loop:
    if (!POINTER_P(obj) || FORWARDED_P(obj))
        return;
    new = (object)heap_pointer;
    i = POINTER_LENGTH(obj);
    memcpy(heap_pointer,(void *)obj,i);
    heap_pointer += i;
    POINTER_HEADER(obj) = (POINTER_LENGTH(obj) << 8) + FORWARDED_TYPE;
    FORWARDED_POINTER(obj) = new;
    obj = new;
    switch (POINTER_TYPE(obj)) {
    case PAIR_TYPE:
        migrate_object(CAR(obj));
        obj = CDR(obj);
        goto loop;
    case SYMBOL_TYPE:
        obj = SYMBOL_VALUE(obj);
        goto loop;
    case VECTOR_TYPE:
        max = VECTOR_LENGTH(obj);
        p = VECTOR_ELEMENTS(obj);
        for (i=0; i<max; i++)
            migrate_object((object)*p++);
        obj = VECTOR_TAG(obj);
        goto loop;
    case PROCEDURE_TYPE:
        obj = (object)PROC_MODULE(obj);
        goto loop;
    case FRAME_TYPE:
        migrate_object((object)FRAME_ENV(obj));
        max = (POINTER_LENGTH(obj) - sizeof(struct frame_heap_structure))/sizeof(long);
        p = FRAME_ELEMENTS(obj);
        for (i=0; i<max; i++)
            migrate_object((object)*p++);
        obj = (object)FRAME_PREVIOUS(obj);
        goto loop;
    case CLOSURE_TYPE:
        migrate_object((object)CLOSURE_PROC(obj));
        obj = (object)CLOSURE_ENV(obj);
        goto loop;
    case CONTINUATION_TYPE:
        max = CONTINUATION_STACKSIZE(obj);
        p = CONTINUATION_STACK(obj);
        for (i=0; i<max; i++)
            migrate_object((object)*p++);
        obj = (object)CONTINUATION_FRAME(obj);
        goto loop;
    case SYMBOLTABLE_TYPE:
        max = SYMBOLTABLE_SIZE(obj);
        p = VECTOR_ELEMENTS(SYMBOLTABLE_MAPPINGS(obj));
        for (i=0; i<max; i++) {
            if (!UNBOUND_P(p[i])) { 
                if (SYMBOL_P(p[i]) && !UNBOUND_P(SYMBOL_VALUE(p[i])))
                    migrate_object(p[i]);
            } else
                p[i] = unbound_object;
        }
        migrate_object(SYMBOLTABLE_MAPPINGS(obj));
        break;
    case PORT_TYPE:
        obj = PORT_BUFFER(obj);
        goto loop;
    case WEAK_TYPE:
    case BUFFER_TYPE:
    case SIGNAL_TYPE:
        break;
    }
}

int gc_count = 0;
void garbage_collect(long min_space) {
    char *p;
    object **gcp;
    object *op;
    long i, max, count;
    int old_interrupt;
    
    if (*will_gc_hook) (*will_gc_hook)();
    old_interrupt = enable_interrupts(0);
    /* switch heap space */
    gc_count++;
    printf("[GC]\n");
    heap += heap_size;
    if (heap >= max_heap)
	heap = min_memory;
    heap_pointer = heap;
    heap_end = heap + heap_size;
    /* migrate objects */
    count = gc_root_stack_pointer - gc_root_stack_begin;
    migrate_object(gc_root_stack_buffer);
    if (FORWARDED_P(gc_root_stack_buffer)) gc_root_stack_buffer = FORWARDED_POINTER(gc_root_stack_buffer);
    gc_root_stack_begin = (object **)BUFFER_DATA(gc_root_stack_buffer);
    gc_root_stack_end = gc_root_stack_begin + GC_ROOT_STACK_MAX;
    gc_root_stack_pointer = gc_root_stack_begin + count;
    gcp = gc_root_stack_begin;
    for (i=0; i<count; i++)
	migrate_object(*gcp[i]);
    for (op = sp; op < stack_top; op++)
	migrate_object(*op);
    /* eliminate forwarding pointers */
    gcp = gc_root_stack_begin;
    for (i=0; i<count; i++) {
	object o = *gcp[i];
	if (FORWARDED_P(o))
	    *gcp[i] = FORWARDED_POINTER(o);
    }
    for (op = sp; op < stack_top; op++) {
	object o = *op;
	if (FORWARDED_P(o))
	    *op = FORWARDED_POINTER(o);
    }
    p = heap;
    while (p < heap_pointer) {
	object *q, obj, o;
	obj = (object)p;
	switch (POINTER_TYPE(obj)) {
	case PAIR_TYPE:
	    o = CAR(obj); if (FORWARDED_P(o)) CAR(obj) = FORWARDED_POINTER(o);
	    o = CDR(obj); if (FORWARDED_P(o)) CDR(obj) = FORWARDED_POINTER(o);
	    break;
	case WEAK_TYPE:
	    if (FORWARDED_P(WEAK_VALUE(obj))) {
		WEAK_BOUND(obj) = 1;
	    } else {
		WEAK_BOUND(obj) = 0;
		migrate_object(WEAK_VALUE(obj));
	    }
	    o = WEAK_VALUE(obj); if (FORWARDED_P(o)) WEAK_VALUE(obj) = FORWARDED_POINTER(o);
	    break;
	case SYMBOL_TYPE:
	    o = SYMBOL_VALUE(obj); if (FORWARDED_P(o)) SYMBOL_VALUE(obj) = FORWARDED_POINTER(o);
	    break;
	case VECTOR_TYPE:
	    max = VECTOR_LENGTH(obj);
	    q = VECTOR_ELEMENTS(obj);
	    for (i=0; i<max; i++) {
		o = q[i]; if (FORWARDED_P(o)) q[i] = FORWARDED_POINTER(o);
	    }
	    o = VECTOR_TAG(obj); if (FORWARDED_P(o)) VECTOR_TAG(obj) = FORWARDED_POINTER(o);
	    break;
	case PROCEDURE_TYPE:
	    o = PROC_MODULE(obj); if (FORWARDED_P(o)) PROC_MODULE(obj) = FORWARDED_POINTER(o);
	    break;
	case FRAME_TYPE:
	    o = FRAME_PREVIOUS(obj); if (FORWARDED_P(o)) FRAME_PREVIOUS(obj) = FORWARDED_POINTER(o);
	    o = FRAME_ENV(obj); if (FORWARDED_P(o)) FRAME_ENV(obj) = FORWARDED_POINTER(o);
	    max = (POINTER_LENGTH(obj) - sizeof(struct frame_heap_structure))/sizeof(long);
	    q = FRAME_ELEMENTS(obj);
	    for (i=0; i<max; i++) {
		o = q[i]; if (FORWARDED_P(o)) q[i] = FORWARDED_POINTER(o);
	    }
	    break;
	case CLOSURE_TYPE:
	    o = CLOSURE_PROC(obj); if (FORWARDED_P(o)) CLOSURE_PROC(obj) = FORWARDED_POINTER(o);
	    o = CLOSURE_ENV(obj); if (FORWARDED_P(o)) CLOSURE_ENV(obj) = FORWARDED_POINTER(o);
	    break;
	case CONTINUATION_TYPE:
	    o = CONTINUATION_FRAME(obj); if (FORWARDED_P(o)) CONTINUATION_FRAME(obj) = FORWARDED_POINTER(o);
	    max = CONTINUATION_STACKSIZE(obj);
	    q = CONTINUATION_STACK(obj);
	    for (i=0; i<max; i++) {
		o = q[i]; if (FORWARDED_P(o)) q[i] = FORWARDED_POINTER(o);
	    }
	    break;
	case SYMBOLTABLE_TYPE:
	    o = SYMBOLTABLE_MAPPINGS(obj); if (FORWARDED_P(o)) SYMBOLTABLE_MAPPINGS(obj) = FORWARDED_POINTER(o);
	    break;
	case PORT_TYPE:
	    o = PORT_BUFFER(obj); if (FORWARDED_P(o)) PORT_BUFFER(obj) = FORWARDED_POINTER(o);
	    break;
	}
	p += POINTER_LENGTH(obj);
    }
    /* finalization of ports */
    close_stale_ports();
    fix_runtime_pointers();
    /* Finish up */
    enable_interrupts(old_interrupt);
    i = heap_size - (heap_pointer - heap);
    if (i < min_space)
	fatal_error("out of heap space: %d\n", i);
    if (*did_gc_hook) (*did_gc_hook)();
}

object make_heap_object(long type, long size) {
    long i = ALIGN_BYTE_POINTER(size);
    void *h = (void *)heap_pointer;
    heap_pointer += i;
    if (heap_pointer >= heap_end) {
        garbage_collect(size);	
        h = (void *)heap_pointer;
        heap_pointer += i;
    }
    POINTER_HEADER(h) = (i << 8) + type;
    return (object)h;
}

object weak_binding(object value) {
    object p;
    gc_tmp1 = value;
    p = make_heap_object(WEAK_TYPE,sizeof(struct pair_heap_structure));
    WEAK_BOUND(p) = true_object;
    WEAK_VALUE(p) = gc_tmp1;
    return p;
}

object make_buffer(long capacity) {
    long size = sizeof(struct buffer_heap_structure) + capacity;
    object result = make_heap_object(BUFFER_TYPE,size);
    BUFFER_LENGTH(result) = 0;
    return result;
}

object grow_buffer(object old, long new_capacity) {
    long old_capacity = BUFFER_CAPACITY(old);
    long old_length = BUFFER_LENGTH(old);
    if (old_capacity >= new_capacity) return old;
    gc_tmp1 = old;
    gc_tmp2 = make_buffer(new_capacity);
    memcpy(BUFFER_DATA(gc_tmp2),BUFFER_DATA(gc_tmp1),old_length);
    BUFFER_LENGTH(gc_tmp2) = BUFFER_LENGTH(old);
    return gc_tmp2;
}

void *allocate_code_space(long byte_count) {
    char *result = code_pointer;
    code_pointer += byte_count;
    if (code_pointer >= (char *)stack_bottom)
        fatal_error("out of code space!\n");
    return result;
}

void init_heap(long heap_kbytes, long stack_kbytes, long code_kbytes) {
    long i, code_size;
    long hemiCount = 2;
    will_gc_hook = did_gc_hook = NULL;
    heap_size = heap_kbytes * 1024;
    stack_size = stack_kbytes * 1024;
    code_size = code_kbytes * 1024;
    i = (hemiCount * heap_size) + stack_size + code_size;
    heap = allocate_memory(i + sizeof(long));
    if (!heap)
        fatal_error("Cannot allocate %ld bytes for the heap!", i);
    /* printf("INIT: heap = %x\n",heap); */
    min_memory = heap = (void *)ALIGN_BYTE_POINTER((long)heap);
    max_memory = heap + i;
    heap_pointer = heap;
    heap_end = heap + heap_size;
    max_heap = heap + (hemiCount * heap_size);
    code_pointer = max_heap;
    stack_bottom = (object *)(code_pointer + code_size);
    stack_top = (object *)((long)stack_bottom + stack_size);
    sp = stack_top;
    i = (long)GC_ROOT_STACK_MAX * (long)sizeof(object *);
    gc_root_stack_buffer = make_buffer(i);
    gc_root_stack_begin = (object **)BUFFER_DATA(gc_root_stack_buffer);
    gc_root_stack_end = gc_root_stack_begin + i;
    gc_root_stack_pointer = gc_root_stack_begin;
    gc_tmp1 = null_object;
    gc_tmp2 = null_object;
    gc_tmp3 = null_object;
    gc_tmp4 = null_object;
    gc_tmp5 = null_object;
    PUSH_GC_PROTECT(gc_tmp1);
    PUSH_GC_PROTECT(gc_tmp2);
    PUSH_GC_PROTECT(gc_tmp3);
    PUSH_GC_PROTECT(gc_tmp4);
    PUSH_GC_PROTECT(gc_tmp5);
}

