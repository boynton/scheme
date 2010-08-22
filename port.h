/* LeeScheme/port.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

#ifndef _PORT_
#define _PORT_


typedef int (*char_ready_proc_pointer)(object inport);
typedef int (*getc_proc_pointer)(object inport);
typedef void (*putc_proc_pointer)(object outport,int c);
typedef void (*flush_proc_pointer)(object outport);
typedef void (*close_proc_pointer)(object port);

typedef struct port_heap_structure {
    long header;
    char_ready_proc_pointer ready_proc;
    getc_proc_pointer getc_proc;
    putc_proc_pointer putc_proc;
    flush_proc_pointer flush_proc;
    close_proc_pointer close_proc;
    void *stream;       /* for file and pipe ports */
    object buffer;      /* for string and terminal ports */
    long position;      /* for string ports */
    long char_buf;      /* To implement general ungetc */
    long flags;
#define PORT_FLAG_READ 1
#define PORT_FLAG_WRITE 2
#define PORT_FLAG_STRING 4
#define PORT_FLAG_CLOSED 8
#define PORT_FLAG_COOKED 16
    /*        char name[]; */
} *port_pointer;

#define PORT_P(obj) (POINTER_TYPE_P(obj,PORT_TYPE))
#define PORT_READY_PROC(obj) (((port_pointer)(obj))->ready_proc)
#define PORT_GETC_PROC(obj) (((port_pointer)(obj))->getc_proc)
#define PORT_PUTC_PROC(obj) (((port_pointer)(obj))->putc_proc)
#define PORT_FLUSH_PROC(obj) (((port_pointer)(obj))->flush_proc)
#define PORT_CLOSE_PROC(obj) (((port_pointer)(obj))->close_proc)
#define PORT_FLAGS(obj) (((port_pointer)(obj))->flags)
#define PORT_CHAR_BUF(obj) (((port_pointer)(obj))->char_buf)
#define PORT_READABLE_P(obj) (PORT_FLAGS(obj) & PORT_FLAG_READ)
#define PORT_WRITABLE_P(obj) (PORT_FLAGS(obj) & PORT_FLAG_WRITE)
#define PORT_STRING_P(obj) (PORT_FLAGS(obj) & PORT_FLAG_STRING)
#define INPUT_PORT_P(obj) (PORT_P(obj) && PORT_READABLE_P(obj))
#define OUTPUT_PORT_P(obj) (PORT_P(obj) && PORT_WRITABLE_P(obj))
#define PORT_STREAM(obj) (((port_pointer)(obj))->stream)
#define PORT_BUFFER(obj) (((port_pointer)(obj))->buffer)
#define PORT_POSITION(obj) (((port_pointer)(obj))->position)
#define PORT_NAME(obj) ((char *)((obj) + sizeof(struct port_heap_structure)))

extern object curin_port(void);
extern object curout_port(void);

extern object open_output_file(char *name);
extern object open_input_file(char *name);

extern object open_input_string(object a_string);
extern object open_output_string(object a_string);


extern void close_port(object the_port);
extern void close_stale_ports(void); /* called by the garbage collector */
extern void close_stale_port(object p);

extern object make_port(void * stream, long flags, char *name);

extern void init_port(void);

#endif
