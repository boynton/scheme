/* LeeScheme/sys.h - Copyright (C) Lee Richard Boynton, 1993-2000. */

/* system-independent layer for memory, filesystem, etc. */

#ifndef _SYS_
#define _SYS_

#define MAX_FILENAME_LENGTH (1024)

extern void init_system(void);

extern int enable_interrupts(int flag); /* returns the previous flag */
extern void quit(long exit_code);

extern void *allocate_memory(long size);
extern void free_memory(void *ptr);

extern void get_home_directory(char *buffer);
extern void expand_path(char *buf, char *name);
extern void convert_path(char *buf);
extern int does_file_exist(char *name);
extern void remove_file(char *name);

object open_input_file(char *path);
object open_output_file(char *path);
char *newline_string(object port);

extern void msec_sleep(long msec);
extern long msec_since_launch(void);

extern int execute_system_command(char *the_command);

extern void check_interrupts(void);

#endif /* SYS  */
