#include "scheme.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/*
DO: make state attached to the socket, not global!
use socket_port_ready_p to hook into the poll-for-interrupt stuff.
	-> this will require an extra input queue (of, say, 32 chars)
	-> this should be done globally, from repld.scm.
	-> i.e. (system:set-interrupt-check-hook client)
line editing, emacs-style, including esc-f and the like
command history, bash-style
paren matching
tab completion? should just call out to scheme, since that is app-dependent
*/

#define CTRL_A 1
#define CTRL_B 2
#define CTRL_C 3
#define CTRL_D 4
#define CTRL_E 5
#define CTRL_F 6
#define CTRL_G 7
#define CTRL_H 8
#define TAB 9
#define LINEFEED 10
#define CTRL_K 11
#define CTRL_L 12
#define RETURN 13
#define CTRL_N 14
#define CTRL_O 15
#define CTRL_P 16
#define CTRL_Q 17
#define CTRL_R 18
#define CTRL_S 19
#define CTRL_T 20
#define CTRL_U 21
#define CTRL_V 22
#define CTRL_W 23
#define CTRL_X 24
#define CTRL_Y 25
#define CTRL_Z 26
#define ESCAPE 27
#define DELETE 127



static char buf[1024];
static int max=1022, in=0, eol=0, out=0, cursor=0, history=0;
static int static_meta = 0;
/*static char *break_chars=" ();";*/

static int nextChar(object p) {
    /* printf("next: cursor = %d, in = %d, out = %d, eol = %d\n", cursor, in, out, eol); */
	if (out < eol)
		return buf[out++];
	if (in == out)
		in = out = eol = cursor = 0;
	return 0;
}

static void beep(object p) {
	write_character(p, CTRL_G);
}

void add_to_history(char *line) {
	object s = make_string(line);
	object p = null_object;
	PUSH_GC_PROTECT(s);
	PUSH_GC_PROTECT(p);
	p = global("system:*edlin-history*");
	if (UNBOUND_P(p))
		p = null_object;
	p = cons(s, p);
	define("system:*edlin-history*", p);
	POP_GC_PROTECT(2);
}

int yank_from_history(int i, char *line) {
	object p = global("system:*edlin-history*");
	while (PAIR_P(p)) {
		if (--i <= 0) {
			object s = CAR(p);
			strcpy(line, STRING_VALUE(s));
			return 1;
		}
		p = CDR(p);
	}
	return 0;
}


static void getMoreData(object p) {
	int i;
	int c = (*(PORT_GETC_PROC(p)))(p);
	int meta = static_meta;
	static_meta = 0;
	switch (c) {
	case RETURN:
		buf[in] = 0;
		if (in > 0)
			add_to_history(buf);
		history = 0;
		buf[in++] = c;
		buf[in] = 0;
		write_character(p, RETURN);
		write_character(p, LINEFEED);
		eol = in;
		break;
	case CTRL_A:
		while (cursor > 0) {
			write_character(p, CTRL_H);
			cursor--;
		}
		meta = 0;
		break;
	case CTRL_B:
		if (cursor > 0) {
			write_character(p, CTRL_H);
			cursor--;
		}
		break;
	case CTRL_C:
		interrupt();
		break;
	case CTRL_D:
		if (cursor < in) {
			memmove(buf+cursor, buf+cursor+1, in-cursor);
			for (i=cursor; i<in; i++)
				write_character(p, buf[i]);
			write_character(p, ' ');
			for (i=cursor; i<in; i++)
				write_character(p, CTRL_H);
			in--;
		}
		break;
	case CTRL_E:
		while (cursor < in) {
			write_character(p, buf[cursor++]);
		}
		break;
	case CTRL_F:
		if (cursor < in)
			write_character(p, buf[cursor++]);
		break;
	case CTRL_G:
	    /* reset search mode, etc */
		break;
	case CTRL_H:
	case DELETE:
		if (cursor == 0)
			beep(p);
		else {
			if (meta) {
				printf("NYI: back delete a word\n");
			} else {
				if (cursor-- == in) {
					in--;
					write_character(p, CTRL_H);
					write_character(p, ' ');
					write_character(p, CTRL_H);
				} else {
					i = cursor;
					memmove(buf+cursor, buf+cursor+1, in-cursor);
					write_character(p, CTRL_H);
					in--;
					while (i < in)
						write_character(p, buf[i++]);
					write_character(p, ' ');
					write_character(p, CTRL_H);
					while (i-- > cursor)
						write_character(p, CTRL_H);
				}
			}
		}
		break;
	case TAB:
		printf("NYI: complete current token\n");
		/* inserting it, keeping state, so next tab gets next possiblity */
		break;
	case CTRL_K:
		for (i=cursor; i<in; i++)
			write_character(p, ' ');
		for (i=cursor; i<in; i++)
			write_character(p, CTRL_H);
		in = cursor;
		break;
	case CTRL_N:
		if (history > 0 && yank_from_history(history-1, buf)) {
			int oldin = in;
			for (i=cursor; i>0; i--)
				write_character(p, CTRL_H);
			history--;
			out = 0;
			cursor = in = strlen(buf);
			for (i=0; i<in; i++)
				write_character(p, buf[i]);
			for (i=in; i<oldin; i++)
				write_character(p, ' ');
			for (i=in; i<oldin; i++)
				write_character(p, CTRL_H);
		}
		break;
	case CTRL_P:
		if (yank_from_history(history+1, buf)) {
			int oldin = in;
			for (i=cursor; i>0; i--)
				write_character(p, CTRL_H);
			history++;
			out = 0;
			cursor = in = strlen(buf);
			for (i=0; i<in; i++)
				write_character(p, buf[i]);
			for (i=in; i<oldin; i++)
				write_character(p, ' ');
			for (i=in; i<oldin; i++)
				write_character(p, CTRL_H);
		}
		break;
	case ESCAPE:
		static_meta = 1;
		break;
	default:
		if (meta) {
			switch (c) {
			case 'f':
			    /*
				move forward a word
				while (cursor < in && strchar(break_chars,c) < 0) next char
			    */
				printf("NYI: forward cursor a word\n");
				break;
			case 'b':
				/*move back a word
				  while (cursor > 0 && strchar(break_chars,c) < 0) prev char */
				printf("NYI: back cursor a word\n");
				break;
			}
		} else {
			if (c < ' ' || in == max)
				beep(p);
			else {
				write_character(p, c);
				if (cursor == in) {
					buf[in++] = c;
					cursor++;
				} else {
					memmove(buf+cursor+1, buf+cursor, in-cursor);
					buf[cursor++] = c;
					in++;
					for (i=cursor; i<in; i++)
						write_character(p, buf[i]);
					for (i=cursor; i<in; i++)
						write_character(p, CTRL_H);
				}
				if (c == ')') {
					int j, k=0;
					for (j=cursor; j>=0; j--) {
						if (buf[j] == ')') k++;
						if (buf[j] == '(') {
							if (! --k)
								break;
						}
					}
					if (j >= 0) {	
						for (i=cursor; i>j; i--)
							write_character(p, CTRL_H);
						write_character(p, ' ');
						write_character(p, CTRL_H);
						msec_sleep(200);
						for (i=j; i<cursor; i++)
							write_character(p, buf[i]);

					} else {
						beep(p);
					}
				}
			}
		}
		break;
	}
}

int edlin_getc(object p) {
	int c = nextChar(p);
	while (!c) {
		getMoreData(p);
		c = nextChar(p);
	}
	return c;
}
