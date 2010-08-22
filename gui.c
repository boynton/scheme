/* LeeScheme/gui.c - Copyright (C) Lee Richard Boynton, 1993-2000. */
#include "scheme.h"
#include "gui.h"


static void primop_make_window(long argc) {
	char *title;
	TYPE_CHECK(STRING_P(sp[0]),1,"string",sp[0]);
	title = STRING_VALUE(*sp++);
	handle = gui_create_window(title, 10, 10, 320, 240)
}


void init_gui(void) {
	define_primop("gui:make-window",primop_make_window,1,6);

}

