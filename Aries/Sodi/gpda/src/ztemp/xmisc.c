#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include "forms.h"

#include "xmisc.h"

/*----------------------------------------------------------------------*/

void xfl_set_input_time(FL_OBJECT *obj, time_t ts)
{
	if (ts == 0)
	{
		fl_set_input(obj, "");
	}
	else
	{
		char buf [sizeof("mm/dd/yy hh:mm:ss")+10];
		struct tm *tp = localtime(&ts);

		strftime(buf, sizeof(buf), "%D %T", tp);
		fl_set_input(obj, buf);
	}
}

/*----------------------------------------------------------------------*/

void xfl_set_input_long(FL_OBJECT *obj, long value)
{
	char buf [sizeof("12345678901234567890")];

	sprintf(buf, "%ld", value);
	fl_set_input(obj, buf);
}

/*----------------------------------------------------------------------*/

void xfl_set_input_short(FL_OBJECT *obj, short value)
{
	char buf [sizeof("12345678901234567890")];

	sprintf(buf, "%hd", value);
	fl_set_input(obj, buf);
}

/*----------------------------------------------------------------------*/

void xfl_deactivate_button(FL_OBJECT *obj)
{
		if (obj->lcol != FL_INACTIVE_COL)
			obj->u_ldata = obj->lcol;

		fl_deactivate_object(obj);
		fl_set_object_lcol(obj, FL_INACTIVE_COL);
}

/*----------------------------------------------------------------------*/

void xfl_activate_button(FL_OBJECT *obj)
{
		fl_set_object_lcol(obj, obj->u_ldata);
		fl_activate_object(obj);
}

/*----------------------------------------------------------------------*/

static char *Events[] =
{
	"FL_NOEVENT",
	"FL_DRAW",
	"FL_PUSH",
	"FL_RELEASE",
	"FL_ENTER",
	"FL_LEAVE",
	"FL_MOUSE",
	"FL_FOCUS",
	"FL_UNFOCUS",
	"FL_KEYBOARD",
	"FL_MOTION",
	"FL_STEP",
	"FL_SHORTCUT",
	"FL_FREEMEM",
	"FL_OTHER",
	"FL_DRAWLABEL",
	"FL_DBLCLICK",
	"FL_TRPLCLICK",
	"FL_ATTRIB",
	"FL_PS"
};

/*----------------------------------------------------------------------*/

void xfl_trace_event(
char *func,
FL_OBJECT *o,
int event,
FL_Coord mx,
FL_Coord my,
int key,
void *xev)
{
	switch (event)
	{
	case FL_MOTION:
	case FL_MOUSE:
	case FL_DRAW:
		return;

	default:
		fprintf(stderr,	"%s: %s mx %d my %d ox %d oy %d "
			"key %d label %s\n",
			func, Events[event], mx, my, o->x, o->y,
			key, o->label);
	}
}
