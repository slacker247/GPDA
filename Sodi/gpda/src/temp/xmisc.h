#ifndef ERROR
#define ERROR	(-1)
#endif

#ifndef OK
#define OK	0
#endif

#ifndef YES
#define YES	1
#endif

#ifndef NO
#define NO	0
#endif

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

/*----------------------------------------------------------------------*/

/* xmisc.c */
void xfl_set_input_time(FL_OBJECT *obj, time_t ts);
void xfl_set_input_long(FL_OBJECT *obj, long value);
void xfl_set_input_short(FL_OBJECT *obj, short value);
void xfl_deactivate_button(FL_OBJECT *obj);
void xfl_activate_button(FL_OBJECT *obj);
void xfl_trace_event(char *func, FL_OBJECT *o, int event, FL_Coord mx, FL_Coord my, int key, void *xev);
