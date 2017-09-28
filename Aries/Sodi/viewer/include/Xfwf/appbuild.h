/* Joel N. Weber II <nemo@koa.iolani.honolulu.hi.us>
 * Public Domain
 *
 * This file declares utility functions used by appbuild.  They may or
 * may not be useful otherwise.
 */

/* This function is used to get out of modal event handling loops.  You pass
 * it the address of a variable it should change.
 */

void XfwfAppSetToOne(Widget w, XtPointer closure, XtPointer call_data);
void XfwfAppDie(Widget w, XtPointer closure, XtPointer call_data);
Widget XfwfOpenApplication(XtAppContext *app, WidgetClass shell, char *class,
	int *argc, char *argv[], String *fallback);
