/*		@(#)GR_Shell.C	1.5		2/5/93		*/

#include "GR_Shell.H"
#include <X11/Shell.h>
#include "GR_Interface.H"

Widget
GR_Shell::v_createWidget (char *name, Widget)
{
	Arg args[10];
	static int first_time = 1;
	Widget w;

	if (first_time)
	{
		first_time = 0;
		w = GR_toplevel;
	}
	else
	{
		//XtSetArg (args[0], XmNdeleteResponse, XmDO_NOTHING);
		w = XtAppCreateShell ( 	name, NULL, 
					topLevelShellWidgetClass, 
					XtDisplay (GR_toplevel), 
					0, 0);
					//args, 1);
	}
	return w;
}
