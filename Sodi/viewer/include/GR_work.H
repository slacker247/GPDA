/*********************************************************************

  11/06/92, Tung: A work dialog used to report work progress.
  06/22/93: add an argument to the work_progress function --
            the parent widget, instead of hard-coded gwindow;

*********************************************************************/
#ifndef GR_WORK
#define GR_WORK				"GR_workclass"
#include <stdio.h>
#include <Xm/MessageB.h>
#include "GR_Interface.H"

#define WP_CREATE	0
#define WP_UPDATE	1
#define WP_DONE		2
#define WP_DESTROY	3

void work_progress (int workstate, char* msgstr, Widget parent = GR_toplevel);

#endif
