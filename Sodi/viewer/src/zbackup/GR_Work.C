/*********************************************************************

  11/06/92, Tung: A work dialog used to report work progress.
  06/22/93: add an argument to the work_progress function --
            the parent widget, instead of hard-coded gwindow;

*********************************************************************/
#include "GR_work.H"

void
work_progress (int workstate, char* msgstr, Widget parent)
{
   static Widget wdialog=NULL;
   char str[80];
   XmString  msgtext, wait;
   Arg      args[2];
   Display *dpy = XtDisplay (GR_toplevel);

   switch (workstate)
   {
     case 0: // create dialog and display the first message:
      if (parent)
      {      
	 wdialog = XmCreateWorkingDialog (parent,"work",args,0);
	 XtUnmanageChild (XmMessageBoxGetChild(wdialog,XmDIALOG_OK_BUTTON));
	 XtSetSensitive (XmMessageBoxGetChild(wdialog,XmDIALOG_CANCEL_BUTTON),
			 False);
	 XtUnmanageChild (XmMessageBoxGetChild(wdialog,XmDIALOG_HELP_BUTTON));
	 XtManageChild (wdialog);
	 XtPopup (XtParent(wdialog), XtGrabNone);
	 msgtext = XmStringCreateSimple (msgstr);
	 wait = XmStringCreateSimple ("Please Wait");
	 XtSetArg (args[0], XmNmessageString, msgtext);
	 XtSetArg (args[1], XmNcancelLabelString, wait);
	 XtSetValues (wdialog, args, 2);
      }
      else
	fprintf (stderr, "Error: NULL widget?\007\n");
      break;
    case 1:  // display working progress report messages:
      sprintf (str, "Progress: %s", msgstr);
      printf ("%s\n", str);
      msgtext = XmStringCreateSimple (str);
      XtSetArg (args[0], XmNmessageString, msgtext);
      XtSetValues (wdialog, args, 1);
      break;
    case 2: // display done message:
      sprintf (str, "Progress: All Done");
      msgtext = XmStringCreateSimple (str);
      XtSetArg (args[0], XmNmessageString, msgtext);
      XtSetValues (wdialog, args, 1);
      break;
    case 3: // destroy dialog:
      XtPopdown (XtParent(wdialog));
      XtDestroyWidget(wdialog);
      break;
    default:
      printf ("Warning: Did you forget to reset workstate?\007\n");
      break;
   }
   if (workstate < 4)
   {
      XFlush (dpy);
      XmUpdateDisplay (GR_toplevel);
   }   
}
