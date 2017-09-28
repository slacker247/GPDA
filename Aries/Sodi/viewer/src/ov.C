/************************************************************
  ov.C is a object viewer attached to the rsd.C.
  
  -- 01/25/93: created by Tung; 
  -- 06/14/93: changed "Done" to "Quit";
  
************************************************************/

#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>

#include "GR_Shell.H"
#include "GR_Model.H"

#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
//#include "Xfwf/Cmap.h"

struct objinfo {
    int        id;
    int        icontype;
    double     rmin;
    double     rmax;
    double     px;
    double     py;
    double     pz;
    double     vx;
    double     vy;
    double     vz;
};

Widget         ov_shell;
GR_DispList    *ov_displist;
GR_Window      *ovwindow;
Boolean        ov_active = FALSE;
Boolean        first_objectviewer = TRUE;  
GR_Model       *objmodels;
int            modeltype = 1;   // some default type;
Widget         count_box;
char           count_text[10];
float          object_factor = 1.0;

int            objcount = 0;
struct objinfo objects[200];

void object_viewerCB (Widget);
void ov_doneCB ();
void object_textCB ();
void count_textCB ();
void ov_listCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void ov_sliderCB (Widget, char client_data, XmScaleCallbackStruct* call_data);

void
ov_init ()
{
   Widget ov_form;
   Widget ov_list; 
   Widget ov_frame;
   Widget ov_control, done_button;
   Widget ov_cmap;
   XmString *xstr, title;
   int i;
   int count; 
   char line[80]; 
   float px, py, pz;


   if (objmodels == NULL)
   {
      objmodels = new GR_Model;
      objmodels->parse_model ("Models.desc");
   }
   count = (int)(objmodels->get_p_files_read ());
   xstr = (XmString*)malloc(count*sizeof(XmString));
      
   for (i=0; i<count; i++)
   {
      sprintf (line, "%5d  %s", 
               objmodels->get_p_filetypes(i),
               objmodels->get_p_filenames(i));
      xstr[i] = XmStringCreate (line, XmSTRING_DEFAULT_CHARSET);        
   }
   
   ov_shell = XtCreatePopupShell("ObjectViewer", topLevelShellWidgetClass,
                                GR_toplevel, 0, 0);
   ov_form = XmCreateForm (ov_shell, "OVForm", NULL, 0);

   ov_control = XtVaCreateManagedWidget ("OVControl", xmRowColumnWidgetClass, ov_form,
                XmNleftAttachment,   XmATTACH_FORM,
                XmNbottomAttachment, XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_FORM,
                XmNheight,           40,
                XmNtopOffset,        10,
                XmNleftOffset,       10,
                XmNspacing,          10,
                XmNorientation,      XmHORIZONTAL,
                XmNpacking,          XmPACK_COLUMN,
                XmNnumColumns,       1,
                NULL);

   ov_list = XmCreateScrolledList (ov_form, "OVList", NULL, 0);
   XtVaSetValues (ov_list,
                XmNitems,            xstr,
                XmNitemCount,        count,
                XmNvisibleItemCount, 20,
                NULL);
   XtAddCallback (ov_list, XmNbrowseSelectionCallback, (XtCallbackProc)ov_listCB, NULL);
   XtVaSetValues (XtParent(ov_list),
                XmNtopAttachment,    XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_FORM,
                XmNbottomAttachment, XmATTACH_WIDGET,
                XmNbottomWidget,     ov_control, 
                NULL);

   ov_frame = XtVaCreateManagedWidget ("OVFrame", xmFrameWidgetClass, ov_form,
               XmNshadowType,        XmSHADOW_IN,
               XmNtopAttachment,     XmATTACH_FORM,
               XmNleftAttachment,    XmATTACH_FORM,
               XmNbottomAttachment,  XmATTACH_WIDGET,
               XmNbottomWidget,      ov_control,
               XmNrightAttachment,   XmATTACH_WIDGET,
               XmNrightWidget,       XtParent(ov_list),
               NULL);

   done_button = XtVaCreateManagedWidget ("Quit", xmPushButtonWidgetClass, ov_control,
               XmNwidth,             20,
               XmNheight,            30,
               XmNshadowThickness,   4,
               NULL);
   XtAddCallback (done_button, XmNactivateCallback, (XtCallbackProc)ov_doneCB, NULL);
 
   count_box = XtVaCreateManagedWidget("Count", xmTextFieldWidgetClass, ov_control,
               XmNwidth,             30,
               XmNheight,            30,
               XmNshadowThickness,   3,
               NULL);
   XtAddCallback (count_box, XmNactivateCallback, (XtCallbackProc)count_textCB, NULL);
   sprintf (count_text, " %d", objcount);
   XtVaSetValues (count_box, XmNvalue, count_text, NULL);
   /*
   ov_cmap = XtVaCreateManagedWidget("Count", cmapWidgetClass, ov_form,
               XmNwidth,             300,
               XmNheight,            300,
               XmNshadowThickness,   3,
               XmNtopAttachment,     XmATTACH_FORM,
               XmNleftAttachment,    XmATTACH_WIDGET,
               XmNleftWidget,        ov_control,
               NULL);
   */
   //XmStringFree (title);

   XtManageChild (ov_form);
   XtManageChild (ov_list);

   XtPopup(ov_shell, XtGrabNone);
}

void 
object_viewerCB (Widget toplevel)
{
   if (first_objectviewer)
   {
      GR_toplevel = toplevel;
      first_objectviewer = FALSE;
      ov_init ();
      ov_active = TRUE;
   }
   else
   {
       XtPopup(ov_shell, XtGrabNone);
       ov_active = TRUE;
   }
}


void
ov_doneCB ()
{
   ov_active = FALSE;
   XtPopdown (ov_shell);
}

void
count_textCB ()
{
   sprintf (count_text, " %d", objcount);
   XtVaSetValues (count_box, XmNvalue, count_text, NULL);
}

void
ov_listCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
char *text;

  XmStringGetLtoR (call_data->item,
                   XmSTRING_DEFAULT_CHARSET,
                   &text);
}
