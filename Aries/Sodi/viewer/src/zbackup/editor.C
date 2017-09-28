/************************************************************
  mv.C is a model viewer attached to the rsd.C.
  
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
#include "GISP_Globals.H"

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

Widget         ed_shell;
int	       edwindW=680, edwindH=480;
Boolean        ed_first = TRUE;
extern int     n_rtns;
extern struct TRAIL rtninfo[MAXRTNS];
GR_Model       *ed_models;
unsigned short select_color[3];
int            select_icon, select_pos;
GC             gc;
Pixmap         pixmap;
Widget         ed_list, trk_list;
XmString       *rtnstr;
XmString       *newitems;
 
void track_editCB (Widget);
void ed_doneCB ();
void ed_addCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void ed_delCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void ed_modCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void ed_saveCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void ed_helpCB (Widget parent, XtPointer, XmListCallbackStruct* call_data);
void ed_listCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void trk_listCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void ed_sliderCB (Widget, char client_data, 
                  XmScaleCallbackStruct* call_data);
void set_colorCB(Widget widget, XtPointer client_data, XtPointer call_data);

extern float  get_scale (long type);
extern Widget createOneScale (Widget parent, char code, XmString, XtCallbackProc CB);
extern void   parse_model (char* filename, int type_wanted);
extern void   load_model (char* file_name, long type);


void
ed_init ()
{
Widget        ed_form;
Widget        ed_frame, ed_cmap;
Widget        ed_control, done_button, add_button, del_button, mod_button, help_button;
Widget        sav_button;
Widget        label01;
XmString      *xstr, title;
XColor        color, unused;
Pixel         bg_color, fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
Colormap      cmap;
Display       *dpy;
XFontStruct   *font;
XmFontList    fontlist;
int           i;
int           count; 
char          line[80]; 
float         px, py, pz;
Dimension     width, height;           /* dimensions of drawing area (pixmap) */
Widget        rc, pb;
XGCValues     gcv;
String        colors[] = {
    "Black", "Red", "Green", "Blue", "White", "Navy", "Orange", "Yellow",
    "Pink", "Magenta", "Cyan", "Brown", "Grey", "LimeGreen", "Turquoise",
    "Violet", "Wheat", "Purple" };


   if (ed_models == NULL) {
      ed_models = new GR_Model;
      ed_models->parse_model ("Models.desc");
   }
   count = (int)(ed_models->get_p_files_read ());
   xstr = (XmString*)malloc(count*sizeof(XmString));      
   for (i=0; i<count; i++) {
      sprintf (line, "%d  %s", 
               ed_models->get_p_filetypes(i),
               ed_models->get_p_filenames(i));
      xstr[i] = XmStringCreate (line,
                XmSTRING_DEFAULT_CHARSET);        
   }

   rtnstr = (XmString*)malloc(n_rtns*sizeof(XmString));      
   for (i=0; i<n_rtns; i++) {
      sprintf (line, "%5d %5d %5d %5d", rtninfo[i].icon,
                rtninfo[i].r, rtninfo[i].g, rtninfo[i].b);
      rtnstr[i] = XmStringCreate (line, XmSTRING_DEFAULT_CHARSET);        
   }

   newitems = (XmString*)malloc(2*sizeof(XmString));  

   
   ed_shell = XtCreatePopupShell("TrackEdit", topLevelShellWidgetClass, GR_toplevel, 0, 0);
   ed_form  = XmCreateForm (ed_shell, "MVForm", NULL, 0);
   XtVaSetValues(ed_form,
               XmNwidth,             edwindW,
               XmNheight,            edwindH,
               NULL);
   dpy = XtDisplay(GR_toplevel);
   font = XLoadQueryFont(dpy,
          "-adobe-courier-bold-*-*-*-20-*-*-*-*-*-*-*");
   fontlist = XmFontListCreate(font, "charset1");
/*
 *	Setup the various buttons
 *	-------------------------
 */ 
   done_button = XtVaCreateManagedWidget ("Quit", xmPushButtonWidgetClass, ed_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback, (XtCallbackProc)ed_doneCB, NULL);

   add_button = XtVaCreateManagedWidget ("Add", xmPushButtonWidgetClass, ed_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     done_button,
                 NULL);
   XtAddCallback (add_button, XmNactivateCallback, (XtCallbackProc)ed_addCB, NULL);

   del_button = XtVaCreateManagedWidget ("Delete",
                 xmPushButtonWidgetClass, ed_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     add_button,
                 NULL);
   XtAddCallback (del_button, XmNactivateCallback,
                    (XtCallbackProc)ed_delCB, NULL);

   mod_button = XtVaCreateManagedWidget ("Replace", xmPushButtonWidgetClass, ed_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     del_button,
                 NULL);
   XtAddCallback (mod_button, XmNactivateCallback, (XtCallbackProc)ed_modCB, NULL);

   sav_button = XtVaCreateManagedWidget ("Save", xmPushButtonWidgetClass, ed_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     mod_button,
                 NULL);
   XtAddCallback (sav_button, XmNactivateCallback, (XtCallbackProc)ed_saveCB, NULL);

   help_button = XtVaCreateManagedWidget ("Help", xmPushButtonWidgetClass, ed_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_FORM,
                 NULL);
   XtAddCallback (help_button, XmNactivateCallback, (XtCallbackProc)ed_helpCB, NULL);

   /* Create a GC for drawing (callback).  Used a lot -- make global */
   gcv.foreground = WhitePixelOfScreen (XtScreen (ed_form));
   gc = XCreateGC (XtDisplay (ed_form),
        RootWindowOfScreen (XtScreen (ed_form)), GCForeground, &gcv);

   /* Create a 3-column array of color tiles */
   rc = XtVaCreateWidget ("rc", xmRowColumnWidgetClass, ed_form,
        XmNnumColumns,      3,
        XmNpacking,         XmPACK_COLUMN,
        XmNtopAttachment,   XmATTACH_FORM,
        XmNrightAttachment, XmATTACH_FORM,
        NULL);
   for (i = 0; i < XtNumber(colors); i++) {
        /* Create a single tile (pixmap) for each color */
        pixmap = XCreatePixmap (XtDisplay (rc), 
            RootWindowOfScreen (XtScreen (rc)),
            16, 16, DefaultDepthOfScreen (XtScreen (rc)));
        set_colorCB(rc, colors[i], NULL); /* set the gc's color according to name */
        XFillRectangle (XtDisplay (ed_form), pixmap, gc, 0, 0, 16, 16);
        pb = XtVaCreateManagedWidget (colors[i], xmPushButtonWidgetClass, rc,
            XmNlabelType, XmPIXMAP,
            XmNlabelPixmap, pixmap,
            NULL);
        /* callback for this pushbutton sets the current color */
        XtAddCallback (pb, XmNactivateCallback, set_colorCB, colors[i]);
   }
   XtManageChild (rc);
/*
 *	Setup the Lists of things
 *	-------------------------
 */
   sprintf(line, "%s", "     Scheme                   Icon Models");
   title  = XmStringCreateSimple((char *)line);
   label01  = XtVaCreateManagedWidget((String)line, xmLabelWidgetClass, ed_form,
				      //XmNwidth,            100,
                 XmNheight,           20,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       help_button,
                 XmNtopAttachment,    XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_WIDGET,
                 XmNrightWidget,      rc,
                 NULL);
   XmStringFree(title);

   trk_list = XmCreateScrolledList (ed_form, "MVList", NULL, 0);
   XtVaSetValues (trk_list,
               XmNitems,             rtnstr,
               XmNitemCount,         n_rtns,
               XmNvisibleItemCount,  20,
               XmNscrollBarDisplayPolicy, XmSTATIC,
               NULL);
   XtVaSetValues (XtParent(trk_list),
               XmNtopAttachment,     XmATTACH_WIDGET,
               XmNtopWidget,         label01,
               XmNleftAttachment,    XmATTACH_WIDGET,
               XmNleftWidget,        done_button,
               XmNbottomAttachment,  XmATTACH_FORM, 
               NULL);
   XtAddCallback(trk_list, XmNbrowseSelectionCallback, (XtCallbackProc)trk_listCB, NULL);
   XtManageChild(trk_list);

   ed_list = XmCreateScrolledList (ed_form, "MVList", NULL, 0);
   XtVaSetValues(ed_list,
               XmNitems,             xstr,
               XmNitemCount,         count,
               XmNvisibleItemCount,  20,
               XmNscrollBarDisplayPolicy, XmSTATIC,
               NULL);
   XtVaSetValues (XtParent(ed_list),
               XmNtopAttachment,     XmATTACH_WIDGET,
               XmNtopWidget,         label01,
               XmNleftAttachment,    XmATTACH_WIDGET,
               XmNleftWidget,        XtParent(trk_list),
               XmNrightAttachment,   XmATTACH_WIDGET,
	       XmNrightWidget,       rc, 
               XmNbottomAttachment,  XmATTACH_FORM, 
               NULL);
   XtAddCallback (ed_list, XmNbrowseSelectionCallback, (XtCallbackProc)ed_listCB, NULL);
   XtManageChild(ed_list);

   XtManageChild (ed_form);

   XtPopup(ed_shell, XtGrabNone);
}

void 
track_editCB (Widget toplevel)
{
   if (ed_first) {
      GR_toplevel = toplevel;
      ed_first = FALSE;
      ed_init ();
   } else {
       XtPopup(ed_shell, XtGrabNone);
   }
}

void
ed_doneCB ()
{
   XtPopdown (ed_shell);
}

void
ed_helpCB (Widget parent, XtPointer, XmListCallbackStruct* call_data)
{
char         bigstring[1600];
char         littlestr[80];
XmString     xstr;
Arg          args[3];
Widget       dialog;
void         help_doneCB(Widget dialog, XtPointer, XmListCallbackStruct* call_data);

   sprintf(bigstring, "\n");
   sprintf(littlestr, " 1) Select the Scheme entry you wish to change.\n");
   strcat (bigstring, littlestr);
   sprintf(littlestr, " 2) Select the new color for the entry.\n");
   strcat (bigstring, littlestr);
   sprintf(littlestr, " 3) Select the new model (if wanted).\n");
   strcat (bigstring, littlestr);
   sprintf(littlestr, " 4) Push the Replace button to change the entry.\n");
   strcat (bigstring, littlestr);
   sprintf(littlestr, "    Push the Delete button to remove the entry.\n");
   strcat (bigstring, littlestr);
   sprintf(littlestr, "    Push the Add button to add a new entry.\n");
   strcat (bigstring, littlestr);
   sprintf(littlestr, "    Push the Save button to update the scheme.dat file.\n");
   strcat (bigstring, littlestr);
   sprintf(littlestr, " 5) Changes take effect immediately.\n");
   strcat (bigstring, littlestr);
   strcat (bigstring, "\0");
   xstr = XmStringCreateLtoR(bigstring, XmSTRING_DEFAULT_CHARSET);

   XtSetArg(args[0], XmNmessageString, xstr);
   XtSetArg(args[1], XmNautoUnmanage, False);
   dialog = XmCreateInformationDialog(parent, "help", args, 2);
   XmStringFree(xstr);
   XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
   XtSetSensitive(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON), False);
   XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)help_doneCB, NULL);
   XtManageChild(dialog);
   XtPopup(XtParent(dialog), XtGrabNone);
}
void
help_doneCB(Widget dialog, XtPointer, XmListCallbackStruct* call_data)
{
   XtDestroyWidget(dialog);
}

void
ed_addCB (Widget parent, XtPointer, XmListCallbackStruct* call_data)
{
char        text[40];

   sprintf (text, "%5d %5hd %5hd %5hd", select_icon, select_color[0],
                                select_color[1], select_color[2]);
   newitems[0] = XmStringCreate (text, XmSTRING_DEFAULT_CHARSET);
   XmListAddItemUnselected(trk_list, newitems[0], 0);
   rtninfo[n_rtns].icon = select_icon;
   rtninfo[n_rtns].r = select_color[0];
   rtninfo[n_rtns].g = select_color[1];
   rtninfo[n_rtns].b = select_color[2];
   n_rtns = n_rtns + 1;
}

void
ed_delCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
int     pos;

   pos = call_data->item_position;
   //XmListDeletePos(trk_list, pos);
}

void
ed_modCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
char        text[40];
int         n;

   sprintf (text, "%5d %5hd %5hd %5hd", select_icon, select_color[0],
                                select_color[1], select_color[2]);
   newitems[0] = XmStringCreate (text, XmSTRING_DEFAULT_CHARSET);
   XmListReplaceItemsPos(trk_list, newitems, 1, select_pos);
   n = select_pos - 1;
   rtninfo[n].icon = select_icon;
   rtninfo[n].r = select_color[0];
   rtninfo[n].g = select_color[1];
   rtninfo[n].b = select_color[2];
}

void
ed_saveCB (Widget parent, XtPointer, XmListCallbackStruct* call_data)
{
FILE      *trakfile;
int       i;

   if ((trakfile = fopen("scheme.dat", "w+")) != NULL) {
      fprintf(trakfile, "%5d\n", n_rtns);
      for (i=0; i<n_rtns; i++) {
         fprintf(trakfile, "%5d %5hd %5hd %5hd\n", rtninfo[i].icon,
                rtninfo[i].r, rtninfo[i].g, rtninfo[i].b);
      }
      fclose(trakfile);
   } else fprintf(stderr, " Warning: Color Scheme file not saved!!!\n");
}

void
ed_listCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
char *text;

  XmStringGetLtoR (call_data->item, XmSTRING_DEFAULT_CHARSET, &text); 
  sscanf (text, "%d%*s", &select_icon);
}

void
trk_listCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
char *text;

   XmStringGetLtoR (call_data->item, XmSTRING_DEFAULT_CHARSET, &text); 
   sscanf (text, "%d %hd %hd %hd", &select_icon, &select_color[0],
                               &select_color[1], &select_color[2]);
   select_pos = call_data->item_position;
   //fprintf(stderr, "Selected position is %d\n", select_pos);
}
/*
 * callback routine for when any of the color tiles are pressed.
 * This general function may also be used to set the global gc's
 * color directly.  Just provide a widget and a color name.
*/
void
set_colorCB(Widget widget, XtPointer client_data, XtPointer call_data)
{
XColor col, unused;

    String color = (String) client_data;
    Display *dpy = XtDisplay (widget);
    Colormap cmap = DefaultColormapOfScreen (XtScreen (widget));

    if (!XAllocNamedColor (dpy, cmap, color, &col, &unused)) {
        char buf[32];
        sprintf (buf, "Can't alloc %s", color);
        XtWarning (buf);
        return;
    }
    XSetForeground (dpy, gc, col.pixel);
    select_color[0] = (int)((float)col.red/65535.0*255.0);
    select_color[1] = (int)((float)col.green/65535.0*255.0);
    select_color[2] = (int)((float)col.blue/65535.0*255.0);
}
