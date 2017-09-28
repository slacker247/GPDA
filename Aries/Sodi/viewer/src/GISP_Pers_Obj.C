/*********************************************************************
  GISP_Pers_Obj.C:
  
  -- 08/20/93: created by Y. Tung;
  
*******************************************************************/

#include "GISP_Pers_Obj.H"
#include "GR_Idlist.H"
#include "GISP_Globals.H"

#include <Xm/Scale.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>

#define RE 6378.145 

static int      Pers_view;
static int      Pers_monitor;
static int      Pers_timeoutid;
static GISP_Pers_Obj *Pers_objptr;

Widget          GetTopShell (Widget w);
Widget          GetSonOfTopShell (Widget w);

extern IDlist*  GISP_pick_idlist;
extern struct TRACK trackobj[];


GISP_Pers_Obj::GISP_Pers_Obj (long id, long type)
{
   p_id         = id;
   p_type       = type;
   p_host_user  = NULL;
   p_gispobj    = NULL;
   Pers_view    = FALSE;
   Pers_monitor = FALSE;

   init ();
}

void 
GISP_Pers_Obj::init ()
{
Widget          pane, uform, lform, butt1, butt2, butt3, butt4;
XmString        xstr;
char            str[240], pane_name[20];
long            id, type;
 
   id = p_id;
   type = p_type;
   sprintf (str, "Object #%d, type %d", id, type);
   p_dialog = XtVaCreatePopupShell(str,
                 //xmDialogShellWidgetClass, GetTopShell (window->widget()),
                 //xmDialogShellWidgetClass, GetSonOfTopShell (window->widget()),
                 xmDialogShellWidgetClass, GR_toplevel,
                 XmNdeleteResponse,   XmDESTROY,
                 NULL);
  
   if (type == 44) // B.E.
     sprintf (pane_name, "BE_Pane");
   else if (type == 45) // DSP
     sprintf (pane_name, "DSP_Pane");
   else if ((type== 150) || (type==151))  // GBI
     sprintf (pane_name, "GBI_Pane");
   else if ((type==2) || (type==6) || (type==17) || (type==18))  // MISSILE
     sprintf (pane_name, "MISSILE_Pane");
   else 
     sprintf (pane_name, "other_Pane"); 

   pane = XtVaCreateWidget (pane_name, xmPanedWindowWidgetClass, p_dialog, NULL);

   uform = XtVaCreateWidget ("Uform", xmFormWidgetClass, pane, NULL);
  /*  
  p_textw = XmCreateScrolledText (uform, "Textw", NULL, 0);
  XtVaSetValues (p_textw,
     XmNscrollVertical,   True,
     XmNscrollHorizontal, True,
     XmNrows,             10,
     XmNcolumns,          32,
     XmNeditable,         False,
     XmNeditMode,         XmMULTI_LINE_EDIT,
     XmNwordWrap,         True,
     NULL);
  XtVaSetValues (XtParent (p_textw),
     XmNleftAttachment,   XmATTACH_FORM,
     XmNrightAttachment,  XmATTACH_FORM,
     XmNtopAttachment,    XmATTACH_FORM,
     XmNbottomAttachment, XmATTACH_FORM,
     NULL);
  XtManageChild (p_textw);
  */
   p_textw = XtVaCreateManagedWidget("Text", xmLabelWidgetClass, uform,
                 XmNwidth,            240,
                 XmNheight,           160,
                 XmNmarginHeight,     2,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_FORM,
                 NULL);
  
   XtManageChild (uform);
   sprintf(str, " %s\n\n %s\n %s\n %s\n",
	   "Use button to choose desired option:",
	   "Query   - Examine object position",
           "Monitor - Continuous Query (1 sec)",
	   "View    - Object centered view");
   xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(p_textw, XmNlabelString, xstr, NULL);
   XmStringFree (xstr);
  
   lform = XtVaCreateWidget ("Lform", xmRowColumnWidgetClass, pane,
                 XmNorientation,      XmHORIZONTAL,
                 XmNpacking,          XmPACK_COLUMN,
                 XmNnumColumns,       1,
                 NULL);
  
   butt1 = XtVaCreateManagedWidget ("Query", xmPushButtonWidgetClass, lform,
                 XmNheight,           10,     
                 NULL);
   XtAddCallback (butt1, XmNactivateCallback,
		 (XtCallbackProc)GISP_queryCB, (XtPointer)this);
  
   butt2 = XtVaCreateManagedWidget ("Monitor", xmPushButtonWidgetClass, lform, NULL);
   XtAddCallback (butt2, XmNactivateCallback,
		 (XtCallbackProc)GISP_monitorCB, (XtPointer)this);
  
   butt3 = XtVaCreateManagedWidget ("View", xmPushButtonWidgetClass, lform, NULL);
   XtAddCallback (butt3, XmNactivateCallback,
		 (XtCallbackProc)GISP_commandCB, (XtPointer)this);
    
   butt4 = XtVaCreateManagedWidget ("Quit", xmPushButtonWidgetClass, lform, NULL);
   XtAddCallback (butt4, XmNactivateCallback,
		 (XtCallbackProc)GISP_quitCB, (XtPointer)this);
  
   XtManageChild (lform);
  
   {
     Dimension h;
     XtVaGetValues (butt1, XmNheight, &h, NULL);
     XtVaSetValues (lform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
   }
  
   XtManageChild (pane);
   XtPopup (p_dialog, XtGrabNone);

   //create_host_user ();
   Pers_objptr = this;
 
   p_alive = TRUE;

   //GISP_queryCB(NULL, this);
}


void
GISP_Pers_Obj::create_host_user ()
{
   if (!p_host_user) p_host_user = new C_HOST_USER ();
}

C_HOST_USER*
GISP_Pers_Obj::get_host_user ()
{
   return p_host_user;
}

void
GISP_Pers_Obj::delete_host_user ()
{
   if (p_host_user) {
     p_host_user->disconnect ();
     delete p_host_user;
     p_host_user = NULL;
   }
}


void
GISP_Pers_Obj::set_text (char* str)
{
XmString        xstr;

  if (p_dialog && p_textw) {
    if (!p_alive)
      sprintf (str, "This Object does not currently exist.\n");
    xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
    XtVaSetValues(p_textw, XmNlabelString, xstr, NULL);
    XmStringFree (xstr);
  } else
    printf ("Warning: p_dialog or p_textw not defined yet.\007\n");
}


/* ========== */


void
GISP_queryCB (Widget mywidget, GISP_Pers_Obj *objptr1)
{
C_HOST_USER*    huser;
C_QUERY_DATA    *qdata=NULL;
int             total_num=0;
double          ret_val;
int             i;
int             id_found;
int             trkindex;
char            *result_str;
char            *tmp_str;
int             id, type;
float           lat, lon, alt;
float           vLAT, vLON, vALT, vFOV;
int             vMODE;
double          simtime;
GISP_Pers_Obj   *objptr;

extern GR_Window*  gwindow;
extern int      VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI;
extern int      n_gispobjs;
extern Widget   scaleLAT, scaleLON, scaleALT, scaleFOV;
extern void     setvparamsf(GR_Window* win, int vmode,
                            float lat, float lon, float alt, float fov, float azi);
extern char*    get_graphics_type_string (long type);


   objptr = Pers_objptr; 
   if (!objptr) {
     perror ("Passing NULL GISP_Pers_Obj?\007\n");
     return;
   }

   id = (int) objptr->get_id ();
   type = (int) objptr->get_type ();

   if (!GISP_pick_idlist->in_list(id)) {
     printf ("Invalid id %d?\007\n", id);
     objptr->set_alive (FALSE);
     return;
   }

   huser = objptr->get_host_user ();
//
//   If no HOST_ROUTER, use the local data
//
   if (!huser) {
     total_num = 7;   // *** Remember to change this
     tmp_str = new char[80];
     result_str = new char[80*total_num];
     sprintf(result_str, "\n");
     sprintf(tmp_str, " Data Source ...... LOCAL\n");
     strcat (result_str, tmp_str);
     sprintf(tmp_str, " Object ID ........ %d\n", id);
     strcat (result_str, tmp_str);
     sprintf(tmp_str, " Object Type ...... %s\n", get_graphics_type_string((long)type));
     strcat (result_str, tmp_str);

     id_found   = FALSE;
     trkindex   = -1;
     for (i=0; i<n_gispobjs; i++) {
       if (id == trackobj[i].id) {
         id_found = TRUE;
         trkindex = i;
         break;
       }
     }
     if (!id_found) return;

     if (objptr->p_gispobj) {
        simtime = objptr->p_gispobj->get_time();
        lat = trackobj[trkindex].latitude;
        lon = trackobj[trkindex].longitude;
        alt = trackobj[trkindex].altitude;
        //lon = objptr->p_gispobj->get_lon()*180.0/M_PI;
	sprintf(tmp_str, " Sample Time ...... %f\n", simtime);
        strcat (result_str, tmp_str);
        sprintf(tmp_str, " Latitude ......... %f\n", lat);
        strcat (result_str, tmp_str);
        sprintf(tmp_str, " Longitude ........ %f\n", lon);
        strcat (result_str, tmp_str);
        sprintf(tmp_str, " Altitude ......... %f\n", alt);
        strcat (result_str, tmp_str);
     }
     strcat(result_str, "\0");
     objptr->set_text (result_str);
     //fprintf(stderr, "%s\n", result_str);
     delete tmp_str;
     delete result_str;;
   } else {
//
//   HOST_ROUTER connection exists, get live data from simulation
//
     qdata = huser->query (id, total_num);
     printf (".... object %d returns %d items...\n", id, total_num);

     if (qdata && total_num > 0) { 
       tmp_str = new char[80];
       result_str = new char[80*total_num];
       sprintf (result_str, "");   // just to clean up;
       for (i=0; i<total_num; i++) {
         ret_val = qdata[i].get_value ();
         if (ret_val < -1.0e19)
            sprintf (tmp_str, " %-20s\n", qdata[i].get_name());
         else
            sprintf (tmp_str, " %-20s: %g\n", qdata[i].get_name(), ret_val);
         strcat (result_str, tmp_str);
      }
      objptr->set_text (result_str);
      delete tmp_str;
      delete result_str;
     } else {
       printf ("No data returned from object %d.\n", id);
       return;
     }
   }

   if (objptr->p_gispobj && gwindow && Pers_view) {
     vLAT = trackobj[trkindex].latitude;
     vLON = trackobj[trkindex].longitude;
     vALT = (RE+1.0)/10.0 + trackobj[trkindex].altitude;
     vFOV = 15.0;
     vMODE = 0;
     setvparamsf(gwindow, vMODE, vLAT, vLON, vALT, vFOV, 0.0);
     //fprintf(stderr, "------------------- Viewing: %5 %f %f\n", vLAT, vLON, vALT);
     XmScaleSetValue (scaleLAT, v_LAT);
     XmScaleSetValue (scaleLON, v_LON);
     gwindow->draw ();
   }
}

void
timeoutCB (Widget mywidget, XtPointer, GISP_Pers_Obj* myobjptr)
{
GISP_Pers_Obj   *objptr;

   objptr = Pers_objptr;
   if (!objptr) {
      perror ("Passing NULL GISP_Pers_Obj?\007\n");
      return;
   }

   GISP_queryCB(mywidget, objptr);

   if (Pers_monitor)
      Pers_timeoutid = XtAppAddTimeOut(GR_appcontext, 1000,
                   (XtTimerCallbackProc)timeoutCB, (XtPointer)objptr);
}

void
GISP_monitorCB (Widget mywidget, GISP_Pers_Obj* myobjptr)
{
GISP_Pers_Obj   *objptr;

   objptr = Pers_objptr;
   if (!objptr) {
      perror ("Passing NULL GISP_Pers_Obj?\007\n");
      return;
   }

   Pers_monitor = !Pers_monitor;
   GISP_queryCB(mywidget, objptr);

   if (Pers_monitor)
      Pers_timeoutid = XtAppAddTimeOut(GR_appcontext, 1000,
                   (XtTimerCallbackProc)timeoutCB, (XtPointer)objptr);
}

void
GISP_commandCB (Widget mywidget, GISP_Pers_Obj *myobjptr)
{
GISP_Pers_Obj   *objptr;

   objptr = Pers_objptr;
   if (!objptr) {
      perror ("Passing NULL GISP_Pers_Obj?\007\n");
      return;
    }

   Pers_view = !Pers_view;
   GISP_monitorCB(mywidget, objptr);
}

void
GISP_quitCB (Widget w, GISP_Pers_Obj *myobjptr)
{
GISP_Pers_Obj   *objptr;
extern GR_Window*  gwindow;
extern int      VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI;
extern void     setvparams(GR_Window* win, int vmode,
                            int lat, int lon, int alt, int fov, int azi);

   objptr = Pers_objptr;
   if (objptr) {
     Pers_view = 0;
     Pers_monitor = 0;
     //Pers_objptr = NULL;

    objptr->p_gispobj = NULL;
    if (objptr->get_host_user()) {
       objptr->delete_host_user ();
    }
    XtDestroyWidget (XtParent(XtParent(XtParent(w))));  // that is "Pane"

    if (GISP_pick_idlist) {
      GISP_pick_idlist->rm_list (objptr->get_id());
      GISP_pick_idlist->print_list ();  // debugging....
    }
    delete objptr;
  }
  setvparams(gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
}


/* --- for future use (in GISP_coammandCB): object-centered viewing --- */
/*
class GispObj_pick_class
{
 public:
   GispObj_pick_class (GISP_Obj* GispObj, GR_Window* window);
   GISP_Obj* p_GispObj;
   GR_Window* p_window;
};

GispObj_pick_class::GispObj_pick_class(GISP_Obj* GispObj, GR_Window* window)
{
   p_GispObj = GispObj;
   p_window = window;
}

void
GISP_Pers_Obj_centerCB (Widget, XtPointer objptr, XtPointer)
{
   GispObj_pick_class *GispObj_pick = (GispObj_pick_class*)objptr;
   GISP_Obj *GispObj = GispObj_pick->p_GispObj;
   GR_Window *win = GispObj_pick->p_window;

   if (GispObj && win)
   {
      v_LAT = (int) (GispObj->get_lat ())%90;
      v_LON = (int) (GispObj->get_lon ())%180;
      setvparams (win, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
      XmScaleSetValue (scaleLAT, v_LAT);
      XmScaleSetValue (scaleLON, v_LON);
      win->draw ();
   }
}
*/

/* --- utilities --- */

Widget
GetTopShell (Widget w)
{
   if (!w)
   {
      fprintf (stderr, "GetTopShell (NULL)? \007\n");
      return NULL;
   }
   while (w && !XtIsWMShell (w))
     w = XtParent (w);
   return w;
}

Widget
GetSonOfTopShell (Widget w)
{
   if (!w)
   {
      fprintf (stderr, "GetSonOfTopShell (NULL)? \007\n");
      return NULL;
   }
   while (w && !XtIsWMShell (XtParent(w)))
     w = XtParent (w);
   return w;
}
