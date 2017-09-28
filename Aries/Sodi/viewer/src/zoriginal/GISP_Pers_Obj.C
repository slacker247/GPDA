/*********************************************************************
  GISP_Pers_Obj.C:
  
  -- 08/20/93: created by Y. Tung;
  
*******************************************************************/

#include "GISP_Pers_Obj.H"
#include "GR_Idlist.H"
//#include <sys/types.h>
#include <Xm/Scale.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>

/*
extern void setvparams (GR_Window* win, int vmode,
			int lat, int lon, int alt, int fov, int azi);
extern Widget scaleLAT, scaleLON;
extern int v_LAT, v_LON, v_ALT, v_FOV, v_AZI, VMODE;
*/

Widget GetTopShell (Widget w);
Widget GetSonOfTopShell (Widget w);

extern IDlist* GISP_pick_idlist;


GISP_Pers_Obj::GISP_Pers_Obj (long id, long type)
{
  p_id = id;
  p_type = type;
  p_host_user = NULL;
  init ();
}


void 
GISP_Pers_Obj::init ()
{
  Widget pane, uform, lform, butt;
  char str[80], pane_name[20];
  long id, type;
 
  id = p_id;
  type = p_type;
  sprintf (str, "Object #%d, type %d", id, type);
  p_dialog = XtVaCreatePopupShell
    (str,
     //xmDialogShellWidgetClass, GetTopShell (window->widget()),
     //xmDialogShellWidgetClass, GetSonOfTopShell (window->widget()),
     xmDialogShellWidgetClass, GR_toplevel,
     XmNdeleteResponse, XmDESTROY,
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

  pane = XtVaCreateWidget
    (pane_name,
     xmPanedWindowWidgetClass, p_dialog,
     NULL);

  uform = XtVaCreateWidget
    ("Uform",
     xmFormWidgetClass, pane,
     NULL);
  
  p_textw = XmCreateScrolledText (uform, "Textw", NULL, 0);
  XtVaSetValues
    (p_textw,
     XmNscrollVertical, True,
     XmNscrollHorizontal, True,
     XmNrows, 10,
     XmNcolumns, 32,
     XmNeditable, False,
     XmNeditMode, XmMULTI_LINE_EDIT,
     XmNwordWrap, True,
     NULL);
  XtVaSetValues
    (XtParent (p_textw),
     XmNleftAttachment, XmATTACH_FORM,
     XmNrightAttachment, XmATTACH_FORM,
     XmNtopAttachment, XmATTACH_FORM,
     XmNbottomAttachment, XmATTACH_FORM,
     NULL);
  XtManageChild (p_textw);
  
  XtManageChild (uform);
  
  lform = XtVaCreateWidget
    ("Lform",
     xmRowColumnWidgetClass, pane,
     XmNorientation, XmHORIZONTAL,
     XmNpacking, XmPACK_COLUMN,
     XmNnumColumns, 1,
     NULL);
  
  butt = XtVaCreateManagedWidget
    ("Query",
     xmPushButtonWidgetClass, lform,
     XmNheight, 10,     
     NULL);
  XtAddCallback (butt, XmNactivateCallback,
		 (XtCallbackProc)GISP_queryCB, (XtPointer)this);
  
  butt = XtVaCreateManagedWidget
    ("Monitor",
     xmPushButtonWidgetClass, lform,
     NULL);
  XtAddCallback (butt, XmNactivateCallback,
		 (XtCallbackProc)GISP_monitorCB, (XtPointer)this);
  
  butt = XtVaCreateManagedWidget
    ("Command",
     xmPushButtonWidgetClass, lform,
     NULL);
  XtAddCallback (butt, XmNactivateCallback,
		 (XtCallbackProc)GISP_commandCB, (XtPointer)this);
    
  butt = XtVaCreateManagedWidget
    ("Quit",
     xmPushButtonWidgetClass, lform,
     NULL);
  XtAddCallback (butt, XmNactivateCallback,
		 (XtCallbackProc)GISP_quitCB, (XtPointer)this);
  
  XtManageChild (lform);
  
  {
    Dimension h;
    XtVaGetValues (butt, XmNheight, &h, NULL);
    XtVaSetValues (lform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
  }
  
  XtManageChild (pane);
  XtPopup (p_dialog, XtGrabNone);

  create_host_user ();
 
  p_alive = TRUE;
}


void
GISP_Pers_Obj::create_host_user ()
{
  if (!p_host_user)
  {
     p_host_user = new C_HOST_USER ();
  }
}

C_HOST_USER*
GISP_Pers_Obj::get_host_user ()
{
   return p_host_user;
}

void
GISP_Pers_Obj::delete_host_user ()
{
  if (p_host_user)
  {
    p_host_user->disconnect ();
    delete p_host_user;
    p_host_user = NULL;
  }
}


void
GISP_Pers_Obj::set_text (char* str)
{
   if (p_dialog && p_textw)
   {
     if (!p_alive)
       sprintf (str, "This Object does not currently exist.\n");

     XtVaSetValues
       (p_textw,
	XmNvalue, str,
	NULL);
   }
   else
     printf ("Warning: p_dialog or p_textw not defined yet.\007\n");
}


/* ========== */


void
GISP_queryCB (Widget, GISP_Pers_Obj *objptr)
{
  C_HOST_USER* huser;
  C_QUERY_DATA *qdata=NULL;
  int total_num=0;
  double ret_val;
  int i;
  char *result_str;
  char *tmp_str;
  int id, type;

  if (!objptr)
  {
     perror ("Passing NULL GISP_Pers_Obj?\007\n");
     return;
  }
  else
     printf (" starting query function...\n"); 
  
  id = (int) objptr->get_id ();
  type = (int) objptr->get_type ();

  if (!GISP_pick_idlist->in_list(id))
  {
    printf ("Invalid id %d?\007\n", id);
    objptr->set_alive (FALSE);
    return;
  }

  huser = objptr->get_host_user ();
  if (!huser)
  {
    printf (" No host user available???\007\n");
    return;
  }
  qdata = huser->query (id, total_num);
  printf (".... object %d returns %d items...\n", id, total_num);

  if (qdata && total_num > 0)
  { 
     tmp_str = new char[80];
     result_str = new char[80*total_num];
     sprintf (result_str, "");   // just to clean up;
     for (i=0; i<total_num; i++)
     {
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
  }
  else
    printf ("No data returned from object %d.\n", id);
}

void
GISP_monitorCB (Widget, GISP_Pers_Obj* objptr)
{
  printf ("Monitor not implemented yet\n");
 
  C_HOST_USER* huser;
  C_QUERY_DATA *qdata=NULL;
  int total_num=0;
  int id;

  if (!objptr)
  {
     perror ("Passing NULL GISP_Pers_Obj?\007\n");
     return;
  }
 
  id = (int) objptr->get_id ();
  printf ("  ==> Testing query to object id=%d type=%d\n",
           id, objptr->get_type ());

  if (!GISP_pick_idlist->in_list(id))
  {
    printf ("Invalid id %d?\007\n", id);
    objptr->set_alive (FALSE);
    return;
  }

  huser = objptr->get_host_user ();
  if (huser)
    huser->query (id);
  else
  {
    printf (" No host user available???\007\n");
  }
}

void
GISP_commandCB (Widget, GISP_Pers_Obj*)
{
  printf ("Command not implemented yet\n");
}

void
GISP_quitCB (Widget w, GISP_Pers_Obj *objptr)
{
  if (objptr)
  {
    if (objptr->get_host_user())
       objptr->delete_host_user ();
    XtDestroyWidget (XtParent(XtParent(XtParent(w))));  // that is "Pane"
    if (GISP_pick_idlist)
    {
      GISP_pick_idlist->rm_list (objptr->get_id());
      GISP_pick_idlist->print_list ();  // debugging....
    }
    delete objptr;
  }
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
