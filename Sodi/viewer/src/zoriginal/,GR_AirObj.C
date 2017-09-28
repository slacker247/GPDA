/***************************************************************************
  See GR_AirObj.H
  
  -- 04/29/93: created by Y. Tung;
  -- 05/03/93: modified;
  -- 06/29/93: added a get_type_string function that returns a character
      string that represents the type:  in rsd -- asf and asfeom interface,
      is the type of the <type,id> pair the application (say, airsim)
      provides (say BOEING_747 or AWACS), otherwise (sps interface)
      this is the graphics icon type (say 747, F15 or AWACS).
      
****************************************************************************/

#include "GR_AirObj.H"
#include <Xm/Scale.h>

void AirObj_trailCB (Widget, XtPointer objptr, XtPointer);
void AirObj_centerCB (Widget, XtPointer objptr, XtPointer);
extern void setvparams (GR_Window* win, int vmode,
			int lat, int lon, int alt, int fov, int azi);
extern Widget scaleLAT, scaleLON;
extern int v_LAT, v_LON, v_ALT, v_FOV, v_AZI, VMODE;
extern char*  get_type_string (long id, long type);


GR_AirObj::GR_AirObj (long id, long type): GR_Model (id, type)
{
   p_trail = NULL;
   p_update_cycle = 0;
}

void
GR_AirObj::add_trail (GR_DispList* trail_displist,
		      short r, short g, short b)
{
   p_trail = new GR_Trail (trail_displist, p_id, p_type, r, g, b);
}

void
GR_AirObj::delete_trail (GR_DispList* trail_displist)
{
   if (this)
   {
      if (p_trail)
      {
         if (trail_displist)
           trail_displist->delete_object ((GR_DispObj*)p_trail); // ???
	 delete p_trail;   // ???
      }
   }
}

void
GR_AirObj::add_trail_point (float x, float y, float z)
{
   if (!p_trail)
     printf ("Warning: you forget to add trail first.\n");
   else
     p_trail->push_tpoint (x, y, z);   
}

void
GR_AirObj::add_trail_point (short per_num_update, float x, float y, float z)
{
   if (!p_trail)
     printf ("Warning: you forget to add trail first.\n");
   else
   {
      if (p_update_cycle % per_num_update == 0)
	p_trail->push_tpoint (x, y, z);
      p_update_cycle++;
   }
}

void
GR_AirObj::trail_on ()
{
   if (!p_trail)
     printf ("Warning: you forget to add trail first.\n");
   else
     p_trail->set_visible_flag (1);
}

void
GR_AirObj::trail_off ()
{
   if (!p_trail)
     printf ("Warning: you forget to add trail first.\n");
   else
     p_trail->set_visible_flag (0);
}

void
GR_AirObj::trail_toggle ()
{
   if (!p_trail)
     printf ("Warning: you forget to add trail first.\n");
   else
     p_trail->set_visible_flag (!p_trail->get_visible_flag());
}



void
GR_AirObj::objdraw ()
{
   GR_Model::objdraw();
}

/* --- for mouse picking purpose --- */

class airobj_pick_class
{
 public:
   airobj_pick_class (GR_AirObj* airobj, GR_Window* window)
   {
      p_airobj = airobj;
      p_window = window;
   }
   GR_AirObj* p_airobj;
   GR_Window* p_window;
};



void
GR_AirObj::pickEvent(GR_MouseEvent& event, GR_Window* window)
{
   Widget dialog;
   char str [80];
   XmString xstr, trailstr, centerstr;
   airobj_pick_class *airobj_pick = new airobj_pick_class (this, window);

   printf ("AirObj id #%d, type %d, is picked..",
	   (p_id & 0x000fffff),
	   p_type
	   );
   switch (event.button)
   {
    case GR_LEFTMOUSE:
      if (event.down)
      {
	 printf ("..by GR_LEFTMOUSE..");
	 //dialog = XmCreateMessageDialog (window->widget(),"message",NULL,0);
	 dialog = XmCreatePromptDialog (window->widget(),"message",NULL,0);

	 sprintf (str, "Air Object ID #%d, Type %s",
		  (p_id & 0x000fffff),
		  get_type_string (p_id, p_type)
		  );

	 xstr = XmStringCreateSimple (str);
	 trailstr = XmStringCreateSimple ("Trail");
	 centerstr = XmStringCreateSimple ("Center");
	 XtVaSetValues (dialog,
			//XmNmessageString, xstr,
			XmNselectionLabelString, xstr,
			XmNhelpLabelString, trailstr,
			XmNapplyLabelString, centerstr,
			NULL);
	 XtAddCallback (dialog,
			XmNhelpCallback,
			(XtCallbackProc)AirObj_trailCB,
			(XtPointer)airobj_pick);	 
	 XtAddCallback (dialog,
			XmNapplyCallback,
			(XtCallbackProc)AirObj_centerCB,
			(XtPointer)airobj_pick);	 
	 XmStringFree (xstr);
	 XmStringFree (trailstr);
	 XmStringFree (centerstr);
	 //XtManageChild (XmMessageBoxGetChild(dialog,XmDIALOG_APPLY_BUTTON));
	 XtManageChild (XmSelectionBoxGetChild(dialog,XmDIALOG_APPLY_BUTTON));
	 XtManageChild (dialog);
	 XtPopup (XtParent(dialog), XtGrabNone);
      }
      break;
    case GR_MIDDLEMOUSE:
      if (!event.down)
      {
	 printf (".. by GR_MIDDLEMOUSE..");
      }
      break;
    case GR_RIGHTMOUSE:
      if (event.down)
      {
	 printf (".. by GR_RIGHTMOUSE..");
      }
      break;
   }
   printf("\n");
}

void
AirObj_trailCB (Widget, XtPointer objptr, XtPointer)
{
   airobj_pick_class *airobj_pick = (airobj_pick_class*)objptr;
   airobj_pick->p_airobj->trail_toggle ();
   airobj_pick->p_window->draw ();   
}

/* --- also do object-centered viewing --- */

void
AirObj_centerCB (Widget, XtPointer objptr, XtPointer)
{
   airobj_pick_class *airobj_pick = (airobj_pick_class*)objptr;
   GR_AirObj *airobj = airobj_pick->p_airobj;
   GR_Window *win = airobj_pick->p_window;

   if (airobj && win)
   {
      v_LAT = (int) (airobj->get_lat ())%90;
      v_LON = (int) (airobj->get_lon ())%180;
      setvparams (win, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
      XmScaleSetValue (scaleLAT, v_LAT);
      XmScaleSetValue (scaleLON, v_LON);
      win->draw ();
   }

}
