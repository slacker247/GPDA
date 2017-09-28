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

#include <malloc.h>
#include "GR_AirObj.H"
#include "def.H"
#include <Xm/Scale.h>

void AirObj_trailCB (Widget, XtPointer objptr, XtPointer);
void AirObj_centerCB (Widget, XtPointer objptr, XtPointer);
extern void setvparams (GR_Window* win, int vmode,
			int lat, int lon, int alt, int fov, int azi);
extern Widget scaleLAT, scaleLON, scaleALT;
extern int v_LAT, v_LON, v_ALT, v_FOV, v_AZI, VMODE;
extern char*  get_type_string (long id, long type);


GR_AirObj::GR_AirObj (long id, long type): GR_Model (id, type)
{
   p_trail = NULL;
   p_update_cycle = 0;
   p_bufsize = NUM_PAIRS;

   if (p_bufsize > 0) {
     p_trail_ptr = (trail_data *)malloc(sizeof(trail_data)*p_bufsize); //new trail_data[num_pairs];
   } else {
     p_trail_ptr = NULL;
   }

   for (int i=0; i<NUM_PAIRS; i++) {
      for (int j=0; j<3; j++) {
         p_trail_ptr[i].ploc[j] = 0.0;
         p_trail_ptr[i].tloc[j] = 0.0;
      }
      p_trail_ptr[i].rgb[0] = 255;  // default link color is yellow;
      p_trail_ptr[i].rgb[1] = 255;
      p_trail_ptr[i].rgb[2] = 0;
   }

   p_total_pairs = 0;
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
   if (this) {
      if (p_trail) {
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
     printf ("Warning: you forgot to add trail first.\n");
   else
     p_trail->set_visible_flag (!p_trail->get_visible_flag());
}

void
GR_AirObj::add_link (int iindex, double* ploc, double* tloc, short* rgb)
{
int i, index;

   index = p_total_pairs;
   if (index >= 0 && index < NUM_PAIRS) {
      for (i=0; i<3; i++) {
         p_trail_ptr[index].ploc[i] = ploc[i];
         p_trail_ptr[index].tloc[i] = tloc[i];
	 p_trail_ptr[index].rgb[i]  = rgb[i];
      }
   } else {
      fprintf(stderr, "Warning: array index %d out of bound, [0..%d]???\007\n",
               index, p_total_pairs-1);
      p_bufsize = p_bufsize+NUM_PAIRS;
      p_trail_ptr = (trail_data *)realloc(p_trail_ptr, sizeof(trail_data)*p_bufsize);
      //p_total_pairs = 0;				// Start over 
   }
   p_total_pairs = p_total_pairs + 1;
}

void
GR_AirObj::drop_track()
{
   p_trail = NULL;
   p_total_pairs = -1;
   free(p_trail_ptr);
   p_bufsize = 0;
}

void
GR_AirObj::objdraw ()
{
int    i, j;
double ploc[3], tloc[3];
short  rgb[3];

   GR_pushattributes ();
   //linewidth (getlwidth()*2);
   glLineWidth(2);

   for (j=0; j<p_total_pairs; j++) {
      for (i=0; i<3; i++) {
         ploc[i] = p_trail_ptr[j].ploc[i];
         tloc[i] = p_trail_ptr[j].tloc[i];
         rgb[i] = p_trail_ptr[j].rgb[i];
      }
      GR_RGBcolor (rgb[0], rgb[1], rgb[2]);
      GR_bgnline ();
      GR_v3d (ploc);
      GR_v3d (tloc);
      GR_endline ();
   }

   GR_popattributes ();
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
   extern void TrackPicked(int);

   airobj_pick_class *airobj_pick = new airobj_pick_class (this, window);

   printf ("AirObj id #%d, type %d, is picked..",
	   (p_id & 0x000fffff),
	   p_type
	   );
   switch (event.button)
   {
    case GR_RIGHTMOUSE:
      if (event.down)
      {
	 printf ("..by GR_RIGHTMOUSE..");
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
    case GR_LEFTMOUSE:
      if (event.down)
      {
	 printf (".. by GR_LEFTMOUSE..");
      }
      TrackPicked(p_id & 0x000fffff);
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
      v_LAT = (int) (airobj->get_lat())%90;
      v_LON = (int) (airobj->get_lon())%180;
      v_ALT = (int) (airobj->get_alt());;
      setvparams (win, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
      XmScaleSetValue (scaleLAT, v_LAT);
      XmScaleSetValue (scaleLON, v_LON);
      XmScaleSetValue (scaleALT, v_ALT);
      win->draw ();
   }

}
