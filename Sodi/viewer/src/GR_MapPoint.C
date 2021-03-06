/***************************************************************************

	See GR_MapPoint.H
  
****************************************************************************/

#include <unistd.h>

#include "GR_MapPoint.H"

#define RADDEG  0.0174532925199432958

void MapPointCB (Widget, XtPointer objptr, XtPointer);
extern char*  get_type_string (long id, long type);

Mappoint::Mappoint (float x, float y, float z)
{
   xyz[0] = x;
   xyz[1] = y;
   xyz[2] = z;
   p_next = NULL;
}

GR_MapPoint::GR_MapPoint (GR_DispList* trail_displist, long id, long type,
		    short r, short g, short b)
{
   p_trail_displist = trail_displist;
   if (!p_trail_displist)
     printf ("Warning: NULL trail displist given.\n");
   else
     p_trail_displist->add_object (this);

   set_id (id);
   set_type (type);
   p_head = NULL;
   p_tail = NULL;
   p_num_tpoints = 0;
   p_r = r;
   p_g = g;
   p_b = b;
   p_push_count = 0;
   p_push_every = 1;
}

void
GR_MapPoint::push_tpoint (float x, float y, float z)
{
   if (p_push_count % p_push_every == 0) {
      if (p_num_tpoints >= MAX_NUM_TPOINTS) thin_tpoints ();

      Mappoint* tpoint = new Mappoint (x, y, z);  // create one trail point element:
      p_num_tpoints++;

      if (!p_tail) {                              // empty trail stack:
	 p_head = tpoint;
	 p_tail = tpoint;
      } else {
	 p_tail->set_next (tpoint);
	 p_tail = tpoint;
      }
   }
   p_push_count++;                                // increment no matter what;   
}

void
GR_MapPoint::thin_tpoints ()
{
   printf (" Trail number exceeding limits: thin stack by half...");

   Mappoint *evenptr;
   Mappoint *oddptr;
   Mappoint *goneptr;

   evenptr = p_head;
   oddptr = evenptr->get_next ();
   goneptr = oddptr;

   for (; oddptr->get_next();) {
      evenptr->set_next (oddptr->get_next());
      evenptr= oddptr->get_next();
      oddptr->set_next (evenptr->get_next());
      oddptr= evenptr->get_next();
      if (goneptr)
	delete goneptr;
      else
        fprintf (stderr, "Warning: thin_tpoints garbage collection error 1.\007\n");
      goneptr = oddptr;
   }
   p_tail = evenptr;
   p_tail->set_next (NULL);
   if (goneptr)
     delete goneptr;
   else
     fprintf (stderr, "Warning: thin_tpoints garbage collection error 2.\007\n");

   p_num_tpoints /= 2;
   p_push_every *= 2;

   printf (".......done.\n");
}

void
GR_MapPoint::draw_all_tpoints (short r, short g, short b)
{
Mappoint*       curr_ptr = p_head;
extern GR_Window *mapwindow;
 
   GR_pushmatrix (); 
   GR_pushattributes ();
   
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   //gluOrtho2D(124.5*RADDEG, 129.67*RADDEG, 36.33*RADDEG, 39.67*RADDEG);
   gluOrtho2D(mapwindow->left(), mapwindow->right(),
	   mapwindow->bottom(), mapwindow->top() );
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   GR_linewidth ((short)GR_getlwidth()*2);
   GR_color (r, g, b);
   GR_bgnline ();
   for (curr_ptr=p_head; curr_ptr!=NULL; curr_ptr=curr_ptr->get_next() ) {
     //printf (" >>> draw point (%f %f %f)\n", 
     //curr_ptr->xyz[0], curr_ptr->xyz[1], curr_ptr->xyz[2]); 
      GR_v3f (curr_ptr->xyz);
   }
   GR_endline ();

   GR_popattributes ();
   GR_popmatrix ();
}

void
GR_MapPoint::objdraw ()
{
   draw_all_tpoints (p_r, p_g, p_b);
}


/* --- for mouse picking purpose --- */

class map_pick_class
{
 public:
   map_pick_class (GR_MapPoint* trail, GR_Window* window)
   {
      p_trail = trail;
      p_window = window;
   }
   GR_MapPoint* p_trail;
   GR_Window* p_window;
};


void
GR_MapPoint::pickEvent(GR_MouseEvent& event, GR_Window* window)
{
Widget          dialog;
char            str[80];
XmString        xstr, trailstr;
map_pick_class  *trail_pick = new map_pick_class (this, window);

   printf ("Trail of Object id #%d, type %d, was picked..",
	   (p_id & 0x000fffff),
	   p_type
	   );
   switch (event.button)
   {
    case GR_LEFTMOUSE:
      if (event.down)
      {
	 printf ("..by GR_LEFTMOUSE..");
	 dialog = XmCreateMessageDialog (window->widget(), "message", NULL, 0);
	 sprintf (str, "Trail of Object id #%d, type %s",
		  (p_id & 0x000fffff),
		  get_type_string (p_id, p_type)
		  );
	 xstr = XmStringCreateSimple (str);
	 trailstr = XmStringCreateSimple ("Trail");
	 XtVaSetValues (dialog,
			XmNmessageString, xstr,
			XmNhelpLabelString, trailstr,
			NULL);
	 XtAddCallback (dialog,
			XmNhelpCallback,
			(XtCallbackProc)MapPointCB,
			(XtPointer)trail_pick);
	 XmStringFree (xstr);
	 XmStringFree (trailstr);
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
MapPointCB (Widget, XtPointer objptr, XtPointer)
{
   long flag = 0;
   
   map_pick_class *trail_pick = (map_pick_class*)objptr;
   if (trail_pick->p_trail->get_visible_flag () == 0) flag = 1;
   trail_pick->p_trail->set_visible_flag (flag);
   trail_pick->p_window->draw();
}
