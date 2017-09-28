/**********************************************************
  The OTH-B object.
  -- 02/01/93: created by Tung;
  -- 04/27/93: use mask 0x000fffff to get picked object id;
  
**********************************************************/

#include "GR_Othb.H"
#include "malloc.h"
#include "math.h"
#include <Xm/MessageB.h>

/* ---------------------- */
GR_Othb::GR_Othb (long pid, long sid, float bs, float coverage, float max, float min)
{
   float theta1, theta2, theta, delta_theta;
   float r, delta_r;
   float vert[3];
  
   GR_DispObj::set_id (sid);
   p_pid = pid;
   p_sid = sid;

   theta1 = 90 - bs - coverage/2;
   theta2 = 90 - bs + coverage/2;
   delta_theta = 7.5;
   delta_r = (max - min)/3.01; 
  
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   GR_color (200, 100, 100);
   GR_circf (0.0, 0.0, 0.0002907); // draw a disk w. radius = 1 nm
   GR_color (180, 250, 180);
   for (theta = theta1; theta < theta2; theta += delta_theta)
   {
      GR_bgnline ();
      for (r = min; r < max; r += delta_r)
      {
         vert[0] = r * GR_cosf (theta*M_PI/180.00);
         vert[1] = r * GR_sinf (theta*M_PI/180.00);
         vert[2] = 0.005 - GR_sinf (r) * GR_tanf (r/2.0);
         GR_v3f (vert);
      }
      GR_endline ();
   }
   for (r = min; r < max; r += delta_r)
   {
      GR_bgnline ();
      for (theta = theta1; theta < theta2; theta += delta_theta)
      {
         vert[0] = r * GR_cosf (theta*M_PI/180.00);
         vert[1] = r * GR_sinf (theta*M_PI/180.00);
         vert[2] = 0.005 - GR_sinf (r) * GR_tanf (r/2.0);
         GR_v3f (vert);
      }
      GR_endline ();
   }
   GR_closeobj();
}



void
GR_Othb::objdraw ()
{
  GR_callobj (p_gr_objid);
}


void
GR_Othb::pickEvent (GR_MouseEvent& event, GR_Window* window)
{
   Widget dialog;
   Arg arg[10];
   char str [80]; XmString xstr;

   printf ("Othb object #%d was picked..",
	   (get_id() & 0x000fffff)
	   );
   switch (event.button)
   {
    case GR_LEFTMOUSE:
      if (event.down)
      {
         printf ("..by GR_LEFTMOUSE..");
         sprintf (str, "Object #%d",
		  (get_id() & 0x000fffff)
		  );
         xstr = XmStringCreateSimple (str);
         XtSetArg (arg[0], XmNmessageString, xstr);
         dialog = XmCreateMessageDialog (window->widget(), "message", arg, 1);
         XmStringFree (xstr);
         XtManageChild (dialog);
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
