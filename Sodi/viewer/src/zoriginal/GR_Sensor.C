/**********************************************************
  Sensor object.
 
  -- 11/12/92: created by Y. Tung;
  -- 11/19/92: cleaned up the links;
  -- 12/04/92: added hemisphere and torus type sensors;
  -- 01/08/93: modified torus drawing to use wir frame plus pattern;
  -- 01/19/93: used the name pickEvent instead of v_process_pick;
  -- 04/27/93: add mask 0x000fffff for the picked object id;
  -- 07/20/93: comment out print statments "-- a new ...sensor ...";
  
**********************************************************/

#include "GR_Sensor.H"
#include "malloc.h"
#include "math.h"
#include <Xm/MessageB.h>

extern char*  get_type_string (long id, long type);


/* ---------------------- */

GR_Sensor::GR_Sensor (long pid, long sid, float bs, float coverage, float max, float min)
{
   float theta1, theta2;
   float vert1max[2], vert1min[2], vert2max[2], vert2min[2];

   GR_DispObj::set_id (sid);
   p_pid = pid;
   p_sid = sid;

   theta1 = 90 - bs - coverage/2;
   theta2 = 90 - bs + coverage/2;

   vert1max[0] = max * GR_cosf (theta1*M_PI/180.00);
   vert1max[1] = max * GR_sinf (theta1*M_PI/180.00);
   vert1min[0] = min * GR_cosf (theta1*M_PI/180.00);
   vert1min[1] = min * GR_sinf (theta1*M_PI/180.00);

   vert2max[0] = max * GR_cosf (theta2*M_PI/180.00);
   vert2max[1] = max * GR_sinf (theta2*M_PI/180.00);
   vert2min[0] = min * GR_cosf (theta2*M_PI/180.00);
   vert2min[1] = min * GR_sinf (theta2*M_PI/180.00);
  
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   GR_color (255, 100, 100);
   GR_circf (0.0, 0.0, 0.002907); // draw a disk w. radius = 10 nm
   GR_color (100, 255, 100);
   if (min != 0)
     GR_arc (0.0, 0.0, min, theta1*10, theta2*10);
   if (coverage != 360.0)
   {
      GR_arc (0.0, 0.0, max, theta1*10, theta2*10);
      GR_bgnline ();
      GR_v2f (vert1max);
      GR_v2f (vert1min);
      GR_endline ();
      GR_bgnline ();
      GR_v2f (vert2max);
      GR_v2f (vert2min);
      GR_endline ();
   }
   else
   {
      GR_circ (0.0, 0.0, max);
   }
      
   GR_closeobj();
}

GR_Sensor::GR_Sensor (long pid, long sid, float cone_angle, float aperture) 
// this is a cone mobile sensor where cone_angle is the angle between the
//  cone's aiming line and the normal direction (face straight down the earth)
//  assuming th cone can only aim towards the platform's heading direction.
//  the aperture is the aperture angle of the cone.  Both are in degrees.
{
   int i, n;
   float vert[3], point[3];
   float psi, alpha, beta, len, hd;  
   // psi is use to draw a circle, alpha is aperture in radian;
   float k; // radius of the cone bottom;
   static Boolean first_Cone = TRUE;
   //unsigned short conemask[16*16];
   unsigned short conemask[16];

   if (first_Cone)
   {
      first_Cone = FALSE;
      for (i=0; i<16; i++)
         conemask[i] = 170*256+170;
      GR_defpattern (101, 16, conemask);
   }

   GR_DispObj::set_id (sid);
   p_pid = pid;
   p_sid = sid;

   n = 6; // 4n is the number of tmeshes used;
   alpha = aperture*M_PI/180.0;
   beta = cone_angle*M_PI/180.0;
   len = 0.5; // cone's side length, take a value < 1;
   hd = 0.00; // cone's vertex position relative to the aircraft center;
   //hd = 0.02; 
   k = len*sin(alpha/2); 
   vert[0] = 0.0;
   vert[1] = hd; 
   vert[2] = 0.0; 

   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   GR_pushmatrix ();
   GR_pushattributes ();
   GR_setpattern (101);
   GR_color (225, 155, 20);
   GR_bgntmesh ();
     GR_v3f (vert); 
     for (i=0, psi=0.0; i<=4*n; i++, psi+=M_PI_2/n)
     {
         // first draw cone bottom's point:
         //point[0] = k*cos(psi);
         //point[1] = hd - k*sin(psi);
         //point[2] = -len*cos(alpha/2);
         // now rotate about 'x' by 'cone_angle':
         point[0] = k*cos(psi);
         point[1] = (hd-k*sin(psi))*cos(beta) - (-len*cos(alpha/2))*sin(beta);
         point[2] = (hd-k*sin(psi))*sin(beta) + (-len*cos(alpha/2))*cos(beta);
         GR_v3f (point);
         GR_swaptmesh();
     }
   GR_endtmesh ();
   GR_popattributes ();
   GR_popmatrix ();
   GR_closeobj();
}

// a hemisphrical ground sensor:
GR_Sensor::GR_Sensor (long pid, long sid, float radius)
{
   int n=6; // curviture resolution
   int i, j, k;
   float phi, theta; 
   float vert[3];

   static Boolean first_Dome = TRUE;
   //unsigned short domemask[16*16];
   unsigned short domemask[16];

   //printf ("   --- a new dome sensor has been created. --- \n");

   if (first_Dome)
   {
      first_Dome = FALSE;
      for (i=0; i<16; i++)
         domemask[i] = 170*256+170;
      GR_defpattern (102, 16, domemask);
   }

   GR_DispObj::set_id (sid);
   p_pid = pid;
   p_sid = sid;

   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   GR_pushmatrix ();
   GR_pushattributes ();
   GR_color (255, 100, 40);
   GR_circf (0.0, 0.0, 0.002907);
   GR_color (200, 255, 150);
   GR_circ (0.0, 0.0, radius);
   GR_color (155, 255, 100);
   GR_setpattern (102);
   for (k=1, theta=-M_PI; k<=4*n; k++, theta+= M_PI_2/n)
   {
      GR_bgnqstrip ();
      for (j=1, phi=0; j<=n+1; j++, phi+= M_PI_2/n)
      {
         vert[0] = radius * cos(phi) * sin (theta);
         vert[2] = radius * sin(phi);
         vert[1] = radius * cos(phi) * cos (theta);
         GR_v3f(vert);
 
         vert[0] = radius * cos(phi) * sin (theta+M_PI_2/n);
         vert[2] = radius * sin(phi);
         vert[1] = radius * cos(phi) * cos (theta+M_PI_2/n);
         GR_v3f(vert);
      }
      GR_endqstrip ();
   }
   GR_popattributes ();
   GR_popmatrix ();
   GR_closeobj ();
}

// a torus sensor:
GR_Sensor::GR_Sensor (long pid, long sid, int typeflag, float aperture, float range)
{
   int n=8; 
   int i, j, k;
   float alpha, psi;
   float phi, theta; 
   float center[3], rimpoint[3];

   static Boolean first_Torus = TRUE;
   //unsigned short torusmask[16*16];
   //unsigned short torus_side_mask[16*16];
   unsigned short torusmask[16];
   unsigned short torus_side_mask[16];
   
   int old_linewidth;

   //printf ("   --- a new torus sensor has been created. --- \n");

   if (first_Torus)
   {
      first_Torus = FALSE;
      for (i=0; i<16; i++)
         torusmask[i] = 170*256+170;
      GR_defpattern (103, 16, torusmask);
      for (i=0; i<16; i++)
         torus_side_mask[i] = (16384+2048+256+32+4)*2; //10010010 01001000;
      GR_defpattern (104, 16, torus_side_mask);

   }

   GR_DispObj::set_id (sid);
   p_pid = pid;
   p_sid = sid;
   alpha = aperture*M_PI/180.0/2;
   center[0] = 0.0;
   center[1] = 0.0;
   center[2] = 0.0;

   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   GR_pushmatrix ();
   GR_pushattributes ();

   // first draw side of torus, this should be more transparent;
 /*   
   //GR_color (255, 70, 55);
   //GR_color (255, 120, 75);
   GR_color (255, 200, 185);
   //setpattern (103);
   GR_setpattern (104);
   for (k=1, theta=-M_PI; k<=4*n; k++, theta+= M_PI_2/n)
   {
      GR_bgnqstrip ();
      for (j=1, phi=-alpha; j<=2*n+1; j++, phi+= alpha/n)
      {
         rimpoint[0] = range * cos(phi) * sin (theta);
         rimpoint[2] = range * sin(phi);
         rimpoint[1] = range * cos(phi) * cos (theta);
         GR_v3f(rimpoint);
 
         rimpoint[0] = range * cos(phi) * sin (theta+M_PI_2/n);
         rimpoint[2] = range * sin(phi);
         rimpoint[1] = range * cos(phi) * cos (theta+M_PI_2/n);
         GR_v3f(rimpoint);
      }
      GR_endqstrip ();
   }
 */

   old_linewidth = GR_getlwidth ();
   GR_color (255, 30, 25);
   for (k=1, theta=-M_PI; k<=8*n; k++, theta+= M_PI_2/n/2)
   {
      GR_linewidth (old_linewidth*3);
      GR_bgnline ();
      for (j=1, phi=-alpha; j<=2*n+1; j++, phi+= alpha/n)
      {
         rimpoint[0] = range * cos(phi) * sin (theta);
         rimpoint[2] = range * sin(phi);
         rimpoint[1] = range * cos(phi) * cos (theta);
         GR_v3f(rimpoint);
      }
      GR_endline ();
   }
   GR_linewidth (old_linewidth);
   // add two solid-line circles:
   GR_bgnline ();
   for (k=0, theta=-M_PI; k<=8*n; k++, theta+= M_PI_2/n/2)
   {
         rimpoint[0] = range * cos(-alpha) * sin (theta);
         rimpoint[2] = range * sin(-alpha);
         rimpoint[1] = range * cos(-alpha) * cos (theta);
         GR_v3f(rimpoint);
   }
   GR_endline ();
   GR_bgnline ();
   for (k=0, theta=-M_PI; k<=8*n; k++, theta+= M_PI_2/n/2)
   {
         rimpoint[0] = range * cos(alpha) * sin (theta);
         rimpoint[2] = range * sin(alpha);
         rimpoint[1] = range * cos(alpha) * cos (theta);
         GR_v3f(rimpoint);
   }
   GR_endline ();



   // now draw top and bottom of the torus; 
   GR_color (255, 155, 90);
   GR_setpattern (103);
   GR_bgntmesh ();
   GR_v3f (center);
   for (i=0, psi=0.0; i<=4*n; i++, psi+=M_PI_2/n)
   {
      rimpoint[0] = range * cos(alpha) * cos(psi);
      rimpoint[1] = range * cos(alpha) * sin(psi);
      rimpoint[2] = range * sin(alpha);
      GR_v3f (rimpoint);
      GR_swaptmesh ();
   }
   
   GR_v3f (center);
   for (i=0, psi=0.0; i<=4*n; i++, psi+=M_PI_2/n)
   {
      rimpoint[0] = range * cos(alpha) * cos(psi);
      rimpoint[1] = range * cos(alpha) * sin(psi);
      rimpoint[2] = -range * sin(alpha);
      GR_v3f (rimpoint);
      GR_swaptmesh ();
   }
   GR_endtmesh ();

   GR_popattributes ();
   GR_popmatrix ();
   GR_closeobj ();
}



void
GR_Sensor::objdraw ()
{
  GR_callobj (p_gr_objid);
}


void
GR_Sensor::pickEvent (GR_MouseEvent& event, GR_Window* window)
{
   Widget dialog;
   Arg arg[10];
   char str [80]; XmString xstr;

   printf ("Sensor object #%d was picked..",
	   (get_id() & 0x000fffff)
	   );
   switch (event.button)
   {
    case GR_LEFTMOUSE:
      if (event.down)
      {
         printf ("..by GR_LEFTMOUSE..");
         //sprintf (str, "Object #%d", get_id());
         if (p_pid != p_sid)
         {
           sprintf (str, "Sensor ID #%d, Type %s -- on Platform ID %d",
                    (get_id() & 0x000fffff),
                    get_type_string (get_id(), get_type()),
                    (p_pid & 0x000fffff)
                    );
         }
         else
         {
	   sprintf (str, "Sensor ID #%d, Type %s",
		    (get_id() & 0x000fffff),
                    get_type_string (get_id(), get_type())
		    );
         }
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
