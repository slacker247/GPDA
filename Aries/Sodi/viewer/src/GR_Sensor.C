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
#include "def.H"
#include <Xm/MessageB.h>

extern char*   get_type_string (long id, long type);

GLfloat mat0_ambient[]    = { 0.2, 0.2, 0.2, 0.3 };   // Define a transparent material
GLfloat mat0_diffuse[]    = { 0.8, 0.8, 0.8, 0.3 };   // Define a transparent material
GLfloat mat0_specular[]   = { 1.0, 1.0, 1.0, 1.0 };   // NOT default values
GLfloat mat0_shininess[]  = { 20.0 };                 // NOT default values
GLfloat light3_diffuse[]  = {1.0, 0.0, 0.0, 1.0};
GLfloat light3_position[] = {-1.0, 1.0, 2.0, 0.0};

int            senseR = 250, senseG = 0, senseB = 0;

void GR_Sensor::gen_sensor()
{
float           newx;
GLUquadricObj   *quadobj;
//
//   This routine builds a GL display list for a cone sensor. The 'p_xx' variables
//   defining the sensor characteristics may be changed at any time. Thus, we need
//   to delete the existing display list and create a new one with the same id.
//
   if (glIsList(p_gr_objid)) glDeleteLists(p_gr_objid, 1);
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);

     p_base = p_rmax*sin(p_fov/2.0*M_PI/180.0);
     newx = 1.0 + (p_rmax+p_alt)/RE;              // Take into account the length of the cone
     glRotatef(p_lon, 0.0, 1.0, 0.0);             // Rotate to correct longitude from Greenwich
     glRotatef(-p_lat, 1.0, 0.0, 0.0);            // Rotate to correct latitude from Equator
     glTranslatef(0.0, 0.0, newx);                // Move apex to correct location on surface
     glRotatef(180.0-p_azi, 0.0, 0.0, 1.0);       // Rotate to correct orientation from North
     glRotatef(90.0-p_elev, 1.0, 0.0, 0.0);       // Rotate to correct elevation from horizon

     quadobj = gluNewQuadric();
     gluQuadricDrawStyle(quadobj, (GLenum)GLU_FILL);
     gluQuadricOrientation(quadobj, (GLenum)GLU_OUTSIDE);
     gluQuadricTexture(quadobj, (GLboolean)GL_FALSE);
     gluQuadricNormals(quadobj, (GLenum)GLU_SMOOTH);
     gluCylinder(quadobj, (GLdouble)0.0, (GLdouble)p_base, (GLdouble)p_rmax, 32, 32);
     gluDeleteQuadric(quadobj);

   GR_closeobj ();
}
/*==============================================================================
 *
 *      sentype = 0         Cone sensor
 *                              params[0] = elevation angle
 *                              params[1] = azimuth angle
 *                              params[2] = maximum range
 *                              params[3] = minimum range
 *                              params[4] = Field-of-View
 *
 *      sentype = 1         Dome sensor
 *                              params[2] = range (kilometers)
 *
 *      sentype = 2         Disk sensor
 *                              params[0] = beginning angle
 *                              params[1] = coverage angle
 *                              params[2] = maximum range
 *                              params[3] = minimum range
 *
 *      sentype = 3         Torus sensor
 *                              params[0] = aperature
 *                              params[2] = range (kilometers)
 *
 *      sentype = 4         Over The Horizon Backscatter
 *                              params[0] = beginning angle
 *                              params[1] = coverage angle
 *                              params[2] = maximum range
 *                              params[3] = minimum range
 *
 *============================================================================*/
GR_Sensor::GR_Sensor(long pid, long sid, int sentype, float latitude, float longitude, 
                     float *params, int cover)
{
int            n=6;                           // curvature resolution
int            i, j, k, flags[10];
float          phi; 
float          vert[3];
float          lon, lat, fov;
float          factor = 1.0000;               // to compensate for normal lines to show
float          theta1, theta2, theta, delta_theta;
float          r, delta_r;
float          radius, base, height, apex, newx;
float          vert1max[2], vert1min[2], vert2max[2], vert2min[2];
float          bs = 45.0, coverage = 125.0, max = 0.2, min = 0.05;
float          center[3], rimpoint[3];
float          alpha, psi;
float          aperture = 240, range = 0.25;
int            old_linewidth;

static Boolean first_Dome = TRUE;
unsigned long  domemask[32];
unsigned long  conemask[32];
unsigned long  torusmask[32];
unsigned long  torusside[32];

GLUquadricObj   *quadobj;

   if (first_Dome) {
      first_Dome = FALSE;
      for (i=7; i<24; i++) {
         domemask[i] = 170*32768+170*256;
         conemask[i] = 170*32768+170*256;
         torusmask[i] = 170*32768+170*256;
         torusside[i] = (16384+2048+256+32+4)*2; //10010010 01001000;
      }
      GR_defpattern (11, 32, conemask);
      GR_defpattern (12, 32, domemask);
      GR_defpattern (13, 32, torusmask);
      GR_defpattern (14, 32, torusside);
      glLightfv(GL_LIGHT3, GL_DIFFUSE, light3_diffuse);
      glLightfv(GL_LIGHT3, GL_POSITION, light3_position);
   }

   GR_DispObj::set_id (sid);

   p_pid   = pid;
   p_sid   = sid;
   p_coverage = cover;
   p_stype = sentype;
   p_lat   = latitude;
   p_lon   = longitude;
   p_alt   = 0.0;

   lat = latitude*M_PI/180;
   lon = longitude*M_PI/180;
   factor = 1.0; // + params[1]/RE;
   //lat = atan2(tan(lat), (1.0-0.003359)*(1.0-0.003359));
   vert[0] = cos(lat) * sin(lon) * factor;
   vert[1] = sin(lat) * factor;
   vert[2] = cos(lat) * cos(lon) * factor;

   if (sentype < 0) {
      p_gr_objid = -1;
      return;
   }

   switch (sentype) {

     case S_CONE:
       if (INFOfp != NULL)
          fprintf(INFOfp, "--- A new CONE sensor has been created. --- \n");

       p_elev = params[0];
       p_azi  = params[1];
       p_rmax = params[2]/RE;
       p_rmin = params[3]/RE;
       p_fov  = params[4];

       gen_sensor();
       break;

     case S_DOME:
       if (INFOfp != NULL)
         fprintf(INFOfp, "--- A new DOME sensor has been created. --- \n");

       p_range = params[2]/RE;

       p_gr_objid = GR_genobj ();
       GR_makeobj (p_gr_objid);

       glRotatef(longitude, 0.0, 1.0, 0.0);     // Rotate to correct longitude from Greenwich
       glRotatef(-latitude, 1.0, 0.0, 0.0);     // Rotate to correct latitude from Equator
       glTranslatef(0.0, 0.0, 1.0);             // Move to correct location on surface

       quadobj = gluNewQuadric(); 
       gluQuadricDrawStyle(quadobj, (GLenum)GLU_FILL); 
       gluQuadricOrientation(quadobj, (GLenum)GLU_OUTSIDE); 
       gluQuadricTexture(quadobj, (GLboolean)GL_FALSE); 
       gluQuadricNormals(quadobj, (GLenum)GLU_SMOOTH); 
       gluSphere(quadobj, (GLdouble)p_range, (GLint)32, (GLint)32); 
       gluDeleteQuadric(quadobj);

       GR_closeobj ();
       break;

     case S_DISK:
       if (INFOfp != NULL)
         fprintf(INFOfp, "--- A new DISK sensor has been created. --- \n");

       bs = params[0];
       coverage = params[1];
       max = params[2]/RE;
       min = params[3]/RE;

       p_gr_objid = GR_genobj ();
       GR_makeobj (p_gr_objid);

       glRotatef(longitude, 0.0, 1.0, 0.0);     // Rotate to correct longitude from Greenwich
       glRotatef(-latitude, 1.0, 0.0, 0.0);     // Rotate to correct latitude from Equator
       glTranslatef(0.0, 0.0, 1.0);             // Move to correct location on surface

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
       GR_color (255, 100, 100);
       GR_circf (0.0, 0.0, 0.002907); // draw a disk w. radius = 10 nm
       GR_color (100, 255, 100);
       if (min != 0)
         GR_arc (0.0, 0.0, min, theta1*10, theta2*10);
       if (coverage != 360.0) {
          GR_arc (0.0, 0.0, max, theta1*10, theta2*10);
          GR_bgnline ();
          GR_v2f (vert1max);
          GR_v2f (vert1min);
          GR_endline ();
          GR_bgnline ();
          GR_v2f (vert2max);
          GR_v2f (vert2min);
          GR_endline ();
       } else {
          GR_circ (0.0, 0.0, max);
       }

       GR_closeobj ();
       break;

     case S_TORUS:
       if (INFOfp != NULL)
         fprintf(INFOfp, "--- A new TORUS sensor has been created. --- \n");

       aperture = params[0];
       p_range = params[2]/RE;

       p_gr_objid = GR_genobj ();
       GR_makeobj (p_gr_objid);

       glRotatef(longitude, 0.0, 1.0, 0.0);     // Rotate to correct longitude from Greenwich
       glRotatef(-latitude, 1.0, 0.0, 0.0);     // Rotate to correct latitude from Equator
       glTranslatef(0.0, 0.0, 1.0);             // Move to correct location on surface 

       GR_torus(p_range/2.0, p_range);

       GR_closeobj ();
       break;

     case S_OTHB:
       if (INFOfp != NULL)
         fprintf(INFOfp, "--- A new OTHB sensor has been created. --- \n");

       bs = params[0];
       coverage = params[1];
       max = params[2]/RE;
       min = params[3]/RE;
       theta1 = 90 - bs - coverage/2;
       theta2 = 90 - bs + coverage/2;
       delta_theta = 7.5;
       delta_r = (max - min)/3.01;

       p_gr_objid = GR_genobj ();
       GR_makeobj (p_gr_objid);

       glTranslatef(vert[0], vert[1], vert[2]);

       GR_color (200, 100, 100);
       GR_circf (0.0, 0.0, 0.0002907); // draw a disk w. radius = 1 nm
       GR_color (180, 250, 180);
       for (theta = theta1; theta < theta2; theta += delta_theta) {
         GR_bgnline ();
         for (r = min; r < max; r += delta_r) {
           vert[0] = r * GR_cosf (theta*M_PI/180.00);
           vert[1] = r * GR_sinf (theta*M_PI/180.00);
           vert[2] = 0.005 - GR_sinf (r) * GR_tanf (r/2.0);
           GR_v3f (vert);
         }
         GR_endline ();
       }
       for (r = min; r < max; r += delta_r) {
         GR_bgnline ();
         for (theta = theta1; theta < theta2; theta += delta_theta) {
           vert[0] = r * GR_cosf (theta*M_PI/180.00);
           vert[1] = r * GR_sinf (theta*M_PI/180.00);
           vert[2] = 0.005 - GR_sinf (r) * GR_tanf (r/2.0);
           GR_v3f (vert);
         }
         GR_endline ();
       }

       GR_closeobj ();
       break;

    default:
      break;
   }
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
   unsigned long conemask[32];

   if (first_Cone)
   {
      first_Cone = FALSE;
      for (i=7; i<24; i++)
         conemask[i] = 170*32768+170*256;
      GR_defpattern (11, 32, conemask);
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
   unsigned long torusmask[32];
   unsigned long torus_side_mask[32];
   
   int old_linewidth;

   //printf ("   --- a new torus sensor has been created. --- \n");

   if (first_Torus)
   {
      first_Torus = FALSE;
      for (i=7; i<24; i++)
         torusmask[i] = 170*32768+170*256;
      GR_defpattern (13, 32, torusmask);
      for (i=7; i<24; i++)
         torus_side_mask[i] = (16384+2048+256+32+4)*2; //10010010 01001000;
      GR_defpattern (14, 32, torus_side_mask);

   }

   GR_DispObj::set_id (sid);
   p_pid = pid;
   p_sid = sid;
   alpha = aperture*M_PI/180.0/2;

   float lat = 0.8465;
   float lon = -2.155;
   float factor = 1.0015;

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
   GR_setpattern (13);
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

void GR_Sensor::objdraw ()
{
float           radius;
GLUquadricObj   *quadobj;
 static float  DE_Range = 10.0;

   if (p_gr_objid < 0) return;

   GR_pushmatrix (); 
   GR_pushattributes ();

   glColor3f(0.75, 0.0, 0.0);
   glMaterialfv(GL_FRONT, GL_AMBIENT, mat0_ambient);
   glMaterialfv(GL_FRONT, GL_DIFFUSE, mat0_diffuse);
   glMaterialfv(GL_FRONT, GL_SPECULAR, mat0_specular);
   glMaterialfv(GL_FRONT, GL_SHININESS, mat0_shininess);

   glShadeModel(GL_SMOOTH);
   glEnable(GL_LIGHTING);
   glDisable(GL_COLOR_MATERIAL);
   glEnable(GL_DEPTH_TEST);
   glEnable(GL_CULL_FACE);
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   glEnable(GL_LINE_SMOOTH);
   glDepthFunc(GL_LESS);
   glDisable(GL_LIGHT0);
   glEnable(GL_LIGHT3);
   //
   //   Cone sensors can change FOV, Range, Altitude, Azimuth, and Elevation
   //   at any time, so a new display list may have been generated if a cone
   //   sensor parameter was changed.
   //
   if (p_coverage) GR_callobj (p_gr_objid);

   glDisable(GL_BLEND);
   glDisable(GL_LIGHT3);
   glEnable(GL_LIGHT0);

   GR_popattributes ();
   GR_popmatrix ();
}

void
GR_Sensor::pickEvent (GR_MouseEvent& event, GR_Window* window)
{
Widget      dialog;
Arg         arg[10];
char        str [80]; 
XmString    xstr;
extern void AssetPicked(int);

   fprintf (stderr, "Sensor object #%d was picked..", (get_id() & 0x000fffff) );

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
	 /*
         xstr = XmStringCreateSimple (str);
         XtSetArg (arg[0], XmNmessageString, xstr);
         dialog = XmCreateMessageDialog (window->widget(), "message", arg, 1);
         XmStringFree (xstr);
         XtManageChild (dialog);
	 */
         AssetPicked(get_id() & 0x000fffff);
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
