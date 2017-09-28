
#include "malloc.h"
#include "math.h"

#include "def.H"
#include "GR_TrackErr.H"
#include "convert.H"
#include "GISP_Globals.H"

GR_TrackErr::GR_TrackErr (long impid, float latitude, float longitude,
                      float Radius, float Length, 
                      float Px, float Py, float Pz, int trkid)
{
double          vert[3];
double          lon, lat;
double          factor = 1.00; 

   p_type  = impid;
   p_trkid = trkid;
   p_ulen  = (double)Radius/RE*2.0;
   p_vlen  = (double)Length/RE*2.0;
   p_major = Radius;
   p_minor = Length;
   p_lat   = latitude;
   p_lon   = longitude;
   p_r     = 0;
   p_g     = 0;
   p_b     = 255;

   if (INFOfp != NULL)
      fprintf(INFOfp, "TRACKERR:  Laydown of Cylinder at %f %f %f of size %f %f %d\n",
              Px, Py, Pz, p_ulen, p_vlen, p_trkid);
   /*
   lat = (double)latitude*M_PI/180.0;
   lon = (double)longitude*M_PI/180.0;
   vert[0] = cos(lat) * sin(lon) * factor;
   vert[1] = sin(lat) * factor;
   vert[2] = cos(lat) * cos(lon) * factor;
   */
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   //draw_cylinder((double)Px, (double)Py, (double)Pz);
   GR_closeobj();
}

void GR_TrackErr::update(float Px, float Py, float Pz)
{
   if (glIsList(p_gr_objid)) glDeleteLists(p_gr_objid, 1);
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
   draw_cylinder((double)Px, (double)Py, (double)Pz);
   GR_closeobj ();
}

void
GR_TrackErr::draw_cylinder(double xx, double yy, double zz)
{
int             i, j, ix, iy;
double          a, b, x, y, a2, b2, oldx, oldy, dx;
int             slices = 32;
int             fill = GL_FALSE;
GLUquadricObj   *quadobj; 

   p_vert[0] = xx;
   p_vert[1] = yy;
   p_vert[2] = zz;

   GR_pushmatrix();

   //glRotated(p_lon, 0.0, 1.0, 0.0);          // Rotate to correct longitude from Greenwich
   //glRotated(-p_lat, 1.0, 0.0, 0.0);         // Rotate to correct latitude from Equator
   //glTranslated(0.0, 0.0, zz-p_vlen/2.0);    // Move to correct location on surface
   //glRotated(180.0-p_angle, 0.0, 0.0, 1.0);  // Rotate to correct orientation from North
   glTranslated(xx, yy, zz);                   // Move to correct location on surface
   glRotated(-90.0, 0.0, 1.0, 0.0);            // Point cykinder down range
   glRotated(-25.0, 1.0, 1.0, 1.0);            // Point cylinder to correct elevation
   glTranslated(0.0, 0.0, -p_vlen/2.0);        // Center cylinder on object

   quadobj = gluNewQuadric();
   gluQuadricDrawStyle(quadobj, (GLenum)GLU_FILL);
   gluQuadricOrientation(quadobj, (GLenum)GLU_OUTSIDE);
   gluQuadricTexture(quadobj, (GLboolean)GL_FALSE);
   gluQuadricNormals(quadobj, (GLenum)GLU_SMOOTH);
   gluCylinder(quadobj, (GLdouble)p_ulen, (GLdouble)p_ulen, (GLdouble)p_vlen, 32, 32);
   gluDeleteQuadric(quadobj); 

   GR_popmatrix();    
}

void
GR_TrackErr::set_rgb (short r, short g, short b)
{ 
   p_r = r;
   p_g = g;
   p_b = b;
}

void
GR_TrackErr::objdraw ()
{
GLfloat trk0_ambient[]    = { 0.2, 0.2, 0.2, 0.3 };   // Define a transparent material
GLfloat trk0_diffuse[]    = { 0.8, 0.8, 0.8, 0.3 };   // Define a transparent material
GLfloat trk0_specular[]   = { 1.0, 1.0, 1.0, 1.0 };   // NOT default values
GLfloat trk0_shininess[]  = { 20.0 };                 // NOT default values
GLfloat light4_diffuse[]  = { 0.0, 0.0, 1.0, 1.0};    // Defaults, these may change
GLfloat light4_position[] = {-1.0, 1.0, 2.0, 0.0};

   GR_pushmatrix (); 
   GR_pushattributes();

   //glColor3f(0.75, 0.0, 0.0);
   glMaterialfv(GL_FRONT, GL_AMBIENT, trk0_ambient);
   glMaterialfv(GL_FRONT, GL_DIFFUSE, trk0_diffuse);
   glMaterialfv(GL_FRONT, GL_SPECULAR, trk0_specular);
   glMaterialfv(GL_FRONT, GL_SHININESS, trk0_shininess);

   light4_diffuse[0] = (float)p_r/255.0;
   light4_diffuse[1] = (float)p_g/255.0;
   light4_diffuse[2] = (float)p_b/255.0;
   light4_diffuse[3] = 1.0;
   glLightfv(GL_LIGHT4, GL_DIFFUSE, light4_diffuse);
   glLightfv(GL_LIGHT4, GL_POSITION, light4_position);
 
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
   glEnable(GL_LIGHT4);

   //GR_color(p_r, p_g, p_b); 
   GR_callobj (p_gr_objid);

   glDisable(GL_BLEND);
   glDisable(GL_LIGHT4);
   glEnable(GL_LIGHT0); 

   GR_popattributes();
   GR_popmatrix ();  
}

