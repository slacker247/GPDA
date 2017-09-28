
#include "malloc.h"
#include "math.h"

#include "def.H"
#include "GR_Impact.H"
#include "convert.H"
#include "GISP_Globals.H"

GR_Impact::GR_Impact (long impid, float latitude, float longitude, 
                      float major, float minor, float orient)
{
double          vert[3];
double          lon, lat;
double          factor = 1.00; 

   p_type  = T_IMPACT;
   p_ulen  = (double)major/RE;
   p_vlen  = (double)minor/RE;
   p_major = major;
   p_minor = minor;
   p_angle = orient;
   p_lat   = latitude;
   p_lon   = longitude;
   p_r     = 180;
   p_g     = 0;
   p_b     = 0;

   lat = (double)latitude*M_PI/180.0;
   lon = (double)longitude*M_PI/180.0;
   vert[0] = cos(lat) * sin(lon) * factor;
   vert[1] = sin(lat) * factor;
   vert[2] = cos(lat) * cos(lon) * factor;

   draw_ellipse(vert[0], vert[1], vert[2]);
}

void
GR_Impact::draw_ellipse(double xx, double yy, double zz)
{
int            i, j, ix, iy;
double         a, b, x, y, a2, b2, oldx, oldy, dx;
int            slices = 32;
int            fill = GL_FALSE;

   p_vert[0] = xx;
   p_vert[1] = yy;
   p_vert[2] = zz;

   a = p_ulen;   a2 = a*a;
   b = p_vlen;   b2 = b*b;

   if (INFOfp != NULL)
      fprintf(INFOfp, "IMPACT:  Laydown of Impact Ellipse at %f %f of size %f %f %f\n",
              p_lat, p_lon, p_ulen*RE, p_vlen*RE, p_angle);

   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
      GR_pushmatrix();

      glRotated(p_lon, 0.0, 1.0, 0.0);         // Rotate to correct longitude from Greenwich
      glRotated(-p_lat, 1.0, 0.0, 0.0);        // Rotate to correct latitude from Equator
      glRotated(180.0-p_angle, 0.0, 0.0, 1.0); // Rotate to correct orientation from North
      glTranslated(0.0, 0.0, 1.0);             // Move to correct location on surface

      GR_linewidth (2*GR_getlwidth());

      dx = a/(double)slices;

   if (fill) {
      glBegin(GL_TRIANGLE_FAN);
        oldx = -a;
        oldy = 0;
        for (i=1; i<=2*slices; i++) {
          x = oldx + dx;
          y = 0 + (double)sqrt(b2 - ((x*x*b2)/a2));
          glVertex3d(a,    b,    0.0);
          glVertex3d(oldx, oldy, 0.0);
          glVertex3d(x,    y,    0.0);
          oldx = x;
          oldy = y;
        }
        for (i=1; i<=2*slices; i++) {
          x = oldx - dx;
          y = 0 - (double)sqrt(b2 - ((x*x*b2)/a2));
          glVertex3d(a,    b,    0.0);
          glVertex3d(oldx, oldy, 0.0);
          glVertex3d(x,    y,    0.0);
          oldx = x;
          oldy = y;
        }
      glEnd();
   } else {
      glBegin(GL_LINE_LOOP);
        oldx = -a;
        oldy = 0;
        glVertex3d(oldx, oldy, 0.0);
        for (i=1; i<=2*slices; i++) {
          x = oldx + dx;
          y = 0 + (double)sqrt(b2 - ((x*x*b2)/a2));
          glVertex3d(x, y, 0.0);
          oldx = x;	
        }
        for (i=1; i<=2*slices-1; i++) {
          x = oldx - dx;
          y = 0 - (double)sqrt(b2 - ((x*x*b2)/a2));
          glVertex3d(x, y, 0.0);
          oldx = x;
        }
      glEnd();
   }

      GR_popmatrix();
    
   GR_closeobj();
}

void
GR_Impact::set_rgb (short r, short g, short b)
{ 
   p_r = r;
   p_g = g;
   p_b = b;
}

void
GR_Impact::objdraw ()
{
   GR_pushattributes();

   GR_color(p_r, p_g, p_b); 
   GR_callobj (p_gr_objid);

   GR_popattributes();  
}

