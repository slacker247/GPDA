
#include "malloc.h"
#include "math.h"
#include "def.H"
#include <Xm/MessageB.h>

#include "GR_Defend.H"
#include "GISP_Globals.H"

/* ---------------------- */

GR_Defend::GR_Defend (long defid, int shape, float latitude, float longitude, float *size)
{
float          vert[3];
float          lon, lat;
float          factor = 1.00;               // to compensate for normal lines to show

   p_type   = T_DEFEND;
   p_ulen   = size[0]/RE;
   p_vlen   = size[1]/RE;
   p_orient = size[2];
   p_lat    = latitude;
   p_lon    = longitude;
   p_shape  = shape;
   p_r      = 0;
   p_g      = 0;
   p_b      = 255;

   p_base[0] = -p_ulen/2.0;
   p_base[1] = -p_ulen/2.0;

   p_lupper[0] = -p_ulen/2.0;
   p_lupper[1] = p_vlen/2.0;

   p_rupper[0] = p_ulen/2.0;
   p_rupper[1] = p_vlen/2.0;

   p_rlower[0] = p_ulen/2.0;
   p_rlower[1] = -p_vlen/2.0;

   p_topline_base[0] = 0.6*p_ulen;
   p_topline_base[1] = 1.1*p_ulen;

   lat = latitude*M_PI/180;
   lon = longitude*M_PI/180;
   vert[0] = cos(lat) * sin(lon) * factor;
   vert[1] = sin(lat) * factor;
   vert[2] = cos(lat) * cos(lon) * factor;
 
   p_gr_objid = GR_genobj ();
   GR_makeobj (p_gr_objid);
      GR_pushattributes();
      GR_pushmatrix();

      glRotatef(p_lon, 0.0, 1.0, 0.0);           // Rotate to correct longitude from Greenwich
      glRotatef(-p_lat, 1.0, 0.0, 0.0);          // Rotate to correct latitude from Equator
      glRotatef(180.0-p_orient, 0.0, 0.0, 1.0);  // Rotate to correct orientation from North
      glTranslatef(0.0, 0.0, 1.0);               // Move to correct location on surface
      //glTranslatef(vert[0], vert[1], vert[2]);
  
      GR_linewidth (2*GR_getlwidth());
      GR_color (0, 0, 255);

      if (shape == AREA_BOX) {
	 draw_box();
      }
      else //if (shape == AREA_CIRCLE) {
         GR_circf(0.0, 0.0, p_ulen);

      GR_popmatrix();
      GR_popattributes();      
   GR_closeobj();
}

void
GR_Defend::draw_box ()
{  
   GR_bgnpolygon();
     GR_v2f(p_base);
     GR_v2f(p_lupper);
     GR_v2f(p_rupper);
     GR_v2f(p_rlower);
   GR_endline();
}

void
GR_Defend::set_rgb (short r, short g, short b)
{ 
   p_r = r;
   p_g = g;
   p_b = b;
}

void
GR_Defend::objdraw ()
{
   GR_color(p_r, p_g, p_b);
   GR_callobj (p_gr_objid);
}

