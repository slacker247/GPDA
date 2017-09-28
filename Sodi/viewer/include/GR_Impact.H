
#ifndef		GR_IMPACT
#define		GR_IMPACT		"GR_Impactclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

class GR_Impact: public GR_DispObj
{
 private:
//   long   p_type;
   double p_ulen;
   double p_vlen;
   double p_major;
   double p_minor;
   double p_angle;
   double p_lat;
   double p_lon;
   double p_vert[3];
   short  p_r;
   short  p_g;
   short  p_b;

   void draw_ellipse(double x, double y, double z);

 public:
   GR_Impact(long type, float latitude, float longitude, 
             float major, float minor, float orient);
   void  set_rgb(short r, short g, short b);
   float get_orient() { return ((float)p_angle); }
   float get_major()  { return ((float)p_major); }
   float get_minor()  { return ((float)p_minor); }
   void  objdraw ();
};

#endif

