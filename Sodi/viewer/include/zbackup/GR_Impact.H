
#ifndef		GR_IMPACT
#define		GR_IMPACT		"GR_Impactclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

class GR_Impact: public GR_DispObj
{
 private:
   long  p_type;
   float p_ulen;
   float p_vlen;
   float p_angle;
   float p_lat;
   float p_lon;
   float p_vert[3];

   void draw_ellipse(float x, float y, float z);

 public:
   GR_Impact(long type, float latitude, float longitude, 
             float major, float minor, float orient);
   GR_Impact(long type, float x, float y, float z, 
             float major, float minor, float orient);
   void objdraw ();
};

#endif

