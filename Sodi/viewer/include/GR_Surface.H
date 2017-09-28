
#ifndef		GR_SURFACE
#define		GR_SURFACE		"GR_Surfaceclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

class GR_Surface: public GR_DispObj
{
 private:
   long   p_type;
   double p_ulen;
   double p_vlen;
   double p_angle;
   double p_lat;
   double p_lon;
   double p_vert[3];
   short  p_r;
   short  p_g;
   short  p_b;

   void SurfaceRead(char *filename);

 public:
   GR_Surface(char *filename, int type, float latitude, float longitude, float major);
  //void set_rgb(short r, short g, short b);
   void objdraw ();
};

#endif

