
#ifndef		GR_DEFEND
#define		GR_DEFEND		"GR_Defendclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

#define   AREA_CIRCLE    0
#define   AREA_BOX       1
#define   AREA_ELLIPSE   2
#define   AREA_POLY      3

class GR_Defend: public GR_DispObj
{
 private:
   //long  p_deftype;
   float p_ulen;
   float p_vlen;
   float p_orient;
   float p_lat;
   float p_lon;
   int   p_shape;
   float p_base[2];
   float p_lupper[2];
   float p_rupper[2];
   float p_rlower[2];
   float p_topline_base[2];
   short  p_r;
   short  p_g;
   short  p_b;

   void draw_box ();

 public:
   GR_Defend(long type, int shape, float latitude, float longitude, float size[]);
   //long get_type () { return p_deftype; }
   void set_rgb(short r, short g, short b);
   void objdraw ();
};

#endif

