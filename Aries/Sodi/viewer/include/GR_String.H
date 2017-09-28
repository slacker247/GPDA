
#ifndef GR_STRING
#define GR_STRING

#include "GR_Interface.H"
#include "GR_DispObj.H"


class GR_String: public GR_DispObj
{
 private:
   float p_x, p_y, p_z;
   float p_r, p_g, p_b;
   float p_lat, p_lon;
   char  p_text[24];
   long  p_gr_objid;
   
 public:
   GR_String (float x = 0.0, float y = 0.0, float z = 0.0, char* s = NULL);
   GR_String (float latitude = 0.0, float longitude = 0.0, char* s = NULL);
   void set_color (int r, int g, int b);
   void set_text (char *s = NULL);
   void objdraw ();
};

#endif
