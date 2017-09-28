#ifndef GR_BEARTH
#define GR_BEARTH                            "GR_Bearthclass"

#include "GR_Interface.H"
#include "GR_DispObj.H"

class GR_Blueearth: public GR_DispObj
{
 private:
   
 protected:
   void draw_sphere (int n, Boolean nflag, Boolean tflag);
   
 public:
   GR_Blueearth (long type = 200, int n = 18);
   void objdraw ();
};

#endif
