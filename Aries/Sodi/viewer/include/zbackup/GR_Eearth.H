/************************************************************************

  Elevation earth;
  -- created by Tung, 10/8/92
  --  

*************************************************************************/

#ifndef GR_EEARTH
#define GR_EEARTH			"GR_Eearthclass"

#include "GR_Interface.H"
#include "GR_DispObj.H"


struct cpoint
{
  short r, g, b;
  float vec[3];
};

class GR_Eearth: public GR_DispObj
{
 private:
   
 protected:   

 public:
   GR_Eearth (char* filename = NULL, long type = 311);
   void objdraw ();
};


#endif
