/************************************************************************

*************************************************************************/

#ifndef GR_STARS
#define GR_STARS

#include "GR_Interface.H"
#include "GR_DispObj.H"

class GR_Stars: public GR_DispObj
{
 private:
   char* p_filename;
   short p_R, p_G, p_B;
   
 public:
   GR_Stars (short R = 255,
	     short G = 255,
	     short B = 255,
	     char* filename = NULL,
             long type = 820);
   void objdraw ();
};

#endif
