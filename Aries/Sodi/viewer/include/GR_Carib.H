/**********************************************************************
  This loads the detailed Caribbean areas -- ugly but fast.
  -- Tung, 3/18/93
     
********************************************************************/
#ifndef GR_CARIB
#define GR_CARIB                        "GR_Caribclass"
#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <Xm/MessageB.h>
#include "GR_Interface.H"
#include "GR_DispObj.H"

class Carib: public GR_DispObj
{
 private:
   char p_filename[9][80];
   float p_texps[7];
   float p_tevps[1];
   int p_xsize;
   int p_ysize;
   
 protected:
   int texture_init();
   void draw_Carib (long lon, long lat);

 public:
   Carib (char *dirname=NULL, long type=201);
   unsigned long* take_rgbdata (char* infile);
   void objdraw ();
};

#endif
