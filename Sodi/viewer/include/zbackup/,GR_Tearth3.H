/**********************************************************************
  Second algorithm of the texture earth -- this could replace the older
  algorithm GR_Tearth.H if desired.
  
  GR_Tearth3 draws 14 textures for the earth: 2 big low-resolution areas
  for the poles (30 degree by 360 degree each, south pole and north pole),
  and 12 equal size squares (60 degree by 60 degree each). For instance,
  if extract from a 7926x3963 rgb file, we may have two 3072x256 textures
  plus twelve 1024x1024 textures.
  
  -- 06/29/93: created, Tung.
  -- 07/13/93: changed the granularity;
     
********************************************************************/
#ifndef GR_TEARTH3
#define GR_TEARTH3                       "GR_Tearth3class"
#include <math.h>
#include <gl/image.h>
#include <sys/types.h>
#include <malloc.h>
#include <Xm/MessageB.h>
#include "GR_Interface.H"
#include "GR_DispObj.H"

extern "C" IMAGE* iopen (const char*, const char*); 
extern "C" void iclose (IMAGE*);
extern "C" void getrow (IMAGE*,unsigned short*, int, int);

class GR_Tearth3: public GR_DispObj
{
 private:
   char p_filename[52][80];
   float p_texps[7];
   float p_tevps[1];
   int p_xsize;
   int p_ysize;
   
 protected:
   void texture_init();
   void draw_area (long slon, long slat, long elon, long elat);

 public:
   GR_Tearth3 (char *dirname=NULL, long type=301);
   unsigned long* take_rgbdata (char* infile);
   void objdraw ();
};

#endif
