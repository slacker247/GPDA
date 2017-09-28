/********************************************************************
  Texture mapped earth, use tiling for higher resolution version, 
  with area texture capability;

  -- created by Y. Tung, 9/1/92:

*********************************************************************/

#ifndef GR_IMAGE
#define GR_IMAGE                            "GR_Imageclass"

#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <Xm/MessageB.h>
#include "GR_Interface.H"
#include "GR_DispObj.H"
#include "gltk.h"

class GR_Image: public GR_DispObj
{
 private:
   char*    p_filename;
   long     p_tiled_gr_objid;
   Boolean  p_dotiling;
   int      p_component;
   int      p_xsize;
   int      p_ysize;
   float    p_lon;
   float    p_lat;
   float    p_alt;
   TK_RGBImageRec  *mapimage;
   IMAGE           *mapsized;
   
 protected:

 public:
   GR_Image (char* mapfile, int type, int width, int height, float lat, float lon);
   void set_lla(float lat, float lon, float alt);
   void objdraw ();
};


#endif
