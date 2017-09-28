/********************************************************************
  Texture mapped earth, use tiling for higher resolution version, 
  with area texture capability;

  -- created by Y. Tung, 9/1/92:

*********************************************************************/

#ifndef GR_MAP2D
#define GR_MAP2D                            "GR_Map2Dclass"

#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <Xm/MessageB.h>
#include "GR_Interface.H"
#include "GR_DispObj.H"
#include "gltk.h"

class GR_Map2D: public GR_DispObj
{
 private:
   char*    p_filename;
   long     p_tiled_gr_objid;
   float    p_texps[14];
   float    p_tevps[7];
   Boolean  p_dotiling;
   int      p_component;
   float    p_grid;
   int      p_xsize;
   int      p_ysize;
   float    p_lonleft;
   float    p_lonright;
   float    p_latupper;
   float    p_latlower;
   TK_RGBImageRec  *mapimage;
   IMAGE           *mapsized;
   
 protected:

 public:
   GR_Map2D (char* mapfile, int type, int width, int height, float lat, float lon);
   void set_grid(float degree);
   void set_corners(float lonNW, float latNW, float lonSE, float latSE);
   void objdraw ();
};


#endif
