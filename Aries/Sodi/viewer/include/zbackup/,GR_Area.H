/************************************************************************

  Area elevation terrain, takes a 4320x2160 rgb file for color, and a dir
  name to locate the 4320x2160 earth data set (12 files name earth_000_000
  etc, each is 1080 by 720) for elevation, generates a 10 degree by 10 
  degree area as a graphics object (i.e., 120 by 120 polygons);
 
  -- created by Tung, 10/22/92
  --  

*************************************************************************/

#ifndef GR_AREA
#define GR_AREA			 	"GR_Areaclass"

#include "GR_Interface.H"
#include "GR_DispObj.H"

/*
struct cpoint
{
  short r, g, b;
  float vec[3];
};
*/

class GR_Area: public GR_DispObj
{
 private:
   
 protected:   

 public:
   GR_Area (char* rgbfile, char* elevdir, long type, int focus_lon = v_LON, int fous_lat = v_LAT);
   void objdraw ();
};


#endif
