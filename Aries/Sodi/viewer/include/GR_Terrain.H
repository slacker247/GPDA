/*********************************************************************
 *
 * Texture mapped earth, use tiling for higher resolution version, 
 * with area texture capability;
 *
 *********************************************************************/

#ifndef GR_TERRAIN
#define GR_TERRAIN  "terrainclass"

#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <stdio.h>

#include "GR_Interface.H"
#include "GR_DispObj.H"
#include "def.H"

//typedef int Boolean;

class GR_Terrain: public GR_DispObj
{
 private:
   char*          p_filename;
   int            p_tlid;
   Boolean        p_terrain;
   int            p_type;
   int            p_xsize;
   int            p_ysize;
   float          p_lat;
   float          p_lon;
   float          p_alt;
   float          p_azi;
   
 protected:
  void TNorm(double v[3]);
  void TCross(double v1[3], double v2[3], double result[3]);
  void ConvertShort(short *array, long length);

 public:
   GR_Terrain(long id, long type, float latitude, float longitude, char *terrafile);
   void objdraw ();
};
#endif
