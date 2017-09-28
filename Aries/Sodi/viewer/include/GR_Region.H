/**********************************************************************
  This loads the detailed GR_Regionbean areas -- ugly but fast.
  -- Tung, 3/18/93
     
********************************************************************/
#ifndef GR_REGION
#define GR_REGION                        "GR_Regionclass"

#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <Xm/MessageB.h>
#include "GR_Interface.H"
#include "GR_DispObj.H"

class GR_Region: public GR_DispObj
{
 private:
   char*         p_filebase;
   char          p_filename[9][80];
   float         p_texps[7];
   float         p_tevps[1];
   float         p_lon;
   float         p_lat;
   float         p_Dlat;
   float         p_Dlon;
   int           p_xsize;
   int           p_ysize;
   
 protected:
   int texture_init(int ntiles, unsigned int *texnames);
   void DrawRegion (float lon, float lat, float width, float height);
   unsigned char *AlphaPadImage(int bufSize, unsigned char *inData, int alpha);

 public:
   GR_Region (char *dirname=NULL, long type=201, int ntiles=1);
   unsigned long* take_rgbdata (char* infile);
   void objdraw ();
};

#endif
