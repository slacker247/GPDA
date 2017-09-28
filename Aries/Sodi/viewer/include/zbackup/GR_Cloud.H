#ifndef GR_CLOUD
#define GR_CLOUD                              "GR_Cloudclass"
#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <malloc.h>
#include "GR_Interface.H"
#include "GR_DispObj.H"

class GR_Cloud: public GR_DispObj
{
 private:
   char* p_filename;
   float p_texps[7];
   float p_tevps[7];
   int   p_xsize;
   int   p_ysize;

 protected:
   int texture_init(char *filename);
   void draw_cloud (float lonNW, float latNW, float lonSE, float latSE);

 public:
   GR_Cloud (long type=202, char* filename=NULL);
   void objdraw ();
};

#endif
