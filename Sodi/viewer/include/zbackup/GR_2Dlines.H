/************************************************************************

  World Boundary lines -- coast lines, political boundaries;
  -- 12/07/92: attempt to draw a 2D map;

*************************************************************************/

#ifndef GR_2DLINES
#define GR_2DLINES                            "GR_2Dlinesclass"

#include "GR_Interface.H"
#include "GR_DispObj.H"

#define MAXSEGMENTS 2*1024

struct point
{
  float lon;
  float lat;
};


class GR_2Dlines: public GR_DispObj
{
 private:
   struct point* p_polylines[MAXSEGMENTS];
   long p_polycount[MAXSEGMENTS];
   char* p_filename;
   short p_R, p_G, p_B;
   
 protected:   
   void read_lines ();

 public:
   GR_2Dlines (short R = 255,
	     short G = 255,
	     short B = 255,
	     char* filename = NULL,
             long type = 329);
   void objdraw ();
};

#endif
