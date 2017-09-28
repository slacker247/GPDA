/************************************************************************
  World Boundary lines -- coast lines, political boundaries;
  -- 9/11/92, Tung: created by adapting JBL's method in read_map.c;

*************************************************************************/

#ifndef GR_LINES
#define GR_LINES                            "GR_Linesclass"

#include "GR_Interface.H"
#include "GR_DispObj.H"

struct 3Dpoint
{
   float x;
   float y;
   float z;
}

class GR_Lines: public GR_DispObj
{
 private:
   struct 3Dpoint* p_polylines[5*1024];
   long p_polycount[5*1024];
   char* p_filename;
   short p_R, p_G, p_B;
   
 protected:   
   void read_lines ();

 public:
   GR_Lines (short R = 255,
	     short G = 255,
	     short B = 255,
	     char* filename = NULL);
   void objdraw ();
};

#endif
