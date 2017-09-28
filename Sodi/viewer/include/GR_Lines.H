/************************************************************************

  World Boundary lines -- coast lines, political boundaries;
  -- 9/11/92, Tung: created by adapting JBL's method in read_map.c;
  -- 9/24/92: several improvements;
  -- 10/20/92: added grid lines object;

*************************************************************************/

#ifndef GR_LINES
#define GR_LINES                            "GR_Linesclass"

#include "GR_Interface.H"
#include "GR_DispObj.H"

#define MAXSEGMENTS 2*1024

struct point
{
  float lon;
  float lat;
};

class GR_Lines: public GR_DispObj
{
 private:
   struct point* p_polylines[MAXSEGMENTS];
   long p_polycount[MAXSEGMENTS];
   char* p_filename;
   short p_R, p_G, p_B;
   
 protected:   
   void read_lines ();

 public:
   GR_Lines (short R = 255,
	     short G = 255,
	     short B = 255,
	     char* filename = NULL,
             long type = 320);
   void objdraw ();
};

class GR_Gridlines: public GR_DispObj
{
 public:
   GR_Gridlines (short R = 255,
                 short G = 255,
                 short B = 255,
                 int degree = 30,  
                 long type = 330);
   void objdraw ();
};

#endif
