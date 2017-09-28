/********************************************************************
  Texture mapped earth, use tiling for higher resolution version, 
  with area texture capability;

  -- created by Y. Tung, 9/1/92:

*********************************************************************/

#ifndef GR_TEARTH
#define GR_TEARTH                            "GR_Tearthclass"

#include <math.h>
#include <sys/types.h>
#include <malloc.h>
#include <Xm/MessageB.h>
#include "GR_Interface.H"
#include "GR_DispObj.H"

class GR_Tearth: public GR_DispObj
{
 private:
   char* p_filename;
   long p_tiled_gr_objid;
   float p_texps[14];
   float p_tevps[7];
   Boolean p_dotiling;
   int p_component;
   int p_xsize;
   int p_ysize;
   
 protected:
   int texture_init();
   void draw_sphere (int n, Boolean nflag, Boolean tflag);
   void draw_sphere (int n, Boolean nflag, Boolean tflag, int quadrant);

 public:
   GR_Tearth (char* filename=NULL, int component=4,
	      int xsize=2048, int ysize=1024);
   unsigned long* take_hdfdata (char* infile, int xsize, int ysize);
   unsigned long* take_rgbdata (char* infile);
   void set_dotiling (Boolean dotiling)
   {
      p_dotiling = dotiling;
   }
   Boolean get_dotiling ()
   {
      return p_dotiling;
   }
   void objdraw ();
   void v_process_pick (GR_MouseEvent&, GR_Window*);
};


#endif
