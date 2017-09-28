/********************************************************************
  Texture mapped earth, similar to GR_Tearth but is meant for no 
  tiling higher resolution version, with area texture capability;

  -- created by Y. Tung, 10/1/92:

*********************************************************************/

#ifndef GR_TEARTH2
#define GR_TEARTH2                            "GR_Tearth2class"

#include "GR_Interface.H"
#include "GR_DispObj.H"

class GR_Tearth2: public GR_DispObj
{
 private:
   char* p_filename;
   long p_tiled_objid;
   float p_texps[14];
   float p_tevps[7];
   Boolean p_dotiling;
   int p_component;
   int p_xsize;
   int p_ysize;
   
 protected:
   void texture_init();
   void draw_sphere (int n, Boolean nflag, Boolean tflag);
   void draw_sphere (int n, Boolean nflag, Boolean tflag, int quadrant);

 public:
   GR_Tearth2 (char* filename=NULL, int component=1,
	      int xsize=2048, int ysize=1024);
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
