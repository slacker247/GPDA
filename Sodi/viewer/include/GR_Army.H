/***************************************************************************
  The army object.

  -- 3/5/93: created by Y. Tung;
   
****************************************************************************/

#ifndef		GR_ARMY
#define		GR_ARMY		"GR_Armyclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

class GR_Army: public GR_DispObj
{
 private:
   long p_armytype;
   float p_ulen;
   float p_base[2];
   float p_lupper[2];
   float p_rupper[2];
   float p_rlower[2];
   float p_topline_base[2];

   void draw_box ();
   void draw_inner ();
   void draw_top ();

   void draw_cross ();
   void draw_round ();
   void draw_line ();
   void draw_chopper ();
   void draw_bird ();
   void draw_y ();
   void draw_eyes ();


 public:
   GR_Army (long armytype, float latitude, float longitude, float size);
   void objdraw ();
};

#endif

