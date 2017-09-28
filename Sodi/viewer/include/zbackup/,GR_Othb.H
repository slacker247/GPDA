/***************************************************************************
  The OTH-B object.
  -- 02/01/93: created by Tung.
****************************************************************************/

#ifndef		GR_OTHB
#define		GR_OTHB		"GR_Othbclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"


class GR_Othb: public GR_DispObj
{
 private:
   long p_pid;
   long p_sid;

 public:
   GR_Othb (long pid, long sid, float bs, float coverage, float max, float min);

   void objdraw ();
   void pickEvent (GR_MouseEvent& event, GR_Window* window);
};

#endif

