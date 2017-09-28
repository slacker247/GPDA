/***************************************************************************
  The army object.

  -- 3/5/93: created by Y. Tung;
   
****************************************************************************/

#ifndef		GR_Army
#define		GR_Army		"GR_Armyclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

class GR_Army: public GR_DispObj
{
 private:
   long p_armytype;

 public:
   GR_Army (long armytype);
   void objdraw ();
};

#endif

