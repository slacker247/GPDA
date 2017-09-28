/***************************************************************************
  GR_Spslinks:
  -- 12/14/92: Link structure good for Speedes;
  created by Y. Tung;
 
****************************************************************************/

#ifndef		GR_SPSLINKS
#define		GR_SPSLINKS		"GR_Spslinksclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispList.H"
#include "GR_DispObj.H"
	       
struct loc_pair
{
   double ploc[3];
   double tloc[3];
}


class GR_Spslinks: public GR_DispObj
{
 protected:
   loc_pair *p_array_ptr;
   int p_total_pairs;

 public:
   GR_Spslinks (int num_pairs);
   void add_link (int index, double* ploc, double* tloc);
   void objdraw ();
};

#endif

