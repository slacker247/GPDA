/***************************************************************************
  GR_Spslinks:
  -- 12/14/92: Link structure good for Speedes; created by Y. Tung;
  -- 07/30/93: added color to link;
 
****************************************************************************/

#ifndef		GR_SPSLINKS
#define		GR_SPSLINKS		"GR_Spslinksclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispList.H"
#include  "GR_DispObj.H"
	       
struct data_pair
{
   double ploc[3];
   double tloc[3];
   short  rgb[3];
};


class GR_Spslinks: public GR_DispObj
{
 protected:
  data_pair *p_array_ptr;
  int p_total_pairs;

 public:
  GR_Spslinks (int num_pairs);
  ~GR_Spslinks ();
  void add_link (int index, double* ploc, double* tloc);
  void add_link (int index, double* ploc, double* tloc, short* rgb);
  void objdraw ();

};

#endif

