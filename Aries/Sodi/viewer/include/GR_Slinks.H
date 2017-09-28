/***************************************************************************
  GR_Slinks:
  -- 12/11/92: adpapted from GR_Links but make it use
                    (Paltform_loc, Target_loc)
               pairs instead of (Platform_id, Target_id) pairs;
	       this will support a more general interface such as the
	       Speedes interface;  Sensor_loc and Target_loc are float[3]
	       with each component a ratio to the earth radius

	       This is more general because:
	       1.  since location pairs are stored directly, no id search,
	        and thus display list, is needed any more; so the name of such
		list need not be given when create a link;
	       2. the class GR_Slinks is made to be a GR_DispObj, so it can be
		placed into a GR_DispList, thus no localdraw stuff is needed;
                 
               by Y. Tung;
 
****************************************************************************/

#ifndef		GR_SLINKS
#define		GR_SLINKS		"GR_Slinksclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispList.H"
#include "GR_DispObj.H"
	       
#define MAX_TARGETS  128                   // if exceed, need to malloc;

class Tloc_array
{
 private:
   float p_Ploc[3];
   float p_Tloc[MAXTARGETS][3];
   Tloc_array *next;
   Tloc_array *prev;
   int total_targets;
   
 public:
   Tloc_array (float* Ploc);
   float* get_Ploc () { return p_Ploc;}
   float* get_Tloc (int index) { return p_Tloc[index];}
   // next two are newly added:
   float get_Ploc (int dim) { return p_Ploc[dim];}
   float get_Tloc (int index, int dim) { return p_Tloc[index][dim];}
   //
   void add_Tloc (float* Tloc);
   void delete_Tloc (float* Tloc);
   int get_total_targets () { return total_targets;}
   Tloc_array* get_next () { return next;}
   void set_next (Tloc_array* ptr) { next = ptr;}
   Tloc_array* get_prev () { return prev;}
   void set_prev (Tloc_array* ptr) { prev = ptr;}

};


class GR_Slinks: public GR_DispObj
{
 protected:
   Tloc_array *head;
   Tloc_array *last;
   int total_platforms;
   Tloc_array* get_Tloc_array (float* Ploc);

 public:
   GR_Slinks (long id=999999);
   void add_link (float* Ploc, float* Tloc);
   void delete_link (float* Ploc, float* Tloc);
   //void draw_links (); // replaced by objdraw () below:
   void objdraw ();
   int get_total_platforms () { return total_platforms;}

};

#endif

