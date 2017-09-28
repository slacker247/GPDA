/***************************************************************************
  An airborne object that can do trail and so on. It is crurrently built on
  top of GR_Model, but will be replaced later with faster icons;
  an GR_AirObj is a derived class that has a pointer to a newobject
  GR_Trail. 
  
  -- 04/29/93: created by Y. Tung;  
  -- 05/03/93: created a separate GR_Trail and let GR_AirObj point to it;

****************************************************************************/

#ifndef		GR_AIROBJ
#define		GR_AIROBJ		"GR_Airobjclass"

#include  "GR_Trail.H"
#include  "GR_Model.H"

struct trail_data
{
   double ploc[3];
   double tloc[3];
   short  rgb[3];
};

class GR_AirObj: public GR_Model
{
 private:
   GR_Trail* p_trail;
   long p_update_cycle;
   trail_data *p_trail_ptr;
   int p_total_pairs;
   
 public:
   GR_AirObj (long id, long type);

   void add_trail (GR_DispList* trail_displist,
		   short r=255, short g=0, short b=0);
   void delete_trail (GR_DispList* trail_displist);
   void add_trail_point (float x, float y, float z);
   void add_trail_point (short per_num_update, float x, float y, float z);
   void trail_on ();
   void trail_off ();
   void trail_toggle ();
   void add_link (int index, double* ploc, double* tloc, short* rgb);   
   void GR_AirObj::drop_track();
   void objdraw ();
   //void pickEvent (GR_MouseEvent& event,
   //                GR_DispObj* airobj,
   //                GR_Window* window);
   void pickEvent (GR_MouseEvent& event, GR_Window* window);
};

#endif

