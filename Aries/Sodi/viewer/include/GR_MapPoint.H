/***************************************************************************

  GR_MapPoint is a generic trail object to be pointed to by other object
  that needs a trail -- such as GR_AirObj;
  
****************************************************************************/

#ifndef		GR_MAPPOINT
#define		GR_MAPPOINT		"GR_MapPointclass"

#include  <stdio.h>
#include  <string.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"
#include  "GR_DispList.H"
#include  <Xm/MessageB.h>
#include  <Xm/SelectioB.h>

#define MAX_NUM_TPOINTS 1024

class Mappoint;
class Mappoint // trail point element
{
 private:
   Mappoint* p_next;

 public:
   float xyz[3];
   Mappoint (float x=0.0, float y=0.0, float z=0.0);
   void  set_next (Mappoint* tpoint) {p_next = tpoint;}
   Mappoint* get_next () {if (this) return p_next; else return NULL;}
};

class GR_MapPoint: public GR_DispObj
{
 private:
   Mappoint* p_head;  // trail stack
   Mappoint* p_tail;
   GR_DispList* p_trail_displist;
   long p_num_tpoints;
   long p_push_count;
   long p_push_every;
   short p_r, p_g, p_b;

 protected:
   void thin_tpoints ();
   
 public:
   GR_MapPoint (GR_DispList* trail_displist, long id, long type,
	     short r, short g, short b);
   void push_tpoint (float x, float y, float z);
   void draw_all_tpoints (short r=255, short g=0 , short b=0);
   void objdraw ();
   void pickEvent (GR_MouseEvent& event, GR_Window* window);
   void set_push_every (long push_every) { p_push_every = push_every; }
};

#endif

