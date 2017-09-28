/***************************************************************************
  An airborne object that can do trail and so on. It is crurrently built on
  top of GR_Model, but will be replaced later with faster icons;
  an GR_AirObj is a derived class that has a pointer to a newobject
  GR_Trail. 
  
  -- 04/29/93: created by Y. Tung;  
****************************************************************************/

#ifndef		GR_AIROBJ
#define		GR_AIROBJ		"GR_Airobjclass"

#include  <stdio.h>
#include  <string.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"
#include  "GR_Model.H"
#include  <Xm/MessageB.h>

#define MAX_NUM_TPOINTS 1024

class Tpoint;

class Tpoint // trail point element
{
 private:
   Tpoint* p_next;

 public:
   float xyz[3];
   Tpoint (float x=0.0, float y=0.0, float z=0.0);
   void  set_next (Tpoint* tpoint) {p_next = tpoint;}
   Tpoint* get_next () {return p_next;}
};

class GR_AirObj: public GR_Model
{
 private:
   Tpoint* p_head;  // trail stack
   Tpoint* p_tail;
   int p_num_tpoints;
   Boolean p_draw_trail;
   
 public:
   GR_AirObj ();
   GR_AirObj (long id, long type);

   void push_tpoint (float x, float y, float z);
   void thin_tpoint () {};  // to be implemented later;
   void draw_all_tpoints (short r=255, short g=0 , short b=0);

   void trail_on () {p_draw_trail = TRUE;}
   void trail_off () {p_draw_trail = FALSE;}
   
   void objdraw ();
   void pickEvent (GR_MouseEvent& event, GR_Window* window);
};

#endif

