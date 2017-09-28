/***************************************************************************
  An airborne object that can do trail and so on. It is initially built on
  top of GR_Model, but will be replaced later with faster icons;
   Tpoint *next;

  -- 04/29/93: created by Y. Tung;  
****************************************************************************/

#ifndef		GR_AIROBJ
#define		GR_AIROBJ		"GR_Airobjclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

class Tpoint;

class Tpoint // trail point element
{
 private:
   float p_x;
   float p_y;
   float p_z;
   Tpoint* p_next;

 public:
   Tpoint (float x=0.0, float y=0.0, float z=0.0);
   void set_xyz (float x, float y, float z) {p_x=x, p_y=y, p_z=z;}
   float getx () {return p_x;}
   float gety () {return p_y;}
   float getz () {return p_z;}
   void  set_next (Tpoint tpoint=NULL) {this->p_next = Tpoint;}
   Tpoint* get_next () {return this->p_next;)
     
};

class GR_AirObj: public GR_Model
{
 private:
   Tpoint* p_head;  // trail stack
   Tpoint* p_tail;
   
 public:
   GR_AirObj ();

   void push_tpoint (float x, float y, float z);
   void thin_tpoint () {};  // to be implemented later;
   void draw_all_tpoints (short r=255, short g=0 , short b=0);

   void objdraw ();
   void pickEvent (GR_MouseEvent& event, GR_Window* window);
};

#endif

