/***************************************************************************
  The Sensor object: fixed disk (2D) sensor and mobile cone (3D) sensor.
 
  -- 11/12/92: created by Y. Tung; 
  --
 
****************************************************************************/

#ifndef		GR_SENSOR
#define		GR_SENSOR		"GR_Sensorclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

#define   MAXSENSORS 8

class GR_Sensor: public GR_DispObj
{
 private:
   long p_pid;
   long p_sid;
   long *p_tid_list;
   int  p_total_links;

 public:
   GR_Sensor (long pid, long sid, float bs, float coverage, float max, float min);
   GR_Sensor (long pid, long sid, float cone_angle, float aperture); // a cone;
   void add_target (long tid);
   void delete_target (long tid);
   int get_total_links () {return p_total_links;}
   void objdraw ();
   void v_process_pick (GR_MouseEvent& event, GR_Window* window);
};

#endif

