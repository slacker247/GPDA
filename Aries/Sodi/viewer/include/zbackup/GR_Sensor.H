/***************************************************************************
  The Sensor object: fixed disk (2D) sensor and mobile cone (3D) sensor.
 
  -- 11/12/92: created by Y. Tung; 
  -- 11/19/92: cleaned up the links;
  -- 12/04/92: added hemisphere and torus type sensors;
 
****************************************************************************/

#ifndef		GR_SENSOR
#define		GR_SENSOR		"GR_Sensorclass"

#include  <stdio.h>
#include  "GR_Interface.H"
#include  "GR_DispObj.H"

#define   MAXSENSORS 4

#define       S_CONE    0               // Cone sensor
#define       S_DOME    1               // DOME sensor
#define       S_DISK    2               // Disk sensor
#define       S_TORUS   3		// Torus sensor
#define       S_OTHB    4		// Over the horizon backscatter sensor

class GR_Sensor: public GR_DispObj
{
 private:
   long  p_pid;
   long  p_sid;
   float p_elev;
   float p_azi;
   float p_rmax_hi;
   float p_rmin_hi;
   float p_rmax_lo;
   float p_rmin_lo;
   float p_fov_hi;
   float p_fov_lo;
   float p_base;
   float p_range;
   int   p_coverage;
   int   p_power;

 public:
   GR_Sensor (long pid, long sid, float cone_angle, float aperture); // a cone;
   GR_Sensor (long pid, long sid, float radius); // a hemisphere;
   GR_Sensor (long pid, long sid, int typeflag, float aperture, float range);// torus;
   GR_Sensor (long pid, long sid, int sentype, float lon, float lat, float params[10]);

   void objdraw ();
   //void v_process_pick (GR_MouseEvent& event, GR_Window* window);
   void pickEvent (GR_MouseEvent& event, GR_Window* window);
};

#endif

