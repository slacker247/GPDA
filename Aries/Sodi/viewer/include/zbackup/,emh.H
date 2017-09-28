/**********************************************************
  emh.H: external message handler, to be included by 
         rsd or another program;
  
  -- 10/12/92 created by Y. Tung;
  -- 10/19/92 added sensor;
  -- 11/03/92 added sensor-track message handling;
  -- 11/05/92 added target list to the sensor;
  -- 11/09/92 added cone type mobile sensor;

**********************************************************/

#include "GR_Interface.H"
#include "GR_DispList.H"
#include "GR_DispObj.H"
#include "GR_Model.H"
#include "GR_Sensor.H"
#include "GR_Links.H"

//#define LISTSIZE 128

//extern GR_DispList* displist;
extern GR_Links* links;

void get_aircraft_message (void*, char* buf, int size);
void get_sensor_message (void*, char* buf, int size);
void get_track_message (void*, char* buf, int size);

/* ------ air ------ */

struct Ext_Airmsg
{
   long time;
   long cmnd;
   long type;
   long obj_id;
   long lat; // in radian, from -M_PI_2 to M_PI_2
   long lon; // in radian, from -M_PI to M_PI
   long heading; // in radian, from -M_PI to M_PI
   long alt; // in KM
};


class Airmsg
{
 private:
   float  p_time;
   int    p_cmnd;
   long   p_type;
   long   p_airobj_id;
   float  p_lat;
   float  p_lon;
   float  p_alt;
   float  p_heading;
 public:
   Airmsg () {p_time=0;}
   void fill_msg (Ext_Airmsg* in_msg);
   int get_cmnd () {return p_cmnd;}
   long get_airobj_id () {return p_airobj_id;}
   long get_type () {return p_type;}
   float get_time() {return p_time;}
   void move_to_pos (GR_Model* airobj);
};



/* old stuff: ======= 
struct Ext_Airmsg
{
   long time;
   long cmnd;
   long type;
   long obj_id;
   long px;
   long py;
   long pz;
   long vx;
   long vy;
   long vz;
   long h;
};
===================== */


/* -------- sensor ---------- */

struct Ext_Sensormsg
{
   long time;
   int cmnd;  // 1: create, 2: update;
   char sensorobj_type [64];
   int sensorobj_id;
   char platobj_type [64];
   int platobj_id;
   long lat;
   long lon;
   long alt;
   int sensor_type; // 0: fixed, 1: mobile;
   int sensor_status; // 0: online, 1: offline, 2: terminated;
   long bs;           // or cone angle if this is a cone mobile sensor;
   long coverage;     // or aperture if this is a cone mobile sensor;
   long max;
   long min;
   long scan_type;
   long scan_period;
   long scan_starttime;
};

class Sensormsg
{
 private:
   float p_time;
   int  p_cmnd;   // 1: create, 2: update;
   char p_sensorobj_type [64];
   int  p_sensorobj_id;
   char p_platobj_type [64];
   int  p_platobj_id;
   float p_lat;
   float p_lon;
   float p_alt;
   int  p_sensor_type; // 0: fixed, 1: mobile-cone;
   int  p_sensor_status; // 0: online, 1: offline, 2: terminated;
   float p_bs;           // or cone angle if this is a cone mobile sensor;
   float p_coverage;     // or aperture if this is a cone mobile sensor;
   float p_max;
   float p_min;
   float p_scan_type;
   float p_scan_period;
   float p_scan_starttime;
   
   float p_heading;

 public:
   Sensormsg () {p_time=0; p_bs=0; p_coverage=360; p_max=0; p_min=0;}
   void fill_msg (Ext_Sensormsg* in_msg);
   int get_sensorobj_id () {return p_sensorobj_id;}
   int get_platobj_id () {return p_platobj_id;}
   int get_cmnd () {return p_cmnd;}
   int get_sensor_type () {return p_sensor_type;}
   int get_sensor_status () {return p_sensor_status;}
   float get_bs () {return p_bs;} 
   float get_coverage () {return p_coverage;} 
   float get_max() {return p_max;} 
   float get_min() {return p_min;} 
   void move_to_pos (GR_DispObj* sensorobj);

};

/* ------ track ------ */

struct Ext_Trackmsg
{
   long time;
   int cmnd;  // 18: unlink, 20: link;
   char targetobj_type [64];
   int  targetobj_id;
   char sensorobj_type [64];
   int  sensorobj_id;
   char platobj_type [64];
   int  platobj_id;
};


class Trackmsg
{
 private:
   float  p_time;
   int  p_cmnd;  // 18: unlink, 20: link;
   char p_targetobj_type [64];
   int  p_targetobj_id;
   char p_sensorobj_type [64];
   int  p_sensorobj_id;
   char p_platobj_type [64];
   int  p_platobj_id;

 public:
   Trackmsg () {p_time=0;}
   void fill_msg (Ext_Trackmsg* in_msg);
   int get_cmnd () {return p_cmnd;}
   int get_targetobj_id () {return p_targetobj_id;}
   int get_sensorobj_id () {return p_sensorobj_id;}
   int get_platobj_id () {return p_platobj_id;}
};
