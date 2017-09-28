/**********************************************************
  asf.H: external message handler, to be included by asf.C
  
  -- 10/12/92 created by Y. Tung;
  -- 10/19/92 added sensor;
  -- 11/03/92 added sensor-track message handling;
  -- 11/05/92 added target list to the sensor;
  -- 11/09/92 added cone type mobile sensor;
  -- 04/26/93 simplified by getting rid of airmsg/senmsg/trackmsg
              objects;

**********************************************************/
#include "sim_interface.H"

#include "GR_Interface.H"
#include "GR_DispList.H"
#include "GR_DispObj.H"
#include "GR_Model.H"
#include "GR_Links.H"
#include "GR_AirObj.H"

#define FIXED_SENSOR_TYPE 9990
#define MOBILE_SENSOR_TYPE 9991
#define OTHB_TYPE 9992

void asf_CB ();
void asf_msg_setup ();
void asf_msg_unsetup ();
void asf_init ();
void asf_warnCB ();
void asf_cpanelCB ();
void asf_startCB ();
void asf_pauseCB ();
void asf_linksCB ();
void asf_trailsCB ();
void asf_resetCB ();
void asf_quitCB ();
void asf_statisticsCB ();
void asf_statistics_init ();
void asf_statistics_quitCB ();
void asf_statistics_update ();
Widget asf_statistics_textw;

XtWorkProcId asf_Id = NULL;
Boolean asf_WP (XtPointer);
void asf_textCB (Widget, int);

GR_Shell *asf_panel = NULL;
GR_Shell *asf_statistics_panel = NULL;

GR_DispList *asf_displist;
GR_DispList *asf_links_displist;
GR_DispList *asf_trails_displist;
GR_DispList *offlinelist;
extern GR_Window *gwindow;
Widget asf_form;
Widget asf_simtime_text;
Widget asf_group_text;

float asf_simtime = 0.0;
float asf_prevtime = 0.0;
Boolean draw_asf_links;
Boolean draw_asf_trails;

GR_Links    *asf_links;
extern float get_scale (long type);
extern char* msg_get_groupname ();
extern get_statis (GR_DispList*, long);


void get_aircraft_message (void*, char* buf, int size);
void get_sensor_message (void*, char* buf, int size);
void get_track_message (void*, char* buf, int size);
void get_missile_message (void*, char* buf, int size);

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
   int  p_sensor_type; // 0: fixed, 1: mobile-torus or cone, 2: oth-b;
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

/* ------ track, or in fact: link ------ */

struct Ext_Linkmsg
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


class Linkmsg
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
   Linkmsg () {p_time=0;}
   void fill_msg (Ext_Linkmsg* in_msg);
   int get_cmnd () {return p_cmnd;}
   int get_targetobj_id () {return p_targetobj_id;}
   int get_sensorobj_id () {return p_sensorobj_id;}
   int get_platobj_id () {return p_platobj_id;}
};


/* ----- missile ---- */

struct Airobjmsg
{
   long time;
   long cmnd;
   long type;
   long obj_id;
   long lat; // in radian, from -M_PI_2 to M_PI_2
   long lon; // in radian, from -M_PI to M_PI
   long alt; // in KM
   long heading; // in radian, from -M_PI to M_PI
   long pitch;   // in radian, from -M_PI to M_PI
   long roll;    // in radian, from -M_PI to M_PI
};
