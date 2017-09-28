/**************************************************************
  asfeom.H
  -- 06/22/93, Tung;

**************************************************************/
#include "sim_interface.H" 
#include "GR_AirObj.H"

//#include "framework.H"
#include "rsdgw93/genlist.H"
#include "rsdgw93/Fwmgr.H"
#include "rsdgw93/rsd_eom.H"
#include "rsdgw93/rsd_msg.H"
#include "rsdgw93/rsdXfw.H"
#include "sensim3/senmsg.H"
#include "rsdgw93/rsd_missile.H"
#include "simglobal.H"
#include "sim_extern.H"

#define FIXED_SENSOR_TYPE 9990
#define MOBILE_SENSOR_TYPE 9991
#define OTHB_TYPE 9992

C_FWMGR *fw=NULL;  // to be defined only when it is used, to avoid coredump;
IDlist *asfeom_idlist=NULL;

void asfeom_CB ();
void asfeom_init ();
void asfeom_warnCB ();
void asfeom_cpanelCB ();  // a feature to get the C Panel currently not used;
void asfeom_startCB ();
void asfeom_pauseCB ();
void asfeom_linksCB ();
void asfeom_trailsCB ();
void asfeom_resetCB ();
void asfeom_quitCB ();
void asfeom_statisticsCB ();
void asfeom_statistics_init ();
void asfeom_statistics_quitCB ();
void asfeom_statistics_update ();
Widget asfeom_statistics_textw;

XtWorkProcId asfeom_Id = NULL;
Boolean asfeom_WP (XtPointer);
void asfeom_textCB (Widget, int);

GR_Shell *asfeom_panel = NULL;
GR_Shell *asfeom_statistics_panel = NULL;

GR_DispList *asfeom_displist;
GR_DispList *asfeom_links_displist;
GR_DispList *asfeom_trails_displist;
GR_DispList *asfeom_offlinelist;
extern GR_Window *gwindow;
Widget asfeom_form;
Widget asfeom_simtime_text;
Widget asfeom_deltatime_text;
Widget asfeom_group_text;

double asfeom_simtime = 0.0; 
double asfeom_prevtime = 0.0;
double asfeom_deltatime = 120.0;
Boolean draw_asfeom_links;
Boolean draw_asfeom_trails;

GR_Links  *asfeom_links;
extern float get_scale (long type);
extern char* msg_get_groupname ();
extern get_statis (GR_DispList*, long);

void process_air_msg (C_RSD_MSG*);
void process_sensor_msg (SEN_config_msg_str*);
void process_link_msg (SEN_rsdx_msg_str*);
void process_missile_msg (RSD_UPD*);

