#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <rpc/xdr.h>
/*
 *   Include the network/socket stuff
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#define SIGCHILD SIGCLD 
/*
 *   Include the X-window stuff
 */
#include <Xm/MainW.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/ScrolledW.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/DrawingA.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include "X11/Xc/Led.h"

#include "parser.H"
#include "kepler.H"
#include "host_user.H"
#include "ext_lanl_bp_mess.H"
#include "lanl_bp_input.H"
#include "lanl_bp_output.H"
#include "lanl_threat.H"
#include "def.h"
#include "demo_opcodes.h"
#include "demo_header.h"
#include "demo_msg_body.h"
#include "demo_players.h"
#include "demo_strings.h"

#ifdef CPP20
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif

#ifndef IRIS4D
//EXTERN_C int clock();
#endif

#define YES          1
#define NO           0

#define PRIORITY_OFF 0
#define PRIORITY_HI  1
#define PRIORITY_MED 2
#define PRIORITY_LOW 3

#define OP_BELIEF    5000

#define RADDEG       0.0174532925199432958
#define POLLRATE     10
#define LINKLANL
#define NOISY
//#define TEST9TRACKS

struct tracksinfo {
char       trackid[16];
int        targets;
float      lethality;
float      latitude;
float      longitude;
float      ltime;
float      itime;
char       objtype[16];
char       msltype[16];
char       tgttype[16];
char       impactloc[16];
char       impactime[16];
};

struct engageinfo {
int        n_weapons;
int        n_remain;
int        taskid;
int        hold;
float      Pk;
char       trkstatus[16];
char       lsite[16];
char       engagestat[16];
char       TTI[16];
};

/* --------------------------------------------------------------------- */

FILE       *INFOfp;
FILE       *TIMEfp;
FILE       *DBUGfp;
FILE       *STATfp;
char       *PLANDEBUG;
char       *PLANTIMING;
char       *PLANSTATS;
char       *PLANTEST;
int        PLAYBACK;
int        toprttr;

float      start_time;
float      tend;
float      end_time;
double     GVT_time = 53000.0;
long       cpu_start;
char       *inbuff;
char       *outbuff;
char       *buff;
int        insize;
int        outsize;
long       cpu_time;
int        input_file;
int        output_file;
int        tpt, i;
int        standalone;
char       *input_file_name;
char       *output_file_name;
char       *standalone_file;
int        id;
int        defcon=5, Rposture=2, Simcmdr=0, Realcmdr=0;
int        LaunchDetect = FALSE, WeapRelease = FALSE, WeapBusy = FALSE;
char       chroe[40], chplan[40], chmsnobj[40];
int        EventDone = FALSE;
int        alarm_active = FALSE;
int        YESNO;
float      cycle_time;
char       *name, *cmdlevel, *position, *DEA, *ROE, *MsnObj, *BtlPlan;
char       line[1280];
char       filename[120];
char       *DATADIR;
char       *BITMAPDIR;
//
//   Network socket stuff
//
int        sock;
int        portid;
char       *hostid;
struct sockaddr_in server, client;
int                server_len, client_len;
//
//   SPEEDES interface stuff
//
C_HOST_USER      *host_user;
EXT_LANL_BP_MESS *out_mess;
EXT_LANL_BP_MESS *in_mess;
//
//   X-windows display stuff
//
int           bpwindW=800, bpwindH=500;
int           defcon_yesno = 1, weapons_yesno = 1;
int           MajorMenu, MinorMenu, SumMenuItem, DialogBusy = FALSE;
XtAppContext  appcontext;
Widget        toplevel, asset_shell, track_shell;
Widget        threat_widget, msgtitle, track_widget, gvttime;
Widget        simcmdlevel, defconlevel, deafield, roefield;
Widget        modestat, runstat, simstat, hilstat, bpalarm;
Pixmap        redledpix, grnledpix, yelledpix;
XtWorkProcId  spsId = 0;
XtIntervalId  timeoutid;
Boolean       trkvisible = FALSE;
Widget        piechart;
GC            PieGC[10];
Pixmap        piepix;
Pixel         bg_color, bg_yellow, bg_grey, fg_color, fg_green;
Pixel         fgcolors[10];
float         piesizes[10];

static String fallback_resources [] =
{
   "*background: wheat",
   "*XmScale*foreground: black",
   "*XmScale*resizable: TRUE",
   "*XmScale*XmNhighlightOnEnter: True",
   "*XmMessageBox*foreground: white",
   "*XmMessageBox*background: steelblue",
   "*Logo.foreground: steelblue4",
   NULL
};
//
//   Run-time parameter stuff
//
struct tracksinfo tracks;
struct engageinfo engage;

int     nmissions = 1;          // No. of Mission Objectives
int     missionselect = 1;      // Currently selected Mission Objective
char    **MOnames;
char    **MOstrategy;
char    **MOtactic;
float   *MOpksuccess;
int     *MOmode;
int     *MOwithhold;
int     *MObooster;

int     nrules = 1;             // No. of Rules of Engagements
int     ruleselect = 1;         // Currently selected ROE
char    **ROEnames;
double  *ROE_n_lat;
double  *ROE_w_lon;
double  *ROE_s_lat;
double  *ROE_e_lon;

int     nplans = 1;             // No. of Battle Plans
int     planselect = 1;         // Currently selected Battle Plan
char    **BPnames;
char    **BPmode;
char    **BPcutoff;
char    **BPkill;
int     *BPoverride;
int     *BPlaunch;
int     *BPsalvo;
float   *BPthreshold;
float   *BPpkcutoff;
int     *BPw_pop;
int     *BPw_mil;
int     *BPw_def;
int     *BPw_nca;
int     *BPw_ind;

int     threatcount  = 6;       // No. of threats listed
int     threatselect = 0;       // Currently selected threat
int     threattotal;            // Total Threat count
int     threattrack;            // No. of threats being tracked
float   threatfirst = 100000.0; // Time of 1st launch
char    **threatnames;          // Threat names
char    **threattypes;          // Threat types (ie: SCUD, SS18)
char    **threatlsite;          // Launch site
char    **threattsite;          // Target site
float   *threatltime;           // Launch time
float   *threatrdist;           // Random distance
double  *threatllat;            // Launch latitude
double  *threatllon;            // Launch longitude
double  *threattlat;            // Target latitude
double  *threattlon;            // Target longitude
char    **threatlatNS;
char    **threatlonEW;
char    **threattatNS;
char    **threattonEW;
char    *gbitext, *gbrtext, *threattext;
double  latitude, longitude;
float   Tsave[21][20];

int     gbicount   = 15;        // No. GBIs listed
int     gbiselect  = 0;         // Currently selected GBI
int     gbitotal;               // Total Interceptor count
int     gbiwithheld;            // GBIs withheld for future use
int     gbiexpended = 10;       // GBIs used
int     gbi_per_rv = 2;         // GBIs fired per threat
int     gbiicon;                // GBI display icon id
float   gbipk;                  // Probability of Kill
int     *gbingbis;              // No. of interceptors in farm
int     *gbinhold;              // No. of interceptors withheld
int     *gbiident;              // Farm identifier
float   *gbipkill;              // Probability of Kill
double  *gbilatit;              // Latitude of Farm
double  *gbilongi;              // Longitude of Farm
char    **gbilatNS;             // Latitude is N or S
char    **gbilonEW;             // Longitude is E or W
char    **gbinames;             // Farm names
char    **gbitypes;             // Type of GBI at Farm
char    **gbistatus;            // Status of Farm (R, Y, G)

int     gbrcount   = 15;        // No. GBRs listed
int     gbrselect  = 0;         // Currently selected GBR
int     gbrtotal;               // Total Interceptor count
int     *gbricon;               // GBR display icon id
int     *gbrident;              // Site identifier
float   *gbrscanT;              // Scan Time
float   *gbrrmin;               //
float   *gbrrmax;               //
float   *gbrrdot;               //
float   *gbrsignal;             //
float   *gbrlumin;              // Luminosity
float   *gbrerror;              //
double  *gbrlatit;              // Latitude of Site
double  *gbrlongi;              // Longitude of Site
char    **gbrlatNS;             // Latitude is N or S
char    **gbrlonEW;             // Longitude is E or W
char    **gbrnames;             // Site names
char    **gbrtypes;             // Type of GBR at Site

int     eyecount  = 1;
int     eyeselect = 0;
int     eyetotal;
int     eyerings, eyesize;
float   eyealt, eyeincl, eyescanT, eyermin, eyermax, eyerdot;
float   eyesignal, eyelumin, eyeerror;
char    *eyenames;

int     dspcount   = 5;         // No. GBRs listed
int     dspselect  = 0;         // Currently selected GBR
int     dsptotal;               // Total Interceptor count
float   *dspscanT;              // Scan Time
float   *dsprmin;               //
float   *dsprmax;               //
float   *dsprdot;               //
float   *dspsignal;             //
float   *dsplumin;              // Luminosity
float   *dsperror;              //
double  *dsplongi;
char    **dspnames;             // Sensor names

int     aircount  = 0;
int     airselect = 0;
int     airtotal;

int     seacount  = 0;
int     seaselect = 0;
int     seatotal;

#define GBRTYPE 1               // Ground-based radars
#define EYETYPE 2               // SIBRS
#define DSPTYPE 3               // DSP
#define GBITYPE 4               // Interceptor Farms
#define AIRTYPE 5               // Air breathing (aircraft, cruise, etc)
#define SEATYPE 6               // Sea going (ships, AEGIS, etc)
int     assetcount;
int     *astypes;               // Asset type

int     assign_matrix[100][2];  // Assignment->threat ID and asset ID
int     listselect = 0;
//int     PLANDEBUG = FALSE;
//int     PLANTEST  = TRUE;

#define clips_logo_width 30
#define clips_logo_height 27
static char clips_logo_bits[] = {
   0x80, 0xff, 0x01, 0x00, 0xe0, 0xff, 0x07, 0x00, 0x70, 0x20, 0x1d, 0x00,
   0x38, 0xb0, 0x3c, 0x00, 0x1c, 0x48, 0x72, 0x00, 0x8c, 0x2f, 0xf9, 0x00,
   0x8e, 0xba, 0xc4, 0x00, 0x66, 0xf8, 0xc6, 0x01, 0x37, 0x29, 0x61, 0x03,
   0xb3, 0xb6, 0x31, 0x03, 0x4f, 0x88, 0x18, 0x06, 0x23, 0x6c, 0x0c, 0x0f,
   0x13, 0x22, 0x86, 0x0c, 0x1f, 0xbf, 0xc3, 0x1c, 0x86, 0x08, 0x60, 0x32,
   0x86, 0x04, 0x10, 0x31, 0x7c, 0x02, 0xd8, 0x31, 0x18, 0x05, 0xc4, 0x1d,
   0xb8, 0x8a, 0x36, 0x0e, 0x70, 0x93, 0x11, 0x06, 0xe0, 0xee, 0x11, 0x06,
   0xc0, 0xa9, 0x09, 0x06, 0x80, 0x73, 0x04, 0x06, 0x00, 0x23, 0x02, 0x06,
   0x00, 0x1b, 0xf9, 0x07, 0x00, 0x8b, 0xfc, 0x03, 0x00, 0x87, 0x0c, 0x00};
Pixmap SCpixmap;

/* --------------------------------------------------------------------- */

//...... define routines
#ifdef LINKLANL
extern "C" void gpals_sim_();
extern "C" void set_tpt_();
extern "C" void reset_tpt_();
extern "C" void get_data_();
extern "C" void initial_();
extern "C" void read_data_();
extern "C" void init_threat_play_();
extern "C" void threat_init_(int *index, double *misstype, double *Plaunch,
                             double *Pimpact, float *Tlaunch, float *Limpact);
extern "C" void threat_setup_(double *X1, double *X2, double *X3,
	                      double *V1,  double *V2, double *V3, 
	                      double *time, double *start_time, int *tracks,
                              int *id, int *index);
extern "C" void get_n_assign_(int *i);
extern "C" void get_assign_(int *i, int *thrt, int *weap, double *etime);
extern "C" int  get_n_threats_();
extern "C" void get_plan_line_(int *i, char chline[120]);
#endif

void X_initialize(int argc, char *argv[]);
void X_update_alarm(char *str, int priority);
void X_update_gvt(int gvt_time);
void X_update_roe(char *roe);
void X_update_gbi();
void X_add_list(int pos, char *listitem);
void X_delete_list(int n_assignments);
void X_track_update();
int  X_get_decision(int op, char *str);
void DrawPie(Widget self, int narea, float *areas, Pixel *fillcolors);

int  SC_message(int opcode);
void SC_initialize();

void Net_init(int port, char *host);
void Net_write(char *buf, int bytes);
int  Net_read(char *buf, int bufsize);
void Net_close();

char *process(char *inbuff, int insize, int &outsize,
	double start_time, double end_time);
void process_standalone(char *standalone_file);

void logoCB (Widget, XtPointer, XtPointer);
void optionCB (Widget, XtPointer, XtPointer);
void decideCB (Widget, XtPointer, XtPointer);
void commandCB (Widget, XtPointer, XtPointer);
void helpCB (Widget, XtPointer, XtPointer);
void alarmCB (Widget w, XtPointer client_data, XtPointer);
void submenu1CB (Widget, XtPointer client_data, XtPointer);
void submenu2CB (Widget, XtPointer client_data, XtPointer);
void submenu3CB (Widget, XtPointer client_data, XtPointer);
void submenu4CB (Widget, XtPointer client_data, XtPointer);
void dosubmenu (int, int);
void gbilistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void gbrlistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void threatlistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void assetpopupCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void asset_doneCB ();
void trackpopupCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void track_doneCB ();
Boolean planWP (XtPointer);
void timeoutCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void gbi_infoCB ();
void missionCB ();
void battleCB ();
void pie_infoCB ();
void gbr_infoCB ();
void threat_infoCB();
void track_infoCB();
void globeCB();
static void reset();
void summaryCB (Widget menuitem, XtPointer itemno, XmAnyCallbackStruct* call_data);
void defconCB (Widget menuitem, XtPointer itemno, XmAnyCallbackStruct* call_data);
void toggleCB(Widget toggle_box, XtPointer n, XmToggleButtonCallbackStruct* cbs);
void responseCB(Widget w, int *answer, XmAnyCallbackStruct *cbs);

extern "C" void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
extern "C" void decode_header (XDR *xdrs_de, struct header *de, int *nargs);
extern "C" int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
extern "C" int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);

/* --------------------------------------------------------------------- */

main(int argc, char *argv[])
{
time_t     clock;
struct tm  *loctime;
char       *str;
int        response;

C_PARSER   *lanl_bp_parser;
C_PARSER   *threat_parser = NULL;
C_PARSER   *gbi_parser = NULL;
C_PARSER   *ground_sensor_parser = NULL;
C_PARSER   *eye_parser = NULL;
C_PARSER   *dsp_parser = NULL;

C_BASETYPE *parameters;
C_BASETYPE *missile_names, *missile_name, *basetype;
C_BASETYPE *gbi_sites, *site, *sensor_type;
C_BASETYPE *sensor_names, *sensor_name, *radar_name;
C_BASETYPE *eye_names, *eye_name;
C_BASETYPE *dsp_names, *dsp_name;
C_BASETYPE *mission_obj, *mission, *rules_of_engage, *rule, *battle_plan, *plan;

double     lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec;
float      ltime, rdist;
union {
    double types;
    char chtype[8];
} utypes;
union {
    double types;
    char chtype[8];
} utsite;
union {
    double types;
    char chtype[8];
} ulsite;

   printf("\n\n");
   printf("  -- Los Alamos-based Battle Planner (Planner) --\n");
   printf("                  (Version 0.2)");
   printf("\n\n");

   if ((PLANDEBUG = getenv("PLANDEBUG")) != NULL)
      DBUGfp = fopen("plandbug.file", "w+");
   if ((PLANTIMING = getenv("PLANTIMING")) != NULL)
      TIMEfp = fopen("plantime.file", "w+");
   if ((PLANSTATS = getenv("PLANSTATS")) != NULL)
      STATfp = fopen("planstat.file", "w+");
   PLANTEST = getenv("PLANTEST");                     // For testing  
   if (getenv("PLANSIM") != NULL) Simcmdr = TRUE;     // If "Simulated" commander playing
   if (getenv("PLANREAL") != NULL) Realcmdr = TRUE;   // If "Human" commander playing

   INFOfp = fopen("planinfo.file", "w+");

   time(&clock);
   loctime = localtime(&clock);
   str = asctime(loctime);
   fprintf(INFOfp, "PLAN Information output file for %s\n", str);
   fprintf(INFOfp, "Timing is     %s\n", (PLANTIMING==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Debugging is  %s\n", (PLANDEBUG==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Statistics is %s\n", (PLANSTATS==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Run Mode is   %s\n", (PLANTEST ? "playback" : "realtime"));
   fprintf(INFOfp, "\n");
   fflush(INFOfp);

   //signal(SIGCHILD, reset);
/*
 *      Parse the Battle Planner parameter file
 *      ---------------------------------------
 */
   lanl_bp_parser = new C_PARSER("planner.par");
   parameters = lanl_bp_parser->get_basetype("parameters");
   name = parameters->get_string("command_center");
   cmdlevel = parameters->get_string("command_level");
   position = parameters->get_string("position");
   DEA = parameters->get_string("dea");
   defcon = parameters->get_int("defcon");
   Rposture = parameters->get_int("readiness");

   ROE = parameters->get_string("roe");
   MsnObj = parameters->get_string("mission");
   BtlPlan = parameters->get_string("plan");

   cycle_time = parameters->get_float("cycle_time");
   start_time = parameters->get_float("start_time");
   tend = parameters->get_float("tend");
   cpu_time = parameters->get_int("cpu_time");

   tpt = parameters->get_logical("tpt");
   standalone = parameters->get_logical("standalone");
   input_file = parameters->get_logical("input_file");
   output_file = parameters->get_logical("output_file");
   input_file_name = parameters->get_string("input_file_name");
   output_file_name = parameters->get_string("output_file_name");
   standalone_file = parameters->get_string("standalone_file");
   portid = parameters->get_int("portid");
   hostid = parameters->get_string("hostid");

   mission_obj = lanl_bp_parser->get_basetype(MsnObj);
   nmissions = mission_obj->get_ntypes();
   MOnames     = new char*[nmissions];
   MOstrategy  = new char*[nmissions];
   MOtactic    = new char*[nmissions];
   MOpksuccess = new float[nmissions];
   MOmode      = new int[nmissions];
   MOwithhold  = new int[nmissions];
   MObooster   = new int[nmissions];
   mission = mission_obj->get_first_type();
   for (i=0; i<nmissions; i++) {
     MOnames[i]     = mission->get_name();
     MOstrategy[i]  = mission->get_string("strategy");
     MOtactic[i]    = mission->get_string("tactic");
     MOpksuccess[i] = mission->get_float("p_success");
     MOmode[i]      = mission->get_int("mode");
     MOwithhold[i]  = mission->get_int("withhold");
     MObooster[i]   = mission->get_int("add_bstrs");
     mission = mission_obj->get_next_type();
   }
   missionselect = nmissions-1;
   strcpy(chmsnobj, MOnames[missionselect]);

   rules_of_engage = lanl_bp_parser->get_basetype(ROE);
   nrules = rules_of_engage->get_ntypes();
   ROEnames = new char*[nrules];
   ROE_n_lat = new double[nrules];
   ROE_w_lon = new double[nrules];
   ROE_s_lat = new double[nrules];
   ROE_e_lon = new double[nrules];
   rule = rules_of_engage->get_first_type();
   for (i=0; i<nrules; i++) {
     ROEnames[i] = rule->get_name();
     lat_deg = rule->get_float("n_lat_deg");
     lat_min = rule->get_float("n_lat_min");
     lat_sec = rule->get_float("n_lat_sec");
     lon_deg = rule->get_float("w_lon_deg");
     lon_min = rule->get_float("w_lon_min");
     lon_sec = rule->get_float("w_lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     ROE_n_lat[i] = latitude;
     ROE_w_lon[i] = longitude;
     lat_deg = rule->get_float("s_lat_deg");
     lat_min = rule->get_float("s_lat_min");
     lat_sec = rule->get_float("s_lat_sec");
     lon_deg = rule->get_float("e_lon_deg");
     lon_min = rule->get_float("e_lon_min");
     lon_sec = rule->get_float("e_lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     ROE_s_lat[i] = latitude;
     ROE_e_lon[i] = longitude;
     rule = rules_of_engage->get_next_type();
   }
   ruleselect = nrules-1;
   strcpy(chroe, ROEnames[ruleselect]);

   battle_plan = lanl_bp_parser->get_basetype(BtlPlan);
   nplans = battle_plan->get_ntypes();
   BPnames     = new char*[nplans];
   BPmode      = new char*[nplans];
   BPcutoff    = new char*[nplans];
   BPkill      = new char*[nplans];
   BPoverride  = new int[nplans];
   BPlaunch    = new int[nplans];
   BPsalvo     = new int[nplans];
   BPthreshold = new float[nplans];
   BPpkcutoff  = new float[nplans];
   BPw_pop     = new int[nplans];
   BPw_mil     = new int[nplans];
   BPw_def     = new int[nplans];
   BPw_nca     = new int[nplans];
   BPw_ind     = new int[nplans];
   plan = battle_plan->get_first_type();
   for (i=0; i<nplans; i++) {
     BPnames[i] = plan->get_name();
     BPmode[i]      = plan->get_string("mode");
     BPcutoff[i]    = plan->get_string("cutoff");
     BPkill[i]      = plan->get_string("kill_criteria");
     BPoverride[i]  = (int)plan->get_logical("override_salvo");
     BPlaunch[i]    = plan->get_int("launch_mode");
     BPsalvo[i]     = plan->get_int("salvo");
     BPthreshold[i] = plan->get_float("rv_threshold");
     BPpkcutoff[i]  = plan->get_float("pk_cutoff");
     BPw_pop[i]     = plan->get_int("weight_population");
     BPw_mil[i]     = plan->get_int("weight_military");
     BPw_def[i]     = plan->get_int("weight_selfdefense");
     BPw_nca[i]     = plan->get_int("weight_ncauthority");
     BPw_ind[i]     = plan->get_int("weight_industrial");
     plan = battle_plan->get_next_type();
   }
   planselect = nplans-1;
   strcpy(chplan, BPnames[planselect]);
   gbi_per_rv = BPsalvo[planselect];

   fprintf(INFOfp, "Initial Command Center ........... %s\n", name);
   fprintf(INFOfp, "Initial Command Level ............ %s\n", cmdlevel);
   fprintf(INFOfp, "Initial Command Position ......... %s\n", position);
   fprintf(INFOfp, "Initial DEA ...................... %s\n", DEA);
   fprintf(INFOfp, "Initial Defense Condition ........ %d\n", defcon);
   fprintf(INFOfp, "Initial Readiness Posture ........ %d\n", Rposture);
   fprintf(INFOfp, "Initial Rules of Engagement ...... %s\n", ROEnames[ruleselect]);
   fprintf(INFOfp, "Initial Mission Objective ........ %s\n", MOnames[missionselect]);
   fprintf(INFOfp, "Initial Battle Plan .............. %s\n", BPnames[planselect]);
   fprintf(INFOfp, "ROE Box North Latitude ........... %f\n", ROE_n_lat[ruleselect]);
   fprintf(INFOfp, "ROE Box West Longitude ........... %f\n", ROE_w_lon[ruleselect]);
   fprintf(INFOfp, "ROE Box South Latitude ........... %f\n", ROE_s_lat[ruleselect]);
   fprintf(INFOfp, "ROE Box East Longitude ........... %f\n", ROE_e_lon[ruleselect]);
   fprintf(INFOfp, "Simulated Commander Host ......... %s\n", hostid);
   fprintf(INFOfp, "Communications Port .............. %d\n", portid);
   fprintf(INFOfp, "\n");
   fflush(INFOfp);
/*
 *      Parse the Missile Threat parameter file
 *      ---------------------------------------
 */
   threat_parser = new C_PARSER("missile.par");
   missile_names = threat_parser->get_basetype("missiles");
   threatcount = missile_names->get_ntypes();
   threatnames = new char*[threatcount];
   threattypes = new char*[threatcount];
   threatlsite = new char*[threatcount];
   threattsite = new char*[threatcount];
   threatltime = new float[threatcount];
   threatrdist = new float[threatcount];
   threatlatNS = new char*[threatcount];
   threatlonEW = new char*[threatcount];
   threattatNS = new char*[threatcount];
   threattonEW = new char*[threatcount];
   threatllat  = new double[threatcount];
   threatllon  = new double[threatcount];
   threattlat  = new double[threatcount];
   threattlon  = new double[threatcount]; 
   threattotal = 0;
   missile_name = missile_names->get_first_type();
   for (i=0; i<threatcount; i++) {
     threatnames[i] = missile_name->get_name();
     threattotal = threattotal+1;
     threattypes[i] = missile_name->get_string("missile_type");
     threatlsite[i] = missile_name->get_string("init_position");
     threattsite[i] = missile_name->get_string("final_position");
     threatltime[i] = missile_name->get_float("launch_time");
     threatrdist[i] = missile_name->get_float("random_distance");
     if (threatltime[i] < threatfirst) threatfirst = threatltime[i];
     threatllat[i]  = 0.0;
     threatllon[i]  = 0.0;
     threattlat[i]  = 0.0;
     threattlon[i]  = 0.0;
     basetype = threat_parser->get_basetype(threatlsite[i]);
     lat_deg = basetype->get_float("lat_deg");
     lat_min = basetype->get_float("lat_min");
     lat_sec = basetype->get_float("lat_sec");
     lon_deg = basetype->get_float("lon_deg");
     lon_min = basetype->get_float("lon_min");
     lon_sec = basetype->get_float("lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     threatlatNS[i] = "N";
     if (!basetype->get_logical("north")) threatlatNS[i] = "S";
     threatlonEW[i] = "E";
     if (!basetype->get_logical("east")) threatlonEW[i] = "W";
     threatllat[i] = latitude;
     threatllon[i] = longitude;
     basetype = threat_parser->get_basetype(threattsite[i]);
     lat_deg = basetype->get_float("lat_deg");
     lat_min = basetype->get_float("lat_min");
     lat_sec = basetype->get_float("lat_sec");
     lon_deg = basetype->get_float("lon_deg");
     lon_min = basetype->get_float("lon_min");
     lon_sec = basetype->get_float("lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     threattatNS[i] = "N";
     if (!basetype->get_logical("north")) threattatNS[i] = "S";
     threattonEW[i] = "E";
     if (!basetype->get_logical("east")) threattonEW[i] = "W";
     threattlat[i] = latitude;
     threattlon[i] = longitude;
     ltime = threatltime[i];
     rdist = threatrdist[i];
     utypes.chtype = "SS18   ";
     ulsite.chtype = "Arctic ";
     //utsite.chtype = "Chicago";
     strncpy(utsite.chtype, threattsite[i], 8);
     threat_init_(&i, &utypes.types, &ulsite.types, &utsite.types, &ltime, &rdist);
     missile_name = missile_names->get_next_type();
   }
   threattrack = 0;
/*
 *      Parse the Ground-based Interceptor parameter file
 *      -------------------------------------------------
 */
   gbi_parser = new C_PARSER("gbi.par");
   gbi_sites = gbi_parser->get_basetype("gbi_sites");
   gbipk    = gbi_sites->get_float("pkill");
   gbiicon  = gbi_sites->get_int("icon");
   gbicount = gbi_sites->get_ntypes();
   gbingbis = new int[gbicount];
   gbinhold = new int[gbicount];
   gbiident = new int[gbicount];
   gbipkill = new float[gbicount];
   gbinames = new char*[gbicount];
   gbitypes = new char*[gbicount];
   gbilatNS = new char*[gbicount];
   gbilonEW = new char*[gbicount];
   gbistatus = new char*[gbicount];
   gbilatit = new double[gbicount];
   gbilongi = new double[gbicount];
   gbitotal = 0; gbiwithheld = 0;
   site = gbi_sites->get_first_type();
   for (i=0; i<gbicount; i++) {
     gbinames[i] = site->get_name();
     gbitypes[i] = site->get_string("gbi_type");
     gbiident[i] = site->get_int("gbi_id");
     gbingbis[i] = site->get_int("n_gbi");
     gbinhold[i] = site->get_int("n_hold");
     gbipkill[i] = site->get_float("pkill");
     gbistatus[i] = site->get_string("status");
     gbitotal += gbingbis[i];
     gbiwithheld += gbinhold[i];
     lat_deg = site->get_float("lat_deg");
     lat_min = site->get_float("lat_min");
     lat_sec = site->get_float("lat_sec");
     lon_deg = site->get_float("lon_deg");
     lon_min = site->get_float("lon_min");
     lon_sec = site->get_float("lon_sec");
     latitude = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     gbilatNS[i] = "N";
     if (!site->get_logical("north")) gbilatNS[i] = "S";
     gbilonEW[i] = "E";
     if (!site->get_logical("east"))  gbilonEW[i] = "W";
     gbilatit[i] = latitude;
     gbilongi[i] = longitude;
     site = gbi_sites->get_next_type();
   }
/*
 *      Parse the Ground-based Sensors parameter file
 *      ---------------------------------------------
 */
   ground_sensor_parser = new C_PARSER("ground_sensors.par");
   sensor_names = ground_sensor_parser->get_basetype("sensor_names");
   gbrcount = sensor_names->get_ntypes();
   gbrnames = new char*[gbrcount];
   gbricon = new int[gbrcount];
   gbrident = new int[gbrcount];
   gbrscanT = new float[gbrcount];
   gbrrmin = new float[gbrcount];
   gbrrmax = new float[gbrcount];
   gbrrdot = new float[gbrcount];
   gbrsignal = new float[gbrcount];
   gbrlumin = new float[gbrcount];
   gbrerror = new float[gbrcount];
   gbrlatNS = new char*[gbrcount];
   gbrlonEW = new char*[gbrcount];
   gbrlatit = new double[gbrcount];
   gbrlongi = new double[gbrcount];
   gbrtotal = 0;
   sensor_name = sensor_names->get_first_type();
   for (i=0; i<gbrcount; i++) {
     gbrnames[i] = sensor_name->get_name();
     gbrident[i] = i;
     gbrtotal = gbrtotal+1;
     lat_deg = sensor_name->get_float("lat_deg");
     lat_min = sensor_name->get_float("lat_min");
     lat_sec = sensor_name->get_float("lat_sec");
     lon_deg = sensor_name->get_float("lon_deg");
     lon_min = sensor_name->get_float("lon_min");
     lon_sec = sensor_name->get_float("lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     gbrlatNS[i] = "N";
     if (!sensor_name->get_logical("north")) gbrlatNS[i] = "S";
     gbrlonEW[i] = "E";
     if (!sensor_name->get_logical("east")) gbrlonEW[i] = "W";
     gbrlatit[i] = latitude;
     gbrlongi[i] = longitude;
     radar_name = sensor_name->get_first_type();
        gbrrmin[i]   = radar_name->get_float("rmin");
        gbrrmax[i]   = radar_name->get_float("rmax");
        gbrrdot[i]   = radar_name->get_float("rdotmin");
        gbrsignal[i] = radar_name->get_float("signal");
        gbrlumin[i]  = radar_name->get_float("luminosity");
        gbrerror[i]  = radar_name->get_float("error");
        gbrscanT[i]  = radar_name->get_float("scan_time");
        gbricon[i]   = radar_name->get_int("icon");
     sensor_name = sensor_names->get_next_type();
   }
/*
 *      Parse the SBIRS Sensors parameter file
 *      --------------------------------------
 */
   eye_parser = new C_PARSER("eye.par");
   eye_names = eye_parser->get_basetype("constellation");
   eyecount  = 1;
   eyenames  = "SIBRS";
   eyerings  = eye_names->get_int("n_rings");
   eyesize   = eye_names->get_int("n_per_ring");
   eyealt    = eye_names->get_float("altitude");
   eyeincl   = eye_names->get_float("inclination");
   eye_name  = eye_parser->get_basetype("BRILLIANT_EYE_SENSOR");
   eyescanT  = eye_name->get_float("scan_time");
   eyermin   = eye_name->get_float("rmin");
   eyermax   = eye_name->get_float("rmax");
   eyerdot   = eye_name->get_float("rdotmin");
   eyesignal = eye_name->get_float("signal");
   eyelumin  = eye_name->get_float("luminosity");
   eyeerror  = eye_name->get_float("error");
/*
 *      Parse the DSP Sensors parameter file
 *      ------------------------------------
 */
   dsp_parser = new C_PARSER("dsp.par");
   dsp_names = dsp_parser->get_basetype("dsp_names");
   dspcount = dsp_names->get_ntypes();
   dspnames = new char*[dspcount];
   dspscanT = new float[dspcount];
   dsprmin = new float[dspcount];
   dsprmax = new float[dspcount];
   dsprdot = new float[dspcount];
   dspsignal = new float[dspcount];
   dsplumin = new float[dspcount];
   dsperror = new float[dspcount];
   dsplongi = new double[dspcount];
   dsptotal = 0;
   dsp_name = dsp_names->get_first_type();
   for (i=0; i<dspcount; i++) {
     dspnames[i] = dsp_name->get_name();
     dsptotal = dsptotal+1;
     dsplongi[i] = dsp_name->get_float("longitude");
     radar_name = dsp_name->get_first_type();
        dsprmin[i]   = radar_name->get_float("rmin");
        dsprmax[i]   = radar_name->get_float("rmax");
        dsprdot[i]   = radar_name->get_float("rdotmin");
        dspsignal[i] = radar_name->get_float("signal");
        dsplumin[i]  = radar_name->get_float("luminosity");
        dsperror[i]  = radar_name->get_float("error");
        dspscanT[i]  = radar_name->get_float("scan_time");
     dsp_name = dsp_names->get_next_type();
   }

   assetcount = gbrcount+eyecount+dspcount+gbicount+aircount+seacount;
   astypes = new int[assetcount];
//
//...... Check if standalone
//
   if (standalone) { process_standalone(standalone_file); exit(1); }
//
//...... Build the X window and the Simulated Commander
//
   X_initialize(argc, argv);

   if (Simcmdr) {      
      Net_init(portid, hostid);
      SC_message(OP_INIT_SC);
      SC_initialize();
   }
//
//...... Create the host router
//
   if (PLANTEST) input_file = TRUE;
   if (input_file) {
     host_user = new C_HOST_USER(input_file_name);
   } else {
     host_user = new C_HOST_USER();
   }
   if (output_file && !input_file) {
     host_user->set_output_file(output_file_name);
   }
//
//...... Initialize
//
   id = host_user->getid(name);
   out_mess = new EXT_LANL_BP_MESS();
   out_mess->time_tag = start_time;
   out_mess->EM_done_time = cycle_time;
//
//...... Initialize the Battle Manager (fortran code)
//
   initial_();              // fprintf(stderr,"LANL_BP: initial_()\n");
   read_data_();            // fprintf(stderr,"LANL_BP: read_data_()\n");

   if (tpt) {
     set_tpt_();
     get_data_();           // fprintf(stderr,"LANL_BP: get_data_()\n");
   } else {
     reset_tpt_();
   }
//
//...... Start the whole show going
//
   /*
   timeoutid = XtAppAddTimeOut(appcontext, POLLRATE,
                    (XtTimerCallbackProc)timeoutCB, NULL);
   */
   XtAppMainLoop (appcontext);
}

Boolean
planWP (XtPointer)
{
   return TRUE;
}

void
timeoutCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{

    out_mess->objid = id;
    in_mess = (EXT_LANL_BP_MESS *)host_user->blocking_module(out_mess);
    delete out_mess;

    start_time = in_mess->time_tag;
    end_time = in_mess->EM_done_time;

    inbuff = (char *)in_mess;
    inbuff += sizeof(EXT_LANL_BP_MESS);
    insize = in_mess->data_bytes -
	(sizeof(EXT_LANL_BP_MESS) - sizeof(C_EM_HEADER));

    if (PLANDEBUG)
        fprintf(stderr,"LANL BP input: %d bytes starting...\n", insize);

    //cpu_start = clock();
    //while ((clock() - cpu_start) < cpu_time*1000000);

//...... process the input data by LANL BP and return an output buffer

    outbuff = process(inbuff, insize, outsize, start_time, end_time);
    delete in_mess;

//...... send the answer back to SPEEDES

    out_mess = (EXT_LANL_BP_MESS *)
	(new char[sizeof(EXT_LANL_BP_MESS)+outsize]);
    out_mess->init(outsize);
    buff = (char *)out_mess;
    buff += sizeof(EXT_LANL_BP_MESS);
    memcpy(buff, outbuff, outsize);
    delete outbuff;

    if (PLANDEBUG)
        fprintf(stderr,"... Done at time %f. Sending %d bytes back\n\n",end_time, outsize);

    out_mess->EM_done_time = cycle_time;
    out_mess->time_tag = end_time;
    /* 
    if ( (start_time > tend) || (end_time > tend) ) { 
        if (spsId) XtRemoveWorkProc (spsId);
        return TRUE;
    } else {
    */
    if ((end_time < tend)) {
        timeoutid = XtAppAddTimeOut(appcontext, POLLRATE,
                    (XtTimerCallbackProc)timeoutCB, NULL);
    } else {
        XtVaSetValues(runstat, XmNbackgroundPixmap, redledpix, NULL);
        X_delete_list(0);
	SC_message(OP_EXIT_SC);
    }
}

char *process(char *inbuff, int insize, int &outsize,
	double start_time, double end_time) {
C_KEPLER         kepler;
C_LANL_BP_INPUT  *lanl_bp_input;
C_LANL_BP_OUTPUT *lanl_bp_output;
C_LANL_THREAT    *lanl_threat;
float            fX[3];
float            fV[3];
int              n_tracks;
int              i, ti, idtemp, j, icity, nthreats;
double           timetemp;
double           *X;
double           *V;
int              n_assignments, index, thrt, weap;
double           etime;
char             *outbuff;
static char      chline[120];
static char      chtemp[16];

  if (PLANDEBUG)
      fprintf(stderr,"Starting LANL_BP at time %f, and ending at time %f\n\n",
	          start_time, end_time);

  kepler.set_ECI(1);                             // Initialize the kepler object to ECI
  lanl_bp_input = (C_LANL_BP_INPUT *)inbuff;     // Get the input buffer
  n_tracks = insize / sizeof(C_LANL_BP_INPUT);   // Get the number of tracks
  X_update_gvt((int)start_time);                 // Update GVT display

  if (start_time >= threatfirst && !EventDone) { // Insert a 'Potential Event'
     X_update_alarm("Potential Missile Event", PRIORITY_HI);
     EventDone = TRUE;                           //    message into the processing stream
     SC_message(OP_POT_EVENT);
  }

  if (n_tracks > 0) {                            // At least one threat is being tracked

    threattrack = n_tracks;                      // Save the # of threats being tracked

    if (LaunchDetect == FALSE) {
       LaunchDetect = TRUE;
       i = X_get_decision(OP_DEFCON,"Launch Detected!\nChange DEFCON Level?");
       i = X_get_decision(OP_RP,"Launch Detected!\nChange Readiness Posture?");
    }

    init_threat_play_();                         // Initialize threat play
/*
 *   Loop over the number of tracks setting stuff for LANL
 */
    for (i=0; i<n_tracks; i++) {

      X = lanl_bp_input[i].get_Xtrack();
      V = lanl_bp_input[i].get_Vtrack();

      ti = i + 1;
      idtemp = lanl_bp_input[i].get_id();
      timetemp = lanl_bp_input[i].get_time();

      for (j=0; j<3; j++) {
        fX[j] = X[j]*1000.0;
        fV[j] = V[j]*1000.0;
      }

      lanl_threat = new C_LANL_THREAT(fX, fV, timetemp, idtemp);

      if (PLANDEBUG)
        lanl_threat->print();                    //...... print threat stuff

      latitude  = lanl_threat->get_lat();
      longitude = lanl_threat->get_lon();
      Tsave[i][ 0] = X[0]; Tsave[i][ 1] = X[1]; Tsave[i][ 2] = X[2];  // Current Position
      Tsave[i][ 3] = V[0]; Tsave[i][ 4] = V[1]; Tsave[i][ 5] = V[2];  // Current Velocity
      Tsave[i][ 6] = sqrt(X[0]*X[0]+X[1]*X[1]+X[2]*X[2])-RE;          // Current Altitude
      Tsave[i][ 7] = sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]);             // Current Speed
      Tsave[i][ 8] = longitude; Tsave[i][ 9] = latitude;              // Impact Position
      Tsave[i][10] = 0.0;                                             // Reserved
      Tsave[i][11] = 0.0; Tsave[i][12] = 0.0; Tsave[i][13] = 0.0;     // Reserved
      Tsave[i][14] = 0.0;                                             // Impact Altitude
      Tsave[i][15] = sqrt( (float)lanl_threat->get_Vi(0)*(float)lanl_threat->get_Vi(0) +
                           (float)lanl_threat->get_Vi(1)*(float)lanl_threat->get_Vi(1) +
                           (float)lanl_threat->get_Vi(2)*(float)lanl_threat->get_Vi(2) );
                                                                      // Impact Speed
      Tsave[i][16] = start_time;                                      // Time stamp
      Tsave[i][17] = (float)lanl_threat->get_Ti();                    // Impact Time
      if (Tsave[i][18] == 0.0) {                                      // GBIs Targeted
          Tsave[i][18] = 1.0;
          gbiexpended = gbiexpended + gbi_per_rv;
      }
      Tsave[i][19] = (float)idtemp;                                   // Threat ID
      X_update_gbi();
      X_track_update();
      threat_setup_(&X[0], &X[1], &X[2], &V[0], &V[1], &V[2], 
                    &timetemp, &start_time, &n_tracks, &idtemp, &ti);
      delete lanl_threat;
    }


    gpals_sim_();                                // Call BP now that it has the threats
    get_n_assign_ (&n_assignments);              // Get the plan out of the BP

    lanl_bp_output = new C_LANL_BP_OUTPUT[n_assignments];
    outbuff = (char *)lanl_bp_output;
    outsize = n_assignments * sizeof(C_LANL_BP_OUTPUT);
    X_delete_list(n_assignments);

    for (i=0; i<n_assignments; i++) {
      index = i+1;                               // FORTRAN starts at 1 instead of 0
      get_assign_ (&index, &thrt, &weap, &etime);
      assign_matrix[i][0] = thrt;                // Save Threat ID for this assignment
      assign_matrix[i][1] = weap;                // Save GBI Farm ID for this assignment
      get_plan_line_(&index, chline);            // Get the BMC3 display info
      X_add_list(index, (char *)chline);         // Display it
      sscanf(chline, "%s %s %s %d %f %s %s %s %s %s %s %s %d %s %f %d",
              tracks.trackid,    tracks.objtype,    tracks.msltype,
             &tracks.targets,   &tracks.lethality,  tracks.tgttype,
              tracks.impactloc,  tracks.impactime,  chtemp,
              engage.trkstatus,  engage.lsite,      engage.engagestat,
             &engage.n_weapons,  engage.TTI,       &engage.Pk,
             &engage.n_remain);
      tracks.latitude = threattlat[i];           // Get the threat launch site latitude
      tracks.longitude = threattlon[i];          // Get the threat launch site longitude
      tracks.ltime = threatltime[i];             // Get the threat launch time
      tracks.itime = Tsave[i][17];               // Get the impact time
      engage.taskid = weap;                      // Get farm id
      SC_message(OP_MSL_TRK);                    // Send the SC the track status
      SC_message(OP_TRK_ENGMT);                  // Send the SC the gbi engagement status

      if (PLANDEBUG)
          printf ("returned %d, %d, %f\n", thrt, weap, etime);

      lanl_bp_output[i].set_threat_id(thrt);
      lanl_bp_output[i].set_asset_id(weap);
      lanl_bp_output[i].set_launch_time(etime);
      if (etime == 0.0) lanl_bp_output[i].set_asset_id(-1);
    }

    if (WeapRelease == FALSE) {
      X_get_decision(OP_DEA,"Request to release weapons");
      if (WeapRelease == FALSE) {
	outsize = 0;
        outbuff = NULL;
        printf("Weapons NOT released, no plan returned\n");
      }
    }

  } else {
    outsize = 0;
    outbuff = NULL;
  }

  return outbuff;
}

/* --------------------------------------------------------------------- */

static void
reset()
{
int  pid, i;
int  status;

   if ((pid = wait(&status)) == -1)
       return;
   /*
   for (i=0; i<XtNumber(prog_list); i++)
     if (prog_list[i].pid == pid) {
         XtAppAddTimeOut(app, 0, reset_btn, prog_list[i].drawn_w);
	 return;
     }
   */
   printf("Child error: PID #%d\n", pid);
}

int
X_get_decision(int opcode, char *str)
{
Widget     dialog, toggle;
Arg        arg[10];
static int answer;
int        scn, response, pad1, pad2, pad3, pad4;
XmString   xstr, defval;
int        i, nargs, xferbytes;
char       buf[2000], src[5], dst[5], dea[5];
char       *xdrbuf;
int        sp, dp;
int        u_defcon, u_posture, u_weapon, u_engage, u_mission, u_plan;
bool_t     ret;
int        op, zero = 0;
unsigned int four = 4, bytecnt, xdrsize;
float      gvt, gvtret;
XDR        xdrs;
struct header        BPen, BPde;
struct SEND_ALL      BPen_body;
struct RTRN_ALL      BPde_body;


   answer = -1; YESNO = YES;
   XtVaSetValues(runstat, XmNbackgroundPixmap, yelledpix, NULL);
   XtAppProcessEvent(appcontext, XtIMAll);

   if (Realcmdr) {
/*
 *     The Human Commander is active and should process the request
 *     ------------------------------------------------------------
 */
      xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
      nargs = 0;
      XtSetArg (arg[0], XmNmessageString, xstr); nargs++;
      XtSetArg (arg[1], XmNautoUnmanage,  True); nargs++;
      XtSetArg (arg[2], XmNdialogStyle,   XmDIALOG_FULL_APPLICATION_MODAL); nargs++;
      XtSetArg (arg[3], XmNuserData,      0); nargs++;
      dialog = XmCreateMessageDialog (toplevel, "Dialog", arg, nargs);
      XmStringFree (xstr);
      XtAddCallback(dialog, XmNcancelCallback, responseCB, &answer);
      XtAddCallback(dialog, XmNokCallback,     responseCB, &answer);  
      XmString btn1 = XmStringCreateSimple("No");
      XmString btn2 = XmStringCreateSimple("Yes");
      toggle = XmVaCreateSimpleRadioBox(dialog, "radio", 1, toggleCB,
              XmVaRADIOBUTTON, btn1, 0, NULL, NULL,
              XmVaRADIOBUTTON, btn2, 0, NULL, NULL,
              NULL);
      XmStringFree(btn1);
      XmStringFree(btn2);
      XtManageChild(toggle);
      XtManageChild (dialog);
      XtPopup(XtParent(dialog), XtGrabNone);
      while (answer < 0) XtAppProcessEvent(appcontext, XtIMAll);
      XtDestroyWidget(dialog);

      switch (opcode) {
        case OP_DEFCON:
	  if (answer) {
            defcon = 1;
          }
          break;
        case OP_RP:
	  if (answer) {
            Rposture = 1;
	  }
        case OP_DEA:
          if (answer) {
            WeapRelease = TRUE;
          }
          break;
        default:
          fprintf(stderr, "Illegal Decision Request (%d)\n", opcode);
	  return(NO);
          break;
      }
      sprintf((char *)line, "DEFCON: %d    RP: %d    DEA: %s",
              defcon, Rposture, (WeapRelease ? "Free" : "Hold") );
      XmTextSetString(defconlevel, (char *)line);
   }

   if (Simcmdr) SC_message(opcode);

   XtVaSetValues(runstat, XmNbackgroundPixmap, grnledpix, NULL);
   XtAppProcessEvent(appcontext, XtIMAll);
   return(answer);
}

void
toggleCB(Widget toggle, XtPointer n, XmToggleButtonCallbackStruct *cbs)
{
  if (cbs->set)
    YESNO = (int) n;
  else
    YESNO = NO;
}
void
responseCB(Widget w, int *answer, XmAnyCallbackStruct *cbs)
{
   switch (cbs->reason) {
   case XmCR_OK:
     *answer = YESNO;
     break;
   case XmCR_CANCEL:
     *answer = NO;
     break;
   }
}

/* --------------------------------------------------------------------- */

void
Net_init(int portid, char *host)
{
  struct hostent *hp = gethostbyname(host);
  if (hp == NULL) {
    fprintf(stderr, "%s: unknown host", host); exit(2);
  }
     
  server.sin_family = AF_INET;
  server.sin_port = htons(portid);
  memmove(&server.sin_addr, hp->h_addr, hp->h_length);

  sock = socket(AF_INET, SOCK_DGRAM, 0);           /* Create socket */
  if (sock < 0) {
    perror("opening stream socket"); exit(1);
  }
  client.sin_family = AF_INET;
  client.sin_addr.s_addr = htonl(INADDR_ANY);
  client.sin_port = htons(0);

  if (bind(sock, (struct sockaddr *)&client, sizeof(client)) < 0) {
    perror("Client bind "); exit(4);
  }
  /*
  if (connect(sock, (sockaddr*) &server, sizeof(server)) < 0) {
    perror("connecting stream socket");
    exit(1);
  }
  */
}

void
Net_write(char *buf, int bytes)
{    
    server_len = sizeof(server);
    if (sendto(sock, buf, bytes, 0, (struct sockaddr *)&server, server_len) < 0) {
      perror("writing on stream socket"); close(sock); exit(1);
    }
}

int
Net_read(char *buf, int bufsize)
{
int  rval;

   memset(buf, 0, bufsize);
   rval = recvfrom(sock, buf, bufsize, 0, (struct sockaddr *)&server, &server_len); 
   if (rval < 0) {
	perror("reading stream message"); close(sock); exit(1);
   }
   if (rval == 0) {
	cout << "SimCmdr has exited." << endl;
   }
   return (rval);
}

void
Net_close()
{
  close(sock);
}

/* --------------------------------------------------------------------- */

int
SC_message(int opcode)
{
static int       answer;
int              scn, response, pad1, pad2, pad3, pad4;
int              i, nargs, xferbytes;
char             buf[2000], src[5], dst[5], dea[5];
char             *xdrbuf;
int              sp, dp;
int              u_defcon, u_posture, u_weapon;
char             u_roe[40], u_mission[40], u_plan[40];
bool_t           ret;
int              op, opsub, zero = 0;
unsigned int     four = 4, bytecnt, xdrsize;
float            gvt, gvtret;
XDR              xdrs;
struct header    BPen, BPde;
struct SEND_ALL  BPen_body;
struct RTRN_ALL  BPde_body;
/*
 *     The Simulated Commander is active and should be sent the request
 *     ----------------------------------------------------------------
 */
      if (!Simcmdr) return(-1);

      u_defcon  = defcon;                        // Save
      u_posture = Rposture;                      //   the
      u_weapon  = WeapRelease;                   //     current
      strcpy(u_plan, chplan);                    //       values
      strcpy(u_mission, chmsnobj);
      strcpy(u_roe, chroe);

      response = !Realcmdr;
      scn = 0;
      op = opcode;
      gvt = GVT_time;
      //
      //  Build the request header for the SC
      //
      xdrbuf = (char *)malloc(2000); 
      xdrmem_create(&xdrs, xdrbuf, (unsigned)2000, XDR_ENCODE);
      BPen.SrcID     = BtlP;
      BPen.DstID     = BMDC1_DR1;
      BPen.SCID      = Sim_Cmdr_ID;
      BPen.opcode    = opcode;
      BPen.SCactive  = response;
      BPen.gvt       = gvt;
      BPen.reserved7 = 0;
      BPen.reserved8 = 0;
      BPen.reserved9 = 0;
      encode_header (&xdrs, &BPen, &nargs);
      if (opcode >= OP_BELIEF) {
         opsub = opcode - OP_BELIEF;
         opcode = OP_BELIEF;
      }
      //
      //  Build the request body based on the decision to be made
      //
      switch (opcode) {
        case OP_INIT_SC:
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          if (PLANDEBUG) printf ("Initialize sent.\n");
        case OP_DEFCON:                          // Update DEFCON status
          xdr_int(&xdrs, &u_defcon);
          if (PLANDEBUG) printf ("DEFCON sent is %d.\n", u_defcon);
          break;
        case OP_RP:                              // Update Readiness Posture
          xdr_int(&xdrs, &u_posture);
          if (PLANDEBUG) printf ("RP     sent is %d.\n", u_posture);
          break;
        case OP_DEA:                             // Update DEA
          //strcpy(BPen_body.init_bp_body_struct.DEA, u_weapon);
          if (PLANDEBUG) printf ("DEA    sent is %d.\n", u_weapon);
          break;
        case OP_ROE:                             // Update Rules of Engagement
          strcpy(BPen_body.roe_body_struct.ROE, u_roe);
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          if (PLANDEBUG) printf ("ROE    sent is %s.\n", u_roe);
          break;
        case OP_MSN_OBJ:                         // Update Mission Objective
	  i = missionselect;
          BPen_body.init_mo_body_struct.DEFCON_Level                   = u_defcon;
          BPen_body.init_mo_body_struct.RP                             = u_posture;
          BPen_body.init_mo_body_struct.Msn_Obj_Pms                    = MOpksuccess[i];
          BPen_body.init_mo_body_struct.Msn_Obj_Mode                   = MOmode[i];
          BPen_body.init_mo_body_struct.Msn_Obj_Withhold               = MOwithhold[i];
          BPen_body.init_mo_body_struct.Msn_Obj_Add_Bstr               = MObooster[i];
          strcpy (BPen_body.init_mo_body_struct.DEA,                   DEA_HOLD);
          strcpy (BPen_body.init_mo_body_struct.ROE,                   u_roe);
          strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Name,          MOnames[i]);
          strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Strategy,      MOstrategy[i]);
          strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Tactic,        MOtactic[i]);
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          if (PLANDEBUG) printf ("MsnObj sent is %d.\n", u_mission);
          break;
        case OP_BP:                              // Update Battle Plan
          i = planselect;
          BPen_body.init_bp_body_struct.DEFCON_Level                   = u_defcon;
          BPen_body.init_bp_body_struct.RP                             = u_posture;
          BPen_body.init_bp_body_struct.BP_Override_Salvo              = BPoverride[i];
          BPen_body.init_bp_body_struct.BP_Launch_Mode                 = BPlaunch[i];
          BPen_body.init_bp_body_struct.BP_RV_Likelihood_Threshold     = BPthreshold[i];
          BPen_body.init_bp_body_struct.BP_Pk_Cutoff                   = BPpkcutoff[i];
          BPen_body.init_bp_body_struct.BP_Asset_Weight_Population     = BPw_pop[i];
          BPen_body.init_bp_body_struct.BP_Asset_Weight_Military       = BPw_mil[i];
          BPen_body.init_bp_body_struct.BP_Asset_Weight_Self_Defense   = BPw_def[i];
          BPen_body.init_bp_body_struct.BP_Asset_Weight_NCA            = BPw_nca[i];
          BPen_body.init_bp_body_struct.BP_Asset_Weight_Industrial     = BPw_ind[i];
          strcpy (BPen_body.init_bp_body_struct.DEA,                     DEA_HOLD);
          strcpy (BPen_body.init_bp_body_struct.ROE,                     u_roe);
          strcpy (BPen_body.init_bp_body_struct.BP_Name,                 BPnames[i]);
          strcpy (BPen_body.init_bp_body_struct.BP_Mode,                 BPmode[i]);
          strcpy (BPen_body.init_bp_body_struct.BP_Tgt_Val_Cut_Off,      BPcutoff[i]);
          strcpy (BPen_body.init_bp_body_struct.BP_Accept_Kill_Criteria, BPkill[i]);
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          if (PLANDEBUG) printf ("BP     sent is %d.\n", u_plan);
          break;
        case OP_POT_EVENT:
          BPen_body.pot_event_body_struct.Event_Time                   = BPen.gvt - 739.0;
          BPen_body.pot_event_body_struct.Event_Lat                    = 28.73;
          BPen_body.pot_event_body_struct.Event_Lon                    = 111.07;
          strcpy (BPen_body.pot_event_body_struct.Event_Country,         "China");
          strcpy (BPen_body.pot_event_body_struct.Event_Site,            "Hough Ling");
          strcpy (BPen_body.pot_event_body_struct.Event_Status,          "Quick Alert");
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          break;
        case OP_MSL_TRK:
          BPen_body.msl_trk_body_struct.Exp_Tgts                       = tracks.targets;
          BPen_body.msl_trk_body_struct.Leth_Val                       = tracks.lethality;
          BPen_body.msl_trk_body_struct.Launch_Time                    = tracks.ltime;
          BPen_body.msl_trk_body_struct.Impact_Lat                     = tracks.latitude;
          BPen_body.msl_trk_body_struct.Impact_Lon                     = tracks.longitude;
          BPen_body.msl_trk_body_struct.Earl_Impact_Time               = 3440.2;
          BPen_body.msl_trk_body_struct.Num_Boosters                   = 1;
          BPen_body.msl_trk_body_struct.Leth_Exp                       = 1;
          strcpy (BPen_body.msl_trk_body_struct.Track_ID,                tracks.trackid);
          strcpy (BPen_body.msl_trk_body_struct.Object_Type,             tracks.objtype);
          strcpy (BPen_body.msl_trk_body_struct.Missile_Type,            tracks.msltype);
          strcpy (BPen_body.msl_trk_body_struct.Missile_Class,           "ICBM");
          strcpy (BPen_body.msl_trk_body_struct.Launch_Country,          "China");
          strcpy (BPen_body.msl_trk_body_struct.Launch_Site,             engage.lsite);
          strcpy (BPen_body.msl_trk_body_struct.Pred_Impact_Region,      tracks.impactloc);
          strcpy (BPen_body.msl_trk_body_struct.Trk_Status,              engage.trkstatus);
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d.\n", BPen.opcode);
          if (PLANDEBUG)
             printf("%s %s %s %d %f %s %s %s %s %s %s %s %d %s %f %d %f %f %f %f\n",
                    tracks.trackid,    tracks.objtype,    tracks.msltype,
                    tracks.targets,    tracks.lethality,  tracks.tgttype,
                    tracks.impactloc,  tracks.impactime,  "|",
                    engage.trkstatus,  engage.lsite,      engage.engagestat,
                    engage.n_weapons,  engage.TTI,        engage.Pk,
                    engage.n_remain,   tracks.latitude,   tracks.longitude,
                    tracks.ltime,      tracks.itime); 
          break;
        case OP_TRK_ENGMT:
          BPen_body.trk_engmt_body_struct.Cur_Eng_Num_Wea              = engage.n_weapons;
          BPen_body.trk_engmt_body_struct.Cur_Eng_TTI                  = BPen.gvt + 1800.0;
          BPen_body.trk_engmt_body_struct.Cur_Eng_Pk                   = engage.Pk;
          BPen_body.trk_engmt_body_struct.Eng_Opp_Remain               = engage.n_remain;
          BPen_body.trk_engmt_body_struct.Task_Id                      = engage.taskid;
          strcpy (BPen_body.trk_engmt_body_struct.Eng_Status,          engage.engagestat);
          strcpy (BPen_body.trk_engmt_body_struct.Track_Id,            tracks.trackid);  
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          break;
        case OP_WA:
          BPen_body.wa_body_struct.Num_GBI_Farms = 1;
          BPen_body.wa_body_struct.Farm_Val[ 0][0]                     = engage.taskid;
          BPen_body.wa_body_struct.Farm_Val[ 0][1]                     = engage.n_weapons;
          BPen_body.wa_body_struct.Farm_Val[ 0][2]                     = engage.hold;
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
            printf ("Encode failed for opcode %d .\n", BPen.opcode);
          break;
        case OP_EXIT_SC:                          // Termination
          if (PLANDEBUG) printf ("Termination sent.\n");
          break;
        case OP_BELIEF:                           // Call Dempster-Shafer belief network
          xdr_int(&xdrs, &opsub);
          if (PLANDEBUG) printf ("DS belief sent is %d.\n", opsub);
          break;
        default:
          break;
      }
      xdrsize = xdr_getpos(&xdrs);
      //
      //  Send the request to the SC and cleanup
      //
      //fprintf(stderr, "Send request. Byte count is %d\n", xdrsize);
      Net_write(xdrbuf, xdrsize);
      xdr_destroy(&xdrs);
      free(xdrbuf);
      //
      //  Wait for the response from the SC
      //
      if (PLANDEBUG) fprintf(stderr, "Planner: waiting for response from SimCmdr...\n");
      xferbytes = Net_read(buf, 2000);
      if (PLANDEBUG) fprintf(stderr, "Got response. Byte count is %d\n", xferbytes);
      //
      //  Extract the response header info
      //
      xdrmem_create(&xdrs, buf, (unsigned)2000, XDR_DECODE);
      decode_header (&xdrs, &BPde, &nargs);
      if (PLANDEBUG) fprintf(stderr, "Header decoded with %d args\n", nargs);
      //
      //  Process the response from the SC
      //
      if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs) != 0)
         printf ("Decode failed for opcode %d .\n", BPde.opcode);
      switch (BPde.opcode) {
/*      case OP_DEFCON:                          // Update DEFCON status
          u_defcon = BPde_body.defcon_body_struct.DEFCON;
          if (PLANDEBUG) printf ("DEFCON received is %d.\n", u_defcon);
          break;
        case OP_RP:                              // Update Readiness Posture
          u_posture = BPde_body.rp_body_struct.RP;
          if (PLANDEBUG) printf ("RP     received is %d.\n", u_posture);
          break;
        case OP_DEA:                             // Update DEA
          u_weapon = (strcmp(BPde_body.dea_body_struct.DEA, "Free") ? 1 : 0);
          if (PLANDEBUG) printf ("DEA    received is %d.\n", u_weapon);
          break;
        case OP_ROE:                             // Update Rules of Engagement
          strcpy(u_roe, BPde_body.roe_body_struct.ROE);
          if (PLANDEBUG) printf ("ROE    received is %s.\n", u_roe);
          break;
        case OP_MSN_OBJ:                         // Update Mission Objective
          strcpy(u_mission, BPde_body.msn_obj_body_struct.Msn_Obj);          
          if (PLANDEBUG) printf ("MsnObj received is %s.\n", u_mission);
          break;
        case OP_BP:                              // Update Battle Plan
          strcpy(u_plan, BPde_body.bp_body_struct.BP);
          if (PLANDEBUG) printf ("BP     received is %s.\n", u_plan);
          break;  */
        case OP_RTRN_DCN:                        // Update all six
          u_defcon  = BPde_body.dcn_body_struct.DEFCON;
          u_posture = BPde_body.dcn_body_struct.RP;
          u_weapon  = (strcmp(BPde_body.dcn_body_struct.DEA, "Free") ? 1 : 0);
          strcpy(u_roe, BPde_body.dcn_body_struct.ROE);
          strcpy(u_mission, BPde_body.dcn_body_struct.Msn_Obj);
          strcpy(u_plan, BPde_body.dcn_body_struct.BP);
	  if (alarm_active) X_update_alarm("None", PRIORITY_OFF);
          if (PLANDEBUG) {
             printf ("DEFCON received is %d.\n", BPde_body.dcn_body_struct.DEFCON);
             printf ("RP     received is %d.\n", BPde_body.dcn_body_struct.RP);
             printf ("DEA    received is %s.\n", BPde_body.dcn_body_struct.DEA);
             printf ("ROE    received is %s.\n", BPde_body.dcn_body_struct.ROE);
             printf ("MsnObj received is %s.\n", BPde_body.dcn_body_struct.Msn_Obj);
             printf ("BP     received is %s.\n", BPde_body.dcn_body_struct.BP);
	  }
          break;
        case OP_RTRN_ACK:                        // Don't update anything
          if (PLANDEBUG) printf ("ACK    received. .\n");
          break;
        case OP_RTRN_ERR:                        // Process error return
          if (PLANDEBUG) printf ("Error  received. .\n");
          break;
        default:
          break;
      }

      defcon = u_defcon;
      Rposture = u_posture;
      WeapRelease = u_weapon;
      strcpy(chroe, u_roe);
      strcpy(chplan, u_plan);
      strcpy(chmsnobj, u_mission);

      sprintf((char *)line, "DEFCON: %d    RP: %d    DEA: %s",
              defcon, Rposture, (WeapRelease ? "Free" : "Hold") );
      XmTextSetString(defconlevel, (char *)line);
      X_update_roe(u_roe);

      answer = YES;
      xdr_destroy(&xdrs);

      return(answer);
}

void
SC_initialize()
{
   struct SEND_ALL      BPen_body;
   struct RTRN_ALL      BPde_body;
   struct header        BPen, BPde;
   int                  nargs_in, nargs_out;

   unsigned int         xdrsize;
   int                  xferbytes;
   char                 *xdrbuf_en, *xdrbuf_de, buf[2000];
   XDR                  xdrs;

   if (!Simcmdr) return;

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   //
   //  Send the Simulated Commander all the Mission Objectives
   //  -------------------------------------------------------
   //
   for (i=0; i<nmissions; i++) {
      xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);
      BPen.opcode    = OP_INIT_MO;
      BPen.gvt       = 1234.6;
      BPen_body.init_mo_body_struct.DEFCON_Level                 = defcon;
      BPen_body.init_mo_body_struct.RP                           = Rposture;
      BPen_body.init_mo_body_struct.Msn_Obj_Pms                  = MOpksuccess[i];
      BPen_body.init_mo_body_struct.Msn_Obj_Mode                 = MOmode[i];
      BPen_body.init_mo_body_struct.Msn_Obj_Withhold             = MOwithhold[i];
      BPen_body.init_mo_body_struct.Msn_Obj_Add_Bstr             = MObooster[i];
      strcpy (BPen_body.init_mo_body_struct.DEA,                 DEA_HOLD);
      strcpy (BPen_body.init_mo_body_struct.ROE,                 ROE_NA);
      strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Name,        MOnames[i]);
      strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Strategy,    MOstrategy[i]);
      strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Tactic,      MOtactic[i]);
      encode_header (&xdrs, &BPen, &nargs_in);
      if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
         printf ("Encode failed for opcode %d .\n", BPen.opcode);
      xdrsize = xdr_getpos(&xdrs);
      //
      //  Send the request to the SC and cleanup
      //
      Net_write(xdrbuf_en, xdrsize);
      xdr_destroy(&xdrs);
      //
      //  Wait for the response from the SC
      //
      if (PLANDEBUG) printf("Planner: waiting for response. Opcode %d\n", BPen.opcode);
      xferbytes = Net_read(buf, 2000);
   }
   //
   //  Send the Simulated Commander all the Battle Plans
   //  -------------------------------------------------
   //
   for (i=0; i<nplans; i++) {
      xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);
      BPen.opcode    = OP_INIT_BP;
      BPen.gvt       = 1234.8;
      BPen_body.init_bp_body_struct.DEFCON_Level                   = defcon;
      BPen_body.init_bp_body_struct.RP                             = Rposture;
      BPen_body.init_bp_body_struct.BP_Override_Salvo              = BPoverride[i];
      BPen_body.init_bp_body_struct.BP_Launch_Mode                 = BPlaunch[i];
      BPen_body.init_bp_body_struct.BP_RV_Likelihood_Threshold     = BPthreshold[i];
      BPen_body.init_bp_body_struct.BP_Pk_Cutoff                   = BPpkcutoff[i];
      BPen_body.init_bp_body_struct.BP_Asset_Weight_Population     = BPw_pop[i];
      BPen_body.init_bp_body_struct.BP_Asset_Weight_Military       = BPw_mil[i];
      BPen_body.init_bp_body_struct.BP_Asset_Weight_Self_Defense   = BPw_def[i];
      BPen_body.init_bp_body_struct.BP_Asset_Weight_NCA            = BPw_nca[i];
      BPen_body.init_bp_body_struct.BP_Asset_Weight_Industrial     = BPw_ind[i];
      strcpy (BPen_body.init_bp_body_struct.DEA,                     DEA_HOLD);
      strcpy (BPen_body.init_bp_body_struct.ROE,                     ROE_NA);
      strcpy (BPen_body.init_bp_body_struct.BP_Name,                 BPnames[i]);
      strcpy (BPen_body.init_bp_body_struct.BP_Mode,                 BPmode[i]);
      strcpy (BPen_body.init_bp_body_struct.BP_Tgt_Val_Cut_Off,      BPcutoff[i]);
      strcpy (BPen_body.init_bp_body_struct.BP_Accept_Kill_Criteria, BPkill[i]);
      encode_header (&xdrs, &BPen, &nargs_in);
      if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
         printf ("Encode failed for opcode %d .\n", BPen.opcode);
      xdrsize = xdr_getpos(&xdrs);
      //
      //  Send the request to the SC and cleanup
      //
      Net_write(xdrbuf_en, xdrsize);
      xdr_destroy(&xdrs);
      //
      //  Wait for the response from the SC
      //
      if (PLANDEBUG) printf("Planner: waiting for response. Opcode %d\n", BPen.opcode);
      xferbytes = Net_read(buf, 2000);
   }
   //
   //  Send the Simulated Commander the GBI Farm status
   //  ------------------------------------------------
   //
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);
   BPen.opcode    = OP_INIT_WA;
   BPen.gvt       = 1235.0;

   for (i=0; i<gbicount; i++) {
      BPen_body.wa_body_struct.Farm_Val[ i][0] = gbiident[i];
      BPen_body.wa_body_struct.Farm_Val[ i][1] = gbingbis[i];
      BPen_body.wa_body_struct.Farm_Val[ i][2] = gbinhold[i];
   }

   BPen_body.wa_body_struct.Num_GBI_Farms = gbicount;

   encode_header (&xdrs, &BPen, &nargs_in);
   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);
   xdrsize = xdr_getpos(&xdrs);
   //
   //  Send the request to the SC and cleanup
   //
   Net_write(xdrbuf_en, xdrsize);
   xdr_destroy(&xdrs);
   //
   //  Wait for the response from the SC
   //
   if (PLANDEBUG) printf("Planner: waiting for response. Opcode %d\n", BPen.opcode);
   xferbytes = Net_read(buf, 2000);
   //
   //  Send the Simulated Commander the current defaults
   //  -------------------------------------------------
   //
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);
   BPen.opcode    = OP_DB_STATUS;
   BPen.gvt       = 1235.1;
   BPen_body.db_status_body_struct.DEFCON_Level                   = defcon;
   BPen_body.db_status_body_struct.RP                             = Rposture;
   BPen_body.db_status_body_struct.Msn_Obj_Pms                    = MOpksuccess[missionselect];
   BPen_body.db_status_body_struct.Msn_Obj_Mode                   = MOmode[missionselect];
   BPen_body.db_status_body_struct.Msn_Obj_Withhold               = MOwithhold[missionselect];
   BPen_body.db_status_body_struct.Msn_Obj_Add_Bstr               = MObooster[missionselect];
   BPen_body.db_status_body_struct.BP_Override_Salvo              = BPoverride[planselect];
   BPen_body.db_status_body_struct.BP_Launch_Mode                 = BPlaunch[planselect];
   BPen_body.db_status_body_struct.BP_RV_Likelihood_Threshold     = BPthreshold[planselect];
   BPen_body.db_status_body_struct.BP_Pk_Cutoff                   = BPpkcutoff[planselect];
   BPen_body.db_status_body_struct.BP_Asset_Weight_Population     = BPw_pop[planselect];
   BPen_body.db_status_body_struct.BP_Asset_Weight_Military       = BPw_mil[planselect];
   BPen_body.db_status_body_struct.BP_Asset_Weight_Self_Defense   = BPw_def[planselect];
   BPen_body.db_status_body_struct.BP_Asset_Weight_NCA            = BPw_nca[planselect];
   BPen_body.db_status_body_struct.BP_Asset_Weight_Industrial     = BPw_ind[planselect];
   strcpy (BPen_body.db_status_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.db_status_body_struct.ROE,                     ROE_NA);
   strcpy (BPen_body.db_status_body_struct.Msn_Obj_Name,            MOnames[missionselect]);
   strcpy (BPen_body.db_status_body_struct.Msn_Obj_Strategy,        MOstrategy[missionselect]);
   strcpy (BPen_body.db_status_body_struct.Msn_Obj_Tactic,          MOtactic[missionselect]);
   strcpy (BPen_body.db_status_body_struct.BP_Name,                 BPnames[planselect]);
   strcpy (BPen_body.db_status_body_struct.BP_Mode,                 BPmode[planselect]);
   strcpy (BPen_body.db_status_body_struct.BP_Tgt_Val_Cut_Off,      BPcutoff[planselect]);
   strcpy (BPen_body.db_status_body_struct.BP_Accept_Kill_Criteria, BPkill[planselect]);
   encode_header (&xdrs, &BPen, &nargs_in);
   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);
   xdrsize = xdr_getpos(&xdrs);
   //
   //  Send the request to the SC and cleanup
   //
   Net_write(xdrbuf_en, xdrsize);
   xdr_destroy(&xdrs);
   //
   //  Wait for the response from the SC
   //
   if (PLANDEBUG) printf("Planner: waiting for response. Opcode %d\n", BPen.opcode);
   xferbytes = Net_read(buf, 2000);

   free (xdrbuf_en);
   free (xdrbuf_de);
}

/* --------------------------------------------------------------------- */

void
X_initialize(int argc, char *argv[])
{
   Display       *dpy;
   Widget        form, panel, frame;
   Widget        viewbar, infoform;
   Widget        eventtitle, header, SClogo;
   Widget        menubar, menupane, submenu;
   Widget        septitle, titlewidget, topsep, trailwidget, botsep, midsep;
   Widget        gbi_info, threat_info, track_info, asset_info, shoot_info;
   Widget        hold_info, w_msnobj, w_btlplan;
   Widget        sumtitle, summenu, sepsummary, deftitle, defmenu;
   Widget        globetrack, timetogo, threatass, exceptman, btnsep;
   Widget        dummywidget;
   XmString      logo, options, command, decide, help, quit, redraw, mouse, paws, resu;
   XmString      redraw2, redraw3, redraw4, redraw5, redraw6, redraw7, redraw8;
   XmString      title0, title1, title2, title3, title4, title5, title;
   XmString      title6, title7, title8, title9;
   XmString      none, level1, level2, level3, level4, level5, simlvl;
   XmString      *xstr, classtitle;
   XmStringTable threatlist;
   XFontStruct   *font;
   XmFontList    fontlist;
   XColor        color, unused;
   Pixel         top_shadow, bottom_shadow, fg_ret, select_color;
   Colormap      cmap;
   char          *DATADIR;
   char          *FWDIR;
   int           i;

   toplevel = XtOpenApplication(&appcontext, "planner", NULL, 0, &argc, argv,
                 fallback_resources, applicationShellWidgetClass, NULL, 0);
   dpy = XtDisplay(toplevel);

   frame = XmCreateFrame(toplevel, "frame", NULL, 0);
   XtManageChild(frame);

   form = XmCreateForm(frame, "Form", NULL, 0);
   XtVaSetValues(form,
               XmNwidth,             bpwindW,
               XmNheight,            bpwindH,
               NULL);
   XtManageChild (form);

   XtVaGetValues(frame, XmNcolormap, &cmap, NULL);
   XAllocNamedColor(XtDisplay(frame), cmap, "red", &color, &unused);
   bg_color = color.pixel;
   XmGetColors(XtScreen(frame), cmap, bg_color, &fg_ret, &top_shadow,
               &bottom_shadow, &select_color);
   XAllocNamedColor(XtDisplay(frame), cmap, "blue", &color, &unused);
   fg_color = color.pixel;
   XAllocNamedColor(XtDisplay(frame), cmap, "yellow", &color, &unused);
   bg_yellow = color.pixel;
   XAllocNamedColor(XtDisplay(frame), cmap, "green", &color, &unused);
   fg_green = color.pixel;
   XAllocNamedColor(XtDisplay(frame), cmap, "grey", &color, &unused);
   bg_grey = color.pixel;
   font = XLoadQueryFont(dpy,
          "-adobe-courier-bold-*-*-*-12-*-*-*-*-*-*-*");
   fontlist = XmFontListCreate(font, "charset1");
/*
 *  Build the Menu bar
 *  ------------------
 */
   logo     = XmStringCreateSimple("File");
   options  = XmStringCreateSimple("Edit");
   command  = XmStringCreateSimple("Command");
   decide   = XmStringCreateSimple("Assessment");
   help     = XmStringCreateSimple("Help");
   menubar  = XmVaCreateSimpleMenuBar (form, "Menubar",
                XmVaCASCADEBUTTON, logo,         'F',
                XmVaCASCADEBUTTON, options,      'E',
                XmVaCASCADEBUTTON, command,      'C',
                XmVaCASCADEBUTTON, decide,      'D',
                XmVaCASCADEBUTTON, help,         'H',
                NULL);
   XtVaSetValues (menubar,
                XmNheight,          30,
                XmNtopAttachment,   XmATTACH_FORM,
                XmNleftAttachment,  XmATTACH_FORM,
                XmNrightAttachment, XmATTACH_FORM,
                XmNspacing,         5,
                NULL);
   XmStringFree(logo);
   XmStringFree(options);
   XmStringFree(command);
   XmStringFree(decide);
   XmStringFree(help);  

   quit     = XmStringCreateSimple ("Quit");
   paws     = XmStringCreateSimple ("Pause");
   resu     = XmStringCreateSimple ("Resume");
   menupane = XmVaCreateSimplePulldownMenu (menubar, "File", 0, logoCB,
              XmVaPUSHBUTTON, paws, NULL, NULL, NULL,
              XmVaPUSHBUTTON, resu, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, quit, NULL, NULL, NULL,
              NULL);
   XmStringFree (quit);
   XmStringFree (paws);
   XmStringFree (resu);

   redraw   = XmStringCreateSimple ("Redraw");
   menupane = XmVaCreateSimplePulldownMenu (menubar, "Edit", 1, optionCB,
              XmVaPUSHBUTTON, redraw, NULL, NULL, NULL,
              NULL);
   XmStringFree (redraw);

   none    = XmStringCreateSimple ("Real");
   simlvl  = XmStringCreateSimple ("Simulated");
   level1  = XmStringCreateSimple ("NORAD CCC");
   level2  = XmStringCreateSimple ("BMDC");
   level3  = XmStringCreateSimple ("SCCC");
   level4  = XmStringCreateSimple ("Firing Unit");
   menupane = XmVaCreateSimplePulldownMenu (menubar, "Command", 2, commandCB,
              XmVaRADIOBUTTON,   none,   NULL, NULL, NULL,
              XmVaRADIOBUTTON,   simlvl, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, level1, NULL,
              XmVaCASCADEBUTTON, level2, NULL,
              XmVaCASCADEBUTTON, level3, NULL,
              XmVaCASCADEBUTTON, level4, NULL,
              NULL);
   XmStringFree (none);
   XmStringFree (simlvl);
   XmStringFree (level1);
   XmStringFree (level2);
   XmStringFree (level3);
   XmStringFree (level4);

   XmString do1_text = XmStringCreateSimple ("Def Officer 1");
   XmString do2_text = XmStringCreateSimple ("Def Officer 2");
   XmString cd_text  = XmStringCreateSimple ("CINC/CD");
   XmString dr_text  = XmStringCreateSimple ("Cmdr/Director");
   XmString ba_text  = XmStringCreateSimple ("Battle Analyst");
   XmString sa_text  = XmStringCreateSimple ("Sensor Analyst");
   XmString wa_text  = XmStringCreateSimple ("Weapons Analyst");
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu1", 2,
             submenu1CB,
             XmVaRADIOBUTTON,   cd_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   do1_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   do2_text, NULL, NULL, NULL,
             XmNradioBehavior,  True,
             XmNradioAlwaysOne, True,
             NULL);
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu2", 3,
             submenu2CB,
             XmVaRADIOBUTTON,   dr_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   ba_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   sa_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   wa_text, NULL, NULL, NULL,
             XmNradioBehavior,  True,
             XmNradioAlwaysOne, True,
             NULL);
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu3", 4,
             submenu3CB,
             XmVaRADIOBUTTON,   dr_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   ba_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   sa_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   wa_text, NULL, NULL, NULL,
             XmNradioBehavior,  True,
             XmNradioAlwaysOne, True,
             NULL);
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu4", 5,
             submenu4CB,
             XmVaRADIOBUTTON,   dr_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   ba_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   sa_text, NULL, NULL, NULL,
             XmVaRADIOBUTTON,   wa_text, NULL, NULL, NULL,
             XmNradioBehavior,  True,
             XmNradioAlwaysOne, True,
             NULL);
   XmStringFree (do1_text);
   XmStringFree (do2_text);
   XmStringFree (cd_text);
   XmStringFree (dr_text);
   XmStringFree (ba_text);
   XmStringFree (sa_text);
   XmStringFree (wa_text);
   if (dummywidget = XtNameToWidget (submenu, "button_3"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   redraw2  = XmStringCreateSimple ("Foreign Launch");
   redraw3  = XmStringCreateSimple ("Weapon Release");
   redraw4  = XmStringCreateSimple ("Situational");
   redraw5  = XmStringCreateSimple ("Strike");
   redraw6  = XmStringCreateSimple ("Mission");
   redraw7  = XmStringCreateSimple ("Mission Readiness");
   redraw8  = XmStringCreateSimple ("Decision Fusion");
   menupane = XmVaCreateSimplePulldownMenu (menubar, "Assess", 3, decideCB,
              XmVaPUSHBUTTON, redraw2, NULL, NULL, NULL,
              XmVaPUSHBUTTON, redraw3, NULL, NULL, NULL,
              XmVaPUSHBUTTON, redraw4, NULL, NULL, NULL,
              XmVaPUSHBUTTON, redraw5, NULL, NULL, NULL,
              XmVaPUSHBUTTON, redraw6, NULL, NULL, NULL,
              XmVaPUSHBUTTON, redraw7, NULL, NULL, NULL,
              XmVaPUSHBUTTON, redraw8, NULL, NULL, NULL,
              NULL);
   XmStringFree (redraw2);
   XmStringFree (redraw3);
   XmStringFree (redraw4);
   XmStringFree (redraw5);
   XmStringFree (redraw6);
   XmStringFree (redraw7);
   XmStringFree (redraw8);

   mouse = XmStringCreateSimple ("Mouse Operations");
   menupane = XmVaCreateSimplePulldownMenu (menubar, "help", 4, helpCB,
              XmVaPUSHBUTTON, mouse, NULL, NULL, NULL,
              NULL);
   XmStringFree (mouse);

   XtManageChild(menubar);
/*
 *  Build the Header and Trailer
 *  ----------------------------
 */
   bpalarm = XtVaCreateManagedWidget (" ", xmPushButtonWidgetClass, form,
                 XmNwidth,            200,
                 XmNheight,           30,
                 XmNshadowThickness,  2,
                 XmNbackground,       bg_grey,
                 XmNrecomputeSize,    False,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        menubar,
                 NULL);
   XtAddCallback (bpalarm, XmNactivateCallback,
                    (XtCallbackProc)alarmCB, NULL);

   title0       = XmStringCreateSimple("");
   title1       = XmStringCreateSimple("DEFCON 1");
   title2       = XmStringCreateSimple("DEFCON 2");
   title3       = XmStringCreateSimple("DEFCON 3");
   title4       = XmStringCreateSimple("DEFCON 4");
   title5       = XmStringCreateSimple("DEFCON 5");
   title6       = XmStringCreateSimple("RP 1");
   title7       = XmStringCreateSimple("RP 2");
   title8       = XmStringCreateSimple("Hold");
   title9       = XmStringCreateSimple("Free");
   deftitle = XtVaCreateManagedWidget("rowcol", xmRowColumnWidgetClass, form,
		 XmNwidth,            140,
                 XmNheight,           24,
                 XmNmarginHeight,     0,
                 XmNspacing,          0,
                 XmNisAligned,        True,
                 XmNadjustLast,       True,
                 XmNentryAlignment,   XmALIGNMENT_CENTER,
                 XmNlabelString,      title0,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        menubar,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   defmenu = XmVaCreateSimpleOptionMenu(deftitle, "DefMenu",
                                  title0, 'G', defcon-1, defconCB,
                 XmVaPUSHBUTTON,  title1, 'U', NULL, NULL,
                 XmVaPUSHBUTTON,  title2, 'L', NULL, NULL,
                 XmVaPUSHBUTTON,  title3, 'A', NULL, NULL,
                 XmVaPUSHBUTTON,  title4, 'W', NULL, NULL,
                 XmVaPUSHBUTTON,  title5, 'N', NULL, NULL,
                 XmVaSEPARATOR,
                 XmVaPUSHBUTTON,  title6, '1', NULL, NULL,
                 XmVaPUSHBUTTON,  title7, '2', NULL, NULL,
                 XmVaSEPARATOR,
                 XmVaRADIOBUTTON, title8, NULL, NULL, NULL,
                 XmVaRADIOBUTTON, title9, NULL, NULL, NULL,
                 NULL);
   XtVaSetValues(defmenu,
                 XmNheight,           20,
                 XmNmarginHeight,     0,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   XtManageChild(defmenu);
   XmStringFree(title0);
   XmStringFree(title1);
   XmStringFree(title2);
   XmStringFree(title3);
   XmStringFree(title4);
   XmStringFree(title5);
   XmStringFree(title6);
   XmStringFree(title7);
   XmStringFree(title8);
   XmStringFree(title9);

   classtitle   = XmStringCreateLtoR("Unclassified", "charset1");
   /*
   titlewidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,form,
                 XmNwidth,            bpwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNforeground,       bg_color,
                 XmNshadowThickness,  2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
		 XmNtopWidget,        menubar,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   XtManageChild(titlewidget);
   */
   topsep = XtVaCreateManagedWidget("TopSep", xmSeparatorWidgetClass, form,
                 XmNwidth,            bpwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        bpalarm,
                 NULL);
   XtManageChild(topsep);
   trailwidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,form,
                 XmNwidth,            bpwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNforeground,       bg_color,
                 XmNshadowThickness,  2,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   XtManageChild(trailwidget);
   botsep = XtVaCreateManagedWidget("BotSep", xmSeparatorWidgetClass, form,
                 XmNwidth,            bpwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     trailwidget,
                 NULL);
   XtManageChild(botsep);
   XmStringFree(classtitle);
/*
 *  Build the Pushbuttons
 *  ---------------------
 */
   gbi_info = XtVaCreateManagedWidget ("GBIs",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            64,
                 XmNheight,           32,
                 XmNshadowThickness,  3,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   XtAddCallback (gbi_info, XmNactivateCallback,
                    (XtCallbackProc)gbi_infoCB, NULL);

   threat_info = XtVaCreateManagedWidget ("Threats",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            64,
                 XmNheight,           32,
                 XmNshadowThickness,  3,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       gbi_info,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   XtAddCallback (threat_info, XmNactivateCallback,
                    (XtCallbackProc)threat_infoCB, NULL);

   asset_info = XtVaCreateManagedWidget ("Assets",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            64,
                 XmNheight,           32,
                 XmNshadowThickness,  3,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       threat_info,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   XtAddCallback (asset_info, XmNactivateCallback,
                    (XtCallbackProc)assetpopupCB, NULL);

   track_info = XtVaCreateManagedWidget ("Tracks",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            64,
                 XmNheight,           32,
                 XmNshadowThickness,  3,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       asset_info,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   XtAddCallback (track_info, XmNactivateCallback,
                    (XtCallbackProc)trackpopupCB, NULL);

   shoot_info = XtVaCreateManagedWidget ("Shoot",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            64,
                 XmNheight,           32,
                 XmNshadowThickness,  3,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       track_info,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);

   hold_info  = XtVaCreateManagedWidget ("Hold",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            71,
                 XmNheight,           32,
                 XmNshadowThickness,  3,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       shoot_info,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
/*
 *  Build the Text Display areas
 *  ----------------------------
 */
   sprintf((char *)line, "Msn Obj: %s", MOnames[missionselect]);
   classtitle   = XmStringCreateLtoR(line, "charset1");
   w_msnobj = XtVaCreateManagedWidget ("Msn", xmPushButtonWidgetClass, form,
                 XmNwidth,            190,
                 XmNheight,           24,
                 XmNshadowThickness,  2,
		 XmNlabelType,        XmSTRING,
		 XmNlabelString,      classtitle,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        gbi_info,
                 NULL);
   XtAddCallback (w_msnobj, XmNactivateCallback,
                    (XtCallbackProc)missionCB, NULL);
   XmStringFree(classtitle);

   sprintf((char *)line, "Battle Plan: %s", BPnames[planselect]);
   classtitle   = XmStringCreateLtoR(line, "charset1");
   w_btlplan = XtVaCreateManagedWidget ("Btl", xmPushButtonWidgetClass, form,
                 XmNwidth,            190,
                 XmNheight,           24,
                 XmNshadowThickness,  2,
		 XmNlabelType,        XmSTRING,
		 XmNlabelString,      classtitle,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       w_msnobj,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        hold_info,
                 NULL);
   XtAddCallback (w_btlplan, XmNactivateCallback,
                    (XtCallbackProc)battleCB, NULL);
   XmStringFree(classtitle);

   defconlevel   = XtVaCreateManagedWidget("Defcon", xmTextFieldWidgetClass, form,
                 XmNwidth,            220,
                 XmNheight,           20,
                 XmNmarginHeight,     0,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNvalue,            " ",
                 XmNfontList,         fontlist,
                 XmNforeground,       bg_color,
                 XmNeditable,         False,
                 XmNcursorPositionVisible,  False,
                 XmNblinkRate,        0,
                 XmNshadowThickness,  0,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        w_msnobj,
                 NULL);
   sprintf((char *)line, "DEFCON: %d    RP: %d    DEA: %s",
              defcon, Rposture, (WeapRelease ? "Free" : "Hold") );
   XmTextSetString(defconlevel, (char *)line);

   roefield   = XtVaCreateManagedWidget("ROE", xmTextFieldWidgetClass, form,
                 XmNwidth,            160,
                 XmNheight,           20,
                 XmNmarginHeight,     0,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNvalue,            "ROE: N. America",
                 XmNfontList,         fontlist,
                 XmNforeground,       bg_color,
                 XmNeditable,         False,
                 XmNcursorPositionVisible,  False,
                 XmNblinkRate,        0,
                 XmNshadowThickness,  0,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       defconlevel,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        w_btlplan,
                 NULL);
   sprintf((char *)line, "ROE: %s", chroe);
   XmTextSetString(roefield, (char *)line);

   midsep = XtVaCreateManagedWidget("MidSep", xmSeparatorWidgetClass, form,
                 XmNwidth,            bpwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        defconlevel,
                 NULL);
   XtManageChild(midsep);

   sepsummary = XtVaCreateManagedWidget("SumSep", xmSeparatorWidgetClass, form,
                 XmNwidth,            10,
                 XmNorientation,      XmVERTICAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       hold_info,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     midsep,
                 NULL);

   gvttime   = XtVaCreateManagedWidget("Nthreat", xmTextFieldWidgetClass, form,
                 XmNwidth,            190,
                 XmNheight,           20,
                 XmNmarginHeight,     0,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNvalue,            " ",
                 XmNfontList,         fontlist,
                 XmNforeground,       fg_color,
                 XmNeditable,         False,
                 XmNcursorPositionVisible,  False,
                 XmNblinkRate,        0,
                 XmNshadowThickness,  0,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       sepsummary,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   sprintf((char *)line, "Threats: %3d    GVT: %5d", threattrack, (int)GVT_time);
   XmTextSetString(gvttime, (char *)line);

   simcmdlevel   = XtVaCreateManagedWidget("SClevel", xmTextFieldWidgetClass, form,
                 XmNwidth,            240,
                 XmNheight,           20,
                 XmNmarginHeight,     0,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNvalue,            " ",
                 XmNfontList,         fontlist,
                 XmNforeground,       fg_color,
                 XmNeditable,         False,
                 XmNcursorPositionVisible,  False,
                 XmNblinkRate,        0,
                 XmNshadowThickness,  0,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       sepsummary,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        gvttime,
                 NULL);
   dosubmenu(2, 0);

   if ((BITMAPDIR=getenv("BITMAPDIR")) == NULL) {
        BITMAPDIR = "./BitMaps";
   }
   Boolean pixerror = False;
   sprintf (filename, "%s%s", BITMAPDIR, "/led-yellow.xpm");
   yelledpix = XmGetPixmap(XtScreen(toplevel), filename,
                   BlackPixelOfScreen(XtScreen(toplevel)),
                   WhitePixelOfScreen(XtScreen(toplevel)));
   if (yelledpix == XmUNSPECIFIED_PIXMAP) {
       printf("Can't create Yellow LED pixmap\n"); pixerror = True;
   }
   sprintf (filename, "%s%s", BITMAPDIR, "/led-green.xpm");
   grnledpix = XmGetPixmap(XtScreen(toplevel), filename,
                   BlackPixelOfScreen(XtScreen(toplevel)),
                   WhitePixelOfScreen(XtScreen(toplevel)));
   if (grnledpix == XmUNSPECIFIED_PIXMAP) {
       printf("Can't create Green LED pixmap\n"); pixerror = True;
   }
   sprintf (filename, "%s%s", BITMAPDIR, "/led-red.xpm");
   redledpix = XmGetPixmap(XtScreen(toplevel), filename,
                   BlackPixelOfScreen(XtScreen(toplevel)),
                   WhitePixelOfScreen(XtScreen(toplevel)));
   if (redledpix == XmUNSPECIFIED_PIXMAP) {
       printf("Can't create Red LED pixmap\n"); pixerror = True;
   }
   if (!pixerror) {
       modestat = XtVaCreateManagedWidget("Led", xmDrawingAreaWidgetClass, form,
		 XmNwidth,            16,
                 XmNheight,           16,
                 XmNmarginHeight,     2,
                 XmNspacing,          2,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     midsep,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
       XtVaSetValues(modestat, XmNbackgroundPixmap, yelledpix, NULL);

       runstat = XtVaCreateManagedWidget("Led", xmDrawingAreaWidgetClass, form,
		 XmNwidth,            16,
                 XmNheight,           16,
                 XmNmarginHeight,     2,
                 XmNspacing,          2,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     modestat,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
       XtVaSetValues(runstat, XmNbackgroundPixmap, grnledpix, NULL);

       simstat = XtVaCreateManagedWidget("Led", xmDrawingAreaWidgetClass, form,
		 XmNwidth,            16,
                 XmNheight,           16,
                 XmNmarginHeight,     2,
                 XmNspacing,          2,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     runstat,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
       XtVaSetValues(simstat, XmNbackgroundPixmap, grnledpix, NULL);

       hilstat = XtVaCreateManagedWidget("Led", xmDrawingAreaWidgetClass, form,
		 XmNwidth,            16,
                 XmNheight,           16,
                 XmNmarginHeight,     2,
                 XmNspacing,          2,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     simstat,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
       XtVaSetValues(hilstat, XmNbackgroundPixmap, redledpix, NULL);
   }

   piepix = XCreatePixmap(dpy, XRootWindow(dpy,0), 40, 40, DefaultDepth(dpy,0));

   piechart = XtVaCreateManagedWidget("Led", xmPushButtonWidgetClass, form,
		 XmNwidth,            48,
                 XmNheight,           48,
                 XmNmarginHeight,     0,
                 XmNspacing,          0,
                 XmNentryAlignment,   XmALIGNMENT_CENTER,
                 XmNlabelType,        XmPIXMAP,
                 XmNlabelPixmap,      piepix,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     midsep,
                 XmNrightAttachment,  XmATTACH_WIDGET,
                 XmNrightWidget,      modestat,
                 NULL);
   XtAddCallback (piechart, XmNactivateCallback,
                    (XtCallbackProc)pie_infoCB, NULL);
   XtManageChild(piechart);

   piesizes[1] = ((float)gbitotal-(float)gbiwithheld-(float)gbiexpended)/(float)gbitotal;
   piesizes[2] = (float)gbiwithheld/(float)gbitotal;
   piesizes[3] = (float)gbiexpended /(float)gbitotal;
   fgcolors[1] = fg_green;  // green - available
   fgcolors[2] = bg_yellow; // yellow - withheld
   fgcolors[3] = bg_color;  // red - expended

   DrawPie(piechart, 3, piesizes, fgcolors);
/*
 *  Build the Lower Pushbuttons
 *  ---------------------------
 */
   globetrack = XtVaCreateManagedWidget ("Globe Track ...",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            180,
                 XmNheight,           30,
                 XmNshadowThickness,  2,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);
   XtAddCallback (globetrack, XmNactivateCallback,
                    (XtCallbackProc)globeCB, NULL);

   timetogo = XtVaCreateManagedWidget ("Time To Go ...",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            180,
                 XmNheight,           30,
                 XmNshadowThickness,  2,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       globetrack,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);

   threatass = XtVaCreateManagedWidget ("Threatened Assets ...",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            180,
                 XmNheight,           30,
                 XmNshadowThickness,  2,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       timetogo,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);

   exceptman = XtVaCreateManagedWidget ("Management By Exception ...",
                 xmPushButtonWidgetClass, form,
                 XmNwidth,            180,
                 XmNheight,           30,
                 XmNshadowThickness,  2,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       threatass,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);

   btnsep = XtVaCreateManagedWidget("BtnSep", xmSeparatorWidgetClass, form,
                 XmNwidth,            bpwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     exceptman,
                 NULL);
   XtManageChild(btnsep);
/*
 *  Build the Summary Views
 *  -----------------------
 */
   title0       = XmStringCreateSimple("Summary:");
   title1       = XmStringCreateSimple("Track Engagement Summary");
   title2       = XmStringCreateSimple("Sensor Tracks");
   title3       = XmStringCreateSimple("Engagement Tracks");
   title4       = XmStringCreateSimple("Predictive Tracks");
   sumtitle = XtVaCreateManagedWidget("rowcol", xmRowColumnWidgetClass, form,
				      //XmNwidth,            280,
                 XmNheight,           26,
                 XmNmarginHeight,     0,
                 XmNspacing,          0,
                 XmNisAligned,        True,
                 XmNadjustLast,       True,
                 XmNentryAlignment,   XmALIGNMENT_CENTER,
                 XmNlabelString,      title0,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     midsep,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       sepsummary,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   summenu = XmVaCreateSimpleOptionMenu(sumtitle, "SumMenu",
                                 title0, 'G',    0, summaryCB,
                 XmVaPUSHBUTTON, title1, 'T', NULL, NULL,
                 XmVaPUSHBUTTON, title2, 'S', NULL, NULL,
                 XmVaPUSHBUTTON, title3, 'E', NULL, NULL,
                 XmVaPUSHBUTTON, title4, 'P', NULL, NULL,
                 NULL);
   XtVaSetValues(summenu,
                 XmNmarginHeight,     0,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       sepsummary,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   XmStringFree(title0);
   XmStringFree(title1);
   XmStringFree(title2);
   XmStringFree(title3);
   XmStringFree(title4);

   XtManageChild(summenu);

   char str1[80] = " Trk     Obj     Msl    Exp    Leth     Tgt     Pre Imp    T to  |";
   char str2[80] = " ID     Type     Type   Tgts   Value    Type     Loc.     Impact |";
   char str3[80] = "----  --------  ------  ----   ------  -------- --------  ------ |";
   char str4[80] = "   Trk    Launch    Eng        Current Engagement    |";
   char str5[80] = "  Status   Site    Status   #Wpn  TTI   Pk    Remain |";
   char str6[80] = " ------- -------- --------  ---- ----- -----  ------ |";

   sprintf(line, "%s%s\n%s%s\n%s%s", str1,str4, str2,str5, str3,str6);
   title    = XmStringCreateLtoR(line, "charset1");
   header   = XtVaCreateManagedWidget("Header", xmLabelWidgetClass, form,
				      //XmNwidth,            bpwindW,
                 XmNheight,           60,
                 XmNmarginHeight,     2,
                 XmNmarginWidth,      2,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
		 XmNlabelType,        XmSTRING,
		 XmNlabelString,      title,
                 XmNforeground,       fg_color,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        midsep,
                 NULL);
   XmStringFree(title);

   threatlist = (XmStringTable)XtMalloc(threattotal*sizeof(XmString *));
   for (i=0; i<threattotal; i++) {
       sprintf(line,
 "%4d %8.8s %6.6s  %4d    %6.4f %8.8s   %8.8s  %2d:%2.2d | %7.7s %8.8s %8.8s %4d %2d:%2.2d  %5.3f  %4d", 
               i, "Missile", threattypes[i], 0, 0.67, "Target", threattsite[i], 0, 0,
               " Prop", threatlsite[i], "Planned", gbingbis[1], 0, 0, gbipkill, 2);
       threatlist[i] = XmStringCreateSimple((char *)line);
   }
   threat_widget = XmCreateScrolledList (form, "Threats", NULL, 0);
   XtVaSetValues (threat_widget,
                 XmNitems,             threatlist,
                 XmNitemCount,         threattotal,
                 XmNvisibleItemCount,  16,
                 XmNscrollBarDisplayPolicy, XmSTATIC,
                 NULL);
   XtVaSetValues (XtParent(threat_widget),
                 XmNscrolledWindowMarginWidth, 2,
                 XmNtopAttachment,     XmATTACH_WIDGET,
                 XmNtopWidget,         header,
                 XmNbottomAttachment,  XmATTACH_WIDGET,
                 XmNbottomWidget,      btnsep,
                 XmNleftAttachment,    XmATTACH_FORM,
                 XmNrightAttachment,   XmATTACH_FORM,
                 NULL);
   XtManageChild(threat_widget);
   XtAddCallback(threat_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)threatlistCB, NULL);

   XtRealizeWidget(toplevel);
   XtPopup(toplevel, XtGrabNone);
}

void
X_update_alarm(char *str, int priority)
{
Display       *dpy;
XmString      xstr;

//sprintf((char *)line, "Potential Missle Event");
   xstr  = XmStringCreateLtoR(str, "charset1");
   XtVaSetValues(bpalarm, XmNlabelString, xstr, NULL);
   switch (priority) {
      case PRIORITY_HI:
         XtVaSetValues(bpalarm, XmNbackground,  bg_color, NULL);
	 alarm_active = TRUE;
         break;
      case PRIORITY_MED:
         XtVaSetValues(bpalarm, XmNbackground,  bg_yellow, NULL);
	 alarm_active = TRUE;
         break;
      case PRIORITY_LOW:
         XtVaSetValues(bpalarm, XmNbackground,  fg_green, NULL);
	 alarm_active = TRUE;
         break;
      case PRIORITY_OFF:
         XtVaSetValues(bpalarm, XmNbackground,  bg_grey, NULL);
	 alarm_active = FALSE;
         break;
      default:
         break;
   }
   dpy = XtDisplay(toplevel);
   XBell(dpy, 0);
}

void
X_update_gvt(int gvt_time)
{
   sprintf((char *)line, "Threats: %3d    GVT: %5d", threattrack, (int)gvt_time);
   XmTextSetString(gvttime, (char *)line);
   GVT_time = start_time;
}

void
X_update_roe(char *roe)
{
   sprintf((char *)line, "ROE: %s", roe);
   XmTextSetString(roefield, (char *)line);
}

void
X_delete_list(int n_assignments)
{
   XmListDeleteAllItems(threat_widget);
   X_update_gvt((int)start_time);
}

void
X_add_list(int pos, char *listitem)
{
XmString  item;

   item = XmStringCreateSimple((char *)listitem);   
   XmListAddItem(threat_widget, item, 0);
}


void
X_update_gbi()
{
      piesizes[1] = ((float)gbitotal-(float)gbiwithheld-(float)gbiexpended)/(float)gbitotal;
      piesizes[2] = (float)gbiwithheld/(float)gbitotal;
      piesizes[3] = (float)gbiexpended /(float)gbitotal;
      DrawPie(piechart, 3, piesizes, fgcolors);
      //XtVaSetValues(piechart, XmNlabelPixmap, piepix, NULL);
}

void
X_track_update ()
{
Widget   dialog;
Arg      arg[10];
char     str [2280];
XmString xstr;
int      i;

   if (trkvisible) {
      i = listselect;
      sprintf (str,
" %s%d\n %s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s\n %s%f\n %s%f\n %s%f\n %s%f\n %s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n",
	        "Track Id:              ", (int)Tsave[i][19], 
                "Threat Position:       ",
                "    At Time:           ", Tsave[i][16],
                "    X Position:        ", Tsave[i][ 0],
                "    Y Position:        ", Tsave[i][ 1],
                "    Z Position:        ", Tsave[i][ 2],
                "    X Velocity:        ", Tsave[i][ 3],
                "    Y Velocity:        ", Tsave[i][ 4],
                "    Z Velocity:        ", Tsave[i][ 5],
                "    Altitude:          ", Tsave[i][ 6],
                "    Speed (Kps):       ", Tsave[i][ 7],
                "Predicted Impact:      ",
                "    At Time:           ", Tsave[i][17],
                "    Longitude:         ", Tsave[i][ 8],
                "    Latitude:          ", Tsave[i][ 9],
                "    Speed (Kps):       ", Tsave[i][15],
                "Interceptor:           ",
                "    Intercept Time:    ", 0.0,
                "    Prob. of Kill:     ", 0.0,
                "    Flight Time:       ", 0.0,
                "    Max. Altitude:     ", 0.0,
                "    Min. Altitude:     ", 0.0,
                "    Miss Distance:     ", 0.0);
      xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
      XtVaSetValues(track_widget, XmNlabelString, xstr, NULL);
      //XmTextSetString(track_widget, (char *)str);
      //XtAppProcessEvent(appcontext, XtIMAll);
   }
}

void
DrawPie(Widget self, int narea, float *areas, Pixel *fillcolor)
{
int          i, x, y, len, Sangle, Eangle;
XFontStruct  *fn;
double       a;
unsigned int height = 40, width = 40;
static GC    PieGC=NULL;
XtGCMask     mask;
XGCValues    values;

   if (PieGC != NULL) XtReleaseGC(self, PieGC);
      /*if (DefaultDepthOfScreen(XtScreen(self)) > 4
        && choose_color(self, 0.5, ((XfwfPieMenuWidget)self)->core.background_pixel,
	                &values.foreground)) mask = GCForeground;*/
      mask = GCFillStyle | GCBackground | GCForeground | GCArcMode | GCLineWidth;
      values.fill_style = FillSolid;
      values.background = BlackPixelOfScreen(XtScreen(self));
      values.foreground = WhitePixelOfScreen(XtScreen(self));
      values.arc_mode   = ArcPieSlice;
      values.line_width = 1;
      PieGC = XCreateGC(XtDisplay(self), piepix, mask, &values);

   /*
    * Draw the segments each with size areas[i]
    */
   Sangle = 0;
   for (i = 1; i <= narea; i++) {
       Eangle = (int)(360.0*64.0*areas[i]);
       XSetForeground(XtDisplay(self), PieGC, fillcolor[i]); 
       XFillArc(XtDisplay(self), piepix, PieGC, 0, 0,
                width, height, Sangle, Eangle);
       Sangle = Sangle + Eangle;
   }
   XtVaSetValues(self, XmNlabelPixmap, piepix, NULL); 
}

/* ---------------------- main menu CB's ------------------------ */

void
logoCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   switch (item_no)
   {
     case 0:                                    // Pause
       XtRemoveTimeOut(timeoutid);
       XtVaSetValues(runstat, XmNbackgroundPixmap, redledpix, NULL);
       break;
     case 1:                                    // Resume
       XtVaSetValues(runstat, XmNbackgroundPixmap, grnledpix, NULL);
       timeoutid = XtAppAddTimeOut(appcontext, POLLRATE,
                    (XtTimerCallbackProc)timeoutCB, NULL);
       break;
     case 2:                                    // Quit
       Net_close();
       exit (0);
       break;
     default:
       break;
   } 
}

void
optionCB (Widget, XtPointer client_data, XtPointer)
{
  int memorycode;

  memorycode = (int)client_data;
  switch (memorycode)
  {  
     case 0:
       break;
     default:
       break;
  }
}

void
decideCB (Widget, XtPointer client_data, XtPointer)
{
int index;

   index = (int)client_data;
   SC_message(OP_BELIEF+index);
}

void
commandCB (Widget, XtPointer client_data, XtPointer)
{
   MajorMenu = (int)client_data;
   dosubmenu(MajorMenu, MinorMenu);
}

void
submenu1CB(Widget, XtPointer client_data, XtPointer)
{
   MajorMenu = 2;
   MinorMenu = (int)client_data;
   dosubmenu(MajorMenu, MinorMenu);
}
void
submenu2CB(Widget, XtPointer client_data, XtPointer)
{
   MajorMenu = 3;
   MinorMenu = (int)client_data;
   dosubmenu(MajorMenu, MinorMenu);
}
void
submenu3CB(Widget, XtPointer client_data, XtPointer)
{
   MajorMenu = 4;
   MinorMenu = (int)client_data;
   dosubmenu(MajorMenu, MinorMenu);
}
void
submenu4CB(Widget, XtPointer client_data, XtPointer)
{
   MajorMenu = 5;
   MinorMenu = (int)client_data;
   dosubmenu(MajorMenu, MinorMenu);
}

void
dosubmenu(int Major, int Minor)
{
char   *command[7] = { "Real", "Simulated", "NORAD CCC", "BMDC", "SCCC", "Firing Unit" };
char   *subcmd1[4] = { "CINC/CD", "Def Officer 1", "Def Officer 2", " " };
char   *subcmd2[4] = { "Cmdr/Director", "Battle Analyst", "Sensor Analyst", "Weapons Analyst" };

   switch (Major)
   {  
     case 0:
       Realcmdr = !Realcmdr;
       if (Realcmdr)
          XtVaSetValues(hilstat, XmNbackgroundPixmap, grnledpix, NULL);
       else
          XtVaSetValues(hilstat, XmNbackgroundPixmap, redledpix, NULL);
       break;
     case 1:
       Simcmdr = !Simcmdr;
       if (Simcmdr)
          XtVaSetValues(simstat, XmNbackgroundPixmap, grnledpix, NULL);
       else
          XtVaSetValues(simstat, XmNbackgroundPixmap, redledpix, NULL);
       break;
     case 2:
       sprintf((char *)line, "Command:  %s(%s)", command[Major], subcmd1[Minor]);
       XmTextSetString(simcmdlevel, (char *)line);
       break;
     default:
       sprintf((char *)line, "Command:  %s(%s)", command[Major], subcmd2[Minor]);
       XmTextSetString(simcmdlevel, (char *)line);
       break;
   }
}

void
defconCB (Widget menuitem, XtPointer itemno, XmAnyCallbackStruct* call_data)
{
int    i;

   switch ((int)itemno) {
     case 0:
     case 1:
     case 2:
     case 3:
     case 4:
       defcon = (int) itemno+1;
       i = SC_message(OP_DEFCON);
       break;
     case 5:
     case 6:
       Rposture = (int)itemno-4;
       i = SC_message(OP_RP);
       break;
     case 7:
     case 8:
       WeapRelease = (int)itemno-7;
       break;
     default:
       break;
    }
    sprintf((char *)line, "DEFCON: %d    RP: %d    DEA: %s",
           defcon, Rposture, (WeapRelease ? "Free" : "Hold") );
    XmTextSetString(defconlevel, (char *)line);
}

void
helpCB (Widget w, XtPointer client_data, XtPointer)
{
int      item_no = (int) client_data;
Widget   idialog;
XmString helptext;
Arg      args[2];
char*    helpstr;
 
   switch (item_no)
   {
      case 0:   // mouse operations:
        helpstr = "Mouse Operations:\n Left: picking;\n Shift_Left: track ball action.";
        helptext = XmStringCreateLtoR (helpstr, XmSTRING_DEFAULT_CHARSET); 
        XtSetArg (args[0], XmNmessageString, helptext);
        XtSetArg (args[1], XmNautoUnmanage, False);
        XmStringFree (helptext);
        break;
      default:
        break;
   }
}

void
alarmCB (Widget w, XtPointer client_data, XtPointer)
{
int      item_no = (int) client_data;
XmString xstr;

   sprintf((char *)line, " ");
   xstr  = XmStringCreateLtoR(line, "charset1");
   XtVaSetValues(bpalarm, XmNlabelString, xstr, NULL);
   XtVaSetValues(bpalarm, XmNbackground,  bg_grey, NULL);
}

/* -------------------------- popup box CB's -------------------------- */

void
assetpopupCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
Widget        asset_form, gbr_widget, gbr_info, done_button;
XmStringTable gbrlist;
int           index, i;
Boolean       asfirst = TRUE;

 if (asfirst) {
   asfirst = FALSE;
   asset_shell = XtCreatePopupShell("Assets", topLevelShellWidgetClass,
                                toplevel, 0, 0);
   asset_form = XmCreateForm (asset_shell, "ASForm", NULL, 0);
   XtVaSetValues(asset_form,
               XmNwidth,             200,
               XmNheight,            400,
               NULL);

   gbrlist = (XmStringTable)XtMalloc((assetcount)*sizeof(XmString *));
   index = -1;
   for (i=0; i<gbrcount; i++) {
       index = index+1;
       astypes[index] = GBRTYPE;
       gbrlist[index] = XmStringCreateSimple(gbrnames[i]);
   }
   for (i=0; i<eyecount; i++) {
       index = index+1;
       astypes[index] = EYETYPE;
       gbrlist[index] = XmStringCreateSimple(eyenames);
   }
   for (i=0; i<dspcount; i++) {
       index = index+1;
       astypes[index] = DSPTYPE;
       gbrlist[index] = XmStringCreateSimple(dspnames[i]);
   }
   for (i=0; i<gbicount; i++) {
       index = index+1;
       astypes[index] = GBITYPE;
       gbrlist[index] = XmStringCreateSimple(gbinames[i]);
   }

   gbr_widget = XmCreateScrolledList (asset_form, "GBRs", NULL, 0);
   XtVaSetValues (gbr_widget,
               XmNitems,             gbrlist,
               XmNitemCount,         index+1,
               XmNvisibleItemCount,  14,
               XmNscrollBarDisplayPolicy, XmSTATIC,
               NULL);
   XtVaSetValues (XtParent(gbr_widget),
               XmNtopAttachment,     XmATTACH_FORM,
               XmNleftAttachment,    XmATTACH_FORM,
               XmNrightAttachment,   XmATTACH_FORM,
               NULL);

   sprintf(line, "\n %s %d\n %s %d\n %s %d\n %s %d\n %s %d\n %s %d\n",
           "    GBR Assets .....", gbrcount,
           "    SIBRS Assets ...", eyecount,
           "    DSP Assets .....", dspcount,
           "    GBI Assets .....", gbicount,
           "    Air Assets .....", aircount,
           "    Sea Assets .....", seacount);
   XmString title    = XmStringCreateLtoR(line, "charset1");
   Widget counts = XtVaCreateManagedWidget("Header", xmLabelWidgetClass, asset_form,
		 XmNwidth,            180,
                 XmNheight,           100,
                 XmNmarginHeight,     2,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
		 XmNlabelType,        XmSTRING,
		 XmNlabelString,      title,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        gbr_widget,
                 NULL);
   XmStringFree(title);

   gbr_info = XtVaCreateManagedWidget ("Info",
                 xmPushButtonWidgetClass, asset_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);
   XtAddCallback (gbr_info, XmNactivateCallback,
                    (XtCallbackProc)gbr_infoCB, NULL);

   done_button = XtVaCreateManagedWidget ("Close",
                 xmPushButtonWidgetClass, asset_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)asset_doneCB, NULL);

   XtManageChild(gbr_widget);
   XtAddCallback(gbr_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)gbrlistCB, NULL);

   XtManageChild(asset_form);
 }
 XtPopup(asset_shell, XtGrabNone);
}
void
asset_doneCB ()
{
  XtPopdown (asset_shell);
}

void
trackpopupCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
Widget        track_form, done_button;
int           index, i;
Boolean       trkfirst = TRUE;
char          line[1280];

   if (trkfirst) {
     trkfirst = FALSE;
     track_shell = XtCreatePopupShell("Tracks", topLevelShellWidgetClass,
                                toplevel, 0, 0);
     track_form = XmCreateForm (track_shell, "TRKForm", NULL, 0);

     track_widget   = XtVaCreateManagedWidget("Track", xmLabelWidgetClass, track_form,
                 XmNwidth,            240,
                 XmNheight,           400,
                 XmNmarginHeight,     2,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
		 XmNlabelType,        XmSTRING,
					      //XmNeditable,         False,
					      //XmNautoShowCursorPosition, True,
					      //XmNcursorPositionVisible,  False,
					      //XmNblinkRate,        0,
					      //XmNshadowThickness,  0,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_FORM,
                 NULL);

     done_button = XtVaCreateManagedWidget ("Close",
                 xmPushButtonWidgetClass, track_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);
     XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)track_doneCB, NULL);

     XtManageChild(track_widget);

     XtManageChild(track_form);
   }

   XtPopup(track_shell, XtGrabNone);
   trkvisible = TRUE;

   X_track_update();
}
void
track_doneCB ()
{
  XtPopdown (track_shell);
  trkvisible = FALSE;
}

/* ----------------------- list selection CB's ----------------------- */

void
gbrlistCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
   int  j;
   XmString     nodenoid;
   Arg args[10];

   gbrselect = call_data->item_position - 1;
}

void
threatlistCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
int i;

   i = call_data->item_position - 1;
   listselect = i;
   threatselect = assign_matrix[i][0];
   gbiselect = assign_matrix[i][1];
}

/* -------------------- information dialog CB's -------------------- */

void
missionCB ()
{
Widget   dialog;
Arg      arg[10];
char     str [1280];
XmString xstr;
int      i;

  i = missionselect;
  sprintf (str, " %s%s\n %s%s\n %s%s\n %s%f\n %s%d\n %s%d\n %s%d\n ", 
                "Name of Mission Objective: ", MOnames[i],
                "Strategy:                  ", MOstrategy[i],
                "Tactic:                    ", MOtactic[i],
                "Probability of Success:    ", MOpksuccess[i],
                "Mode:                      ", MOmode[i],
                "Interceptors Withheld:     ", MOwithhold[i],
                "Additional Boosters:       ", MObooster[i] );
  xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
  XtSetArg (arg[0], XmNmessageString, xstr);
  dialog = XmCreateMessageDialog (toplevel, "GBI Info", arg, 1);
  XmStringFree (xstr);
  XtManageChild (dialog);
}

void
battleCB ()
{
Widget   dialog;
Arg      arg[10];
char     str [1280];
XmString xstr;
int      i;

  i = planselect;
  sprintf (str, " %s%s\n %s%s\n %s%s\n %s%s\n %s%s\n %s%d\n %s%d\n %s%f\n %s%f\n %s%d\n %s%d\n %s%d\n %s%d\n %s%d\n", 
                "Name of Battle Plan:     ", BPnames[i],
                "Mode:                    ", BPmode[i],
                "Cutoff:                  ", BPcutoff[i],
                "Kill Criteria:           ", BPkill[i],
                "Override Salvo:          ", (BPoverride[i]==0 ? "True" : "False"),
                "Launch Mode:             ", BPlaunch[i],
                "Interceptor Salvo Count: ", BPsalvo[i],
                "RV Threshold:            ", BPthreshold[i],
                "Pk Cutoff:               ", BPpkcutoff[i], 
                "Population Weighting:    ", BPw_pop[i],
                "Military Weighting:      ", BPw_mil[i],
                "Self Defense Weighting:  ", BPw_def[i],
                "NC Authority Weighting:  ", BPw_nca[i],
                "Industrial Weighting:    ", BPw_ind[i] );
  xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
  XtSetArg (arg[0], XmNmessageString, xstr);
  dialog = XmCreateMessageDialog (toplevel, "BtlPlan", arg, 1);
  XmStringFree (xstr);
  XtManageChild (dialog);
}

void
gbi_infoCB ()
{
Widget   dialog;
Arg      arg[10];
char     str [1280];
XmString xstr;
int      i;

  i = gbiselect;
  sprintf (str, " %s%s\n %s%d\n %s%f\n %s%d\n %s%d\n %s%f%s\n %s%f%s\n %s%s\n %s%d\n", 
                "Name of GBI Farm:      ", gbinames[i],
                "Id of Farm:            ", gbiident[i],
                "Probability of Kill:   ", gbipkill[i],
                "No. of Interceptors:   ", gbingbis[i],
                "No. Withheld:          ", gbinhold[i],
                "Latitude of Farm:      ", gbilatit[i], gbilatNS[i],
                "Longitude of Farm:     ", gbilongi[i], gbilonEW[i],
                "Operational Status:    ", gbistatus[i],
                "SPEEDES Object Id:     ", 9999);
  xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
  XtSetArg (arg[0], XmNmessageString, xstr);
  dialog = XmCreateMessageDialog (toplevel, "GBI Info", arg, 1);
  XmStringFree (xstr);
  XtManageChild (dialog);
}

void
pie_infoCB ()
{
Widget   dialog;
Arg      arg[10];
char     str [1280];
XmString xstr;
int      i;

  i = gbiselect;
  sprintf (str, " %s%d\n %s%d\n %s%d\n ",
                "Weapons Available: ", gbitotal-gbiwithheld-gbiexpended,
                "Weapons Withheld:  ", gbiwithheld,
                "Weapons Expended:  ", gbiexpended,
                "TOTAL:             ", gbitotal);

  xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
  XtSetArg (arg[0], XmNmessageString, xstr);
  dialog = XmCreateMessageDialog (toplevel, "GBI Info", arg, 1);
  XmStringFree (xstr);
  XtManageChild (dialog);
}

void
threat_infoCB ()
{
Widget   dialog;
Arg      arg[10];
char     str [1280];
XmString xstr;
int      i;

  i = threatselect;
  sprintf (str, 
" %s%s\n %s%s\n %s%f\n %s%f\n %s%s\n %s%f%s\n %s%f%s\n %s%s\n %s%f%s\n %s%f%s\n %s%d\n ", 
                "Name of Threat:        ", threatnames[i],
                "Type of Threat:        ", threattypes[i],
                "Threat Launch Time:    ", threatltime[i],
                "Threat Random Distance:", threatrdist[i],
                "Name of Launch Site    ", threatlsite[i],
                "Latitude of Launch:    ", threatllat[i], "N",
                "Longitude of Launch:   ", threatllon[i], "E",
                "Name of Target Site    ", threattsite[i],
                "Latitude of Target:    ", threattlat[i], "N",
                "Longitude of Target:   ", threattlon[i], "E",
                "SPEEDES Object Id:     ", 9999);
  xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
  XtSetArg (arg[0], XmNmessageString, xstr);
  dialog = XmCreateMessageDialog (toplevel, "Threat Info", arg, 1);
  XmStringFree (xstr);
  XtManageChild (dialog);
}

void
gbr_infoCB ()
{
Widget   dialog;
Arg      arg[10];
char     str [1280];
XmString xstr;
int      i, isave;

  switch (astypes[gbrselect]) {
  case GBRTYPE:
    i = gbrselect;
    sprintf (str, 
" %s%s\n %s%d\n %s%f%s\n %s%f%s\n %s%s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%d\n",
                "Name of GBR Site:      ", gbrnames[i],
                "Id of Site:            ", gbrident[i],
                "Latitude of Site:      ", gbrlatit[i], gbrlatNS[i],
                "Longitude of Site:     ", gbrlongi[i], gbrlonEW[i],
                "Operational Status:    ", "GREEN",
                "Scan Time:             ", gbrscanT[i],
                "Minimum Range:         ", gbrrmin[i],
                "Maximum Range:         ", gbrrmax[i],
                "Minimum Rdot:          ", gbrrdot[i],
                "Signal:                ", gbrsignal[i],
                "Luminosity:            ", gbrlumin[i],
                "Error:                 ", gbrerror[i],
                "SPEEDES Object Id:     ", 9999);
    break;
  case EYETYPE:
    sprintf (str, 
" %s%s\n %s%d\n %s%d\n %s%f\n %s%f\n %s%s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%d\n",
                "Name of Sensor:        ", eyenames,
                "No. of Rings:          ", eyerings,
                "No. of Sensors/Ring:   ", eyesize,
                "Altitude:              ", eyealt,
                "Inclination:           ", eyeincl,
                "Operational Status:    ", "GREEN",
                "Scan Time:             ", eyescanT,
                "Minimum Range:         ", eyermin,
                "Maximum Range:         ", eyermax,
                "Minimum Rdot:          ", eyerdot,
                "Signal:                ", eyesignal,
                "Luminosity:            ", eyelumin,
                "Error:                 ", eyeerror,
                "SPEEDES Object Id:     ", 9999);
    break;
  case DSPTYPE:
    i = gbrselect-gbrcount-eyecount;
    sprintf (str, 
" %s%s\n %s%f\n %s%s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%d\n",
                "Name of Sensor:        ", dspnames[i],
                "Longitude:             ", dsplongi[i],
                "Operational Status:    ", "GREEN",
                "Scan Time:             ", dspscanT[i],
                "Minimum Range:         ", dsprmin[i],
                "Maximum Range:         ", dsprmax[i],
                "Minimum Rdot:          ", dsprdot[i],
                "Signal:                ", dspsignal[i],
                "Luminosity:            ", dsplumin[i],
                "Error:                 ", dsperror[i],
                "SPEEDES Object Id:     ", 9999);
    break;
  case GBITYPE:
    i = gbrselect-gbrcount-eyecount-dspcount;
    sprintf (str, " %s%s\n %s%d\n %s%f\n %s%d\n %s%d\n %s%f%s\n %s%f%s\n %s%s\n %s%d\n", 
                "Name of GBI Farm:      ", gbinames[i],
                "Id of Farm:            ", gbiident[i],
                "Probability of Kill:   ", gbipkill[i],
                "No. of Interceptors:   ", gbingbis[i],
                "No. Withheld:          ", gbinhold[i],
                "Latitude of Farm:      ", gbilatit[i], gbilatNS[i],
                "Longitude of Farm:     ", gbilongi[i], gbilonEW[i],
                "Operational Status:    ", gbistatus[i],
                "SPEEDES Object Id:     ", 9999);
    break;
  case AIRTYPE:
    i = gbrselect-gbrcount-eyecount-dspcount-gbicount;
    sprintf(str, "Air information not yet available.\n");
    break;
  case SEATYPE:
    i = gbrselect-gbrcount-eyecount-dspcount-gbicount-aircount;
    sprintf(str, "Sea information not yet available.\n");
    break;
  default:
    sprintf(str, "Error - Illegal asset type chosen.\n");
    break;
  }
  xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
  XtSetArg (arg[0], XmNmessageString, xstr);
  dialog = XmCreateMessageDialog (toplevel, "Asset Info", arg, 1);
  XmStringFree (xstr);
  XtManageChild (dialog);
}

void
globeCB ()
{
Widget   dialog;
Arg      arg[10];
char     str [1280];
XmString xstr;
int      i, pid;

   switch (pid = fork()) {
   case 0:
     execvp("gisp", NULL);
     perror("gisp");
     exit (255);
     break;
   case -1:
     printf("Child process startup failed.\n");
     break;
   }
}

/* -------------------------- text area CB's -------------------------- */

void
summaryCB (Widget menuitem, XtPointer itemno, XmAnyCallbackStruct* call_data)
{
   SumMenuItem = (int) itemno;
}

void process_standalone(char *standalone_file) {
  FILE *f;
  int i;
  int status;
  float fX[3];
  float fV[3];
  float ftime;
  double *X;
  double *V;
  double time;
  float fdummyend;
  int id;
  double start_time;
  int n_tracks;
  C_QUEUE qthreats;
  C_LANL_THREAT *lanl_threat;
  int n_assignments;
  char *outbuff;
  C_LANL_BP_OUTPUT *lanl_bp_output;
  int outsize;
  int thrt;
  int weap;
  double etime;
  int index;


#ifdef LINKLANL
   initial_();         // fprintf(stderr,"LANL_BP: initial_()\n");
   read_data_();       // fprintf(stderr,"LANL_BP: read_data_()\n");

  if (tpt) {
    set_tpt_();
    get_data_();       // fprintf(stderr,"LANL_BP: get_data_()\n");
  } else {
    reset_tpt_();
  }
#endif

  f = fopen(standalone_file,"r");
  if (f == NULL) {
    printf("cannot find %s\n",standalone_file);
    return;
  }

  printf("Opening standalone file %s\n",standalone_file);
  id = 1;

#ifdef LINKLANL
  printf("PROCESS_STANDALONE: calling init_threat_play\n");
  init_threat_play_();
#endif

  start_time = 0.0;

  while (1) {

//...... read in single precision

    status = fscanf(f,"%f %f %f %f %f %f %f %f\n",
	&fX[0],&fX[1],&fX[2],&fV[0],&fV[1],&fV[2],&ftime, &fdummyend);

    if (status == -1) break;

    lanl_threat = new C_LANL_THREAT(fX, fV, ftime, id);
    lanl_threat->print();
    qthreats.push_bot(lanl_threat);

    id++;

  }

  fclose(f);

//...... pass the threats to the battle planner

  n_tracks = qthreats.get_length();

  for (i=0; i<n_tracks; i++) {

    lanl_threat = (C_LANL_THREAT *)qthreats.pop_top();

#ifdef LINKLANL

    printf("PROCESS_STANDALONE: calling threat_setup\n");

    X = lanl_threat->get_X();
    V = lanl_threat->get_V();
    time = lanl_threat->get_time_tag();
    id = lanl_threat->get_id();

    threat_setup_ (	&X[0], &X[1], &X[2],
			&V[0], &V[1], &V[2],
			&time, &start_time,
			&n_tracks, &id, &id	);

#endif

    delete lanl_threat;

  }

//...... call the main lanl program to compute battle plan

#ifdef LINKLANL
  if (n_tracks != 0) {
    gpals_sim_();
  }
#endif


//...... get the battle plan

  if (n_tracks > 0) {

#ifdef LINKLANL
    get_n_assign_ (&n_assignments);
#endif

    lanl_bp_output = new C_LANL_BP_OUTPUT[n_assignments];
    outbuff = (char *)lanl_bp_output;
    outsize = n_assignments * sizeof(C_LANL_BP_OUTPUT);

    for (i=0; i<n_assignments; i++) {
      index = i+1;
#ifdef LINKLANL
      get_assign_ (&index, &thrt, &weap, &etime);
#endif
      printf ("Returned Plan: threat id %3d, weapon id %3d, time %f\n",
	thrt, weap, etime);
      lanl_bp_output[i].set_threat_id(thrt);
      lanl_bp_output[i].set_asset_id(weap);
      lanl_bp_output[i].set_launch_time(etime);
    }

  }else{

    outsize = 0;
    outbuff = NULL;

  }

//...... end of LANL process

  delete outbuff;

}


