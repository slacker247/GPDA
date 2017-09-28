/* *************************************************************************** **
**
**   Version       Modification
**   -------       ------------
**     2.2         Changed the name to 'GPDA' from 'BMC3'
**
** *************************************************************************** */
/*                                                                             */
#define COMPILED __DATE__                  /* Today's date for version info    */
#define VERSION  2                         /* Application Version number       */
#define RELEASE  2                         /* Application Release number       */
/*                                                                             */
/* *************************************************************************** */
/*                                                                             */
#define SC_THREADS                         /* <--- For POSIX Threads           */
//#define LINKLANL			   /* <--- For LANL Battle Planner     */
//#define LINKSC                           /* <--- For Sim. Commander          */
#define LINKCIC                            /* <--- For Message interface       */
/*                                                                             */
/* *************************************************************************** */
/*                                                                             */
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <pwd.h>
#include <grp.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/utsname.h>
#include <sys/sysinfo.h>

#ifdef SC_THREADS
#include "pthread.h"
#endif
/*
**   Include the network/socket stuff
*/
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#ifdef LINKSC
#include <rpc/xdr.h>
#endif

#include "parser.H"
#include "Globals.h"
/*
**   Include the Battle Manager interface stuff
*/
#ifdef LINKLANL
#include "kepler.H"
#include "host_user.H"
#include "ext_lanl_bp_mess.H"
#include "lanl_bp_input.H"
#include "lanl_bp_output.H"
#include "lanl_threat.H"
#include "def.h"
#endif

#include "GL/glx.h" 
#include "GL/gltk.h"
/*
**   Include the Sim Commander stuff
*/
#include "demo_opcodes.h"
#ifdef LINKSC
#include "demo_header.h"
#include "demo_msg_body.h"
#include "demo_players.h"
#include "demo_strings.h"
#endif
/*
**   Include the Xforms stuff
*/
#include "forms.h"
#include "BMCforms.h"
#include "about.h"
#include "PUPproc.H"
#include "DSBproc.h"

#include "alarms.H"
#include "messages.H"
 
/* --------------------------------------------------------------------- */  

#define SIGCHILD          SIGCLD 

#define YES               1
#define NO                0
  
#define OP_BELIEF         5000
 
#define RADDEG            0.0174532925199432958
#define POLLRATE          5000
#define NOISY         

#define WINdX             135
#define WINdY             192

#define IF_HOST           0
#define IF_SOCK           1
#define IF_ORB            2
#define IF_MON            3
#define n_ifs             4

#ifdef SC_THREADS
pthread_t       threadid;
pthread_mutex_t write_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

#ifdef IRIX
typedef int     socklen_t;
#endif
 
/* --------------------------------------------------------------------- */  

struct tracksinfo {
  char          trackid[16];
  int           targets;
  float         lethality;
  float         latitude;
  float         longitude;
  float         ltime;
  float         itime;
  char          objtype[16];
  char          msltype[16];
  char          tgttype[16];
  char          impactloc[16];
  char          impactime[16];
};
 
struct engageinfo {
  int           n_weapons;
  int           n_remain;
  int           taskid;
  int           hold;
  float         Pk;
  char          trkstatus[16];
  char          lsite[16];
  char          engagestat[16];
  char          TTI[16];
};

typedef struct {
  int           alarm;
  int           priority;
  long          timenow;
  char          tod[24];
  char          label[32];
  char          arg[32];
} ALARM;

typedef struct {
  int           in_msgs;
  int           ot_msgs;
  int           in_bytes;
  int           ot_bytes;
} IF_STATS;
 
/* --------------------------------------------------------------------- */  
                
FILE            *INFOfp;
FILE            *TIMEfp;
FILE            *DBUGfp;
FILE            *STATfp;
FILE            *SAVEfp;
FILE            *DUMPfp;

char            *ODISTATS;
char            *ODITEST;

char            *PLANTIME;
char            *PLANTEST;
char            *PLANDATA;
char            *PLANDBUG;              // if NOT NULL, print debugging info
char            *SIMTEST;               // if NOT NULL, Sim Cmdr is used
char            *SOCKET;                // if NOT NULL, socket interface is used
char            *DATADIR;
char            *BITMAPDIR;
char            *CLASS;

int             SOCKOUT = FALSE;
int             ALARMLOG = FALSE;
int             PLAYBACK;
int             UpdateSit = FALSE;
int             FrameNo;
int             toprttr;

int             InitialMode = 6;
char            UserName[48];
char            bmctemp[1024];

int             bmcwindW, bmcwindH;

float           start_time;
float           tend;
float           end_time;
long            cpu_start;
char            *inbuff;
char            *outbuff;
char            *buff;
int             insize;
int             outsize;
int             winposX;
int             winposY;
int             winsizW;
int             winsizH;
long            cpu_time;
int             input_file;
int             output_file;
int             tpt, i;
int             standalone;
char            *input_file_name;
char            *output_file_name;
char            *standalone_file;
int             delay;
int             id;
int             defcon = 5;             // The current DefCon level
int             Rposture = 2;           // The current Readiness Posture
char            chdea[40];              // The current Defense Engagement Authority
char            chroe[40];              // The current Rules-of-Engagement
char            chplan[40];             // The current VBattle Plan
char            chmsnobj[40];           // The current Mission Objectives
int             Simcmdr=0;
int             LaunchDetect = FALSE, WeapRelease = FALSE, WeapBusy = FALSE;

int             EventDone = FALSE;
int             alarm_active = FALSE;
int             alarm_count = 0;
int             alarm_name = 0;
int             update_SC = 0;
int             intel_count = 0;
int             msgno = 0;
float           cycle_time;
float           GVT_time;
char            *name, *cmdlevel, *position, *DEA, *ROE, *MsnObj, *BtlPlan;
char            filename[120]; 
char            scline[1280];
char            BmcToolCmd[32][32];

ALARM           alarms[20];

char str61[80] = " Trk     Obj     Msl    Exp    Leth     Tgt     Pre Imp    T to  |";
char str62[80] = " ID     Type     Type   Tgts   Value    Type     Loc.     Impact |";
char str63[80] = "   Trk    Launch    Eng        Current Engagement   ";
char str64[80] = "  Status   Site    Status   #Wpn  TTI   Pk    Remain";

int             ENGwinX, ENGwinY;
int             ENGwinW, ENGwinH;
char            ENGlabel[32];

int             TRKwinX, TRKwinY;
int             TRKwinW, TRKwinH;
char            TRKlabel[32];
int             TrackAlarm = FALSE;

char            BROlabel[32];

int             GBavail, GBwitheld, GBexpended;
int             BMCinited = FALSE;
int             BMCrun = FALSE;
int             DelayFlag = FALSE;
long            DelayTime = 2000L;

int             decision_total = 0;
int             decision_activ = 0;
int             decision_late  = 0;
int             decision_bad   = 0;
//
//   Network socket stuff
//
int             sock;
int             portid;
char            *hostid;
struct sockaddr_in server, client;
socklen_t       server_len, client_len; 
int             msgsock;

int             cicsock = 0;
int             cicport = 8127;
char            cichost[24] = { "procyon" };

int             grafport;
char            *grafhost;

IF_STATS        commstats[5];
char            IfNames[4][16] = { "Host Router", "Socket", "C2ICE", "Monitor" };
//struct sockaddr_in server;
//socklen_t       server_len; 
//
//   Run-time parameter stuff
//
struct tracksinfo tracks;
struct engageinfo engage; 

int             nmissions = 1;          // No. of Mission Objectives
int             missionselect = 1;      // Currently selected Mission Objective
char            **MOnames;
char            **MOstrategy;
char            **MOtactic;
float           *MOpksuccess;
int             *MOmode;
int             *MOwithhold;
int             *MObooster;
         
int             nrules = 1;             // No. of Rules of Engagements
int             ruleselect = 1;         // Currently selected ROE
char            **ROEnames;
double          *ROE_n_lat;
double          *ROE_w_lon;
double          *ROE_s_lat;
double          *ROE_e_lon;
 
int             nplans = 1;             // No. of Battle Plans
int             planselect = 1;         // Currently selected Battle Plan
char            **BPnames;
char            **BPmode;
char            **BPcutoff;
char            **BPkill;
int             *BPoverride;
int             *BPlaunch;
int             *BPsalvo;
float           *BPthreshold;
float           *BPpkcutoff;
int             *BPw_pop;
int             *BPw_mil;
int             *BPw_def;
int             *BPw_nca;
int             *BPw_ind; 

int             gbicount   = 15;        // No. GBIs listed
int             gbiselect  = 0;         // Currently selected GBI
int             gbitotal;               // Total Interceptor count
int             gbiwithheld;            // GBIs withheld for future use
int             gbiexpended = 10;       // GBIs used
int             gbi_per_rv = 2;         // GBIs fired per threat
int             gbiicon;                // GBI display icon id
float           gbipk;                  // Probability of Kill
int             *gbingbis;              // No. of interceptors in farm
int             *gbinhold;              // No. of interceptors withheld
int             *gbiident;              // Farm identifier
float           *gbipkill;              // Probability of Kill
double          *gbilatit;              // Latitude of Farm
double          *gbilongi;              // Longitude of Farm
char            **gbilatNS;             // Latitude is N or S
char            **gbilonEW;             // Longitude is E or W
char            **gbinames;             // Farm names
char            **gbitypes;             // Type of GBI at Farm
char            **gbistatus;            // Status of Farm (R, Y, G) 

int             threatcount  = 6;       // No. of threats listed
int             threatselect = 0;       // Currently selected threat
int             threattotal;            // Total Threat count
int             threattrack;            // No. of threats being tracked
float           threatfirst = 100000.0; // Time of 1st launch
double          tracklat, tracklon;
char            **threatnames;          // Threat names
char            **threattypes;          // Threat types (ie: SCUD, SS18)
char            **threatlsite;          // Launch site
char            **threattsite;          // Target site
float           *threatltime;           // Launch time
float           *threatrdist;           // Random distance
double          *threatllat;            // Launch latitude
double          *threatllon;            // Launch longitude
double          *threattlat;            // Target latitude
double          *threattlon;            // Target longitude
char            **threatlatNS;
char            **threatlonEW;
char            **threattatNS;
char            **threattonEW;

float           Tsave[21][20];
int             assign_matrix[100][2];  // Assignment->threat ID and asset ID
int             trkvisible = FALSE;

int             ICavail, ICdown, ICused;
int             SLavail, SLdown, SLused;
int             ACavail, ACdown, ACused;
int             REavail, REdown, REused;
int             C2avail, C2down, C2used;
int             TRavail, TRdown, TRused;

int             scwindW=500, scwindH=800;
int             defcon_yesno = 1, weapons_yesno = 1;
int             YESNO;
int             MajorMenu, MinorMenu, SumMenuItem, DialogBusy = FALSE;
#ifdef LINKLANL
//
//   SPEEDES interface stuff
//
C_HOST_USER     *host_user;
EXT_LANL_BP_MESS *out_mess;
EXT_LANL_BP_MESS *in_mess;
#endif
//
//   Run-time forms stuff
//
FD_bmc3         *fd_bmc3;
FD_bmctrack     *fd_bmctrack;
FD_bmcengage    *fd_bmcengage;
FD_infodialog   *fd_infodialog;
FD_drawdown     *fd_drawdown;
FD_sitmonitor   *fd_sitmonitor;
FD_bmcalarms    *fd_bmcalarms;
FD_alrmaccept   *fd_alrmaccept;
FD_alrmdefered  *fd_alrmdefered;
FD_alrmcancel   *fd_alrmcancel;
extern FD_sitaware     *fd_sitaware;
FD_bmcstatus    *fd_bmcstatus;
FD_statcomm     *fd_statcomm;
FD_statsubs     *fd_statsubs;
FD_statuser     *fd_statuser;
FD_stathard     *fd_stathard;
FD_statsyst     *fd_statsyst;
FD_statenvs     *fd_statenvs;
FD_statfile     *fd_statfile;
FD_bmcbrowse    *fd_bmcbrowse;
FD_About_Form   *fd_about;
 
Window          mainwinID;
Window          thiswin;
FL_Coord        mainwinX, mainwinY, mainwinH, mainwinW;
FL_OBJECT       *ddgraph, *rdrgraph, *bdrgraph;
FL_OBJECT       *ipipm;
int             timeoutID;

/* --------------------------------------------------------------------- */

void IdleCB(int tid, void *stuff);
int  IdleWP(XEvent *ev, void *data);
int  EraseActiveEntry(char *text);
int  StoreActiveEntry(char *text);
int  FinishUp();
void PutMessage(char *text);

void Net_init(int port, char *host);
void Net_write(char *buf, int bytes);
int  Net_read(char *buf, int bufsize);
int  CIC_init(int port, char *host);
void CIC_write(int sock, char *buf, int bytes,
               struct sockaddr_in *client, socklen_t Lclient);
int  CIC_read(int sock, char *buf, int bufsize,
              struct sockaddr_in *client,  socklen_t *Lclient);
void Net_close(int sockid); 

void X_add_list(int pos, char *listitem);
void X_delete_list(int n_assignments);
void X_track_update();

int             bmcexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);
int             bmcbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);

       void     TRKinit();
       void     TRKshow(int xpos, int ypos, int width, int height, Window winid);
       void     ENGinit();
       void     ENGshow(int xpos, int ypos, int width, int height, Window winid);
extern void     SITinit();
extern void     SITshow(int xpos, int ypos, int width, int height, Window winid);
extern void     SITdraw();
extern void     SitAwareInit(char *fn);
extern void     SitAwareDisplay(int w, int h);

extern void     CBPinit();
extern void     CBPshow(int xpos, int ypos, int width, int height, Window winid, char *fn);
extern void     CBPJcsExec();
extern void     DSBinit();
extern void     DSBshow(int xpos, int ypos, int w, int h, Window winid, INMSG *p);
extern void     FOWinit();
extern void     FOWshow(int xpos, int ypos, int width, int height, Window winid);
extern void     ITELinit();
extern void     ITELshow(int xpos, int ypos, int width, int height, Window winid, int mode, char *fn);
extern void     ITELnext();
extern void     REDIinit();
extern void     REDIshow(int xpos, int ypos, int width, int height, Window winid);
extern void     TGTinit();
extern void     TGTshow(int xpos, int ypos, int width, int height, Window winid);
extern void     TLEinit();
extern void     TLEshow(int xpos, int ypos, int width, int height, Window winid);
extern void     CLUinit();
extern void     CLUshow(int xpos, int ypos, int width, int height, Window winid);
extern void     OPTinit();
extern void     PUPshow(int xpos, int ypos, int width, int height, Window winid, PUPINFO info);
extern void     PUPinit();
extern void     OPTshow(int xpos, int ypos, int width, int height, Window winid);
extern void     SIMinit();
extern void     SIMshow(int xpos, int ypos, int width, int height, Window winid);
extern void     TCPinit();
extern void     TCPshow(int xpos, int ypos, int width, int height, Window winid);

int             SC_message(int opcode, int itemno);
void            SC_initialize();
extern "C" void SC_update_gvt(float newgvt);
extern "C" void SC_update_def(int newdef);
extern "C" void SC_update_rp(int newrp);
extern "C" void SC_update_dea(char *newdea);
extern "C" void SC_update_roe(char *newroe);
extern "C" void SC_update_plan(char *str);
extern "C" void SC_update_node(char *str);
extern "C" void SC_update_hist(char *str);

#ifdef LINKSC
extern "C" void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
extern "C" void decode_header (XDR *xdrs_de, struct header *de, int *nargs);
extern "C" int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
extern "C" int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);
extern "C" void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
extern "C" void decode_header (XDR *xdrs_de, struct header *de, int *nargs);
extern "C" int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
extern "C" int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);
extern "C" void translator (char *messages, char *decisions, unsigned int *xdrsize);
#endif

#ifdef LINKLANL
extern "C" int  gpalssim_();
extern "C" int  settpt_();
extern "C" int  resettpt_();
extern "C" int  getdata_();
extern "C" int  initial_();
extern "C" int  readdat_();
extern "C" int  initthreatplay_();
extern "C" int  threatinit_(int *index, double *misstype, double *Plaunch,
                             double *Pimpact, float *Tlaunch, float *Limpact);
extern "C" int  threatsetup_(double *X1, double *X2, double *X3,
                          double *V1,  double *V2, double *V3,
                          double *time, double *start_time, int *tracks,
                              int *id, int *index);
extern "C" int  getnassign_(int *i);
extern "C" int  getassign_(int *i, int *thrt, int *weap, double *etime);
extern "C" int  getnthreats_();
extern "C" int  getplanline_(int *i, char chline[120]);
           char *process(char *inbuff, int insize, int &outsize,
                         double start_time, double end_time, int ntracks);
           int  process_standalone(char *standalone_file);
#endif
           int  initlanl();

#ifdef SC_THREADS
extern "C" int pthread_create(pthread_t *, const pthread_attr_t *, void * (*)(void *), void *);
void *CIC_Thread(void *inbuf);
#else
void CIC_Thread(void *inbuf);
#endif
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
main(int argc, char *argv[])
{
float           ltime, rdist;
char            *str;
char            xpmfile[128];

time_t          clock;
struct tm       *loctime;

C_PARSER        *jtamv_parser;
C_BASETYPE      *parameters;


   printf("\n\n");
   printf("  --------- General Purpose Decision Aids ---------\n");
#ifdef SC_THREADS
   printf("               (Threaded Version %d.%d)\n", VERSION, RELEASE);
#else
   printf("                   (Version %d.%d)\n", VERSION, RELEASE);
#endif
   printf("                    (%s)", COMPILED);
   printf("\n\n");

   time(&clock);
   loctime = gmtime(&clock);
   str = asctime(loctime);

   PLANTEST  = getenv("PLANTEST");                // BMC input from file if set
   PLANDATA  = getenv("PLANDATA");                // Generate Playback file if set
   PLANTIME  = getenv("PLANTIME");                // Generate timing stats if set
   PLANDBUG  = getenv("PLANDBUG");                // Generate debugging output if set
   SIMTEST   = getenv("SIMTEST");                 // Forward data to Sim Commander if set
   SOCKET    = getenv("SIMSOCK");                 // Send data to SC via SOCKET if set

   if ((BITMAPDIR = getenv("BITMAPDIR")) == NULL) BITMAPDIR = "../BitMaps";

   INFOfp = fopen("GPDAinfo.log", "w+");

   fprintf(INFOfp, "GPDA Information output file for %s\n", str);
   fprintf(INFOfp, "Timing is     %s\n", (PLANTIME==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Debugging is  %s\n", (PLANDBUG==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Statistics is %s\n", (ODISTATS==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Run Mode is   %s\n", (PLANTEST ? "playback" : "realtime"));
   fprintf(INFOfp, "\n");
   fflush(INFOfp);
//
//   Parse the Control & Battle Planner parameter files
//
   jtamv_parser = new C_PARSER("graphics.par");
   parameters = jtamv_parser->get_basetype("display");
   CLASS = parameters->get_string("class");

   if (strcmp(parameters->get_string("input_source"), "Socket") == 0) {
      SOCKOUT = TRUE;
      parameters = jtamv_parser->get_basetype("input");
      grafport  = parameters->get_int("portid");
      grafhost  = parameters->get_string("hostid");
      Net_init(grafport, grafhost); // Initialize the socket
                 // connections
   }

   initlanl();

   fclose(INFOfp);
//
//   Initialize the 'FORMS' system and create all BMC3 forms
//
   fl_initialize(&argc, argv, 0, 0, 0);

   fd_bmc3 = create_form_bmc3();
   fd_bmctrack = create_form_bmctrack();
   fd_bmcengage = create_form_bmcengage();
   fd_infodialog = create_form_infodialog();
   fd_drawdown = create_form_drawdown();
   fd_sitmonitor = create_form_sitmonitor();
   fd_bmcbrowse  = create_form_bmcbrowse();
   fd_about = create_form_About_Form();

   fd_bmcalarms = create_form_bmcalarms();
   fd_alrmaccept = create_form_alrmaccept();
   fd_alrmdefered = create_form_alrmdefered();
   fd_alrmcancel = create_form_alrmcancel();
   fl_addto_tabfolder(fd_bmcalarms->folder, "Accepted",  fd_alrmaccept->alrmaccept);
   fl_addto_tabfolder(fd_bmcalarms->folder, "Deferred",  fd_alrmdefered->alrmdefered);
   fl_addto_tabfolder(fd_bmcalarms->folder, "Cancelled", fd_alrmcancel->alrmcancel);

   fd_bmcstatus = create_form_bmcstatus();
   fd_statcomm  = create_form_statcomm();
   fd_statsubs  = create_form_statsubs();
   fd_statuser  = create_form_statuser();
   fd_stathard  = create_form_stathard();
   fd_statsyst  = create_form_statsyst();
   fd_statenvs  = create_form_statenvs();
   fd_statfile  = create_form_statfile();
   fl_addto_tabfolder(fd_bmcstatus->folder, "Subsystem",  fd_statsubs->statsubs);
   fl_addto_tabfolder(fd_bmcstatus->folder, "Comm I/F",   fd_statcomm->statcomm);
   fl_addto_tabfolder(fd_bmcstatus->folder, "User Info",  fd_statuser->statuser);
   fl_addto_tabfolder(fd_bmcstatus->folder, "Hardware",   fd_stathard->stathard);
   fl_addto_tabfolder(fd_bmcstatus->folder, "Sys Stats",  fd_statsyst->statsyst);
   fl_addto_tabfolder(fd_bmcstatus->folder, "Env Vars",   fd_statenvs->statenvs);
   fl_addto_tabfolder(fd_bmcstatus->folder, "File Info",   fd_statfile->statfile);
//
//   Fill in a bunch of default values
//
   if (argc > 1) {
     if (strcmp(argv[1], "nmd") == 0) InitialMode = 5;
     if (strcmp(argv[1], "sdf") == 0) InitialMode = 5;
     if (strcmp(argv[1], "aoc") == 0) InitialMode = 6;
     if (strcmp(argv[1], "iow") == 0) InitialMode = 7;
     if (strcmp(argv[1], "doh") == 0) InitialMode = 8;
     setenv("GPDADOMAIN", argv[1], 1);
   }
   fl_set_choice(fd_bmc3->bmc_choice, InitialMode);
   strcpy(UserName, "Dennis Ellis");
   //fl_set_input(fd_login->username, UserName);

   fl_set_object_label(fd_bmc3->classification, CLASS);

   fl_set_browser_fontsize (fd_bmc3->bmctop_browser, 12);
   fl_set_browser_fontstyle(fd_bmc3->bmctop_browser, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontsize (fd_bmcbrowse->bmc_browse, 12);
   fl_set_browser_fontstyle(fd_bmcbrowse->bmc_browse, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontsize(fd_stathard->hard_info, 12);
   fl_set_browser_fontstyle(fd_stathard->hard_info, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontsize(fd_statsyst->syst_info, 12);
   fl_set_browser_fontstyle(fd_statsyst->syst_info, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontsize(fd_statuser->user_info, 12);
   fl_set_browser_fontstyle(fd_statuser->user_info, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontsize(fd_statfile->file_info, 12);
   fl_set_browser_fontstyle(fd_statfile->file_info, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontsize(fd_bmcengage->engage_browser, 10);
   fl_set_browser_fontstyle(fd_bmcengage->engage_browser, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontsize(fd_bmctrack->trk_browser, 10);
   fl_set_browser_fontstyle(fd_bmctrack->trk_browser, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontstyle(fd_statcomm->ifstats_list, FL_FIXED_STYLE|FL_BOLD_STYLE);
 
   fl_set_object_lstyle(fd_infodialog->info_text, FL_FIXEDBOLD_STYLE);

   fl_set_object_lstyle(fd_bmctrack->trk_header, FL_FIXEDBOLD_STYLE);
   fl_set_object_lstyle(fd_bmcengage->engage_header, FL_FIXEDBOLD_STYLE);
   //fl_set_object_lsize(fd_bmctrack->trk_header, FL_MEDIUM_SIZE);

   fl_add_canvas_handler(fd_bmc3->bmc_canvas, Expose,      bmcexposeCB, 0);
   fl_add_canvas_handler(fd_bmc3->bmc_canvas, ButtonPress, bmcbuttonCB, 0);
   //fl_hide_object(fd_bmc3->bmc_canvas);

   wpnchartCB(NULL, 0);

   sprintf(scline, "%d", defcon);
   fl_set_input(fd_bmc3->defcon_val, scline);
   sprintf(scline, "%d", Rposture);
   fl_set_input(fd_bmc3->rp_val, scline);
   fl_set_input(fd_bmc3->dea_val, chdea);
   fl_set_object_label(fd_bmc3->roe_val, chroe);
//
//   If Situation Monitor wanted, initialize it
//
   if (SIMTEST != NULL) SC_initialize();
//
//   If CIC interface wanted, initialize it
//
#ifdef LINKCIC
   CIC_init(cicport, cichost);
#endif
//
//   If Battle Manager wanted, initialize it
//
#ifdef LINKLANL
   DBUGfp = fopen("plandebug.out", "w");
   id = 10;

   if (PLANTEST) {
      SAVEfp = fopen("lanlinput.dat", "r");
      if (!SAVEfp) fprintf(stderr, "Input file not opened\n");
      tend = 63000.0;
      fl_set_idle_delta(2000L);
   } else {
      //
      //...... Create the Host User
      //
      if (input_file) {
         host_user = new C_HOST_USER(input_file_name);
      } else {
         host_user = new C_HOST_USER();
      }
      if (output_file && !input_file) {
         host_user->set_output_file(output_file_name);
      }
      //
      //...... Initialize the Host User for simulation interface
      //
      id = host_user->getid(name);
      fprintf(stderr, "Host Router name %s and id %d\n", name, id);
      out_mess = new EXT_LANL_BP_MESS();
      out_mess->time_tag = start_time;
      out_mess->EM_done_time = cycle_time;
   }

   if (PLANDATA) {
      fprintf(stderr, "Opening PLAYBACK play for writing\n");
      DUMPfp = fopen("lanldump.dat", "w");
      if (!DUMPfp) fprintf(stderr, "Output file not opened\n");
   }
   //
   //...... Initialize the Battle Manager (fortran code)
   //      
   initial_();              fprintf(DBUGfp, "LANL_BP: initial_()\n");
   readdat_();              fprintf(DBUGfp, "LANL_BP: read_data_()\n");

   if (tpt) {
     settpt_();
     getdata_();            fprintf(DBUGfp, "LANL_BP: get_data_()\n");
   } else {
     resettpt_();
   } 
#endif 
//
//   Initialization complete, set the main work loop
//
   BMCinited = TRUE;

   SC_update_gvt(0.0);

   fl_set_idle_callback(IdleWP, 0);
//
// Popup the 'BMC3' form and get the show going
//
   loginCB(NULL, (long)10);

   fl_do_forms();
}

void IdleCB(int tid, void *stuff)
{
   DelayFlag = FALSE;
}

int IdleWP(XEvent *ev, void *data)
{
int             ntracks;
int             incount;
int             Thread;
char            *inbuf, *outbuf; 
char            xdrbuf[2000];
socklen_t       Lclient;
struct sockaddr_in client;

   if (SIMTEST) {                         // Flush outstanding SC messages
      if (update_SC) {
         SC_message(update_SC, 0);
         update_SC = 0;
      }
   }

#ifdef LINKCIC
   // Check for a message from CIC
   incount = CIC_read(cicsock, xdrbuf, 2000, &client, &Lclient);
 
   if (incount > 0) {
      //fprintf(stderr, "Returned from CIC-Read with length %d\n", incount); 
      //
      //  A message has arrived
      //
      commstats[IF_SOCK].in_msgs++;
      commstats[IF_SOCK].in_bytes = commstats[IF_SOCK].in_bytes + incount;
      //
      inbuf = (char *)malloc(sizeof(Lclient)+sizeof(client)+incount);
      memcpy(inbuf, &Lclient, sizeof(Lclient));
      memcpy(inbuf+sizeof(Lclient), &client, sizeof(client));
      memcpy(inbuf+sizeof(Lclient)+sizeof(client), xdrbuf, incount);
      //
      //  Start a CIC processing thread
      //
      Thread = 1;
#ifdef SC_THREADS
      Thread = pthread_create(&threadid, NULL, CIC_Thread, (void *)inbuf);
#endif
      if (Thread != 0) {                // If Thread not created, just do it!
          CIC_Thread(xdrbuf);
      }
   }
#endif

   if (UpdateSit) SITdraw();

   if (!BMCrun) return(0);

#ifdef LINKLANL
   if (DelayFlag) return(0);

   if (PLANTEST) {
      //
      //   We are reading simulation data from playback file
      //
      fscanf(SAVEfp, "%5d %f %f\n", &msgno, &start_time, &end_time);
      fscanf(SAVEfp, "%5d\n", &ntracks);
      inbuff  = NULL;
      insize  = ntracks;
      outsize = 0;
      if (ntracks < 1) DelayTime = 200L;
         else DelayTime = 2000L;
      timeoutID = fl_add_timeout(DelayTime, IdleCB, NULL); DelayFlag = TRUE;
   } else {
      //
      //   We are reading simulation data from SPEEDES
      //
      out_mess->objid = id;
      in_mess = (EXT_LANL_BP_MESS *)host_user->blocking_module(out_mess);
      msgno = msgno + 1;
      delete out_mess;

      start_time = in_mess->time_tag;
      end_time = in_mess->EM_done_time;
      ntracks = 0;

      inbuff = (char *)in_mess;
      inbuff += sizeof(EXT_LANL_BP_MESS);
      insize = in_mess->data_bytes -
               (sizeof(EXT_LANL_BP_MESS) - sizeof(C_EM_HEADER));
   }

   if (PLANDATA != NULL)
      fprintf(DUMPfp, "%5d %f %f\n", msgno, start_time, end_time);

   if (PLANDBUG)
      fprintf(stderr, "LANL BP input: %d bytes starting...\n", insize);

   //...... update statistics
   commstats[IF_HOST].in_msgs++;
   commstats[IF_HOST].in_bytes = commstats[IF_HOST].in_bytes + insize;
/*
   //...... if it's the 1st time there is track data, sound alarm
   if ((insize > 0) && (TrackAlarm == FALSE))
     Alarm(ALARM_TRACK, "Track Report", PRIORITY_HI, "*");
*/
   //...... process the input data by LANL BP and return an output buffer
   SC_update_gvt(start_time); 
   outbuff = process(inbuff, insize, outsize, start_time, end_time, ntracks);

   if (!PLANTEST) {    
      delete in_mess;
      //...... send the answer back to SPEEDES next time we visit this routine
      out_mess = (EXT_LANL_BP_MESS *)(new char[sizeof(EXT_LANL_BP_MESS)+outsize]);
      out_mess->init(outsize);
      out_mess->EM_done_time = cycle_time;
      out_mess->time_tag = end_time;
      buff = (char *)out_mess;
      buff += sizeof(EXT_LANL_BP_MESS);
      memcpy(buff, outbuff, outsize);
      delete outbuff;
      //
      commstats[IF_HOST].ot_msgs++;
      commstats[IF_HOST].ot_bytes = commstats[IF_HOST].ot_bytes + outsize;
   }

   if (PLANDBUG)
      fprintf(stderr,"... Done at time %f. Sending %d bytes back\n\n",end_time, outsize);

   if (outsize == 0) X_delete_list(0);

   if ((end_time >= tend)) {
      BMCrun = FALSE;
      if (PLANTEST) fclose(SAVEfp);
      if (PLANDATA != NULL) fclose(DUMPfp);
   }

#endif
   return (0);
}

#ifdef LINKLANL

char *process(char *inbuff, int insize, int &outsize,
              double start_time, double end_time, int ntracks)
{
C_KEPLER         kepler;
C_LANL_BP_INPUT  *lanl_bp_input;
C_LANL_BP_OUTPUT *lanl_bp_output;
C_LANL_THREAT    *lanl_threat;
float            fX[3];
float            fV[3];
double            X[3];
double            V[3];
int              n_tracks;
int              i, ti, idtemp, j, icity, nthreats;
double           timetemp;
double           *XX;
double           *VV;
int              n_assignments, index, thrt, weap;
double           etime;
char             *outbuff;
static char      chtemp[16];
static char      chline[120];
char             chout[128];

  char            xdrbuf[2000];
  char            buf[180];
  int             xferbytes; 
  int             drop, stn, incolor;
  int             trkid_S = 0, trkid_A = 0, trkid_E = 0;
  int             xdrsize;
  float           orient, major, minor;
  float           tadilj_time;
  char            buffer[180];
  char            chmsgno[8];
  char            chmark[40];
  char            chmsgtyp;


   if (PLANDBUG)
      fprintf(DBUGfp, "Starting LANL_BP at time %f, and ending at time %f\n\n",
              start_time, end_time);

   kepler.set_ECI(1);                             // Initialize the kepler object to ECI

   if (PLANTEST) {
      n_tracks = ntracks;
   } else {
      lanl_bp_input = (C_LANL_BP_INPUT *)inbuff;  // Get the input buffer
      n_tracks = insize / sizeof(C_LANL_BP_INPUT);   // Get the number of tracks
   }

   if (PLANDATA)
      fprintf(DUMPfp, "%5d\n", n_tracks);

   //X_update_gvt((int)start_time);               // Update GVT display

   if (PLANDBUG)
      fprintf(DBUGfp, "# tracks is %d\n", n_tracks);

   if (n_tracks > 0) {                            // At least one threat is being tracked
      threattrack = n_tracks;                     // Save the # of threats being tracked
      initthreatplay_();                          // Initialize threat play
/*
 *   Loop over the number of tracks setting stuff for LANL
 */
      for (i=0; i<n_tracks; i++) {
         ti = i + 1;

         if (PLANTEST) {
	    fscanf(SAVEfp, " %d %lf %lf %lf %lf %lf %lf %lf\n",
	           &idtemp, &X[0], &X[1], &X[2], &V[0], &V[1], &V[2], &timetemp);
         } else {
            XX = lanl_bp_input[i].get_Xtrack();
	    X[0] = XX[0]; X[1] = XX[1]; X[2] = XX[2];
            VV = lanl_bp_input[i].get_Vtrack();
	    V[0] = VV[0]; V[1] = VV[1]; V[2] = VV[2];
            idtemp = lanl_bp_input[i].get_id();
            timetemp = lanl_bp_input[i].get_time();
         }

	 if (PLANDATA)
	    fprintf(DUMPfp, " %d %lf %lf %lf %lf %lf %lf %lf\n",
	           idtemp, X[0], X[1], X[2], V[0], V[1], V[2], timetemp);

         for (j=0; j<3; j++) {
           fX[j] = X[j]*1000.0;
           fV[j] = V[j]*1000.0;
         }

	 if (SOCKOUT) {                   // Forward to FOP Visualizer
           stn = 13;
           strcpy(chmark, "RV"); incolor = 6;
           orient = 90.0; major = 50.0; minor = 30.0;
           sprintf(buffer, "%f %c %s %d %d %d %d %f %f %f %s %d %f %f %f",
                   end_time, 'J', "3.6",
                   trkid_S, trkid_A, idtemp, stn, fX[0], fX[1], fX[2],
                   chmark, incolor, orient, major, minor);
           //n_records = n_records+1;  
           sprintf(xdrbuf, "%8s %8s %d %d %s\n", "Patriot ", "JTAMV   ", 0, 0, buffer);
           xdrsize = strlen(xdrbuf);
           Net_write(xdrbuf, xdrsize);
	 }

         lanl_threat = new C_LANL_THREAT(fX, fV, timetemp, idtemp);

         if (PLANDBUG) lanl_threat->print();     //...... print threat stuff

         tracklat = lanl_threat->get_lat();
         tracklon = lanl_threat->get_lon();
		 fprintf(stderr, " Threat lat = %f, lon = %f\n", tracklat, tracklon);
         Tsave[i][ 0] = X[0]; Tsave[i][ 1] = X[1]; Tsave[i][ 2] = X[2];  // Current Position
         Tsave[i][ 3] = V[0]; Tsave[i][ 4] = V[1]; Tsave[i][ 5] = V[2];  // Current Velocity
         Tsave[i][ 6] = sqrt(X[0]*X[0]+X[1]*X[1]+X[2]*X[2])-RE;          // Current Altitude
         Tsave[i][ 7] = sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]);             // Current Speed
         Tsave[i][ 8] = tracklon; Tsave[i][ 9] = tracklat;               // Impact Position
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

         //X_update_gbi();   update pie chart
         X_track_update();
         threatsetup_(&X[0], &X[1], &X[2],
                      &V[0], &V[1], &V[2],
                      &timetemp, &start_time, &n_tracks, &idtemp, &ti);
         delete lanl_threat;
      }

      gpalssim_();                                // Call BP now that it has the threats
      getnassign_ (&n_assignments);               // Get the plan out of the BP

      lanl_bp_output = new C_LANL_BP_OUTPUT[n_assignments];
      outbuff = (char *)lanl_bp_output;
      outsize = n_assignments * sizeof(C_LANL_BP_OUTPUT);
      X_delete_list(n_assignments);

      for (i=0; i<n_assignments; i++) {
        index = i+1;                              // FORTRAN starts at 1 instead of 0
        getassign_(&index, &thrt, &weap, &etime);
        assign_matrix[i][0] = thrt;               // Save Threat ID for this assignment
        assign_matrix[i][1] = weap;               // Save GBI Farm ID for this assignment
        getplanline_(&index, chline);             // Get the BMC3 display info
        strnsub(chline, '\0', ' ', 118);
        chline[119] = '\0';
        //strcpy(chout, "@C1");
        //strcat(chout, chline);
        X_add_list(index, (char *)chline);        // Display it

        if (SIMTEST) {
           sscanf(chline, "%s %s %s %d %f %s %s %s %s %s %s %s %d %s %f %d",
                tracks.trackid,    tracks.objtype,    tracks.msltype,
               &tracks.targets,   &tracks.lethality,  tracks.tgttype,
                tracks.impactloc,  tracks.impactime,  chtemp,
                engage.trkstatus,  engage.lsite,      engage.engagestat,
               &engage.n_weapons,  engage.TTI,       &engage.Pk,
               &engage.n_remain);
           //tracks.latitude = threattlat[i];     // Get the threat launch site latitude
           //tracks.longitude = threattlon[i];    // Get the threat launch site longitude
           //tracks.ltime = threatltime[i];       // Get the threat launch time
           tracks.itime = Tsave[i][17];           // Get the impact time
           engage.taskid = weap;                  // Get farm id
           SC_message(OP_MSL_TRK, 0);             // Send the SC the track status
           SC_message(OP_TRK_ENGMT, 0);           // Send the SC the gbi engagement status
        }

        if (PLANDBUG)
           fprintf (DBUGfp, "returned %d, %d, %f\n", thrt, weap, etime);

        lanl_bp_output[i].set_threat_id(thrt);
        lanl_bp_output[i].set_asset_id(weap);
        lanl_bp_output[i].set_launch_time(etime);
        if (etime == 0.0) lanl_bp_output[i].set_asset_id(-1);
      }
/*
      if (WeapRelease == FALSE) {
         outsize = 0;
         outbuff = NULL;
         printf("Weapons NOT released, no plan returned\n");
      }
*/
   } else {
      outsize = 0;
      outbuff = NULL;
   }
   /*
   if (SOCKOUT) {
      xferbytes = Net_read((char *)buf, 200);
   }
   */  
   return outbuff;
}
#endif

int initlanl()
{
C_PARSER        *lanl_bp_parser, *jtamv_parser;
C_PARSER        *threat_parser = NULL;

C_BASETYPE      *parameters;
C_BASETYPE      *mission_obj, *mission, *rules_of_engage, *rule, *battle_plan, *plan; 
C_BASETYPE      *missile_names, *missile_name, *basetype;

FILE            *fp;
double          lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec;
double          latitude, longitude;
float           ltime, rdist;

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
/*
 *      Parse the Battle Planner parameter file
 *      ---------------------------------------
*/
   lanl_bp_parser = new C_PARSER("planner.par");
   parameters = lanl_bp_parser->get_basetype("parameters");
   name = parameters->get_string("command_center");
   cmdlevel = parameters->get_string("command_level");
   position = parameters->get_string("position");
   defcon = parameters->get_int("defcon");
   Rposture = parameters->get_int("readiness");

   DEA = parameters->get_string("dea"); strcpy(chdea, DEA);
   ROE = parameters->get_string("roe"); strcpy(chroe, ROE);
   MsnObj = parameters->get_string("mission"); strcpy(chmsnobj, MsnObj);
   BtlPlan = parameters->get_string("plan"); strcpy(chplan, BtlPlan);
 
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
     sprintf(scline, "%s.txt", MOnames[i]);
     strsub(scline, ' ', '_');
     fp = fopen(scline, "w");
     fprintf (fp, "\n\n %s%s\n %s%s\n %s%s\n %s%f\n %s%d\n %s%d\n %s%d\n ",
	      "Name of Mission Objective: ", MOnames[i],
	      "Strategy:                  ", MOstrategy[i],
	      "Tactic:                    ", MOtactic[i],
	      "Probability of Success:    ", MOpksuccess[i],
	      "Mode:                      ", MOmode[i],
	      "Interceptors Withheld:     ", MOwithhold[i],
	      "Additional Boosters:       ", MObooster[i] );
     fclose(fp);
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
     BPnames[i]     = plan->get_name();
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
     sprintf(scline, "%s.txt", BPnames[i]);
     strsub(scline, ' ', '_');
     fp = fopen(scline, "w");
     fprintf (fp, "\n\n %s%s\n %s%s\n %s%s\n %s%s\n %s%s\n %s%d\n %s%d\n %s%f\n %s%f\n %s%d\n %s%d\n %s%d\n %s%d\n %s%d\n",
                "Name of Battle Plan:       ", BPnames[i],
                "Mode:                      ", BPmode[i],
                "Cutoff:                    ", BPcutoff[i],
                "Kill Criteria:             ", BPkill[i],
                "Override Salvo:            ", (BPoverride[i]==0 ? "True" : "False"),
                "Launch Mode:               ", BPlaunch[i],
                "Interceptor Salvo Count:   ", BPsalvo[i],
                "RV Threshold:              ", BPthreshold[i],
                "Pk Cutoff:                 ", BPpkcutoff[i],
                "Population Weighting:      ", BPw_pop[i],
                "Military Weighting:        ", BPw_mil[i],
                "Self Defense Weighting:    ", BPw_def[i],
                "NC Authority Weighting:    ", BPw_nca[i],
                "Industrial Weighting:      ", BPw_ind[i] );
     fclose(fp);
     plan = battle_plan->get_next_type();
   }
   planselect = nplans-1;
   strcpy(chplan, BPnames[planselect]);
   gbi_per_rv = BPsalvo[planselect];

   fprintf(INFOfp, "Initial Command Center ........... %s\n", name);
   fprintf(INFOfp, "Initial Command Level ............ %s\n", cmdlevel);
   fprintf(INFOfp, "Initial Command Position ......... %s\n", position);
   fprintf(INFOfp, "Initial DEA ...................... %s\n", chdea);
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
     strcpy(utypes.chtype, "SS18   ");
     strcpy(ulsite.chtype, "Arctic ");
     //utsite.chtype = "Chicago";
     strncpy(utsite.chtype, threattsite[i], 8);
#ifdef LINKLANL
     threatinit_(&i, &utypes.types, &ulsite.types, &utsite.types, &ltime, &rdist);
#endif
     missile_name = missile_names->get_next_type();
   }
   threattrack = 0;

   return(0); 
}

#ifdef LINKCIC
#ifdef SC_THREADS
void *
#else
void
#endif
CIC_Thread(void *inbuf)
{
FILE            *fp;
int             msg_type = 2;
int             ret_ack  = 0;
int             bytes;
int             alarm_code;
int             alarm_prior;
unsigned int    xdrsize = 2000;
char            *outbuf;
char            *buffer;
char            *p;
char            chsrc[12];
char            chdst[12];
char            alarm_text[32];
char            filename[64];
char            chevid[1024];
int             Lclient;
struct sockaddr_in client;
//
//   Input buffer:
//
//      Alpha string   - 1 to 8 chars   Source ID
//      Alpha string   - 1 to 8 chars   Destination ID
//      Numeric string - 1 to 4 chars   Message type (see below)
//      Numeric string - 1 to 8 chars   Message length (in bytes)
//      Numeric string - 0 or 1         if = 1, return ack to sender
//      Alpha string   - 1 to ? chars   Message (NULL terminated)
// 
   //fprintf(stderr, "New Thread %d Started...\n", pthread_self() );

   buffer = (char *)inbuf+sizeof(Lclient)+sizeof(client);
   //fprintf(stderr, "[%d] [%s]\n", strlen(buffer), buffer);
   //
   //   Extract message header and get message body
   //
   p = strtok(buffer, " ");                       // Get source
   p = strtok(NULL, " ");                         // Get destination
   p = strtok(NULL, " ");                         // Get mission area
   p = strtok(NULL, " ");                         // Get message type
   msg_type = atoi(p);
   p = strtok(NULL, " ");                         // Get byte count
   bytes = atoi(p);
   p = strtok(NULL, " ");                         // Get ACK flag
   ret_ack = atoi(p);
   p = strtok(NULL, "\n");                        // Point to RQ fields
   strcpy(chevid, p);                             // Put message where we can operate on it
   //
   //   Process message depending on type
   //
   alarm_code = ALARM_NONE;
   //
   switch (msg_type) {
   case M_NUDET: // NuDet
     alarm_code  = ALARM_POTEVENT;
     alarm_prior = PRIORITY_HI;
     strcpy(alarm_text, "Potential Event");
     sprintf(filename, "NuDet-%d-%d.tmp", getpid(), commstats[IF_SOCK].in_msgs);
     fp = fopen(filename, "w");
     fprintf(fp, "%s\n", chevid);
     fclose(fp);
     break;

   case M_EVID: // Intel data
     alarm_code  = ALARM_INTEL;
     alarm_prior = PRIORITY_MED;
     strcpy(alarm_text, "Evidence Report");
     sprintf(filename, "NewEvid-%d-%d.tmp", getpid(), commstats[IF_SOCK].in_msgs);
     fp = fopen(filename, "w");
     fprintf(fp, "%s\n", chevid);
     fclose(fp);
     break;

   case M_LIMIT: // Limit
     alarm_code  = ALARM_LIMIT;
     alarm_prior = PRIORITY_MED;
     strcpy(alarm_text, "Limit Exceeded");
     sprintf(filename, chevid);
     break;

   case M_ASSESS: // Do assessment
     alarm_code  = ALARM_POTEVENT;
     alarm_prior = PRIORITY_MED;
     strcpy(alarm_text, "Assessment Request");
     break;

   case M_STATS: // Box Scores
     /*
     sscanf(chevid, "%d %s %s %s %s %s %s %d %d %d %d %d %d %d %d",
            &defcon,
	    chstat[0], chstat[1], chstat[2],
	    chstat[3], chstat[4], chstat[5],
	    &field[0], &field[1], &field[2], &field[3],
	    &field[4], &field[5], &field[6], &field[7]);
     //
     for (i=0; i<8; i++) {
       if (fields[i] > -1) {
         sprintf(chtemp, "%d", field[i]);
	 fl_set_input(fd_bmc3->summ_value[i], chtemp);
       }
     }
     if (defcon > -1)
       SC_update_def(defcon);
     else if (strcmp(curr_event->action, "RP") == 0)
       SC_update_rp(atoi(curr_event->argument));
     else if (strcmp(curr_event->action, "ROE") == 0)
       SC_update_roe(curr_event->argument);
     else if (strcmp(curr_event->action, "DEA") == 0)
       SC_update_dea(curr_event->argument);
     else if (strcmp(curr_event->action, "PLAN") == 0)
       SC_update_plan(curr_event->argument);
     */
     break;

   default:
     break;
   }
#ifdef SC_THREADS
   //pthread_mutex_lock(&write_lock);
#endif
   //
   //  Send back an answer if requested
   //
   if (ret_ack != 0) {
     xdrsize = 4;
     outbuf = (char *)malloc(xdrsize);
     memcpy(&Lclient, (char *)inbuf, sizeof(Lclient));
     memcpy(&client, (char *)inbuf+sizeof(Lclient), sizeof(client));
     sprintf(outbuf, "ACK");
     CIC_write(cicsock, outbuf, xdrsize, &client, Lclient);
     commstats[IF_SOCK].ot_msgs++;
     commstats[IF_SOCK].ot_bytes = commstats[IF_SOCK].ot_bytes + xdrsize;
   }
   //
   //   Notify user of message arrival
   //
   if (alarm_code != ALARM_NONE)
     Alarm(alarm_code, alarm_text, alarm_prior, filename);
#ifdef SC_THREADS
   //pthread_mutex_unlock(&write_lock);
#endif
   //
   //  Clean up allocated buffers
   //
   if (ret_ack != 0) free(outbuf);
   free(inbuf);

   //fprintf(stderr, "Thread %d exitting\n", pthread_self() );

   return(NULL); 
}
#endif
/*                                                                       */
/* --------------------------------------------------------------------- */
/*                L O C A L   S C R E E N   R O U T I N E S              */
/* --------------------------------------------------------------------- */
/*                                                                        */
void TRKinit()
{
   sprintf(scline, "%s%s\n%s%s", str61,str63, str62,str64);
   fl_set_object_label(fd_bmctrack->trk_header, scline);
}

int TRKclose(FL_FORM *form, void *data)
{
   TRKexitCB(NULL, 0);
   return(0);
}

void TRKshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
Window          winid;

   strcpy(TRKlabel, "Tracks");

   if(!fl_form_is_visible(fd_bmctrack->bmctrack) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_bmctrack->bmctrack,
                                     FL_PLACE_POSITION,FL_TRANSIENT, TRKlabel);
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_get_wingeometry(winid, &TRKwinX, &TRKwinY, &TRKwinW, &TRKwinH);
      fl_show_form_window(fd_bmctrack->bmctrack);
      fl_set_form_atclose(fd_bmctrack->bmctrack, TRKclose, 0);
      StoreActiveEntry(TRKlabel);

      TRKinit();
   }

   return;
}

void TRKexitCB(FL_OBJECT *object, long item_no)
{
   fl_hide_form(fd_bmctrack->bmctrack);
   EraseActiveEntry(TRKlabel);

   return;
}

void TRKnoneCB(FL_OBJECT *object, long item_no)
{

}

/* --------------------------------------------------------------------- */

void ENGinit()
{
char            chformat[64];
char str11[180] = "              Wpns        Wpns       Wpns       Wpns       Wpns      Wpns    Positive   Negative     Exp       Unexp";
char str12[180] = " System     Available   Withheld   Expended   Remaining   Planned   Failed     KAs        KAs       No KAs     No KAs";

char            chasset[12];

   strcpy(chformat,"%s%-8s %9d %9d %10d %10d %10d %9d %9d %10d %10d %10d");

   sprintf(scline, "%s\n%s", str11,str12);
   fl_set_object_label(fd_bmcengage->engage_header, scline);
   fl_clear_browser(fd_bmcengage->engage_browser);
   //
   //   Show defensive asset status
   //
   strcpy(chasset, "Alaska");
   sprintf(scline, chformat, "@C1", "Fighter", 20, 2, 5, 13, 5, 1, 2, 2, 1, 1);
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   sprintf(scline, chformat, "@C1", "PAC3",    20, 2, 5, 13, 5, 1, 2, 2, 1, 1);
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   sprintf(scline, chformat, "@C1", "Thaad",   20, 2, 5, 13, 5, 1, 2, 2, 1, 1);
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   sprintf(scline, chformat, "@C1", "Aegis",   20, 2, 5, 13, 5, 1, 2, 2, 0, 0);
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   //
   //   Show offensive asset status
   //
   sprintf(scline, " ");
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   sprintf(scline, chformat, "@C4", "Fighter", 20, 2, 5, 13, 5, 1, 2, 2, 1, 1);
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   sprintf(scline, chformat, "@C4", "Bombers", 20, 2, 5, 13, 5, 1, 2, 2, 1, 1);
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   sprintf(scline, chformat, "@C4", "SLBMs",   20, 2, 5, 13, 5, 1, 2, 2, 1, 1);
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   sprintf(scline, chformat, "@C4", "Aegis",   20, 2, 5, 13, 5, 1, 2, 2, 1, 1);
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   //
   //   Show total asset status
   //
   sprintf(scline, " ");
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
   sprintf(scline, chformat, "@C0", "TOTAL", 160, 16, 40, 104, 40, 8, 16, 16, 8, 8);
   fl_addto_browser(fd_bmcengage->engage_browser, scline);
}

int ENGclose(FL_FORM *form, void *data)
{
   ENGexitCB(NULL, 0);
   return(0);
}

void ENGshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
Window          winid;

   strcpy(ENGlabel, "Engagement Summary");

   if(!fl_form_is_visible(fd_bmcengage->bmcengage) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_bmcengage->bmcengage,
                                     FL_PLACE_POSITION,FL_TRANSIENT, ENGlabel);
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_get_wingeometry(winid, &ENGwinX, &ENGwinY, &ENGwinW, &ENGwinH);
      fl_show_form_window(fd_bmcengage->bmcengage);
      fl_set_form_atclose(fd_bmcengage->bmcengage, ENGclose, 0);
      StoreActiveEntry(ENGlabel);
   }

   ENGinit();

   return;
}

void ENGexitCB(FL_OBJECT *object, long item_no)
{
   fl_hide_form(fd_bmcengage->bmcengage);
   EraseActiveEntry(ENGlabel);

   return;
}

void ENGnoneCB(FL_OBJECT *object, long item_no)
{

}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              N E T W O R K   S O C K E T   R O U T I N E S            */
/* --------------------------------------------------------------------- */
/*                                                                       */
void Net_init(int portid, char *host)
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

void Net_write(char *buf, int bytes)
{
    server_len = sizeof(server);
    if (sendto(sock, buf, bytes, 0, (struct sockaddr *)&server, server_len) < 0) {
      perror("writing on stream socket"); close(sock); exit(1);
    }
}
 
int Net_read(char *buf, int bufsize)
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
 
void Net_close()
{
   close(sock);
}

/* --------------------------------------------------------------------- */
 
int CIC_init(int portid, char *host)
{
int             flag = 1;
 
   cicsock = socket(AF_INET, SOCK_DGRAM, 0);           /* Create socket */
   if (cicsock < 0) {
     perror("opening CIC stream socket"); exit(10);
   }
 
   flag = 1;
   if (ioctl(cicsock, FIONBIO, &flag) < 0) {
     perror("Server: ioctl "); exit(40);
   }
   /* 
   struct hostent *hp = gethostbyname(host);
   if (hp == NULL) {
     fprintf(stderr, "%s: unknown host", host); exit(20);
   }
   */
   memset((char *)&server, sizeof(server), 0);
   server.sin_family      = AF_INET;
   server.sin_addr.s_addr = htonl(INADDR_ANY);
   server.sin_port        = htons((short)portid);
   //memmove(&server.sin_addr, hp->h_addr, hp->h_length);
 
   if (bind(cicsock, (sockaddr*) &server, sizeof(server)) < 0) {
     perror("binding CIC stream socket"); exit(10);
   }
   /* 
   if (getsockname(cicsock, (struct sockaddr *)&server, &server_len) < 0) {
     perror("Server getsocketname "); exit(30);
    }
   */
   return(0);
}

void CIC_write(int sock, char *buf, int bytes,
               struct sockaddr_in *client, socklen_t Lclient)
{
int             i;

   if (sock == 0) return;
 
   if (sendto(sock, buf, bytes, 0, (struct sockaddr *)client, Lclient) < 0) {
     perror("writing on CIC stream socket"); close(sock); exit(10);
   }
}
 
int CIC_read(int sock, char *buf, int bufsize,
             struct sockaddr_in *client,  socklen_t *Lclient)
{
static int      cicwait = FALSE;
int             rval;
socklen_t       clientl;

   if (sock == 0) return (0);
 
   if (!cicwait) {
     memset(buf, 0, bufsize);
     cout << "Waiting for client to connect..." << endl;
     //fl_set_object_label(fd_simcmdr->status_text, "Waiting...");
     cicwait = TRUE;
   }
 
   clientl = sizeof(struct sockaddr_in);
   rval = recvfrom(sock, buf, bufsize, 0, (struct sockaddr *)&client, &clientl);
   if (rval < 0) {
    if (errno == EWOULDBLOCK) {
          return(rval);
        } else { perror("reading CIC stream message"); close(sock); exit(20); }
   }
 
   //*Lclient = clientl;
   cicwait = FALSE;

   if (rval == 0) {
     cout << "CIC has exited." << endl;
   }

   return(rval);
}

void Net_close(int sockid)
{
   if (sockid) close(sockid);
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*          S U P P O R T   R O U T I N E S   S T A R T   H E R E        */
/* --------------------------------------------------------------------- */
/*                                                                       */
void PutMessage(char *msg)
{
   fl_set_input(fd_bmc3->bmc_message, msg);

   return;
}

int FinishUp()
{
//
//   Don't need to do anythning right now...
//
   return(0);
}

int EraseActiveEntry(char *text)
{
int             i, total_lines;

   total_lines = fl_get_browser_maxline(fd_bmc3->act_windows);
   for (i=1; i<=total_lines; i++)
     if (strcmp(text, fl_get_browser_line(fd_bmc3->act_windows, i)) == 0) {
       fl_delete_browser_line(fd_bmc3->act_windows, i);
       break;
     }

   return (i);
}

int StoreActiveEntry(char *text)
{
int             i, total_lines;

   fl_add_browser_line(fd_bmc3->act_windows, text);
   i = fl_get_browser_maxline(fd_bmc3->act_windows);

   return (i);
}

void Alarm(int alarm, char *label, int priority, char *arg)
{
int             item = 0;
long            TimeNow, Microsec;
char            chline[64];
FL_COLOR        color;
time_t          clock;
struct tm       *loctime;
char            *str;

   alarm_name = alarm;

   alarm_count = alarm_count + 1;

   alarms[alarm_count].alarm = alarm;
   alarms[alarm_count].priority = priority;
   strcpy(alarms[alarm_count].label, label);
   strcpy(alarms[alarm_count].arg, arg);

   fl_gettime(&alarms[alarm_count].timenow, &Microsec);
   time(&clock);
   loctime = localtime(&clock);
   strftime(alarms[alarm_count].tod, 24, "%D-%T", loctime);

   switch (priority) {
      case PRIORITY_CRIT:
	 color = FL_RED;
         alarm_active = TRUE;
         break;
      case PRIORITY_HI:
	 color = FL_YELLOW;
         alarm_active = TRUE;
         break;
      case PRIORITY_MED:
	 color = FL_GREEN;
         alarm_active = TRUE;
         break;
      case PRIORITY_LOW:
	 color = FL_BLUE;
         alarm_active = TRUE;
         break;
      case PRIORITY_OFF:
	 color = FL_INACTIVE_COL;
         alarm_active = FALSE;
         break;
      default:
         break;
   }

   fl_set_object_color(fd_bmc3->alarm_button, color, FL_INACTIVE_COL);
   fl_set_object_label(fd_bmc3->alarm_text, label);
   fl_activate_object(fd_bmc3->alarm_button);

   fl_ringbell(25);

   if (alarm == ALARM_TRACK) TrackAlarm = TRUE;
}

int OperQuery(char *who, char *what, char *why, char *value)
{
int             item;

   fl_set_object_label(fd_sitmonitor->sit_what, what);
   fl_set_object_label(fd_sitmonitor->sit_value, value);

   //fl_show_form(fd_sitmonitor->sitmonitor, FL_PLACE_POSITION,FL_NOBORDER,
   //"Situation Monitor");
   fl_ringbell(25);

   return TRUE;
}

void X_delete_list(int n_assignments)
{
   fl_clear_browser(fd_bmctrack->trk_browser);
}

void X_add_list(int pos, char *listitem)
{
   fl_add_browser_line(fd_bmctrack->trk_browser, listitem);
}

void X_track_update ()
{
char            str [2280];
int             i;

   if (trkvisible) {
      i = 0;
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
      //xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
      //XtVaSetValues(track_widget, XmNlabelString, xstr, NULL);
      //XmTextSetString(track_widget, (char *)str);
      //XtAppProcessEvent(appcontext, XtIMAll);
   }
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void BMCnoneCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

//<------------------------expose-------------------------------------->

//Run when the opengl screen needs to be refreshed 'cause it is "expose"

int bmcexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{

   return(0);
}

//<---------------------button press-------------------------------->

//when the user clicks in the opengl canvas window...the program goes here

int bmcbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
FL_Coord        width, height;
float           xpos, ypos, closest, d;
float           slat, slon, salt, shead;
int             i, xval, yval;

   fl_ringbell(0);

   xval = xev->xbutton.x;
   yval = xev->xbutton.y;

   fl_get_winsize(win, &width, &height);
   //xpos = (-180.0*RADDEG) + ((float)xval/(float)width)*(360.0*RADDEG);
   //ypos = (90.0*RADDEG) - ((float)yval/(float)height)*(180.0*RADDEG);
   xpos = xval; ypos = yval;
   fprintf(stderr, "User clicked in canvas at %f %f\n", xpos, ypos);
   /*
   xpos = (xpos*DEGRAD) + 180.0;
   ypos = (ypos*DEGRAD) + 90.0;
   closest = 99999.9;
   for (i=0; i<n_assets; i++) {
     AssetGetLLAH(i, &slat, &slon, &salt, &shead);
     slon = slon + 180.0;
     slat = slat + 90.0;
     d = sqrt( (xpos-slon)*(xpos-slon) + (ypos-slat)*(ypos-slat) );
     if (d < closest) {
       closest = d;
       asset_index = i;
     }
   }
   fprintf(stderr, "We seem to have a match at %s\n", AssetGetName(asset_index));
   */

   return(0);
}
/*                                                                       */
/* --------------------- Login/Signoff Control CB's -------------------- */
/*                                                                       */
void ConfigTopLevel(int mode)
{
FILE            *infp, *MENUfp;
int             i, j, k, n_items, num_types;
int             greyed = 0;
char            chstatus[8];
char            chvalue[16];
char            chlabel[32];
char            filename[64];
//
//   Do this every time we change the Mission Domain
//
   sprintf(scline, "Mission domain ........ %s",
	   fl_get_choice_text(fd_bmc3->bmc_choice));
   fl_replace_browser_line(fd_bmc3->bmctop_browser, 4, scline);
   //
   //   The 'Control' menu is common to all domains
   //
   fl_clear_menu(fd_bmc3->util_menu);
   fl_addto_menu(fd_bmc3->util_menu, "Asset Maintenance...");
   fl_addto_menu(fd_bmc3->util_menu, "Authority Management...");
   fl_addto_menu(fd_bmc3->util_menu, "Print Controls...");
   fl_addto_menu(fd_bmc3->util_menu, "Fog-of-War Control...");
   fl_addto_menu(fd_bmc3->util_menu, "System Status...");
   fl_addto_menu(fd_bmc3->util_menu, "Vulnerability Status...");
   fl_addto_menu(fd_bmc3->util_menu, "About GPDA...");
   fl_set_menu_item_mode(fd_bmc3->util_menu, 1, FL_PUP_GREY);
   fl_set_menu_item_mode(fd_bmc3->util_menu, 2, FL_PUP_GREY);
   fl_set_menu_item_mode(fd_bmc3->util_menu, 3, FL_PUP_GREY);
   //
   switch (mode) {

     case 0:
     case 1:
     case 2:
     case 3:
     case 4:
     case 5: // Strategic NMD/SDF
       setenv("GPDADOMAIN", "nmd", 1);
       setenv("SEARCHDIR", "JCSorders", 1);
       setenv("SEARCH", "Isearch -q -d JCSindexes/JCS_WO", 1);

       fl_set_choice(fd_bmc3->node_center, 1);
       fl_set_object_label(fd_bmc3->node_position, "Commander");
       fl_set_object_label(fd_bmc3->bmc_mission, "Missile Defense");
       break;

     case 6: // Theater/Tactical AOC
       setenv("GPDADOMAIN", "aoc", 1);
       setenv("SEARCHDIR", "JCSorders", 1);
       setenv("SEARCH", "Isearch -q -d JCSindexes/JCS_WO", 1);

       fl_set_choice(fd_bmc3->node_center, 5);
       fl_set_object_label(fd_bmc3->node_position, "CCO");
       fl_set_object_label(fd_bmc3->bmc_mission, "Air Operations");
       break;

     case 7: // Information Operations Warfare
       setenv("GPDADOMAIN", "iow", 1);
       setenv("SEARCHDIR", "IDSattack", 1);
       setenv("SEARCH", "Isearch -q -d IDSindexes/Snort", 1);

       fl_set_choice(fd_bmc3->node_center, 7);
       fl_set_object_label(fd_bmc3->node_position, "Sys Admin");
       fl_set_object_label(fd_bmc3->bmc_mission, "IO Warfare");
       break;

     case 8: // Information Operations Warfare
       setenv("GPDADOMAIN", "doh", 1);
       setenv("SEARCHDIR", "JCSorders", 1);
       setenv("SEARCH", "Isearch -q -d JCSindexes/JCS_WO", 1);

       fl_set_choice(fd_bmc3->node_center, 8);
       fl_set_object_label(fd_bmc3->node_position, "CCO");
       fl_set_object_label(fd_bmc3->bmc_mission, "Homeland Defense");
       break;

     default:
       break;
   }
   //
   fl_clear_browser(fd_bmc3->bmctop_browser);
   sprintf(scline, "Logged in user name ... %s", UserName);
   fl_addto_browser(fd_bmc3->bmctop_browser, scline);
   sprintf(scline, "Logged in position .... %s",
	   fl_get_object_label(fd_bmc3->node_position));
   fl_addto_browser(fd_bmc3->bmctop_browser, scline);
   sprintf(scline, "Command Center ........ %s",
	   fl_get_choice_text(fd_bmc3->node_center));
   fl_addto_browser(fd_bmc3->bmctop_browser, scline);
   sprintf(scline, "Mission domain ........ %s",
	   fl_get_choice_text(fd_bmc3->bmc_choice));
   fl_addto_browser(fd_bmc3->bmctop_browser, scline);
   sprintf(scline, "Classification level .. %s", CLASS);
   fl_addto_browser(fd_bmc3->bmctop_browser, scline);
   sprintf(scline, " - - - - - - - - - - - - - - - - - - - - -");
   fl_addto_browser(fd_bmc3->bmctop_browser, scline);
   sprintf(scline, "Run Identifier ........ %d", getpid());
   fl_addto_browser(fd_bmc3->bmctop_browser, scline);
   //
   fl_show_object(fd_bmc3->simtime);
   fl_show_object(fd_bmc3->runmode);
   fl_show_object(fd_bmc3->projectname);
   fl_show_object(fd_bmc3->clocktime);
   //
   strcpy(filename, "gpdarc");
   if ((infp = fopen(filename, "r")) == NULL) {
     fprintf(stderr, "Initialization file %s not available!\n", filename);
     exit(-1);
   }
   while (!feof(infp)) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));
     if (feof(infp)) {
       fprintf(stderr, "Specified Domain (%s) not found in initialization file (%s)!\n",
	       getenv("GPDADOMAIN"), filename);
       exit(-1);
     }
     if (scline[0] == '[') {
       sscanf(scline, "[%s]", chvalue);
       strsub(chvalue, ']', '\0');
       if (strcmp(chvalue, getenv("GPDADOMAIN")) == 0) break;
     }
   }
   //
   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get ID
   char MissionID[32];
   sscanf(scline, "%s", MissionID);
   strsub(MissionID, '_', ' ');

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Run Mode
   char RunMode[32];
   sscanf(scline, "%s", RunMode);
   strsub(RunMode, '_', ' ');
   fl_set_object_label(fd_bmc3->runmode, RunMode);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Mission Area
   char MissionArea[32];
   sscanf(scline, "%s", MissionArea);
   strsub(MissionArea, '_', ' ');
   fl_set_object_label(fd_bmc3->projectname, MissionArea);
//
//   Define the user menu selections for the dynamic menus
//
   fl_clear_menu(fd_bmc3->assess_menu);
   do fgets(scline, 128, infp); while ((scline[0] == '#'));
   sscanf(scline, "%d %s %d", &n_items, chlabel, &greyed);       // Get # menu items
   strsub(chlabel, '_', ' ');
   fl_set_object_label(fd_bmc3->assess_menu, chlabel);
   if (greyed)
     fl_hide_object(fd_bmc3->assess_menu);
   for (i=0; i<n_items; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get next menu item
     sscanf(scline, "%s %d", chlabel, &greyed);                  // Get item name
     strsub(chlabel, '_', ' ');
     fl_addto_menu(fd_bmc3->assess_menu, chlabel);               // Add to menu
     if (greyed)
       fl_set_menu_item_mode(fd_bmc3->assess_menu, i+1, FL_PUP_GREY);
   }
   //
   fl_clear_menu(fd_bmc3->determine_menu);
   do fgets(scline, 128, infp); while ((scline[0] == '#'));
   sscanf(scline, "%d %s %d", &n_items, chlabel, &greyed);       // Get # menu items
   strsub(chlabel, '_', ' ');
   fl_set_object_label(fd_bmc3->determine_menu, chlabel);
   if (greyed)
     fl_hide_object(fd_bmc3->determine_menu);
   for (i=0; i<n_items; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get next menu item
     sscanf(scline, "%s %d", chlabel, &greyed);                  // Get item name
     strsub(chlabel, '_', ' ');
     fl_addto_menu(fd_bmc3->determine_menu, chlabel);            // Add to menu
     if (greyed)
       fl_set_menu_item_mode(fd_bmc3->determine_menu, i+1, FL_PUP_GREY);
   }
   //
   fl_clear_menu(fd_bmc3->coadev_menu);
   do fgets(scline, 128, infp); while ((scline[0] == '#'));
   sscanf(scline, "%d %s %d", &n_items, chlabel, &greyed);       // Get # menu items
   strsub(chlabel, '_', ' ');
   fl_set_object_label(fd_bmc3->coadev_menu, chlabel);
   if (greyed)
     fl_hide_object(fd_bmc3->coadev_menu);
   for (i=0; i<n_items; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get next menu item
     sscanf(scline, "%s %d", chlabel, &greyed);                  // Get item name
     strsub(chlabel, '_', ' ');
     fl_addto_menu(fd_bmc3->coadev_menu, chlabel);               // Add to menu
     if (greyed)
       fl_set_menu_item_mode(fd_bmc3->coadev_menu, i+1, FL_PUP_GREY);
   }
   //
   fl_clear_menu(fd_bmc3->detailed_menu);
   do fgets(scline, 128, infp); while ((scline[0] == '#'));
   sscanf(scline, "%d %s %d", &n_items, chlabel, &greyed);       // Get # menu items
   strsub(chlabel, '_', ' ');
   fl_set_object_label(fd_bmc3->detailed_menu, chlabel);
   if (greyed)
     fl_hide_object(fd_bmc3->detailed_menu);
   for (i=0; i<n_items; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get next menu item
     sscanf(scline, "%s %d", chlabel, &greyed);                  // Get item name
     strsub(chlabel, '_', ' ');
     fl_addto_menu(fd_bmc3->detailed_menu, chlabel);             // Add to menu
     if (greyed)
       fl_set_menu_item_mode(fd_bmc3->detailed_menu, i+1, FL_PUP_GREY);
   }
   //
   fl_clear_menu(fd_bmc3->monitor_menu);
   do fgets(scline, 128, infp); while ((scline[0] == '#'));
   sscanf(scline, "%d %s %d", &n_items, chlabel, &greyed);       // Get # menu items
   strsub(chlabel, '_', ' ');
   fl_set_object_label(fd_bmc3->monitor_menu, chlabel);
   if (greyed)
     fl_hide_object(fd_bmc3->monitor_menu);
   for (i=0; i<n_items; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get next menu item
     sscanf(scline, "%s %d", chlabel, &greyed);                  // Get item name
     strsub(chlabel, '_', ' ');
     fl_addto_menu(fd_bmc3->monitor_menu, chlabel);              // Add to menu
     if (greyed)
       fl_set_menu_item_mode(fd_bmc3->monitor_menu, i+1, FL_PUP_GREY);
   }
//
//   Load the 'Tools' menu
//
   fl_clear_menu(fd_bmc3->tool_menu);
   do fgets(scline, 128, infp); while ((scline[0] == '#'));
   sscanf(scline, "%d %s %d", &n_items, chlabel, &greyed);       // Get # menu items
   strsub(chlabel, '_', ' ');
   fl_set_object_label(fd_bmc3->tool_menu, chlabel);
   if (greyed)
     fl_hide_object(fd_bmc3->tool_menu);
   for (i=0; i<n_items; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get next menu item
     sscanf(scline, "%s %d %s", chlabel, &j, BmcToolCmd[i+1]);   // Get tool name and command
     strsub(chlabel, '_', ' ');
     strsub(BmcToolCmd[i+1], '_', ' ');
     fl_addto_menu(fd_bmc3->tool_menu, chlabel);                 // Add to menu
   }
//
//   Re-label "STATUS" area
//   (There is a conflict here between values from the init file & the planner.par file)
//
   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Status Area Label
   sscanf(scline, "%s", chlabel);
   strsub(chlabel, '_', ' ');
   fl_set_object_label(fd_bmc3->sum_label, chlabel);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Status Field 1
   sscanf(scline, "%s %s %s", chlabel, chvalue, chstatus);
   strsub(chlabel, '_', ' ');
   strsub(chvalue, '_', ' ');
   defcon = atoi(chvalue);
   fl_set_object_label(fd_bmc3->defcon_label, chlabel);
   fl_set_input(fd_bmc3->defcon_val, chvalue);
   fl_set_object_label(fd_sitaware->sit_defcon, chlabel);
   fl_set_input(fd_sitaware->sit_defcon, chvalue);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Status Field 2
   sscanf(scline, "%s %s %s", chlabel, chvalue, chstatus);
   strsub(chlabel, '_', ' ');
   strsub(chvalue, '_', ' ');
   Rposture = atoi(chvalue);
   fl_set_object_label(fd_bmc3->rp_label, chlabel);
   fl_set_input(fd_bmc3->rp_val, chvalue);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Status Field 3
   sscanf(scline, "%s %s %s", chlabel, chdea, chstatus);
   strsub(chlabel, '_', ' ');
   strsub(chdea, '_', ' ');
   fl_set_object_label(fd_bmc3->dea_label, chlabel);
   fl_set_input(fd_bmc3->dea_val, chdea);
   fl_set_object_label(fd_sitaware->sit_dea, chlabel);
   fl_set_input(fd_sitaware->sit_dea, chvalue);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Status Field 4
   sscanf(scline, "%s %s %s", chlabel, chroe, chstatus);
   strsub(chlabel, '_', ' ');
   strsub(chroe, '_', ' ');
   fl_set_object_label(fd_bmc3->roe_label, chlabel);
   fl_set_object_label(fd_bmc3->roe_val, chroe);
   fl_set_object_label(fd_sitaware->sit_roe, chlabel);
   fl_set_input(fd_sitaware->sit_roe, chvalue);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Status Field 5
   sscanf(scline, "%s %s %s", chlabel, chvalue, chstatus);
   strsub(chlabel, '_', ' ');
   strsub(chvalue, '_', ' ');
   fl_set_object_label(fd_bmc3->msncon_label, chlabel);
   fl_set_object_label(fd_bmc3->msncon_val, chvalue);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Status Field 6
   sscanf(scline, "%s %s %s", chlabel, chvalue, chstatus);
   strsub(chlabel, '_', ' ');
   strsub(chvalue, '_', ' ');
   fl_set_object_label(fd_bmc3->msnobj_label, chlabel);
   fl_set_object_label(fd_bmc3->msnobj_val, chvalue);
   fl_set_object_label(fd_sitaware->sit_strategy, chlabel);
   fl_set_input(fd_sitaware->sit_strategy, chvalue);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Status Field 7
   sscanf(scline, "%s %s %s", chlabel, chplan, chstatus);
   strsub(chlabel, '_', ' ');
   strsub(chplan, '_', ' ');
   fl_set_object_label(fd_bmc3->plan_label, chlabel);
   fl_set_object_label(fd_bmc3->plan_val, chplan);
//
//   Re-label "Mission Summary" area
//
   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Summary Area Label
   sscanf(scline, "%s", chlabel);
   strsub(chlabel, '_', ' ');
   fl_set_object_label(fd_bmc3->sum_label, chlabel);
   //
   for (i=0; i<8; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get Summary Field 1..8
     sscanf(scline, "%s %s", chlabel, chvalue);
     strsub(chlabel, '_', ' ');
     fl_set_object_label(fd_bmc3->summ_label[i], chlabel);
     fl_set_object_label(fd_bmc3->summ_value[i], chvalue);
   }
//
//   Re-label "Asset Summary" area
//
   fl_clear_choice(fd_bmc3->nmd_weap_menu);
   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Summary label
   sscanf(scline, "%d", &n_items);                               // Get # menu items
   //fl_set_object_label(fd_bmc3->tool_menu, "Tools");
   for (i=0; i<n_items; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get next menu item
     sscanf(scline, "%s", chlabel);                              // Get item name
     strsub(chlabel, '_', ' ');
     fl_addto_choice(fd_bmc3->nmd_weap_menu, chlabel);           // Add to menu
   }
   //
   fl_clear_choice(fd_bmc3->sdf_weap_menu);
   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Summary label
   sscanf(scline, "%d", &n_items);                               // Get # menu items
   //fl_set_object_label(fd_bmc3->tool_menu, "Tools");
   for (i=0; i<n_items; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get next menu item
     sscanf(scline, "%s", chlabel);                              // Get item name
     strsub(chlabel, '_', ' ');
     fl_addto_choice(fd_bmc3->sdf_weap_menu, chlabel);           // Add to menu
   }
//
//   Display Situation Awareness map on Top Level window  
//
   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Sit. Awareness map fn
   sscanf(scline, "%s", filename);
   if (strcmp(filename, "*None*") != 0) {                        // If there is a filename,
     ipipm = fd_bmc3->bmctop_map;                                //   setup the pixmap
     fl_show_object(ipipm);
     fl_free_pixmap_pixmap(ipipm);
     sprintf (scline, "%s/%s", BITMAPDIR, filename);
     fl_set_pixmap_file(ipipm, scline);
     fl_hide_object(fd_bmc3->bmc_canvas);
     fl_show_object(fd_bmc3->bmctop_map);
   } else {
     fl_show_object(fd_bmc3->bmc_canvas);
     fl_hide_object(fd_bmc3->bmctop_map);
   }
//
//   Display other maps  
//
   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get 
   sscanf(scline, "%s %d", filename, &UpdateSit);
   ipipm = fd_sitaware->sit_namap;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (scline, "%s/%s", BITMAPDIR, filename);
   fl_set_pixmap_file(ipipm, scline);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get 
   sscanf(scline, "%s", filename);
   ipipm = fd_sitaware->sit_riskmap;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (scline, "%s/%s", BITMAPDIR, filename);
   fl_set_pixmap_file(ipipm, scline);

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get 
   sscanf(scline, "%s", filename);
   ipipm = fd_bmc3->bmc_mission;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (scline, "%s/%s", BITMAPDIR, filename);
   fl_set_pixmap_file(ipipm, scline);
   //
   for (i=0; i<8; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get Overlay Label 1..8
     sscanf(scline, "%s %d", chlabel, &j);
     strsub(chlabel, '_', ' ');
     fl_set_object_label(fd_sitaware->sit_overlay[i], chlabel);
     if (j == 1)
       fl_set_button(fd_sitaware->sit_overlay[i], 1);
     else
       fl_set_button(fd_sitaware->sit_overlay[i], 0);
   }
   //
   for (i=0; i<6; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get Durability Label 1..6
     sscanf(scline, "%s %d", chlabel, &j);
     strsub(chlabel, '_', ' ');
     fl_set_object_label(fd_sitaware->sit_durable[i], chlabel);
     if (j == 0) {
       fl_hide_object(fd_sitaware->sit_durable[i]);
     } else {
       fl_show_object(fd_sitaware->sit_durable[i]);
       fl_clear_chart(fd_sitaware->sit_durable[i]);
       for (k=0; k<j; k++) {       
	 fl_add_chart_value(fd_sitaware->sit_durable[i], (double)2016.0, "Active", FL_RED+k);
       }
     }
   }
//
//   All done, close file and leave
//
   fclose(infp);
//
//   Load the initial Mission types
   /*
   fl_clear_choice(fd_bmc3->bmc_select); 
   infp = fopen("DSBinit.dat", "r");
   do fgets(scline, 128, infp); while (scline[0] == '#');    // Read # assessments
   sscanf(scline, "%d", &num_types);
   for (j=0; j<num_types; j++) {
     do fgets(scline, 128, infp); while (scline[0] == '#');
     // 
     //   Read Mission type and file base names
     //
     sscanf(scline, "%s %s", chlabel, filename);
     strsub(chlabel, '_', ' ');
     fl_addto_choice(fd_bmc3->bmc_select, chlabel);
   }
   fclose(infp); 
   */
   fl_redraw_form(fd_bmc3->bmc3);                                // Clean up any overdraws
}

void loginCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             pid;
char            *display;

   switch (item) {

     case 0:
       /*
       strcpy(UserName, fl_get_input(fd_login->username));
       if (fl_form_is_visible(fd_login->login))
         fl_hide_form(fd_login->login);
       fl_show_form(fd_cmocnode->cmocnode, FL_PLACE_CENTER,FL_FULLBORDER, "Position");
       */
       break;

     case 10:
       //
       //   Do this ONCE per run
       //   --------------------
       //
       //if (fl_form_is_visible(fd_cmocnode->cmocnode))
       //fl_hide_form(fd_cmocnode->cmocnode);

       fl_set_form_position(fd_bmc3->bmc3, 0, 0);
       mainwinID = fl_show_form(fd_bmc3->bmc3, FL_PLACE_GEOMETRY, FL_FULLBORDER,
                   "TopLevel");
       fl_get_winsize(mainwinID, &mainwinW, &mainwinH);
       fl_get_winorigin(mainwinID, &mainwinX, &mainwinY);
       winposX = mainwinX+130;
       winposY = mainwinY+195;
       winsizW = 850;
       winsizH = 460;
       fl_set_app_mainform(fd_bmc3->bmc3);

       SITinit();

       ConfigTopLevel(fl_get_choice(fd_bmc3->bmc_choice));

       FOWinit();
       DSBinit();
       CBPinit();
       REDIinit();
       TGTinit();
       TLEinit();
       CLUinit();
       ITELinit();
       OPTinit();
       PUPinit();
       SIMinit();
       TCPinit();

       if (!PLANTEST) BMCrun = TRUE;

       StoreActiveEntry("TopLevel");

       fl_set_idle_callback(IdleWP, 0);
       break;

     default:
       break;
   } 
}

void opsmodeCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   ConfigTopLevel(fl_get_choice(fd_bmc3->bmc_choice));
}

void positionCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   position = object->label;
   fl_set_object_label(fd_bmc3->node_position, position);
}

void cmdcenterCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   cmdlevel = object->label;
   //fl_set_object_label(fd_bmc3->node_center, cmdlevel);
   fl_set_choice(fd_bmc3->bmc_choice, item+1);
}

void filemanCB(FL_OBJECT *object, long item_no)
{
int             irc, item = 0;

   irc = fl_exe_command("fileman", 0);
}

void cmdlogCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_show_command_log(FL_FULLBORDER);
}

void snapCB(FL_OBJECT *object, long item_no)
{
int             item = 0;
char            *snapdir;

   snapdir = getenv("SNAPDIR"); 
   int maxline = fl_get_browser_maxline(fd_bmc3->act_windows);
   if (maxline > 0) {
     sprintf(scline, "xwd -name %s | convert - gif:%s%s.gif",
	     fl_get_browser_line(fd_bmc3->act_windows, maxline),
	     snapdir,
	     fl_get_browser_line(fd_bmc3->act_windows, maxline));
     system(scline);
   }

   return;
}

void lockCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   system("xlock");
}

void signoffCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   if (SIMTEST) SC_message(OP_EXIT_SC, 0);

   fl_finish();
   exit(0);
} 
/*                                                                       */
/* -------------------------- Menu Control CB's ------------------------ */
/*                                                                       */
void assessmenuCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   item = fl_get_menu(fd_bmc3->assess_menu);

   switch (item) {
     case 1: // Situation Assessment
       DSBshow(winposX, winposY, winsizW, winsizH, mainwinID, NULL);
       break;

     case 2: // Readiness Assessment
       REDIshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 3: // Attack Assessment
       //CCPshow(winposX, winposY, winsizW, winsizH, mainwinID, 3);  // NMD
       break;

     case 4: // Force Management
       //CCPshow(winposX, winposY, winsizW, winsizH, mainwinID, 1);
       break;

     case 5: // Force Survival
       //CCPshow(winposX, winposY, winsizW, winsizH, mainwinID, 2);
       break;

     case 7: // Drawdown Curves
       fl_set_form_position(fd_drawdown->drawdown, mainwinX+WINdX, mainwinY+WINdY);
       fl_set_form_size(fd_drawdown->drawdown, 850, 460);
       fl_show_form(fd_drawdown->drawdown, FL_PLACE_POSITION,FL_NOBORDER,
		    "Drawdown Assessment");
       fl_raise_form(fd_drawdown->drawdown);
       StoreActiveEntry("Drawdown Assess");
       break;

     default:
       break;
   }
}

void determineCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             irc;
char            filename[64];

   item = fl_get_menu(fd_bmc3->determine_menu);

   switch (item) {

     case 1:  // Data mining
       fl_exe_command("java -jar weka.jar", 0);
       break;

     case 2:  // Data clustering
       CLUshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 3:  // DB Maint.
       ITELshow(winposX, winposY, winsizW, winsizH, mainwinID, 1, NULL);
       break;

     default:
       break;
   }
}

void coadevmenuCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   item = fl_get_menu(fd_bmc3->coadev_menu);

   switch (item) {

     case 1:  // Combined Planning
       if (strcmp(getenv("GPDADOMAIN"), "nmd") == 0)
	 CBPshow(winposX, winposY, winsizW, winsizH, mainwinID, "JCS-Order-1437.5");
       else
	 CBPshow(winposX, winposY, winsizW, winsizH, mainwinID, "IDS-Attack-1234.5");
       break;

     default:
       break;
   }
}

void detailmenuCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             pid;
PUPINFO         pupinfo;

   item = fl_get_menu(fd_bmc3->detailed_menu);

   switch (item) {

     case 1: // Plan Update
       pupinfo.col       = 0;
       pupinfo.row       = 0;
       pupinfo.belief    = 0.6;
       pupinfo.disbelief = 0.2;
       pupinfo.time      = 0.1;
       //
       strcpy(pupinfo.nodelabel, " ");
       //
       strcpy(pupinfo.filename, "DSBFiles/CND.plan");
       //
       PUPshow(winposX, winposY, winsizW, winsizH, mainwinID, pupinfo);
       break;

     case 2:  // Plan Optimization
       OPTshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 3:  // Target planning
       TGTshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 4:  // Engagement planner  
       switch (pid = fork()) {
         case 0:
           execlp("compose", "compose", NULL);
           perror("compose");
           exit (255);
           break;
         case -1:
           printf("Child process startup failed. PID = %d\n", pid);
           break;
       }
       break;

     default:
       break;
   }
}

void monitormenuCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   item = fl_get_menu(fd_bmc3->monitor_menu);

   switch (item) {
     case 1:  // Decision Timeline
       TLEshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 2:  // Situation Awareness
       SITshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 3:  // Engagement Summary
       ENGshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 4:  // Tracks
       TRKshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 5:  // Defense Options
       break;

     case 6:  // Management By Exception
       break;

     case 7:  // Missile Orcer of Battle
       break;

     case 8:  // Defend Task Plan
       break;

     case 9:  // Survey Task Plan
       break;

     case 10:  // Threat Characteristics
       break;

     case 11:  // Threatened Assets
       break;

     default:
       break;
   }
}

void showstatsCB(FL_OBJECT *object, long item_no)
{
int             i, item = item_no;
char            *varptr;
char            *notset = "<Not Set>";
char            ch[80];
struct sysinfo  info;
struct utsname  osname;

   fl_clear_browser(fd_statcomm->ifstats_list);
   for (i=0; i<n_ifs; i++) {
     sprintf(scline, "%-18s %8d %12d %10d %12d", IfNames[i], commstats[i].in_msgs,
	     commstats[i].in_bytes, commstats[i].ot_msgs, commstats[i].ot_bytes);
     fl_addto_browser(fd_statcomm->ifstats_list, scline);
   }

   sysinfo(&info);
   uname(&osname);

   fl_clear_browser(fd_stathard->hard_info);
   varptr = getenv("VENDOR"); if (varptr==NULL)   varptr = "Unknown";
   sprintf(ch, "Machine hardware architecture .. %s [%s]\n", osname.machine, varptr);
   fl_addto_browser(fd_stathard->hard_info, ch);
   sprintf(ch, "Total usable main memory size .. %dMb\n",    info.totalram/(1024*1000));
   fl_addto_browser(fd_stathard->hard_info, ch);
   sprintf(ch, "Available main memory size ..... %dMb\n",    info.freeram/(1024*1000));
   fl_addto_browser(fd_stathard->hard_info, ch);
   sprintf(ch, "Seconds since last boot ........ %d\n",      info.uptime);
   fl_addto_browser(fd_stathard->hard_info, ch);

   fl_clear_browser(fd_statsyst->syst_info);
   sprintf(ch, "Operating System name .......... %s [%s]",
	   osname.sysname, osname.release);
   fl_addto_browser(fd_statsyst->syst_info, ch);
   sprintf(ch, "Operating System release ....... %s",
	   osname.version);
   fl_addto_browser(fd_statsyst->syst_info, ch);
   sprintf(ch, "This node's network name ....... %s\n",
	   osname.nodename);
   fl_addto_browser(fd_statsyst->syst_info, ch);
   sprintf(ch, "Current Working Directory ...... %s", getcwd(NULL,0));
   fl_addto_browser(fd_statsyst->syst_info, ch);
   sprintf(ch, "ID of current process .......... %d", getpid());
   fl_addto_browser(fd_statsyst->syst_info, ch);
   sprintf(ch, "Timing collection is ........... %s",
	   (PLANTIME==NULL ? "Disabled" : "Enabled"));
   fl_addto_browser(fd_statsyst->syst_info, ch);
   sprintf(ch, "Debugging is ................... %s",
	   (PLANDBUG==NULL ? "Disabled" : "Enabled"));
   fl_addto_browser(fd_statsyst->syst_info, ch);
   sprintf(ch, "Statistics collection is ....... %s",
	   (ODISTATS==NULL ? "Disabled" : "Enabled"));
   fl_addto_browser(fd_statsyst->syst_info, ch);
   sprintf(ch, "Run Mode is .................... %s",
	   (PLANTEST ? "Playback" : "Realtime"));
   fl_addto_browser(fd_statsyst->syst_info, ch);

   fl_clear_browser(fd_statuser->user_info);
   sprintf(ch, "User's real name ............... %s\n",      UserName);
   fl_addto_browser(fd_statuser->user_info, ch);
   sprintf(ch, "User's position ................ %s\n",      "Sys Admin");
   fl_addto_browser(fd_statuser->user_info, ch);
   sprintf(ch, "User's login name .............. %s\n",      getlogin() );
   fl_addto_browser(fd_statuser->user_info, ch);
   varptr = getenv("GROUP"); if (varptr==NULL)   varptr = "Unknown";
   sprintf(ch, "User's Group name .............. %s\n",      varptr);
   fl_addto_browser(fd_statuser->user_info, ch);

   fl_clear_browser(fd_statenvs->bmc_env_vars);
   fl_addto_browser(fd_statenvs->bmc_env_vars, "DATADIR");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "MODELDIR");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "BITMAPDIR");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "PARMDIR");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "SNAPDIR");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "SEARCH");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "PLANTEST");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "PLANDATA");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "PLANTIME");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "PLANDBUG");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "PLANLOGIN");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "SIMTEST");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "SIMSOCK");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "GPDADOMAIN");
   fl_addto_browser(fd_statenvs->bmc_env_vars, "GPDABATCH");

   fl_clear_browser(fd_statenvs->bmc_env_setting);
   varptr = getenv("DATADIR"); if (varptr==NULL)   varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("MODELDIR"); if (varptr==NULL)  varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("BITMAPDIR"); if (varptr==NULL) varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("PARMDIR"); if (varptr==NULL)   varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("SNAPDIR"); if (varptr==NULL)   varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("SEARCH"); if (varptr==NULL)    varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("PLANTEST"); if (varptr==NULL)  varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("PLANDATA"); if (varptr==NULL)  varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("PLANTIME"); if (varptr==NULL)  varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("PLANDBUG"); if (varptr==NULL)  varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("PLANLOGIN"); if (varptr==NULL) varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("SIMTEST"); if (varptr==NULL)   varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("SIMSOCK"); if (varptr==NULL)   varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("GPDADOMAIN"); if (varptr==NULL)   varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);
   varptr = getenv("GPDABATCH"); if (varptr==NULL)   varptr = notset;
   fl_addto_browser(fd_statenvs->bmc_env_setting,  varptr);

   fl_show_form(fd_bmcstatus->bmcstatus, FL_PLACE_CENTER,FL_FULLBORDER, "I/F Statistics");
}

void bmcenvironCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
char            ch[80];
struct stat     statbuf;
struct group    *grpbuf;
struct passwd   *passbuf;
time_t          clock;
char            filetypes[16][16] = { "Unknown",      "FIFO",    "Char. Device",  "Unknown",
				      "Directory",    "Unknown", "Block Device",  "Unknown",
				      "Regular File", "Unknown", "Symbolic Link", "Unknown",
				      "Unknown",      "Socket",  "Unknown",       "Unknown" };

   switch (item) {
     case 0: // 
       break;

     case 1: // 
       break;

     case 2: // 
       break;

     case 3: // 
       break;

     case 4: // 
       break;

     case 5: // 
       stat(fl_get_input(fd_statfile->bmc_file_name), &statbuf);
       passbuf = getpwuid(statbuf.st_uid);
       grpbuf = getgrgid(statbuf.st_gid);

       fl_clear_browser(fd_statfile->file_info);
       sprintf(ch, "User ID of file owner .......... %s", passbuf->pw_name);
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "Group ID of file owner ......... %s", grpbuf->gr_name);
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "Size of file (bytes) ........... %d", statbuf.st_size);
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "No. 512-byte blocks alloc'ed ... %d", statbuf.st_blocks);
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "File type ...................... %s",
	       filetypes[((statbuf.st_mode>>12)&017)]);
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "File access permissions ........ %o", (statbuf.st_mode&0777));
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "Link count ..................... %d", statbuf.st_nlink);
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "Device type .................... %d / %d",
	       (int)(statbuf.st_dev/255), (int)(statbuf.st_dev%255));
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "Device number (if device) ...... %d", statbuf.st_rdev);
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "Time of last access ............ %s", "Unknown" /*st_atime*/);
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "Time of last modification ...... %s", "Unknown" /*st_mtime*/);
       fl_addto_browser(fd_statfile->file_info, ch);
       sprintf(ch, "Time of last status change ..... %s", "Unknown" /*st_ctime*/);
       fl_addto_browser(fd_statfile->file_info, ch);
       break;

     default:
       break;
   }
}

void filestatCB(FL_OBJECT *ob, long data)
{
int        itemno;
const char title[64]     = { "File Status" };
const char directory[12] = { "./" };
const char pattern[12]   = { "*" };
const char initial[128]  = { "" };
const char *filename;

   filename = fl_show_fselector(title, directory, pattern, initial);
   if (filename != NULL) {
     fl_set_input(fd_statfile->bmc_file_name, filename);
   }
}

void toolmenuCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   item = fl_get_menu(fd_bmc3->tool_menu);

   //sprintf(scline, "%s.sh", fl_get_menu_text(fd_bmc3->tool_menu));
   sprintf(scline, "%s", BmcToolCmd[item]);
   fl_exe_command(scline, 0);
}

void utilmenuCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   item = fl_get_menu(fd_bmc3->util_menu);

   switch (item) {
     case 1: // Asset Maintenance
       break;

     case 2: // Authority Management
       break;

     case 3: // Print Controls
       break;

     case 4: // Fog-of-War
       FOWshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 5: // Interface Statistics
       showstatsCB(NULL, 0);
       break;

     case 6: // Network port status
       TCPshow(winposX, winposY, winsizW, winsizH, mainwinID);
       break;

     case 7: // About
       sprintf(scline, "Compiled: %s", COMPILED);
       fl_set_object_label(fd_about->about_date, scline);
       fl_show_form(fd_about->About_Form, FL_PLACE_CENTER,FL_FULLBORDER, "About GPDA");       
       break;

     default:
       break;
   }
}

void aboutexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   fl_hide_form(fd_about->About_Form);
}

/*                                                                       */
/* ----------------------- Other Main Control CB's --------------------- */
/*                                                                       */
void simtimeCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   if (strcmp(getenv("GPDADOMAIN"), "iow") == 0)
     fl_exe_command("IOWSim.sh", 0);     
   else if (strcmp(getenv("GPDADOMAIN"), "nmd") == 0)
     fl_exe_command("NMDSim.sh", 0);
   else if (strcmp(getenv("GPDADOMAIN"), "aoc") == 0)
     fl_exe_command("AOCSim.sh", 0);
   else if (strcmp(getenv("GPDADOMAIN"), "doh") == 0) {
     SIMshow(winposX, winposY, winsizW, winsizH, mainwinID);
     //fl_exe_command("DOHSim.sh", 0);
   }
}

void dirstatusCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             i;
int             BROwinX, BROwinY, BROwinW, BROwinH; 
char            chtitle[32];
PUPINFO         pupinfo;
Window          winid;

   strcpy(scline, "\0");

   switch (item) {

     case 5:
       strcpy(scline, fl_get_object_label(fd_bmc3->msncon_val));
       if (strcmp(scline, "None") != 0) {
	 strcpy(BROlabel, "Constraint-Viewer");
	 fl_clear_browser(fd_bmcbrowse->bmc_browse);
	 //
	 if(!fl_form_is_visible(fd_bmcbrowse->bmcbrowse) ) {
	   fl_transient();
	   fl_winposition(winposX, winposY);
	   fl_initial_winsize(winsizW, winsizH);
	   winid = fl_prepare_form_window(fd_bmcbrowse->bmcbrowse,
                                     FL_PLACE_POSITION,FL_TRANSIENT, BROlabel);
	   fl_winreshape(winid, winposX, winposY, winsizW, winsizH);
	   fl_get_wingeometry(winid, &BROwinX, &BROwinY, &BROwinW, &BROwinH);
	   fl_show_form_window(fd_bmcbrowse->bmcbrowse);
	   //fl_set_form_atclose(fd_bmcbrowse->bmcbrowse, TRKclose, 0);
	   StoreActiveEntry(BROlabel);
	 }
	 //
	 strsub(scline, ' ', '_');
	 strcat(scline, ".txt");
	 fl_load_browser(fd_bmcbrowse->bmc_browse, scline);
       }
       break;

     case 6:
       strcpy(scline, fl_get_object_label(fd_bmc3->msnobj_val));
       if (strcmp(scline, "None") != 0) {
	 strcpy(BROlabel, "Objective-Viewer");
	 fl_clear_browser(fd_bmcbrowse->bmc_browse);
	 //
	 if(!fl_form_is_visible(fd_bmcbrowse->bmcbrowse) ) {
	   fl_transient();
	   fl_winposition(winposX, winposY);
	   fl_initial_winsize(winsizW, winsizH);
	   winid = fl_prepare_form_window(fd_bmcbrowse->bmcbrowse,
                                     FL_PLACE_POSITION,FL_TRANSIENT, BROlabel);
	   fl_winreshape(winid, winposX, winposY, winsizW, winsizH);
	   fl_get_wingeometry(winid, &BROwinX, &BROwinY, &BROwinW, &BROwinH);
	   fl_show_form_window(fd_bmcbrowse->bmcbrowse);
	   //fl_set_form_atclose(fd_bmcbrowse->bmcbrowse, TRKclose, 0);
	   StoreActiveEntry(BROlabel);
	 }
	 //
	 strsub(scline, ' ', '_');
	 strcat(scline, ".txt");
	 fl_load_browser(fd_bmcbrowse->bmc_browse, scline);
       }
       break;

     case 7:
       strcpy(scline, fl_get_object_label(fd_bmc3->plan_val));
       if (strcmp(scline, "None") != 0) {
	 pupinfo.col       = 0;
	 pupinfo.row       = 0;
	 pupinfo.belief    = 0.6;
	 pupinfo.disbelief = 0.2;
	 pupinfo.time      = 0.1;
	 //
	 strcpy(pupinfo.nodelabel, " ");
	 //
	 strsub(scline, ' ', '_');
	 strcat(scline, ".plan");
	 strcpy(pupinfo.filename, scline);
	 //
	 PUPshow(winposX, winposY, winsizW, winsizH, mainwinID, pupinfo);
       } 
       break;

     default:
       break;
   }
}

void infoexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   fl_hide_form(fd_infodialog->infodialog);
}

void browseexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   fl_hide_form(fd_bmcbrowse->bmcbrowse);
   EraseActiveEntry(BROlabel);

   return;
}

void wpnchartCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   GBavail = 70;
   GBwitheld = 10;
   GBexpended = 16;
   fl_clear_chart(fd_bmc3->nmd_chart);
   fl_add_chart_value(fd_bmc3->nmd_chart, (double)GBavail, "Avail", FL_GREEN);
   fl_add_chart_value(fd_bmc3->nmd_chart, (double)GBwitheld, "Held", FL_YELLOW);
   fl_add_chart_value(fd_bmc3->nmd_chart, (double)GBexpended, "Used", FL_RED);

   ICavail = 100;
   ICdown  = 20;
   ICused  = 0;
   fl_clear_chart(fd_bmc3->sdf_chart);
   fl_add_chart_value(fd_bmc3->sdf_chart, (double)ICavail, "Avail", FL_GREEN);
   fl_add_chart_value(fd_bmc3->sdf_chart, (double)ICdown, "Down", FL_YELLOW);
   fl_add_chart_value(fd_bmc3->sdf_chart, (double)ICused, "Used", FL_RED);
}

void nmdweapsumCB(FL_OBJECT *object, long item_no)
{
int item;

   item = fl_get_choice(fd_bmc3->nmd_weap_menu) - 1;

   fl_replace_chart_value(fd_bmc3->nmd_chart, 1, (double)GBavail, "Avail", FL_GREEN);
   fl_replace_chart_value(fd_bmc3->nmd_chart, 2, (double)GBwitheld, "Held", FL_YELLOW);
   fl_replace_chart_value(fd_bmc3->nmd_chart, 3, (double)GBexpended, "Used", FL_RED);
}

void sdfweapsumCB(FL_OBJECT *object, long item_no)
{
int             item;
int             OffWeapAvail[10] = { 100, 100, 100, 10, 20, 20 };
int             OffWeapDown[10]  = { 20,  10,  10,  2,  3,  3  };
int             OffWeapUsed[10]  = { 0,   0,   30,  3,  8,  5  };

   item = fl_get_choice(fd_bmc3->sdf_weap_menu) - 1;

   fl_replace_chart_value(fd_bmc3->sdf_chart, 1, (double)OffWeapAvail[item], "Avail", FL_GREEN);
   fl_replace_chart_value(fd_bmc3->sdf_chart, 2, (double)OffWeapDown[item],  "Down",  FL_YELLOW);
   fl_replace_chart_value(fd_bmc3->sdf_chart, 3, (double)OffWeapUsed[item],  "Used",  FL_RED);
}

void missummaryCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

void alarmCB(FL_OBJECT *object, long item_no)
{

   if (ALARMLOG)
     fl_show_form(fd_bmcalarms->bmcalarms, FL_PLACE_CENTER,FL_FULLBORDER, "Alarms");
   else
     alrmactionCB(NULL, 0);
}

void alrmactionCB(FL_OBJECT *object, long item_no)
{
int             ialarm;
INMSG           *p;

   int item = item_no;

   if (ALARMLOG) {
     sprintf(scline, "T=%s[%d]  A=%3d  P=%d  N=%s",
	     alarms[alarm_count].tod,
	     alarms[alarm_count].timenow,
	     alarms[alarm_count].alarm,
	     alarms[alarm_count].priority,
	     alarms[alarm_count].label);
     switch (item) {
       case 0:
         fl_addto_browser(fd_alrmaccept->alrm_accepted, scline);       
         break;
       case 1:
         fl_addto_browser(fd_alrmdefered->alrm_defered, scline);       
         break;
       case 2:
         fl_addto_browser(fd_alrmcancel->alrm_canceled, scline);       
         break;
     }
     fl_hide_form(fd_bmcalarms->bmcalarms);
   }

   fl_set_object_color(fd_bmc3->alarm_button, FL_INACTIVE_COL, FL_INACTIVE_COL);
   fl_set_object_label(fd_bmc3->alarm_text, "No Alarms");
   fl_deactivate_object(fd_bmc3->alarm_button);

   ialarm = alarms[alarm_count].alarm;
   strcpy(scline, alarms[alarm_count].arg);

   alarm_count = alarm_count - 1;

   if (item == 0) {
      switch (ialarm) {
        case ALARM_INTEL:
          ITELshow(winposX, winposY, winsizW, winsizH, mainwinID, 2, scline);
          break;

        case ALARM_LIMIT:
          sprintf(bmctemp, "Packets/sec.\nthreshold exceeded on node\n\n%s", scline);
	  fl_show_messages(bmctemp);
          break;

        case ALARM_JCSWARN:
          CBPshow(winposX, winposY, winsizW, winsizH, mainwinID, scline);    
          break;

        case ALARM_JCSEXEC:
          CBPshow(winposX, winposY, winsizW, winsizH, mainwinID, scline);
          break;

        case ALARM_LAUNCH:
	  p = (INMSG *)new(INMSG);
	  strcpy(p->src, "MTIX");
	  strcpy(p->dst, "GPDA");
	  strcpy(p->msn, "CND");
          DSBshow(winposX, winposY, winsizW, winsizH, mainwinID, p);
	  delete (p);
          break;

        case ALARM_TRACK:
          TRKshow(winposX, winposY, winsizW, winsizH, mainwinID);
          BMCrun = TRUE;
          break;

        case ALARM_POTEVENT:
          break;

        case ALARM_TIMER:
          TLEshow(winposX, winposY, winsizW, winsizH, mainwinID);
          break;

        default:
          break;
      }
   }
}

void ALRMexitCB(FL_OBJECT *object, long item_no)
{
   fl_hide_form(fd_bmcalarms->bmcalarms);

}

void map2dCB(FL_OBJECT *object, long item_no)
{
int      item = 0;
int      pid;
/*
   if (fl_mouse_button() == FL_RIGHTMOUSE) {
     switch (pid = fork()) {
     case 0:
       execlp("jtamved", "jtamved", NULL);
       perror("jtamved");
       exit (255);
       break;
     case -1:
       printf("Child process startup failed. PID = %d\n", pid);
       break;
     }
   } else {
     switch (pid = fork()) {
     case 0:
       execlp("jtamv", "jtamv", NULL);
       perror("jtamv");
       exit (255);
       break;
     case -1:
       printf("Child process startup failed. PID = %d\n", pid);
       break;
     }
   }
*/
   DSBshow(winposX, winposY, winsizW, winsizH, mainwinID, NULL);
}

void actwindowsCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

void drawdownCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             n_segs = 5;
float           xx[7], y1[7], y2[7], y3[7], Xp[2], Yp[2], Xo[2], Yo[2];
float           ParityLineX0, ParityLineY0;
float           ParityLineXm, ParityLineYm;
float           ParityPointX, ParityPointY;
float           OffsetPointX, OffsetPointY;
float           AlphaRad;
static float    Alpha = 60.0;


   xx[0] = log10(1.0);
   xx[1] = log10(3.0);
   xx[2] = log10(10.0);
   xx[3] = log10(30.0);
   xx[4] = log10(100.0);
   xx[5] = log10(300.0);
 
   switch (item) {
     case 0:
       fl_hide_form(fd_drawdown->drawdown);
       EraseActiveEntry("Drawdown Assess");
       break;

     case 1:
       //
       //   Set the angle at which to draw the parity line
       //
       AlphaRad = Alpha/(180.0/M_PI);
       sprintf(scline, "%f", Alpha);
       fl_set_input(fd_drawdown->drawdown_alpha, scline);
       //
       //   Set the Parity Point
       //
       ParityPointX = 1000.0;
       ParityPointY = 1000.0 * tan(AlphaRad);
       sprintf(scline, "%d,%d", (int)ParityPointX, (int)ParityPointY);
       fl_set_input(fd_drawdown->drawdown_parity, scline);
       //
       //   Set the Offset Point
       // 
       OffsetPointX = 800.0;
       OffsetPointY = 500.0;
       sprintf(scline, "%d,%d", (int)OffsetPointX, (int)OffsetPointY);
       fl_set_input(fd_drawdown->drawdown_offset, scline);
       //
       //   Create and define the graph
       //
       ddgraph = fd_drawdown->drawdown_plot;
       fl_set_xyplot_overlay_type(ddgraph, 1, FL_POINTS_XYPLOT);
       fl_set_xyplot_xbounds(ddgraph, 0, log10(10000.0));
       fl_set_xyplot_ybounds(ddgraph, 0, log10(10000.0));
       fl_set_xyplot_ytics(ddgraph, 8, 0);
       fl_set_xyplot_symbolsize(ddgraph, 4);
       //
       //   Draw the Parity Line
       //
       Xp[0] = 0.0; Yp[0] = 0.0;
       Xp[1] = log10(3000.0); Yp[1] = log10(3000.0*tan(AlphaRad));
       fl_set_xyplot_data(ddgraph, Xp, Yp, 2, "",
                "Enemy Deliverable Warheads","U.S. Deliverable Warheads");
       fl_add_xyplot_text(ddgraph, Xp[1], Yp[1],
                "Parity Line", FL_ALIGN_LEFT, FL_BLACK);
       //
       //   Draw the Parity Point and Offset Point
       //
       Xo[0] = log10(ParityPointX); Yo[0] = log10(ParityPointY);
       Xo[1] = log10(OffsetPointX); Yo[1] = log10(OffsetPointY);
       fl_add_xyplot_overlay(ddgraph, 1, Xo, Yo, 2, FL_GREEN);
       fl_add_xyplot_text(ddgraph, Xo[0], Yo[0],
                          "Parity Point", FL_ALIGN_LEFT, FL_GREEN);
       fl_add_xyplot_text(ddgraph, Xo[1], Yo[1],
                          "Offset Point", FL_ALIGN_RIGHT, FL_GREEN);
       break;

     case 2:
       rdrgraph = fd_drawdown->red_diminish_plot;
       fl_set_xyplot_overlay_type(rdrgraph, 1, FL_DASHED_XYPLOT);
       fl_set_xyplot_xbounds(rdrgraph, 0, log10(1000.0));
       fl_set_xyplot_ybounds(rdrgraph, 0, log10(100.0));
       fl_set_xyplot_ytics(rdrgraph, 3, 0);
     case 3:
       fl_delete_xyplot_overlay(rdrgraph, 0);
       y1[0] = log10((float)atoi(fl_get_input(fd_drawdown->no_odi[0])));
       y1[1] = log10((float)atoi(fl_get_input(fd_drawdown->no_odi[1])));
       y1[2] = log10((float)atoi(fl_get_input(fd_drawdown->no_odi[2])));
       y1[3] = log10((float)atoi(fl_get_input(fd_drawdown->no_odi[3])));
       y1[4] = log10((float)atoi(fl_get_input(fd_drawdown->no_odi[4])));
       y1[5] = log10(1.0);
       fl_set_xyplot_data(rdrgraph, xx, y1, 6, " ","No. of Warheads Delivered","Ratio");
       break;

     case 4:
       fl_delete_xyplot_overlay(rdrgraph, 1);
       y2[0] = log10((float)atoi(fl_get_input(fd_drawdown->with_odi[0])));
       y2[1] = log10((float)atoi(fl_get_input(fd_drawdown->with_odi[1])));
       y2[2] = log10((float)atoi(fl_get_input(fd_drawdown->with_odi[2])));
       y2[3] = log10((float)atoi(fl_get_input(fd_drawdown->with_odi[3])));
       y2[4] = log10((float)atoi(fl_get_input(fd_drawdown->with_odi[4])));
       y2[5] = log10(1.0);
       fl_add_xyplot_overlay(rdrgraph, 1, xx, y2, 6, FL_RED);
       break;

    case 5:
       bdrgraph = fd_drawdown->blue_diminish_plot;
       //fl_set_xyplot_xscale(bdrgraph, FL_LOG, 10.0);
       //fl_set_xyplot_yscale(bdrgraph, FL_LOG, 10.0);
       fl_set_xyplot_xbounds(bdrgraph, 0, log10(1000.0));
       fl_set_xyplot_ybounds(bdrgraph, 0, log10(100.0));
       fl_set_xyplot_ytics(bdrgraph, 3, 0);
     case 6:
       fl_delete_xyplot_overlay(bdrgraph, 0);
       y3[0] = log10((float)atoi(fl_get_input(fd_drawdown->triad[0])));
       y3[1] = log10((float)atoi(fl_get_input(fd_drawdown->triad[1])));
       y3[2] = log10((float)atoi(fl_get_input(fd_drawdown->triad[2])));
       y3[3] = log10((float)atoi(fl_get_input(fd_drawdown->triad[3])));
       y3[4] = log10((float)atoi(fl_get_input(fd_drawdown->triad[4])));
       y3[5] = log10(1.0);
       fl_set_xyplot_data(bdrgraph, xx, y3, 6, " ","No. of Warheads Delivered","Ratio");
       break;

     case 10:
       Alpha = atof(fl_get_input(fd_drawdown->drawdown_alpha));
       //drawdownCB(NULL, 1);
       break;

     default:
       break;
   }
}

void sitmonitorCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
 
   switch (item) {
     case 0:
       break;

     case 1:
       break;

     case 2:
       break;

     default:
       break;
   }

   fl_hide_form(fd_sitmonitor->sitmonitor);
}

void statexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
 
   fl_hide_form(fd_bmcstatus->bmcstatus);
}
/*                                                                       */
/* ------------------------- Event Injector CB's ----------------------- */
/*                                                                       */
void NMDinjectCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   item = fl_get_menu(fd_bmc3->bmc_inject) - 1;
 
   switch (item) {
     case 0:
       ITELnext();
       intel_count = intel_count + 1;
       Alarm(ALARM_INTEL, "Evidence Report", PRIORITY_LOW, "NewEvid.lock");
       if (intel_count > 4) {
         SC_update_def(defcon-1);
         intel_count = 0;
       }
       break;

     case 1:
       if (strcmp(getenv("GPDADOMAIN"), "nmd") == 0)
         Alarm(ALARM_JCSWARN, "JCS Order", PRIORITY_MED, "JCS-Order-1437.5");
       else
         Alarm(ALARM_JCSWARN, "JCS Order", PRIORITY_MED, "IDS-Attack-1234.5");
       break;

     case 2:
       Alarm(ALARM_LAUNCH, "Potential Event", PRIORITY_HI, "*");
       break;

     case 3:
       Alarm(ALARM_TRACK, "Track Report", PRIORITY_HI, "*");
       break;

     case 4:

       break;

     default:
       break;
   }
}

void nothingCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

/* --------------------------------------------------------------------- */

extern "C" {
void SC_update_gvt(float newgvt)
{
   if (!BMCinited) return;

   sprintf(scline, "%10.2f", newgvt);
   fl_set_object_label(fd_bmc3->simtime, scline);
}

void SC_update_def(int newdef)
{
FL_COLOR        color;

   if (!BMCinited) return;

   color = FL_YELLOW;
   if (newdef < 1) color = FL_RED;
   if (newdef > 4) color = FL_GREEN;
 
   sprintf(scline, "%d", newdef);
   if (OperQuery("Situation Monitor", "DEFCON Level", " ", scline) != 0) {
      fl_set_input(fd_bmc3->defcon_val, scline);
      fl_set_object_color(fd_bmc3->defcon_label, color, color);
      defcon = newdef;
      update_SC = OP_DEFCON;
   }
}

void SC_update_rp(int newrp)
{
FL_COLOR        color;

   if (!BMCinited) return;

   color = FL_GREEN;
   if (newrp < 2) color = FL_RED;
 
   sprintf(scline, "%d", newrp);
   if (OperQuery("Situation Monitor", "Readiness Posture", " ", scline) != 0) {
      fl_set_input(fd_bmc3->rp_val, scline);
      fl_set_object_color(fd_bmc3->rp_label, color, color);
      Rposture = newrp;
      update_SC = OP_RP;
   }
}

void SC_update_dea(char *newdea)
{
   if (!BMCinited) return;

   if (OperQuery("Situation Monitor", "DEA", " ", newdea) != 0) {
      fl_set_input(fd_bmc3->dea_val, newdea);
      strcpy(chdea, newdea);
      update_SC = OP_DEA;
   }
}

void SC_update_roe(char *newroe)
{
   if (!BMCinited) return;

   if (OperQuery("Situation Monitor", "Rules of Engagement", " ", newroe) != 0) {
      fl_set_object_label(fd_bmc3->roe_val, newroe);
      fl_redraw_form(fd_bmc3->bmc3);                             // Clean up any overdraws
      strcpy(chroe, newroe);
      update_SC = OP_ROE;
   }
}

void SC_update_plan(char *plan)
{
   if (!BMCinited) return;

   if (OperQuery("Situation Monitor", "Battle Plan", " ", plan) != 0) {
      fl_set_object_label(fd_bmc3->plan_val, plan);
      strcpy(chplan, plan);
      update_SC = OP_BP;
   }
}

void SC_update_node(char *str)
{
   fl_set_object_label(fd_bmc3->node_center, str);
}

void SC_update_hist (char *str)
{
   printf("%s\n", str);
}

void SC_delete_list(int n_assignments)
{
   
}

void SC_add_list(int pos, char *listitem)
{

}

void SC_replace_list(int pos, char *listitem)
{
   fl_replace_browser_line(fd_bmctrack->trk_browser, pos+1, listitem);
}

}  // "C" linkages

#ifdef LINKSC
int SC_message(int opcode, int itemno)
{
static int       answer;
int              scn, response, pad1, pad2, pad3, pad4;
int              i, nargs;
unsigned int     xferbytes;
char             buf[2000], src[5], dst[5], dea[5];
char             *xdrbuf;
int              sp, dp;
int              u_defcon, u_posture;
char             u_roe[40], u_mission[40], u_plan[40], u_dea[40];
bool_t           ret;
int              op, opsub, zero = 0;
unsigned int     four = 4, bytecnt, xdrsize;
float            gvt, gvtret;
XDR              xdrs;
struct header    BPen, BPde;
struct SEND_ALL  BPen_body;
struct RTRN_ALL  BPde_body;
/*
 *     The Situation Monitor is active and should be sent the request
 *     --------------------------------------------------------------
 */
      u_defcon  = defcon;                        // Save
      u_posture = Rposture;                      //   the
      strcpy(u_dea, chdea);                      //     current
      strcpy(u_plan, chplan);                    //       values
      strcpy(u_mission, chmsnobj);
      strcpy(u_roe, chroe);

      response = FALSE;
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
      //
      //  Build the request body based on the decision to be made
      //
      switch (opcode) {
        case OP_INIT_SC:
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          break;

        case OP_DEFCON:                          // Update DEFCON status
          xdr_int(&xdrs, &u_defcon);
          if (PLANDBUG) printf ("DEFCON sent is %d.\n", u_defcon);
          break;

        case OP_RP:                              // Update Readiness Posture
          xdr_int(&xdrs, &u_posture);
          if (PLANDBUG) printf ("RP     sent is %d.\n", u_posture);
          break;

        case OP_DEA:                             // Update DEA
          strcpy(BPen_body.init_bp_body_struct.DEA, u_dea);
          if (PLANDBUG) printf ("DEA    sent is %s.\n", u_dea);
          break;

        case OP_ROE:                             // Update Rules of Engagement
          strcpy(BPen_body.roe_body_struct.ROE, u_roe);
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          if (PLANDBUG) printf ("ROE    sent is %s.\n", u_roe);
          break;

        case OP_MSN_OBJ:                         // Update Mission Objective
	  i = itemno;
          BPen_body.init_mo_body_struct.DEFCON_Level                   = u_defcon;
          BPen_body.init_mo_body_struct.RP                             = u_posture;
          BPen_body.init_mo_body_struct.Msn_Obj_Pms                    = MOpksuccess[i];
          BPen_body.init_mo_body_struct.Msn_Obj_Mode                   = MOmode[i];
          BPen_body.init_mo_body_struct.Msn_Obj_Withhold               = MOwithhold[i];
          BPen_body.init_mo_body_struct.Msn_Obj_Add_Bstr               = MObooster[i];
          strcpy (BPen_body.init_mo_body_struct.DEA,                   u_dea);
          strcpy (BPen_body.init_mo_body_struct.ROE,                   u_roe);
          strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Name,          MOnames[i]);
          strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Strategy,      MOstrategy[i]);
          strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Tactic,        MOtactic[i]);
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          if (PLANDBUG) printf ("MsnObj sent is %d.\n", u_mission);
          break;

        case OP_BP:                              // Update Battle Plan
          i = itemno;
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
          strcpy (BPen_body.init_bp_body_struct.DEA,                     u_dea);
          strcpy (BPen_body.init_bp_body_struct.ROE,                     u_roe);
          strcpy (BPen_body.init_bp_body_struct.BP_Name,                 BPnames[i]);
          strcpy (BPen_body.init_bp_body_struct.BP_Mode,                 BPmode[i]);
          strcpy (BPen_body.init_bp_body_struct.BP_Tgt_Val_Cut_Off,      BPcutoff[i]);
          strcpy (BPen_body.init_bp_body_struct.BP_Accept_Kill_Criteria, BPkill[i]);
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          if (PLANDBUG) printf ("BP     sent is %d.\n", u_plan);
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
          if (PLANDBUG)
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
          strcpy (BPen_body.trk_engmt_body_struct.Eng_Status,            engage.engagestat);
          strcpy (BPen_body.trk_engmt_body_struct.Track_Id,              tracks.trackid);  
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
          if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs) != 0)
             printf ("Encode failed for opcode %d .\n", BPen.opcode);
          break;

        default:
          break;
      }
      xdrsize = xdr_getpos(&xdrs);
      //
      //  Send the request to the SC and cleanup
      //
      if (PLANDBUG)
         fprintf(stderr, "Planner: %d Request Sent. (%d Bytes)\n", BPen.opcode, xdrsize);
      if (SOCKET) {
         Net_write(xdrbuf, xdrsize);
         if (PLANDBUG)
            fprintf(stderr, "Planner: waiting for response.\n");
         xferbytes = Net_read(buf, 2000);  // Wait for the response from the SC
      } else {
         translator(xdrbuf, buf, &xferbytes);
      }
      free(xdrbuf);
      xdr_destroy(&xdrs);
      if (PLANDBUG)
         fprintf(stderr, "Planner: Response Received. (%d Bytes)\n", xferbytes);
      //
      //  Extract the response header info
      //
      xdrmem_create(&xdrs, buf, (unsigned)2000, XDR_DECODE);
      decode_header (&xdrs, &BPde, &nargs);
      if (PLANDBUG)
         fprintf(stderr, "Header decoded with %d args\n", nargs);
      //
      //  Process the response from the SC
      //
      if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs) != 0)
         printf ("Decode failed for opcode %d .\n", BPde.opcode);
      switch (BPde.opcode) {
	/*
        case OP_DEFCON:                          // Update DEFCON status
          u_defcon = BPde_body.defcon_body_struct.DEFCON;
          if (PLANDBUG) printf ("DEFCON received is %d.\n", u_defcon);
          break;

        case OP_RP:                              // Update Readiness Posture
          u_posture = BPde_body.rp_body_struct.RP;
          if (PLANDBUG) printf ("RP     received is %d.\n", u_posture);
          break;

        case OP_DEA:                             // Update DEA
          strcpy(u_dea, BPde_body.dea_body_struct.DEA);
          if (PLANDBUG) printf ("DEA    received is %s.\n", u_dea);
          break;

        case OP_ROE:                             // Update Rules of Engagement
          strcpy(u_roe, BPde_body.roe_body_struct.ROE);
          if (PLANDBUG) printf ("ROE    received is %s.\n", u_roe);
          break;

        case OP_MSN_OBJ:                         // Update Mission Objective
          strcpy(u_mission, BPde_body.msn_obj_body_struct.Msn_Obj);          
          if (PLANDBUG) printf ("MsnObj received is %s.\n", u_mission);
          break;

        case OP_BP:                              // Update Battle Plan
          strcpy(u_plan, BPde_body.bp_body_struct.BP);
          if (PLANDBUG) printf ("BP     received is %s.\n", u_plan);
          break;
	*/
        case OP_RTRN_DCN:                        // Update all six
          u_defcon  = BPde_body.dcn_body_struct.DEFCON;
          u_posture = BPde_body.dcn_body_struct.RP;
          strcpy(u_dea, BPde_body.dcn_body_struct.DEA);
          strcpy(u_roe, BPde_body.dcn_body_struct.ROE);
          strcpy(u_mission, BPde_body.dcn_body_struct.Msn_Obj);
          strcpy(u_plan, BPde_body.dcn_body_struct.BP);
          if (PLANDBUG) {
             printf ("DEFCON received is %d.\n", BPde_body.dcn_body_struct.DEFCON);
             printf ("RP     received is %d.\n", BPde_body.dcn_body_struct.RP);
             printf ("DEA    received is %s.\n", BPde_body.dcn_body_struct.DEA);
             printf ("ROE    received is %s.\n", BPde_body.dcn_body_struct.ROE);
             printf ("MsnObj received is %s.\n", BPde_body.dcn_body_struct.Msn_Obj);
             printf ("BP     received is %s.\n", BPde_body.dcn_body_struct.BP);
	  }
          break;

        case OP_RTRN_ACK:                        // Don't update anything
          if (PLANDBUG) printf ("ACK    received. .\n");
          break;

        case OP_RTRN_ERR:                        // Process error return
          if (PLANDBUG) printf ("Error  received. .\n");
          break;

        default:
          break;
      }

      defcon = u_defcon;
      Rposture = u_posture;
      strcpy(chdea, u_dea);
      strcpy(chroe, u_roe);
      strcpy(chplan, u_plan);
      strcpy(chmsnobj, u_mission);

      answer = YES;
      xdr_destroy(&xdrs);

      return(answer);
}

void SC_initialize()
{
struct SEND_ALL BPen_body;
struct RTRN_ALL BPde_body;
struct header   BPen, BPde;
int             nargs_in, nargs_out;

unsigned int    xdrsize;
unsigned int    xferbytes;
char            *xdrbuf_en, *xdrbuf_de, buf[2000];
XDR             xdrs;
/*
 *     The Simulated Commander is active and should be sent the request
 *     ----------------------------------------------------------------
 */
   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);

   BPen.SrcID     = BtlP;
   BPen.DstID     = BMDC1_DR1;
   BPen.SCID      = Sim_Cmdr_ID;
   BPen.opcode    = OP_INIT_SC;
   BPen.SCactive  = Sim_Cmdr_Active;
   BPen.gvt       = 1234.5;
   BPen.reserved7 = 0;
   BPen.reserved8 = 0;
   BPen.reserved9 = 0; 
//
//  Send the Simulated Commander an Initialization Message
//  ------------------------------------------------------
//
   SC_message(OP_INIT_SC, 0);
//
//  Send the Simulated Commander all the Mission Objectives
//  -------------------------------------------------------
//
   for (i=0; i<nmissions; i++) {
      SC_message(OP_INIT_MO, i);
   }
//
//  Send the Simulated Commander all the Battle Plans
//  -------------------------------------------------
//
   for (i=0; i<nplans; i++) {
      SC_message(OP_INIT_BP, i);
   }
//
//  Send the Simulated Commander the GBI Farm status
//  ------------------------------------------------
//
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);
   BPen.opcode    = OP_INIT_WA;
   BPen.gvt       = 1235.0;
   /*
   BPen_body.wa_body_struct.Num_GBI_Farms = gbicount;
   for (i=0; i<gbicount; i++) {
      BPen_body.wa_body_struct.Farm_Val[ i][0] = gbiident[i];
      BPen_body.wa_body_struct.Farm_Val[ i][1] = gbingbis[i];
      BPen_body.wa_body_struct.Farm_Val[ i][2] = gbinhold[i];
   }
   */
   BPen_body.wa_body_struct.Num_GBI_Farms = 5;
 
   BPen_body.wa_body_struct.Farm_Val[ 0][0] = 1101;
   BPen_body.wa_body_struct.Farm_Val[ 0][1] = 6;
   BPen_body.wa_body_struct.Farm_Val[ 0][2] = 4; 
   BPen_body.wa_body_struct.Farm_Val[ 1][0] = 3101;
   BPen_body.wa_body_struct.Farm_Val[ 1][1] = 2;
   BPen_body.wa_body_struct.Farm_Val[ 1][2] = 1; 
   BPen_body.wa_body_struct.Farm_Val[ 2][0] = 3111;
   BPen_body.wa_body_struct.Farm_Val[ 2][1] = 12;
   BPen_body.wa_body_struct.Farm_Val[ 2][2] = 6; 
   BPen_body.wa_body_struct.Farm_Val[ 3][0] = 3121;
   BPen_body.wa_body_struct.Farm_Val[ 3][1] = 50;
   BPen_body.wa_body_struct.Farm_Val[ 3][2] = 25; 
   BPen_body.wa_body_struct.Farm_Val[ 4][0] = 3131;
   BPen_body.wa_body_struct.Farm_Val[ 4][1] = 80;
   BPen_body.wa_body_struct.Farm_Val[ 4][2] = 20; 
   encode_header (&xdrs, &BPen, &nargs_in);
   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);
   xdrsize = xdr_getpos(&xdrs);
   //
   //  Send the request to the SC and cleanup
   //
   if (SOCKET) {
      Net_write(xdrbuf_en, xdrsize);
      //
      //  Wait for the response from the SC
      //
      if (PLANDBUG) printf("Planner: waiting for response. Opcode %d\n", BPen.opcode);
      xferbytes = Net_read(buf, 2000);
   } else {
      translator(xdrbuf_en, buf, &xferbytes);
   }
   xdr_destroy(&xdrs);
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
   strcpy (BPen_body.db_status_body_struct.DEA,                     chdea);
   strcpy (BPen_body.db_status_body_struct.ROE,                     chroe);
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
   if (SOCKET) {
      Net_write(xdrbuf_en, xdrsize);
      //
      //  Wait for the response from the SC
      //
      if (PLANDBUG) printf("Planner: waiting for response. Opcode %d\n", BPen.opcode);
      xferbytes = Net_read(buf, 2000);
   } else {
      translator(xdrbuf_en, buf, &xferbytes);
   }
   xdr_destroy(&xdrs);

   free (xdrbuf_en);
   free (xdrbuf_de);
}
#else
int SC_message(int opcode, int itemno)
{
   return(0);
}

void SC_initialize()
{
   return;
}
#endif
