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
#include "pthread.h"
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

#include "Globals.h"
#include "isql.h"
/*
**   Include the Xforms stuff
*/
#include "forms.h"
#include "BOXforms.h"

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

/* --------------------------------------------------------------------- */  

pthread_t       threadid;
pthread_mutex_t write_lock = PTHREAD_MUTEX_INITIALIZER;
 
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
//
//   Run-time forms stuff
//
FD_bmc3         *fd_bmc3;
FD_bmcbrowse    *fd_bmcbrowse;
 
Window          mainwinID;
Window          thiswin;
FL_Coord        mainwinX, mainwinY, mainwinH, mainwinW;
FL_OBJECT       *ddgraph, *rdrgraph, *bdrgraph;
FL_OBJECT       *ipipm;
int             timeoutID;

/* --------------------------------------------------------------------- */

void IdleCB(int tid, void *stuff);
int  IdleWP(XEvent *ev, void *data);
void ConfigTopLevel(int mode);

extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();

void Net_init(int port, char *host);
void Net_write(char *buf, int bytes);
int  Net_read(char *buf, int bufsize);
int  CIC_init(int port, char *host);
void CIC_write(int sock, char *buf, int bytes,
               struct sockaddr_in *client, socklen_t Lclient);
int  CIC_read(int sock, char *buf, int bufsize,
              struct sockaddr_in *client,  socklen_t *Lclient);
void Net_close(int sockid); 


extern void     BOXinit();
extern void     BOXshow(int xpos, int ypos, int width, int height, Window winid);

extern "C" int pthread_create(pthread_t *, const pthread_attr_t *, void * (*)(void *), void *);
void *CIC_Thread(void *inbuf);
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */

void BOXinit()
{
float           ltime, rdist;
char            *str;
char            xpmfile[128];

time_t          clock;
struct tm       *loctime;

   time(&clock);
   loctime = gmtime(&clock);
   str = asctime(loctime);

   if ((BITMAPDIR = getenv("BITMAPDIR")) == NULL) BITMAPDIR = "../BitMaps";
//
//   Initialize the 'FORMS' system and create all BMC3 forms
//
   fd_bmc3 = create_form_bmc3();
   fd_bmcbrowse  = create_form_bmcbrowse();

   fl_set_browser_fontsize (fd_bmcbrowse->bmc_browse, 12);
   fl_set_browser_fontstyle(fd_bmcbrowse->bmc_browse, FL_FIXED_STYLE|FL_BOLD_STYLE);

   ConfigTopLevel(0);

   wpnchartCB(NULL, 0);

   sprintf(scline, "%d", defcon);
   fl_set_input(fd_bmc3->defcon_val, scline);
   sprintf(scline, "%d", Rposture);
   fl_set_input(fd_bmc3->rp_val, scline);
   fl_set_input(fd_bmc3->dea_val, chdea);
   fl_set_object_label(fd_bmc3->roe_val, chroe);
//
//   If CIC interface wanted, initialize it
//
#ifdef LINKCIC
   CIC_init(cicport, cichost);
#endif
//
//   Initialization complete, set the main work loop
//
   BMCinited = TRUE;

   fl_set_idle_callback(IdleWP, 0);
}

void BOXshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
FILE            *fp;
FILE            *outfp;
int             i, j, k, m, n, item = 0;
int             nlevels, nnodes, nplans, mode, flags;
int             nCols;
float           Ts, Te, Weight, Tbelief, Tdisbelief;
float           Rbelief;
char            Tu;
const char      *fname;
char            szDSN[MAX_DATA_WIDTH+1] = "MySQL";
char            szUID[MAX_DATA_WIDTH+1] = "root";
char            szPWD[MAX_DATA_WIDTH+1] = "gpda";
char            szSQL[9001]             = "show databases;";
char            chname[64];
char            chclass[64];
char            chcase[32];
char            chvalue[64];
char            chfile[128];
char            chtemp[256];
Window          winid;

SQLINTEGER      nCol                            = 0;
SQLCHAR         szColumn[MAX_DATA_WIDTH+20]     = "";
SQLCHAR         szColumnName[MAX_DATA_WIDTH+1]  = "";
SQLCHAR         szHdrLine[32001]                = "";
SQLUINTEGER     nOptimalDisplayWidth            = 10;
SQLLEN          nIndicator                      = 0;
SQLCHAR         szColumnValue[MAX_DATA_WIDTH+1] = "";
SQLRETURN       nReturn;
SQLCHAR         szSepLine[32001] = "";
SQLSMALLINT     cols;
SQLLEN          nRows = 0;
SQLINTEGER      ret;
int             nUserWidth  = 0;

   if(!fl_form_is_visible(fd_bmc3->bmc3) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);

      winid = fl_prepare_form_window(fd_bmc3->bmc3,
                                     FL_PLACE_POSITION,FL_TRANSIENT, "Box Score");
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_show_form_window(fd_bmc3->bmc3);
      //fl_set_form_atclose(fd_bmc3->bmc3, BOXclose, 0);
      //StoreActiveEntry(PUPlabel);
   }
   //
}

void signoffCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_finish();

   exit(0);
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
      Thread = pthread_create(&threadid, NULL, CIC_Thread, (void *)inbuf);
      if (Thread != 0) {                // If Thread not created, just do it!
          CIC_Thread(xdrbuf);
      }
   }
#endif

   return (0);
}

#ifdef LINKCIC
void *
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
//      Alpha string   - 1 to n chars   Story/Mission/Domain
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
     //cout << "Waiting for client to connect..." << endl;
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
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void BMCnoneCB(FL_OBJECT *object, long item_no)
{
int item = 0;

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

   fl_show_object(fd_bmc3->projectname);
   fl_show_object(fd_bmc3->clocktime);
   //
   strcpy(filename, "datkrc");
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
   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Mission Area
   char MissionArea[64];
   sscanf(scline, "%s", MissionArea);
   strsub(MissionArea, '_', ' ');
   fl_set_object_label(fd_bmc3->projectname, MissionArea);
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

   do fgets(scline, 128, infp); while ((scline[0] == '#'));      // Get Status Field 4
   sscanf(scline, "%s %s %s", chlabel, chroe, chstatus);
   strsub(chlabel, '_', ' ');
   strsub(chroe, '_', ' ');
   fl_set_object_label(fd_bmc3->roe_label, chlabel);
   fl_set_object_label(fd_bmc3->roe_val, chroe);

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
   sscanf(scline, "%d %s", &n_items, chlabel);                               // Get # menu items
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
   sscanf(scline, "%d %s", &n_items, chlabel);                               // Get # menu items
   //fl_set_object_label(fd_bmc3->tool_menu, "Tools");
   for (i=0; i<n_items; i++) {
     do fgets(scline, 128, infp); while ((scline[0] == '#'));    // Get next menu item
     sscanf(scline, "%s", chlabel);                              // Get item name
     strsub(chlabel, '_', ' ');
     fl_addto_choice(fd_bmc3->sdf_weap_menu, chlabel);           // Add to menu
   }
//
//   All done, close file and leave
//
   fclose(infp);

   fl_redraw_form(fd_bmc3->bmc3);                                // Clean up any overdraws
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

void alrmactionCB(FL_OBJECT *object, long item_no)
{
int             ialarm;
INMSG           *p;

   int item = item_no;

   ialarm = alarms[alarm_count].alarm;
   strcpy(scline, alarms[alarm_count].arg);

   alarm_count = alarm_count - 1;

   if (item == 0) {
      switch (ialarm) {
        case ALARM_INTEL:
          //ITELshow(winposX, winposY, winsizW, winsizH, mainwinID, 2, scline);
          break;

        case ALARM_LIMIT:
          sprintf(bmctemp, "Packets/sec.\nthreshold exceeded on node\n\n%s", scline);
	  fl_show_messages(bmctemp);
          break;

        case ALARM_JCSWARN:
          //CBPshow(winposX, winposY, winsizW, winsizH, mainwinID, scline);    
          break;

        case ALARM_JCSEXEC:
          //CBPshow(winposX, winposY, winsizW, winsizH, mainwinID, scline);
          break;

        case ALARM_LAUNCH:
	  p = (INMSG *)new(INMSG);
	  strcpy(p->src, "MTIX");
	  strcpy(p->dst, "GPDA");
	  strcpy(p->msn, "CND");
          //DSBshow(winposX, winposY, winsizW, winsizH, mainwinID, p);
	  delete (p);
          break;

        case ALARM_TRACK:
          //TRKshow(winposX, winposY, winsizW, winsizH, mainwinID);
          BMCrun = TRUE;
          break;

        case ALARM_POTEVENT:
          break;

        case ALARM_TIMER:
          //TLEshow(winposX, winposY, winsizW, winsizH, mainwinID);
          break;

        default:
          break;
      }
   }
}

void alarmCB(FL_OBJECT *object, long item_no)
{

   alrmactionCB(NULL, 0);
}

void map2dCB(FL_OBJECT *object, long item_no)
{
int      item = 0;
int      pid;

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
       Alarm(ALARM_INTEL, "Evidence Report", PRIORITY_LOW, "NewEvid.lock");
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

void dirstatusCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;
int             i;
int             BROwinX, BROwinY, BROwinW, BROwinH;
int             winposX = 120, winposY = 120;
int             winposW = 850, winposH = 460;
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
	   fl_show_form(fd_bmcbrowse->bmcbrowse, FL_PLACE_CENTER,FL_FULLBORDER, BROlabel);
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
	   fl_show_form(fd_bmcbrowse->bmcbrowse, FL_PLACE_CENTER,FL_FULLBORDER, BROlabel);
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
         //PUPshow(winposX, winposY, winsizW, winsizH, mainwinID, pupinfo);
       }
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
}

void SC_update_def(int newdef)
{
FL_COLOR        color;

   if (!BMCinited) return;

   color = FL_YELLOW;
   if (newdef < 1) color = FL_RED;
   if (newdef > 4) color = FL_GREEN;
 
   sprintf(scline, "%d", newdef);
   fl_set_input(fd_bmc3->defcon_val, scline);
   fl_set_object_color(fd_bmc3->defcon_label, color, color);
   defcon = newdef;
}

void SC_update_rp(int newrp)
{
FL_COLOR        color;

   if (!BMCinited) return;

   color = FL_GREEN;
   if (newrp < 2) color = FL_RED;
 
   sprintf(scline, "%d", newrp);
   fl_set_input(fd_bmc3->rp_val, scline);
   fl_set_object_color(fd_bmc3->rp_label, color, color);
   Rposture = newrp;
}

void SC_update_dea(char *newdea)
{
   if (!BMCinited) return;

   fl_set_input(fd_bmc3->dea_val, newdea);
   strcpy(chdea, newdea);
}

void SC_update_roe(char *newroe)
{
   if (!BMCinited) return;

   fl_set_object_label(fd_bmc3->roe_val, newroe);
   fl_redraw_form(fd_bmc3->bmc3);                             // Clean up any overdraws
   strcpy(chroe, newroe);
}

void SC_update_plan(char *plan)
{
   if (!BMCinited) return;

   fl_set_object_label(fd_bmc3->plan_val, plan);
   strcpy(chplan, plan);
}

void SC_update_node(char *str)
{
  //fl_set_object_label(fd_bmc3->node_center, str);
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

}

}  // "C" linkages

