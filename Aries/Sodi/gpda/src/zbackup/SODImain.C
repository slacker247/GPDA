//#define SC_THREADS                         /* <--- For POSIX Threads */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <errno.h>
#ifdef SC_THREADS
#include "pthread.h"
#endif
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
#include <rpc/xdr.h>
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
/*
 *   Include the SimCmdr stuff
 */
#include "parser.H"
#include "demo_opcodes.h"
#include "demo_header.h"
#include "demo_msg_body.h"
#include "demo_players.h"
#include "demo_strings.h"

#include "forms.h"
#include "DSforms.h"
#include "dempster.h"
#include "FOGforms.h"
#include "CBPforms.h"
#include "SODIforms.h"

#define YES      1
#define NO       0
#define POLLRATE 5000

#ifdef SC_THREADS
pthread_t         threadid;
pthread_mutex_t   write_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

FILE       *INFOfp;
FILE       *TIMEfp;
FILE       *DBUGfp;
FILE       *STATfp;
char       *SIMDEBUG;
char       *SIMTIMING;
char       *SIMSTATS;
char       *SIMTEST;
int        PLAYBACK;
int        toprttr;

float      start_time;
float      tend;
float      end_time;
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
int        delay;
int        id;
int        defcon=5, Rposture=2, Simcmdr=0;
int        LaunchDetect = FALSE, WeapRelease = FALSE, WeapBusy = FALSE;
float      cycle_time;
float      GVT_time;
char       *name;
char       scline[1280];

int        Wavail, Wwitheld, Wexpended;

int        decision_total = 0;
int        decision_activ = 0;
int        decision_late  = 0;
int        decision_bad   = 0;

int        msgsock;
int        sock;
int        portid;
char       *hostid;
struct sockaddr_in server; //, client;
socklen_t  server_len;//, client_len;

int           scwindW=500, scwindH=800;
int           defcon_yesno = 1, weapons_yesno = 1;
int           YESNO;
int           MajorMenu, MinorMenu, SumMenuItem, DialogBusy = FALSE;
XtAppContext  appcontext;
Widget        toplevel, asset_shell, track_shell, callout;
Widget        threat_widget, msgtitle, track_widget, gvttime;
Widget        simcmdlevel, defconlevel, deafield, roefield;
Widget        d_total, d_active, d_late, d_bad;
Pixmap        COpixmap, redledpix;
Display       *dpy;
XtWorkProcId  spsId = 0;
XtIntervalId  timeoutid;
Boolean       trkvisible = FALSE;
GC            gc;
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

FD_bmc3              *fd_bmc3;
FD_login             *fd_login;
FD_cmocnode          *fd_cmocnode;
Window               mainwinID;
FL_Coord             mainwinX, mainwinY, mainwinH, mainwinW;

extern FD_input      *fd_input;
extern FD_FogInput   *fd_FogInput;
extern FD_cbpinput   *fd_cbpinput;

/* --------------------------------------------------------------------- */

int  SC_get_winxpos();
int  SC_get_winypos();

void Net_init(int port, char *host);
void Net_write(char *buf, int bytes, struct sockaddr_in *client, socklen_t length);
int  Net_read(char *buf, int bufsize, struct sockaddr_in *client, socklen_t *length);
void Net_close();

void DSinit();
void DSevid();
void DSload(const char *file);
void DSsave(const char *file);
//void DSprocess(char *inbuf, char *outbuf, unsigned int *xdrsize);
void DS_Algo(int, float, float);
void DS_AlgoInit();
void DS_AlgoGetResult(int ix, int iy, float results[]);

extern void CBPinit();
extern void FOGinit();
extern void CBRprocess();

extern "C" void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
extern "C" void decode_header (XDR *xdrs_de, struct header *de, int *nargs);
extern "C" int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
extern "C" int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);
extern "C" void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
extern "C" void decode_header (XDR *xdrs_de, struct header *de, int *nargs);
extern "C" int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
extern "C" int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);
//extern "C" void translator (char *messages, char *decisions, unsigned int *xdrsize);


/* --------------------------------------------------------------------- */

main(int argc, char *argv[])
{
time_t        clock;
struct tm     *loctime;
char          *str;
char          *BITMAPDIR;
char          xpmfile[128];
C_PARSER      *lanl_bp_parser;
C_BASETYPE    *parameters;

   printf("\n\n");
   printf("  --------- NMD Simulated Commander ---------\n");
#ifdef SC_THREADS
   printf("             (Threaded Version 0.1)");
#else
   printf("                 (Version 0.1)");
#endif
   printf("\n\n");

   if ((SIMDEBUG = getenv("SIMDEBUG")) != NULL)
      DBUGfp = fopen("plandbug.file", "w+");
   if ((SIMTIMING = getenv("SIMTIMING")) != NULL)
      TIMEfp = fopen("plantime.file", "w+");
   if ((SIMSTATS = getenv("SIMSTATS")) != NULL)
      STATfp = fopen("planstat.file", "w+");
   SIMTEST = getenv("SIMTEST");

   INFOfp = fopen("scdrinfo.file", "w+");

   time(&clock);
   loctime = localtime(&clock);
   str = asctime(loctime);
   fprintf(INFOfp, "SimCmdr Information output file for %s\n", str);
   fprintf(INFOfp, "Timing is     %s\n", (SIMTIMING==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Debugging is  %s\n", (SIMDEBUG==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Statistics is %s\n", (SIMSTATS==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Run Mode is   %s\n", (SIMTEST==NULL ? "playback" : "realtime"));
   fprintf(INFOfp, "\n");
   fflush(INFOfp);
/*
 *      Parse the Battle Planner parameter file
 *      ---------------------------------------
 */
   lanl_bp_parser = new C_PARSER("planner.par");
   parameters = lanl_bp_parser->get_basetype("parameters");
   portid = parameters->get_int("portid");
   hostid = parameters->get_string("hostid");
   fprintf(INFOfp, "Simulated Commander Host ......... %s\n", hostid);
   fprintf(INFOfp, "Communications Port .............. %d\n", portid);
   fprintf(INFOfp, "\n");
   fflush(INFOfp);
  
   Net_init(portid, hostid);                 // Initialize the socket connections
//
// Initialize the 'FORMS' system and create all forms
//
   fl_initialize(&argc, argv, 0, 0, 0);

   DSinit();
   CBPinit();

   fd_bmc3 = create_form_bmc3();
   fd_login = create_form_login();
   fd_cmocnode = create_form_cmocnode();

   fl_set_browser_fontsize(fd_bmc3->sim_history, 10);
   fl_set_browser_fontstyle(fd_bmc3->sim_history, FL_FIXED_STYLE|FL_BOLD_STYLE);
   //fl_set_browser_fontsize(fd_bmc3->intel_browser, FL_NORMAL_SIZE);
   //fl_set_browser_fontstyle(fd_simcmdr->intel_browser, FL_FIXED_STYLE|FL_BOLD_STYLE);
   //fl_set_browser_fontsize(fd_simcmdr->intel_source, FL_NORMAL_SIZE);
   //fl_set_browser_fontstyle(fd_simcmdr->intel_source, FL_FIXED_STYLE|FL_BOLD_STYLE);

   const char *trkmenustr = { "Engagement Summary...|Defend Task Plan...|Survey Tasl Plan...|Threat Characteristics...|Threatened Assets...|Tracks..." };
   fl_set_menu(fd_bmc3->track_menu, trkmenustr);
   fl_set_menu_item_mode(fd_bmc3->track_menu, 1, FL_PUP_GREY);
   fl_set_menu_item_mode(fd_bmc3->track_menu, 2, FL_PUP_GREY);
   fl_set_menu_item_mode(fd_bmc3->track_menu, 3, FL_PUP_GREY);
   fl_set_menu_item_mode(fd_bmc3->track_menu, 4, FL_PUP_GREY);
   fl_set_menu_item_mode(fd_bmc3->track_menu, 5, FL_PUP_GREY);

   const char *optmenustr = { "Decision Timeline...|Defense Options...|Management By Exception...|Missile Order of Battle...|Case Based Planning...|Assessments..." };
   fl_set_menu(fd_bmc3->option_menu, optmenustr);
   fl_set_menu_item_mode(fd_bmc3->option_menu, 1, FL_PUP_GREY);
   fl_set_menu_item_mode(fd_bmc3->option_menu, 2, FL_PUP_GREY);
   fl_set_menu_item_mode(fd_bmc3->option_menu, 3, FL_PUP_GREY);
   fl_set_menu_item_mode(fd_bmc3->option_menu, 4, FL_PUP_GREY);

   Wavail = 70;
   Wwitheld = 10;
   Wexpended = 16;
   wpnchartCB(NULL, 0);

   FOGinit();
//
// Popup the 'BMC3' form and get the show going
//
   fl_show_form(fd_login->login, FL_PLACE_CENTER,FL_FULLBORDER, "Login");

   fl_do_forms();
}

int
SC_get_winxpos()
{
   return mainwinX+130;
}

int
SC_get_winypos()
{
   return mainwinY+200;
}

extern "C" {
void
SC_update_gvt(float newgvt)
{
   sprintf(scline, "%f", newgvt);
   //fl_set_object_label(fd_simcmdr->gvt_text, scline);
}

void
SC_update_def(int newdef)
{
   sprintf(scline, "%d", newdef);
   fl_set_object_label(fd_bmc3->defcon_val, scline);
}

void
SC_update_rp(int newrp)
{
   sprintf(scline, "%d", newrp);
   fl_set_object_label(fd_bmc3->rp_val, scline);
}

void
SC_update_dea(char *newdea)
{
   fl_set_object_label(fd_bmc3->dea_val, newdea);
}

void
SC_update_roe(char *newroe)
{
   fl_set_object_label(fd_bmc3->roe_val, newroe);
}

void
SC_update_plan(char *str)
{
   fl_set_object_label(fd_bmc3->plan_val, str);
}

void
SC_update_node(char *str)
{
   fl_set_object_label(fd_bmc3->node_val, str);
}

void
SC_intel_time(int day, float hours)
{
char   chline[16];

   sprintf(chline, "%d", day);
   //fl_set_object_label(fd_simcmdr->intel_day, chline);
   sprintf(chline, "%f", hours);
   //fl_set_object_label(fd_simcmdr->intel_hour, chline);     
}

void
SC_intel_text(char *str)
{

  //fl_clear_browser(fd_simcmdr->intel_browser);
  //fl_addto_browser(fd_simcmdr->intel_browser, str);
}

void
SC_intel_src(char *chline)
{

  //fl_clear_browser(fd_simcmdr->intel_source);   
  //fl_addto_browser(fd_simcmdr->intel_source, chline);
}
}  // "C" linkages

/* --------------------------------------------------------------------- */

void
Net_init(int portid, char *host)
{
  //struct sockaddr_in server;
int flag = 1;


   sock = socket(AF_INET, SOCK_DGRAM, 0);           /* Create socket */
   if (sock < 0) {
     perror("opening stream socket"); exit(1);
   }

   flag = 1;
   if (ioctl(sock, FIONBIO, &flag) < 0) {
     perror("Server: ioctl "); exit(4);
   }
     
   struct hostent *hp = gethostbyname(host);
   if (hp == NULL) {
     fprintf(stderr, "%s: unknown host", host); exit(2);
   }
     
   server.sin_family      = AF_INET;
   server.sin_addr.s_addr = htonl(INADDR_ANY);
   server.sin_port        = htons(portid);
   memmove(&server.sin_addr, hp->h_addr, hp->h_length);

   if (bind(sock, (sockaddr*) &server, sizeof(server)) < 0) {
     perror("binding stream socket"); exit(1);
   }

   if (getsockname(sock, (struct sockaddr *)&server, &server_len) < 0) {
     perror("Server getsocketname "); exit(3);
    }
}

void
Net_write(char *buf, int bytes, struct sockaddr_in *client, socklen_t Lclient)
{
int i;
    
   if (sendto(sock, buf, bytes, 0, (struct sockaddr *)client, Lclient) < 0) {
     perror("writing on stream socket"); close(sock); exit(1);
   }
}

int
Net_read(char *buf, int bufsize, struct sockaddr_in *client,  socklen_t *Lclient)
{
static int iowait = FALSE, rval;
socklen_t  clientl;

   if (!iowait) {
     memset(buf, 0, bufsize);
     //cout << "Waiting for PLANNER to connect..." << endl;
     iowait = TRUE;
   }

   clientl = sizeof(struct sockaddr_in);
   rval = recvfrom(sock, buf, bufsize, 0, (struct sockaddr *)client, &clientl);
   if (rval < 0) {	
	if (errno == EWOULDBLOCK) {
          return(rval);
        } else { perror("reading stream message"); close(sock); exit(2); }
   }

   *Lclient = clientl;
   iowait = FALSE;
   if (rval == 0)
      {
	cout << "PLANNER has exited." << endl;
      }

   return(rval);
}

void
Net_close()
{
  close(sock);
}

/* -------------------------- SC Control CB's -------------------------- */
/*
void sim_dempCB(FL_OBJECT *object, long item_no)
{
int item = 0;
char inbuf[512], outbuf[512];
unsigned int xdrsize;

   fl_show_form(fd_input->input, FL_PLACE_CENTER,FL_FULLBORDER, "Dempster-Shafer Input");
   DSevid();
   DS_AlgoInit();
}

void sim_fogCB(FL_OBJECT *object, long item_no)
{
int item = 0;
char inbuf[512], outbuf[512];
unsigned int xdrsize;

   fl_show_form(fd_FogInput->FogInput,FL_PLACE_CENTER,FL_FULLBORDER,"Fog of War Control");
}
*/
void sim_caseCB(FL_OBJECT *object, long item_no)
{
int          item = 0;
char         inbuf[512], outbuf[512];
unsigned int xdrsize;
char         chsource[4][16] = { "Open Source",    "Human Intel",
                                 "Signal Intel",   "Image Intel" };
float        weight[4]       = { 2.0, 4.0, 6.0, 8.0 };
float        persistence[4]  = { 2000.0, 1000.0, 750.0, 500.0 };
/*
   SC_intel_time(12, 12.0000);

   sprintf(scline, "%-16s   %10.4f   %10.4f\n%-16s   %10.4f   %10.4f\n%-16s   %10.4f   %10.4f\n",
           chsource[0], weight[0], persistence[0],
           chsource[1], weight[1], persistence[1],
           chsource[2], weight[2], persistence[2]);
   SC_intel_src(scline);

   sprintf(scline, "%s\n%s\n%s", "China fires short-range ballistic missile",
                                 "over Taiwan. US, Taiwan, Japan, and South",
                                 "Korea issue strongly worded protests");
   SC_intel_text(scline);
*/
   CBRprocess();
}

void trackmenuCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   item = fl_get_menu(fd_bmc3->track_menu);

   switch (item) {

     case 6:
       fl_hide_form(fd_login->login);
       fl_show_form(fd_cmocnode->cmocnode, FL_PLACE_CENTER,FL_FULLBORDER, "Position");
       break;

     default:
       break;
   }
}

void optionmenuCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   item = fl_get_menu(fd_bmc3->option_menu);

   switch (item) {
     case 5:
       fl_set_form_position(fd_cbpinput->cbpinput, SC_get_winxpos(), SC_get_winypos() );
       fl_set_form_size(fd_cbpinput->cbpinput, 850, 460);
       fl_show_form(fd_cbpinput->cbpinput, FL_PLACE_POSITION,FL_NOBORDER, "Planning Input");
       fl_raise_form(fd_cbpinput->cbpinput);
       break;

     case 6:
       fl_set_form_position(fd_input->input, SC_get_winxpos(), SC_get_winypos() );
       fl_set_form_size(fd_input->input, 850, 460);
       fl_show_form(fd_input->input, FL_PLACE_POSITION,FL_NOBORDER, "Dempster-Shafer Input");
       fl_raise_form(fd_input->input);
       DSevid();
       DS_AlgoInit();
       break;

     default:
       break;
   }
}

void loginCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   switch (item) {

     case 0:
       fl_hide_form(fd_login->login);
       fl_show_form(fd_cmocnode->cmocnode, FL_PLACE_CENTER,FL_FULLBORDER, "Position");
       break;

     default:
       break;
   }
}

void positionCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   switch (item) {

     case 0:
       fl_hide_form(fd_cmocnode->cmocnode);
       mainwinID = fl_show_form(fd_bmc3->bmc3, FL_PLACE_CENTER,FL_FULLBORDER, "BMC3");
       fl_get_winsize(mainwinID, &mainwinW, &mainwinH);
       fl_get_winorigin(mainwinID, &mainwinX, &mainwinY);
       fl_set_app_mainform(fd_bmc3->bmc3);
       //fprintf(stderr, "Window parms are %d %d %d %d\n", mainwinX, mainwinY, mainwinW, mainwinH);
       break;

     default:
       break;
   } 
}

void menuCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   switch (item) {

     case 0:
       fl_hide_form(fd_cmocnode->cmocnode);
       fl_show_form(fd_bmc3->bmc3, FL_PLACE_CENTER,FL_FULLBORDER, "BMC3");
       break;

     default:
       break;
   } 
}

void wpnchartCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   fl_clear_chart(fd_bmc3->wpn_chart);

   fl_add_chart_value(fd_bmc3->wpn_chart, (double)Wavail, "Available", FL_GREEN);
   fl_add_chart_value(fd_bmc3->wpn_chart, (double)Wwitheld, "Withheld", FL_YELLOW);
   fl_add_chart_value(fd_bmc3->wpn_chart, (double)Wexpended, "Expended", FL_RED);
}

void missummaryCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

void warfogCB(FL_OBJECT *object, long item_no)
{
int item = item_no;

   switch (item) {
     case 0:
       fl_set_form_position(fd_FogInput->FogInput, SC_get_winxpos(), SC_get_winypos() );
       fl_set_form_size(fd_FogInput->FogInput, 850, 460); 
       fl_show_form(fd_FogInput->FogInput, FL_PLACE_POSITION,FL_NOBORDER, "Fog of War");
       fl_raise_form(fd_FogInput->FogInput);
       break;

     default:
       break;
   }
}

void actwindowsCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

void lockCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   exit(0);
}

void signoffCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   exit(0);
}

void nothingCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

void intel_browserCB(FL_OBJECT *object, long item_no)
{
int item = 0;
 
}
