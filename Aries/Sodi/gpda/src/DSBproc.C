
/************ To do:
- update browser after add, so ordered same
- there's no call to DS_saveConfig
- merge:
    - DS_AlgoMergeTrees doesn't know DB yet
    - leave merge in att cls to merge a tree with self
        tree1 = DSBnet;
        tree2(fname);
        DSBnet.merge(tree2)
    - could add merge to algo to merge 2 trees to get a 3rd
        tree1 = DSBnet;
        tree2(fname);
        DSBnet = merge (tree1, tree2)
************/

// To do now:
// ----------
// test delete from db
// test change rec in db


/* -------------------------------- Includes --------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <signal.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#include <sys/ioctl.h>
#ifdef SC_THREADS
#include "pthread.h"
#endif
/*
**   Include the network/socket stuff
*/
#include <sys/times.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "Globals.h"
#include "isql.h"
/*
**   Include the Xforms & FOW stuff
*/
#include "messages.H"
#include "forms.h"
#include "DSBforms.h"
#include "DSBproc.h"
#include "DSBalgo.h"
#include "DSBevidence.h"
#include "FOWtypes.h"
#include "H3Dproc.H"
#include "savedNet.h"
#include "stacked.h"

/* --------------------------------- Defines --------------------------------- */

#define SIGCHILD    SIGCLD

#define YES         1
#define NO          0
#define POLLRATE    5000
#define PUSHED      1
#define MAXLEVELS   6
#define MAXNODES    6

#define NoBP        0

#define FL_SALMON   FL_FREE_COL1+2           // Define a light red color
#define FL_SPRING   FL_FREE_COL1+3           // Define a spring green color
#define FL_GOLD     FL_FREE_COL1+4           // Define a light yellow


/* -------------------------------- Typedefs --------------------------------- */

typedef struct {
  char          ChType[40];
  char          filebase[128];
} DSBTYPE;

typedef struct {
  float         belief;
  float         disbelief;
  float         old_belief;
  float         old_disbelief;
  double        threshold;
  double        cutoff;
  double        dropdead;
  char          extern_name[32];
} NODEINFO;

typedef struct LEXICAL {
  float         lower;
  float         upper;
  char          desc[32];
};

typedef struct {
  int           Entity_ID;
  char          Entity_Name[32];
  float         Latitude;
  float         Longitude;
  float         Altitude;
  float         Belief;
  float         Disbelief;
  int           PDU_Count;
  void          *next;
  void          *previous;
} LISTNODE;

/* -------------------------------- Globals----------------------------------- */

int             DSBwinX, DSBwinY;
int             DSBx, DSBy;
int             DSBw = 850, DSBh = 460;
int             DSBwinW, DSBwinH;
int             DSBinited = 0;
int             DSBframe  = -1;
int             DSBbatch  = FALSE;
int             numStories = 0;
int             NetMulti = FALSE;
char            *DSBitMaps;
char            DSBlabel[32];
char            DSBFileBase[32];
char            stories[20][128];
char            storyPaths[20][128];
Window          DSBwid = 0;
Window          DSBwinid;
unsigned long long int Frequency;
/*
**   Dempster-Shafer belief network stuff
*/
DSBTYPE         dsbtypes[20];
NODEINFO        nodeinfo[MAXNODES][MAXLEVELS];
struct assesstype currentype;
int             curr_assess;                 // Current assessment
int             discrete = FALSE;
int             NetVisible = FALSE;          // Set if Network screen is visible
int             ExternNets = TRUE;           // Set if we want to handle external networks
int             TableView = FALSE;           // Set if Table View dialog is showing
int             DiffBP = FALSE;              // Set if BP differentiation is wanted in 3D view
int             Winput = TRUE;               // Set if adding input link weights
int             PathNum = 1;                 // Path being analyzed
int             Nlevels;                     // No. of levels
int             Nnodes[MAXLEVELS+1];         // No. of nodes at each level
int             BestNode;                    // Node No. of best assessment
INMSG           inmsg;                       // Message field storage
INMSG           *p_inmsg;                    // Pointer to input message
/*
**   Evidence stuff
*/
DSevidence      ev;
LISTNODE        *first_node = NULL;          // Pointer to first node in queue
LISTNODE        *last_node = NULL;           // Pointer to last node in queue
LISTNODE        *curr_node = NULL;           // Pointer to current node being processed
int             n_nodes = 0;                 // No. of nodes in queue
int             EvidModified = FALSE;        // Set if evidence has been modified
int             curr_evid = 0;               // Current piece of evidence being processed
int             time_unit = 1;               // Evidence time units ('S', 'M', 'H', 'D')
int             case_id = 1111;              // Evidence case identifier
int             browser_line = 0;            // Evidence browser line being processed
int             browser_mod  = 0;            // 0=> 'add', 1=> 'modify'
int             nsteps = 24*10;              // 10 times per hour (= every 6 minutes)
float           plause;                      // Evidence plausibility (1.0-belief);
float           disbelief;                   // Evidence disbelief
float           startT = 0.0;                // Other evidence times
char            startTime[32], endTime[32];
char            evid_lab1[8], evid_lab2[8], evid_lab3[8];
char            NetBasename[128];
/*
**   Story stuff
*/
int             StoryModified = FALSE;       // Set if evidence has been modified
char            *storyName = NULL;
/*
**   Graph & chart stuff
*/
int             osteps;
float           Yb[240], Yp[240], Xt[240], Xover2[3], Yover2[3], Xover3[2], Yover3[2];
float           Xtime[MAXLEVELS+1][8][250];
float           Ybelief[MAXLEVELS+1][8][250];
float           Yplause[MAXLEVELS+1][8][250];
float           YbOld[250];
float           YpOld[250];
float           XtOld[250];
//
const char      ChartLabel[6][2] = { "1", "2", "3", "4", "5", "6" };
FL_COLOR        ChartColor[6] = { FL_SLATEBLUE, FL_DARKCYAN, FL_MAGENTA,
				  FL_DARKGOLD, FL_BLACK, FL_WHEAT };
/*
**   Screen stuff
*/
char            dsbtemp[1024];
char            chtemp[128];
char            dsline[1280];
char            chTunit, chnone;
char            chblank[2];
char            chsource[32];
char            chdescript[128];
/*
**   Edit stuff
*/
int             OverrideLog = FALSE;
int             AutoDeltas  = FALSE;
int             EdRow = 1;
int             EdCol = 0;
int             LinkChange = FALSE;
int             LinkConstrain = DS_Existing_Bounded_Constraint;
int             LinkRange = 6;
float           LinkLearn = 0.5;
int             EditRow, EditCol;
float           EditBelief, EditDisbelief, EditTime;
float           CopyBelief = 0.0;
float           CopyDisbelief = 0.0;
int             CopySelected = FALSE;
int             LexCount = 9;
LEXICAL         LexTable[9] =  { { 0.00,  0.01,  "Impossible"  },
				 { 0.01,  0.05,  "V.Unlikely" },
				 { 0.05,  0.20,  "Low Chance"  },
				 { 0.20,  0.38,  "Sm. Chance"  },
				 { 0.38,  0.60,  "May"         },
				 { 0.60,  0.79,  "Some Chance" },
				 { 0.79,  0.95,  "Good Chance" },
				 { 0.95,  0.99,  "Very Likely" },
				 { 0.99,  1.01,  "Certain"     } };
static GC       canvasGC;
/*
**   Explanation stuff
*/
int             WxFrame = 0;
int             WxCount = 6;
int             WxFiles = 1;
int             WxDelay = 1000;
int             WxStop = 0;
char            WxFormat[64];
char            xpmfiles[10][8] = { "0.xpm", "1.xpm", "2.xpm", "3.xpm", "4.xpm",
                                    "5.xpm", "6.xpm", "7.xpm", "8.xpm", "9.xpm" };
static FLIMAGE_SETUP   mysetup;
/*
**   Plan stuff
*/
float           DSBEstScore[20], DSBActScore[20], DSBWorth[20];
/*
**   SQL stuff
*/
int             DSBMYSQL = 0;
extern int      bHTMLTable;
extern int      bBatch;
extern int      cDelimiter;
extern int      bColumnNames;
extern SQLHSTMT hStmt;
SQLHENV         DSBhEnv      = 0;
SQLHDBC         DSBhDbc      = 0;
/*
**   Form stuff
*/
FD_dsevedit     *fd_dsevedit;
FD_budgraph     *fd_budgraph;
FD_network      *fd_network;
FD_assessadd    *fd_assessadd;
FD_dshelpevid   *fd_dshelpevid;
FD_dsexplain    *fd_dsexplain;
FD_dsstory      *fd_dsstory;
FD_dsnetedit    *fd_dsnetedit;
FD_netednode    *fd_netednode;
FD_netedfuse    *fd_netedfuse;
FD_netedlink    *fd_netedlink;
FD_neteditor    *fd_neteditor;
FD_budtable     *fd_budtable;
FD_dsbstats     *fd_dsbstats;
FD_dsbstory     *fd_dsbstory;
FD_netsnap      *fd_netsnap;
FD_dsbhelp      *fd_dsbhelp;
FD_completion   *fd_completion;
FD_options      *fd_options;
FD_dsb_opt_ctl  *fd_dsb_opt_ctl;
FD_dsb_opt_tim  *fd_dsb_opt_tim;
FD_dsbplan      *fd_dsbplan;
FD_storyBrowser *fd_storyBrowser;
FD_storygraph   *fd_storygraph;
FL_FORM         *fd_storyBUDs;
FL_FORM         *fd_dsbuform1;

FL_OBJECT       *ipm, *npm, *apm, *hpm;
FL_OBJECT       *graph;
Pixmap          LCDigit[10];

float           SrcX[10], SrcY[10], SrcW[10], SrcH[10];
float           Box1X, Box1Y, Box1W, Box1H;
float           HypX[10], HypY[10], HypW[10], HypH[10];
float           Box2X, Box2Y, Box2W, Box2H;
float           ConX[10], ConY[10], ConW[10], ConH[10];
/*
int             n_lines1, n_lines2;
short           LHindex1[100], RHindex1[100];
short           LHindex2[100], RHindex2[100];
float           Slope1[100], Intercept1[100];
float           Slope2[100], Intercept2[100];
*/
/* ----------------------------- Global Entries ------------------------------ */

void DSBinit();
void DSBshow(int xpos, int ypos, int w, int h, Window winid, INMSG *p);
void showStoryBrowser();
void DSBexitCB(FL_OBJECT *ob, long data);
int  DSBclose(FL_FORM *form, void *data);
int  DSBevid(char *chmsn, char *buf);

/* ----------------------------- Local Prototypes ---------------------------- */

void DSinit(int importData, char *filename, char *tablename);
void DSload(char *filename);

void StopLight();
void PlotOldData(int item);
void UpdateNet(int item);
void netdrawCB(FL_OBJECT *object, long item_no);
int  IMGclose(FL_FORM *form, void *data);
void draw_net();
int  DSBexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);
int  DSBbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);
//int buttonpressCB(FL_OBJECT *ob, int event, FL_Coord mx, FL_Coord my, int key, void *raw);
void create_form_dsbuform1(void);
void filenameToTablename(char[], char[]);
void saveNet();
void DSPshow(int xpos, int ypos, int width, int height, Window mainwinID);

/* ------------------------------- Externals --------------------------------- */

extern void     EXPLinit();
extern void     EXPLshow(int xpos, int ypos, int width, int height, Window winid,
                         int mode, char *fname);
extern void     FOWinit();
extern void     FOWshow(int xpos, int ypos, int width, int height, Window winid);
extern void     H3Dinit();
extern void     H3Ddata(int ix, int iy, float table[36][36], H3DINFO info);
extern void     H3Dshow(int xpos, int ypos, int width, int height, Window winid);
extern "C" void spath_(int &J0, int &Ke, int &n_nodes, int icols[], int irows[], int w[]);

extern int      EraseActiveEntry(char *text);
extern int      StoreActiveEntry(char *text);
extern int      FinishUp();

extern __inline__ unsigned long long int rdtsc()
{
unsigned long long int x;

   __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
   return x;
}

/*                                                                             */
/* --------------------------------------------------------------------------- */
/*                 M A I N   P R O G R A M   S T A R T S   H E R E             */
/* --------------------------------------------------------------------------- */
/*                                                                             */
void DSBinit()
{
FILE            *typefp;
int             i, j, level;
int             num_types;                   // No. of assessment types
char            chtemp[128];
time_t          timenow;
struct tm       *loctime;
unsigned long long int Scycles;

   if (DSBinited) return;                    // Should only be called once


   char* str = getenv("DS_USE_MYSQL");
   if (str != NULL)
     sscanf(str, "%d", &DSBMYSQL);


   Scycles = rdtsc();                        // Determine cycles
   sleep(1);                                 //   per second
   Frequency = rdtsc()-Scycles;
   //fprintf(stderr, "Frequency is %llu cycles/sec\n", Frequency);
//
//   Create all the forms
//   --------------------
//
   fd_dsevedit     = create_form_dsevedit();
   fd_budgraph     = create_form_budgraph();
   fd_network      = create_form_network();
   fd_assessadd    = create_form_assessadd();
   fd_dshelpevid   = create_form_dshelpevid();
   fd_dsexplain    = create_form_dsexplain();
   fd_dsstory      = create_form_dsstory();
   fd_dsnetedit    = create_form_dsnetedit();
   fd_netednode    = create_form_netednode();
   fd_netedfuse    = create_form_netedfuse();
   fd_netedlink    = create_form_netedlink();
   fd_neteditor    = create_form_neteditor();
   fd_budtable     = create_form_budtable();
   fd_dsbstats     = create_form_dsbstats();
   fd_dsbstory     = create_form_dsbstory();
   fd_netsnap      = create_form_netsnap();
   fd_dsbhelp      = create_form_dsbhelp();
   fd_completion   = create_form_completion();
   fd_dsbplan      = create_form_dsbplan();
   fd_storyBrowser = create_form_storyBrowser();
   fd_storygraph   = create_form_storygraph();
 
   fd_options = create_form_options();
   fd_dsb_opt_ctl = create_form_dsb_opt_ctl();
   fd_dsb_opt_tim = create_form_dsb_opt_tim();
   fl_addto_tabfolder(fd_options->folder, "Control", fd_dsb_opt_ctl->dsb_opt_ctl);
   fl_addto_tabfolder(fd_options->folder, "Timing",  fd_dsb_opt_tim->dsb_opt_tim);
//
//   Define some new colors
//   ----------------------
//
   fl_mapcolor(FL_SALMON, 255, 140, 105);
   fl_mapcolor(FL_SPRING, 000, 255, 127);
   fl_mapcolor(FL_GOLD,   238, 238, 000);
//
//   Define the look of the browsers
//   -------------------------------
//
   fl_set_browser_fontsize(fd_dsevedit->input_browser, FL_SMALL_SIZE);
   fl_set_browser_fontstyle(fd_dsevedit->input_browser, FL_FIXED_STYLE);
   fl_set_browser_fontsize(fd_dsstory->story_details, FL_NORMAL_SIZE);
   fl_set_browser_fontstyle(fd_dsstory->story_details, FL_FIXEDBOLD_STYLE);
   fl_set_browser_fontsize(fd_dsstory->story_evidence, FL_NORMAL_SIZE);
   fl_set_browser_fontstyle(fd_dsstory->story_evidence, FL_FIXEDBOLD_STYLE);
   fl_set_browser_fontsize(fd_dsbplan->pup_log, FL_NORMAL_SIZE); 
   fl_set_browser_fontstyle(fd_dsbplan->pup_log, FL_FIXEDBOLD_STYLE);
   fl_set_browser_fontsize(fd_dsbplan->pup_strategy, FL_SMALL_SIZE); 
   fl_set_browser_fontstyle(fd_dsbplan->pup_strategy, FL_FIXEDBOLD_STYLE);
   fl_set_browser_fontsize(fd_dsbhelp->dsb_help, FL_MEDIUM_SIZE); 
//
//   Do other misc initializations
//   -----------------------------
//
   fl_set_input_cursor_visible(fd_dsbplan->pup_shell, FALSE);

   for (level=0; level<6; level++) {
     fl_hide_object(fd_network->level_label[level]);
     fl_hide_object(fd_network->net_toomany[level]);
     for (i=0; i<6; i++) {
       fl_hide_object(fd_network->directin[i][level]);
       fl_hide_object(fd_network->override[i][level]);
       fl_hide_object(fd_network->externet[i][level]);
       fl_hide_object(fd_network->node_label[i][level]);
       fl_hide_object(fd_network->node_chart[i][level]);
       fl_hide_object(fd_network->bestpath[i][level]);
       fl_hide_object(fd_network->worstpath[i][level]);
       fl_hide_object(fd_network->leastpath[i][level]);
     }
   }
   //
   fl_hide_object(fd_dsstory->story_sort);
   if (DSBMYSQL)
     fl_deactivate_object(fd_dsstory->saveBtn);
   //
   //fl_set_menu_item_mode(fd_network->menu_file, 4, FL_PUP_GREY);
   //fl_set_menu_item_mode(fd_network->menu_file, 5, FL_PUP_GREY);
   //
   fl_clear_choice(fd_assessadd->belief_by_name);
   fl_clear_choice(fd_assessadd->disbelief_by_name);
   for (i=0; i<LexCount; i++) {
     fl_addto_choice(fd_netednode->netedbelief, LexTable[i].desc);
     fl_addto_choice(fd_netednode->neteddisbelief, LexTable[i].desc);
     fl_addto_choice(fd_netedfuse->netedfuseB, LexTable[i].desc);
     fl_addto_choice(fd_netedfuse->netedfuseD, LexTable[i].desc);
     fl_addto_choice(fd_assessadd->belief_by_name, LexTable[i].desc);
     fl_addto_choice(fd_assessadd->disbelief_by_name, LexTable[i].desc);
   }
   //
   fl_add_canvas_handler(fd_network->net_canvas, Expose,      DSBexposeCB, 0);
   fl_add_canvas_handler(fd_network->net_canvas, ButtonPress, DSBbuttonCB, 0);
   //
   flimage_enable_xpm();
   flimage_enable_gif();
   flimage_enable_bmp();
   //
   EXPLinit();
   H3Dinit();
//
//   Do non-forms initialization
//   ---------------------------------
//
   strcpy(DSBlabel, "Assess-Network");
   //
   if ((DSBitMaps = getenv("BITMAPDIR")) == NULL) DSBitMaps = "../BitMaps";
   //
   first_node = (LISTNODE *)new(LISTNODE);
   first_node->previous = NULL;
   first_node->next = NULL;
   curr_node  = first_node;
   //
   DSBinited = TRUE;
   /*
   if ((typefp = fopen("GPDAusage.log", "a")) != NULL) {
     time(&timenow);
     loctime = gmtime(&timenow);
     fprintf(typefp, "DSB[%s]: Algorithm initialization complete at %s",
	     getlogin(), asctime(loctime));
     fclose(typefp);
   }
   */
   return;
}

int DSBclose(FL_FORM *form, void *data)
{
   DSBexitCB(NULL, 0);

   return(0);
}

void DSBshow(int xpos, int ypos, int width, int height, Window mainwinID, INMSG *p)
{
FILE            *typefp;
int             i, j, k, num_types, num_sets;
char            szDSN[MAX_DATA_WIDTH+1]         = "MySQL";
char            szUID[MAX_DATA_WIDTH+1]         = "root";
char            szPWD[MAX_DATA_WIDTH+1]         = "gpda";

   if (getenv("GPDAONENET") == NULL)
     NetMulti = TRUE;
   else
     NetMulti = FALSE;

   DSBbatch = FALSE;
   //
   fl_clear_menu(fd_network->menu_mission);
   //
   p_inmsg = (INMSG *)p;
   if (p != NULL) {
     strcpy(inmsg.src, p->src);
     strcpy(inmsg.dst, p->dst);
     strcpy(inmsg.msn, p->msn);
     //
     //   Set the only Assessment type to that specified in message
     //
     j = 0;
     num_types = 1;
     strcpy(dsbtypes[j].ChType, p->msn);
     strsub(dsbtypes[j].ChType, '_', ' ');
     fl_addto_menu(fd_network->menu_mission, dsbtypes[j].ChType);
     strcpy(dsbtypes[j].filebase, p->msn);
     strsub(dsbtypes[j].filebase, ' ', '_');

     if (NetMulti) {
       strcpy(dsbtypes[j].filebase, p->msn);
       strsub(dsbtypes[j].filebase, ' ', '_');
     } else {
       typefp = fopen("DSBsingle.dat", "r");
       do fgets(dsbtemp, 128, typefp); while (dsbtemp[0] == '#');    // Read # evidence sets
       sscanf(dsbtemp, "%d %s", &num_sets, NetBasename);
       strcpy(dsbtypes[j].filebase, NetBasename);
       fclose(typefp);
     }

     if (getenv("GPDABATCH") != NULL) DSBbatch = TRUE;
   } else {
     if (DSBMYSQL) {
       //
       //   Open the Database for this Domain
       //
       if ( !OpenDatabase( &DSBhEnv, &DSBhDbc, szDSN, szUID, szPWD ))
         exit(-1);
     }
     if (NetMulti) {
       //
       //   Load the initial Assessment types from initialization file
       //
       typefp = fopen("DSBinit.dat", "r");
       do fgets(dsbtemp, 128, typefp); while (dsbtemp[0] == '#');    // Read # assessments
       sscanf(dsbtemp, "%d", &num_types);
       k = 0;
       for (j=0; j<num_types; j++) {
	 do fgets(dsbtemp, 128, typefp); while (dsbtemp[0] == '#');
	 //
	 //   Read assessment type and file base names
	 //
	 sscanf(dsbtemp, "%s %s", dsbtypes[k].ChType, dsbtypes[k].filebase);
	 // When using the database, don't allow * in the mission menu.
	 if (!(DSBMYSQL && (!strcmp(dsbtypes[k].filebase, "*"))))
	   {
	     strsub(dsbtypes[j].ChType, '_', ' ');
	     fl_addto_menu(fd_network->menu_mission, dsbtypes[j].ChType);
	     k++;
	   }
       }
     } else {
       typefp = fopen("DSBsingle.dat", "r");
       do fgets(dsbtemp, 128, typefp); while (dsbtemp[0] == '#');    // Read # evidence sets
       sscanf(dsbtemp, "%d %s", &num_sets, NetBasename);
       k = 0;
       for (j=0; j<num_sets; j++) {
	 do fgets(dsbtemp, 128, typefp); while (dsbtemp[0] == '#');
	 //
	 //   Read ID # and evidence set name
	 //
	 sscanf(dsbtemp, "%d %s", &i, dsbtypes[k].ChType);
	 strcpy(dsbtypes[k].filebase, NetBasename);
	 // When using the database, don't allow * in the mission menu.
	 if (!(DSBMYSQL && (!strcmp(dsbtypes[k].filebase, "*"))))
	   {
	     strsub(dsbtypes[j].ChType, '_', ' ');
	     fl_addto_menu(fd_network->menu_mission, dsbtypes[j].ChType);
	     k++;
	   }
       }
     }
     fclose(typefp);
     //
     strcpy(inmsg.src, "MTIX");
     strcpy(inmsg.dst, "GPDA");
     strcpy(inmsg.msn, dsbtypes[1].ChType);
   }
   //fl_set_menu(fd_network->menu_mission, 1);
//
//   If form is NOT currently visible, show it to the user
//
   if(!fl_form_is_visible(fd_network->network) ) {
     DSBx = xpos;
     DSBy = ypos;
     fl_transient();
     fl_winposition(xpos, ypos);
     fl_initial_winsize(width, height);
     DSBwinid = fl_prepare_form_window(fd_network->network,
                                     FL_PLACE_POSITION,FL_TRANSIENT, DSBlabel);
     fl_winreshape(DSBwinid, xpos, ypos, width, height);
     fl_get_wingeometry(DSBwinid, &DSBwinX, &DSBwinY, &DSBwinW, &DSBwinH);
     DSBwinX = DSBwinX + 4;
     DSBwinY = DSBwinY + 24;
     fl_set_form_atclose(fd_network->network, DSBclose, 0);
     fl_set_form_title(fd_network->network, DSBlabel);
     StoreActiveEntry(DSBlabel);
     if (!DSBbatch) {
       fl_show_form_window(fd_network->network);
     }
   }

   return;
}

int DSBevid(char *chmission, char *buffer)
{
/*  Purpose:        Add evidence to list from message
**  Screen:         None
**  Invoked By:     Message input
**  Processing:     - Extract message data from input buffer
**                  - Put message data in evidence file
**                  - Assure time is in ascending order
**                  - Evaluate network if evid applicable to current
*/
FILE            *fp;
int             item = 0, i, irow, icol, ext;
int             ievid, level;
float           belief, nonbelief, Tin;
float           duration, latitude, longitude, altitude;
DSevidence      newev;
char            *p;
char            chsource[32] = "JSTARS";
char            chtime[64];
char            chfile[64]; 
char            chtemp[128];
extern void     PutOperator(int Severity, char *msg);
LISTNODE        *save_node; 

//fprintf(stderr, "Intel in is [%s] for mission {%s}\n", buffer, chmission);
//fflush(stderr);
 
   p = strtok(buffer, " ");                       // Get hypothesis
   strcpy(chsource, p);
   p = strtok(NULL, " ");                         // Get belief
   belief = atof(p);
   p = strtok(NULL, " ");                         // Get disbelief
   nonbelief = atof(p);
   p = strtok(NULL, " ");                         // Get timestamp
   strcpy(chtime, p);
   p = strtok(NULL, " ");                         // Get duration
   duration = atof(p);
   p = strtok(NULL, " ");                         // Get latitude
   latitude = atof(p);
   p = strtok(NULL, " ");                         // Get longitude
   longitude = atof(p);
   p = strtok(NULL, " ");                         // Get altitude
   altitude = atof(p);

   if (strcmp(chmission, "?") == 0) {
     //
     //  Don't know what we are talking about. Try to find a previous reference to
     //  something at this location.
     // 
     curr_node = first_node;
     while (curr_node != NULL) {
       if ((latitude == curr_node->Latitude) && (longitude == curr_node->Longitude)) break;
       curr_node = (LISTNODE *)curr_node->next;
     }
     if (curr_node == NULL) {
       //
       //   Location does NOT match any entities in list
       //
       save_node = curr_node;
       curr_node = (LISTNODE *)new(LISTNODE);
       curr_node->previous = (LISTNODE *)save_node;
       curr_node->next = NULL;
       save_node->next = curr_node;
       curr_node->Latitude = latitude;
       curr_node->Longitude = longitude;
       curr_node->Altitude = altitude;
       strcpy(curr_node->Entity_Name, "Unknown");
       strcpy(chmission, curr_node->Entity_Name);
       last_node = curr_node;
     } else {
       //
       //   Location does match an entity in list
       //
       strcpy(chmission, curr_node->Entity_Name);
     }
   }

   if (DSBMYSQL != 0) {
     strcpy(chfile, chmission);
   } else {
     sprintf(chfile, "DSBFiles/%s.evid", chmission);
     if ((fp = fopen(chfile, "r")) == NULL) {
       fp = fopen(chfile, "w");       
       fprintf(fp, "%5d    %s\n", 1, chmission);
       fclose(fp);
     }
     fp = fopen(chfile, "a");
     icol = -1;
     irow = -1;
     fprintf(fp, "%4d   %-24s %8.3f  %8.3f %s %8.1f %c  %8.3f  %8.3f  %8.3f  %s  %d  %d  %s\n", 
	   0,                      // etype
           chsource,               // hypothesis
           belief,                 // confidence
           nonbelief,              // disbelief
	   chtime,                 // timeStamp
           duration,               // duration
	   'M',                    // duration units
           latitude,               // param1
           longitude,              // param2
           altitude,               // param3
           "Message Input",        // description
           icol+1,                 // source
           irow+1,                 // level
	   "None");
     fclose(fp);
   }

   if (DS_AlgoGetNodeCoordinates(chsource, ievid, level) != 0) {
     fprintf(stderr, "%s not applicable to current mission.\n", chsource);
     return(-2);
   }
   //
   //   If evdence applicable to current mission, add it to in-memory evid list
   //
   i = ev.DS_addEvidence(chsource,         // name
                         "Message Input",  // description
                         "49.3",           // param1
                         "121.3",          // param2
                         "160.0",          // param3
                         NULL,             // subfname
                         0,                // caseId
                         TRUE,             // valid
                         ievid+1,          // source
                         level+1,          // level
                         0,                // etype
                         Tin,              // timeStamp
                         99.0,             // duration
                         0.47,             // confidence
                         0.23);            // disbelief

   if (i < 0)
     return i;
    
   sprintf(dsbtemp, "%11s (%4.1f)  %11s (%4.1f)",
	   fl_get_choice_text(fd_assessadd->belief_by_name),
           ev.evidences[i]->confidence*100.0,
	   fl_get_choice_text(fd_assessadd->disbelief_by_name),
           ev.evidences[i]->disbelief*100.0);
   sprintf(chtemp, "%-16s %-34s %8.2f %8.2f %10s %10s %10s",
	   ev.evidences[i]->chsource, dsbtemp, ev.evidences[i]->Tin,
           ev.evidences[i]->duration,
	   ev.evidences[i]->latitude, ev.evidences[i]->longitude,
           ev.evidences[i]->altitude);
   fl_addto_browser(fd_dsevedit->input_browser, chtemp);

   EvidModified = TRUE;

   resetCB(NULL, 0);
   executeCB(NULL, 0);

   return(0);
}

void DSBexitCB(FL_OBJECT *ob, long data)
{
const char      *fname;

   if (EvidModified) {
     if (fl_show_question("Evidence changed! Save?", 0) == TRUE) {
       input_saveCB(NULL, 0);
     }
   }
   EvidModified = FALSE;

   if (DSBMYSQL) CloseDatabase( DSBhEnv, DSBhDbc );

   fl_hide_form(fd_network->network);
   EraseActiveEntry(DSBlabel);

   FinishUp();

   return;
}

void DSBnoneCB(FL_OBJECT *ob, long data)
{
   return;
}

/* --------------------------------------------------------------------------- */

int GetLexical(float belief, char* desc)
{
int             i, j;

   for (i=0; i<LexCount; i++) {
     if ((belief >= LexTable[i].lower) && (belief < LexTable[i].upper)){
       strcpy(desc, LexTable[i].desc);
       return (i+1);
     }
   }

   return (0);
}

void DSinit(int importData, char *filename, char *tablename)
{
/*  Purpose:        Load DS network
**  Screen:         None
**  Invoked By:     User menu selection
**  Processing:     - Load belief network structure
**                  - Initialize variables and widgets
*/
FILE            *fp;
int             i, k, level, curr_type;
double          bthresh, tthresh;
char            *fname;
char            timeUnit;
char            chtemp[128];
char            cfgName[128];

/*
   if (EvidModified) {
     EvidModified = fl_show_question("Evidence changed! Save?", 0);
     if (EvidModified == TRUE) {
       // Save the updated evidence

       if (DSBMYSQL)
       {
         k = fl_get_menu(fd_network->menu_mission) - 1;
         ev.DS_saveEvidence(dsbtypes[k].filebase, DSBMYSQL);
       }
       else
         ev.DS_saveEvidence("DSBFiles/NewEvidence.evid", DSBMYSQL);
     }
   }
   EvidModified = FALSE;
*/
   if (importData)
   {
     curr_type = 0;
     strcpy(currentype.chname, dsbtypes[curr_type].ChType);
     fname = filename;

     if (DSBMYSQL) {
       sprintf(cfgName, "./DSBFiles/%s.cfg", fname);
       DS_AlgoInit(fname, tablename, cfgName, 0, TRUE);
     } else {
       if (filename != NULL) {
         if (strcmp(fname, "*") == 0) {
           (const char*) fname =
             fl_show_fselector("Network file", "./DSBFiles", "*.dsbn", NULL);
         }
       }

       strcpy(cfgName, fname);
       char *ptr = strstr(cfgName, ".dsbn");
       if (ptr != NULL)
         sprintf(ptr, ".cfg");
       else
         strcat(cfgName, ".cfg");
       DS_AlgoInit(fname, "", cfgName, 0, FALSE);
     }

     // numEvid = 0;  DS_loadEvidence() resets this at load time
     LinkChange = FALSE;
   }

   strcpy(dsbtemp, fl_get_menu_text(fd_network->menu_mission));  
   fl_set_object_label(fd_network->assess_type, dsbtemp);

   Nlevels = DS_AlgoGetTreeDepth();
   discrete = DS_AlgoIsDiscrete();
   DS_AlgoGetTimeRange(startTime, endTime);
   //
   fl_clear_choice(fd_assessadd->add_lvl_choice);
   fl_clear_browser(fd_budtable->bud_label);
//
//   Clear leftovers from previous mission 
//
   for (level=0; level<6; level++) {
     fl_hide_object(fd_network->level_label[level]);
     //fl_show_object(fd_network->notused_title[level]);
     fl_hide_object(fd_network->net_toomany[level]);
     for (i=0; i<6; i++) {
       fl_hide_object(fd_network->directin[i][level]);
       fl_hide_object(fd_network->override[i][level]);
       fl_hide_object(fd_network->externet[i][level]);
       fl_hide_object(fd_network->node_label[i][level]);
       fl_hide_object(fd_network->node_chart[i][level]);
       fl_hide_object(fd_network->bestpath[i][level]);
       fl_hide_object(fd_network->worstpath[i][level]);
       fl_hide_object(fd_network->leastpath[i][level]);
       fl_set_chart_maxnumb(fd_network->node_chart[i][level], 3);
       fl_set_chart_autosize(fd_network->node_chart[i][level], 0);
       fl_clear_chart(fd_network->node_chart[i][level]);
     }
   }
//
// Erase any links drawn.
//
   fl_redraw_form(fd_network->network);

//
// Get the initial Evidence sources for current type
//
   currentype.assessment[0].belief = 90.0;
   currentype.assessment[0].plause = 95.0;
   currentype.assessment[1].belief = 5.0;
   currentype.assessment[1].plause = 10.0;
   currentype.assessment[2].belief = 1.0;
   currentype.assessment[2].plause = 1.0;
   //
   curr_assess = 0;
   for (level=0; level<Nlevels; level++) {
     Nnodes[level] = DS_AlgoGetLevelWidth(level);
     DS_AlgoGetLevelName(level, chtemp);
     strsub(chtemp, '_', ' ');
     fl_addto_choice(fd_assessadd->add_lvl_choice, chtemp); //??
     strsub(chtemp, ' ', '\n');
     fl_show_object(fd_network->level_label[level]);
     fl_set_object_label(fd_network->level_label[level], chtemp);
     fl_set_object_label(fd_dsnetedit->dsed_label[level], chtemp); //??
     fl_clear_choice(fd_assessadd->add_src_choice);
     //
     for (i=0; i<Nnodes[level]; i++) {
       fl_set_object_color(fd_network->node_label[i][level], FL_LEFT_BCOL, FL_LEFT_BCOL);
       if (fl_get_button(fd_dsb_opt_ctl->prune) == 0) {
	 fl_show_object(fd_network->node_label[i][level]);
	 fl_show_object(fd_network->node_chart[i][level]);
       }
       //fl_hide_object(fd_network->notused_title[level]);
       DS_AlgoGetNodeName(i, level, chtemp);
       strsub(chtemp, '_', ' ');
       fl_addto_browser(fd_budtable->bud_label, chtemp);
       fl_addto_choice(fd_assessadd->add_src_choice, chtemp);
       //
       DS_AlgoGetNodeThresholds(i, level,
			        nodeinfo[i][level].threshold,
                                nodeinfo[i][level].cutoff);
       nodeinfo[i][level].dropdead = 0.20;
       //
       //   Label node & initialize network chart for this node
       //
       strsub(chtemp, ' ', '\n');
       fl_set_object_label(fd_dsnetedit->dsed_top[i], chtemp);
       fl_set_object_label(fd_network->node_label[i][level], chtemp);
       fl_add_chart_value(fd_network->node_chart[i][level], (double)0.0, "U", FL_YELLOW);
       fl_add_chart_value(fd_network->node_chart[i][level], (double)0.0, "D", FL_RED);
       fl_add_chart_value(fd_network->node_chart[i][level], (double)0.0, "B", FL_BLUE);
       //
       if (level == Nlevels-1) {
         timeUnit = DS_AlgoGetDefaultTimeUnit();
	 DS_AlgoGetNodeThresholds(i, level, bthresh, tthresh);
	 if (toupper(timeUnit) == 'S') tthresh = tthresh/60.0;
	 else if (toupper(timeUnit) == 'H') tthresh = tthresh*60.0;
	 else if (toupper(timeUnit) == 'D') tthresh = tthresh*60.0*24.0;
	 currentype.assessment[i].Bthreshold = bthresh;
	 currentype.assessment[i].Tthreshold = tthresh;
	 currentype.assessment[i].select = i;
       }
     }
   }

   DS_AlgoGetEvidenceLabels(evid_lab1, evid_lab2, evid_lab3);

   chTunit = DS_AlgoGetDefaultTimeUnit();
   if ((chTunit == 'S') || (chTunit == 's')) {
     fl_set_button(fd_dsb_opt_tim->assesstunit[0], PUSHED);
   }
   else if ((chTunit == 'M') || (chTunit == 'm')) {
     fl_set_button(fd_dsb_opt_tim->assesstunit[1], PUSHED);
   }
   else if ((chTunit == 'H') || (chTunit == 'h')) {
     fl_set_button(fd_dsb_opt_tim->assesstunit[2], PUSHED);
   }
   else if ((chTunit == 'D') || (chTunit == 'd')) {
     fl_set_button(fd_dsb_opt_tim->assesstunit[3], PUSHED);
   }

   fl_deactivate_object(fd_budgraph->prev_but);
   //
   //   If we have drawn a graph, save as old graph
   //
   if (fl_get_xyplot_numdata(fd_budgraph->bud_plot, 1) > 0) {
     fl_get_xyplot_overlay_data(fd_budgraph->bud_plot, 1, XtOld, YbOld, &osteps);
     fl_get_xyplot_overlay_data(fd_budgraph->bud_plot, 2, XtOld, YpOld, &osteps);
   }

  return;
}

void DSload(char *filename)
{
int             i, j;
int             ievid;
int             level;
int             hasExtern;
float           xx[2], y1[2];
float           confidence, Tin;
char            chtemp[128];
char            chtmp[32];
char            chmission[32];
char            chlevel[32];

 fprintf(stderr, "Loading evidence from %s\n", filename); 
//
//   Clear left-over stuff from last run
//
   for (level=0; level<MAXLEVELS; level++) {
     for (i=0; i<MAXNODES; i++) {
       fl_hide_object(fd_network->directin[i][level]);
       fl_hide_object(fd_network->override[i][level]);
       nodeinfo[i][level].belief = 0.0;
       nodeinfo[i][level].disbelief = 0.0;
       nodeinfo[i][level].old_belief = 0.0;
       nodeinfo[i][level].old_disbelief = 0.0;
       nodeinfo[i][level].extern_name[0] = '\0';
     }
   }
//
//   Prepare evidence window
//
   fl_clear_browser(fd_dsevedit->input_browser);
   sprintf(dsbtemp, "@b@i@C2@N@_%-16s %-36s   %6s   %-10s   %-8s   %-8s   %-8s",
           "Evidence Source", "    Belief           Disbelief", "Time",
           "Duration", evid_lab1, evid_lab2, evid_lab3);
   fl_addto_browser(fd_dsevedit->input_browser, dsbtemp);
   //
   fl_set_object_label(fd_dsstory->story_evidtitle, "Mission Evidence");
   fl_clear_browser(fd_dsstory->story_evidence);
   sprintf(dsbtemp,
           "@b@i@C2@_%-40s %-38s %-32s %-14s %-20s   %-18s   %-20s   %-20s",
           "Evidence Source", "Belief", "Disbelief", "Time", "Duration",
           evid_lab1, evid_lab2, evid_lab3);
   fl_add_browser_line(fd_dsstory->story_evidence, dsbtemp);
   //
   fl_clear_xyplot(fd_budgraph->bud_plot);
   //
   int irc = ev.DS_loadEvidence(filename, DSBMYSQL,
                      fl_get_button(fd_dsb_opt_ctl->secure), 0, hasExtern);
   if (irc == -1) {
     sprintf(dsbtemp, "Error loading evidence from %s", filename);
     fl_show_messages(dsbtemp);
     return;
   }

   if (hasExtern == 1)  // external network evidence was encountered
   {
     for (i = 0; i < ev.numEvid; i++)
     {
       ievid = ev.evidences[i]->source - 1;
       level = ev.evidences[i]->level - 1;

       if (ev.evidences[i]->type == 2)
       {
         strcpy(nodeinfo[ievid][level].extern_name, ev.evidences[i]->subfname);

         if (strlen(ev.evidences[i]->chmission) > 0)  // no error encountered
         {
           sprintf(dsbtemp, "%s\n%s\n%s\nB = %f\nD = %f\n",
                   ev.evidences[i]->chmission,
                   ev.evidences[i]->chlevel,
                   ev.evidences[i]->chcase,
                   ev.evidences[i]->confidence,
                   ev.evidences[i]->disbelief);
           fl_set_object_helper(fd_network->externet[ievid][level],dsbtemp);
           fl_set_object_color(fd_network->externet[ievid][level],
                               FL_YELLOW, FL_YELLOW);
           fl_show_object(fd_network->externet[ievid][level]);
         }
         else
         {
           fl_show_messages("No data available from higher level net");
         }
       }
       //
       //   Show user for editting or running
       //
       GetLexical(ev.evidences[i]->confidence, chtemp);
       GetLexical(ev.evidences[i]->disbelief, chtmp);
       sprintf(dsbtemp, "%11s (%4.1f)  %11s (%4.1f)",
               chtemp, ev.evidences[i]->confidence*100.0,
               chtmp, ev.evidences[i]->disbelief*100.0);
       strncpy(chsource, ev.evidences[i]->chsource, 15);
       chsource[15] = '\0';                     // Force NULL, in case not there
       sprintf(chtemp, "%-16s %-34s %8.2f %8.2f %10s %10s %10s",
               chsource, dsbtemp,
               ev.evidences[i]->Tin,
               ev.evidences[i]->duration,
               ev.evidences[i]->latitude,
               ev.evidences[i]->longitude,
               ev.evidences[i]->altitude);
       fl_addto_browser(fd_dsevedit->input_browser, chtemp);
       fl_add_browser_line(fd_dsstory->story_evidence, chtemp);
     }
   }
//
//   Display the data epoch time in all forms
//
   fl_set_object_label(fd_network->sdate_text, startTime);

   //
   curr_evid = 0;

   fl_activate_object(fd_dsevedit->save_but);
   fl_activate_object(fd_dsevedit->exec_but);
   fl_activate_object(fd_dsevedit->prop_but);
   fl_activate_object(fd_dsevedit->reset_but);
   fl_activate_object(fd_dsevedit->add_but);
   fl_activate_object(fd_dsevedit->mod_but);
   fl_activate_object(fd_dsevedit->del_but);
//
//   Display an example graph
//
   graph = fd_budgraph->bud_plot;
   fl_set_xyplot_overlay_type(graph, 1, FL_NORMAL_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 2, FL_NORMAL_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 3, FL_DASHED_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 4, FL_DASHED_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 5, FL_DOTTED_XYPLOT);
   fl_set_xyplot_overlay_type(graph, 6, FL_DOTTED_XYPLOT);
   fl_set_xyplot_xbounds(graph, ev.Tstart, ev.Tlast*1.1);
   fl_set_xyplot_ybounds(graph, 0.0, 100.0);
   fl_set_xyplot_ytics(graph, 10, 0);
   fl_set_xyplot_linewidth(graph, 1, 2);
   fl_set_xyplot_linewidth(graph, 2, 2);

   xx[0] = 0.0;
   y1[0] = 0.0;
   fl_set_xyplot_data(graph, xx, y1, 1, "Confidence of Assessment",
                      "Time (min)","%");
}

void RedrawEdit(int level)
{
int             i, j, k, item = 0;
int             lwidth, lstyle, icolor;
int             savwidth;
float           nonbelief, results[6];
char            In_String[64];   /* Out - Parent node name                            */
char            Out_String[64];  /* Out - Child node name                             */
char            B_String[64];    /* Out - String describing belief value              */
char            D_String[64];    /* Out - String describing belief value              */
float           B_Weight;        /* Out - Line-source belief value                    */
float           D_Weight;        /* Out - Line-source disbelief value                 */
FL_Coord        HypX, HypY, HypW, HypH;
FL_Coord        Box1X, Box1Y, Box1W, Box1H;
FL_Coord        xcoord, ycoord, width, height;
FL_COLOR        lcolor[10] = { FL_RED, FL_TOMATO, FL_MAGENTA, FL_ORCHID, FL_DARKGOLD,
                               FL_WHEAT, FL_YELLOW, FL_GREEN, FL_CYAN, FL_BLUE };
 static int flag = true;

   if (level < 6) {
     for (i=0; i<6; i++) {
       fl_hide_object(fd_dsnetedit->dsed_top[i]);
       fl_hide_object(fd_dsnetedit->dsed_top_grp[i]);
       fl_hide_object(fd_dsnetedit->dsed_lower[i]);
       fl_hide_object(fd_dsnetedit->dsed_lower_grp[i]);
     }

     fl_set_object_color(fd_dsnetedit->src_box, FL_BLACK, FL_WHITE);
     fl_set_object_color(fd_dsnetedit->src_box, FL_WHITE, FL_WHITE);

     fl_set_object_label(fd_dsnetedit->dsed_label[0],
			 fl_get_object_label(fd_network->level_label[level-1]));
     fl_set_object_label(fd_dsnetedit->dsed_label[1],
			 fl_get_object_label(fd_network->level_label[level]));

       for (i=0; i<DS_AlgoGetLevelWidth(level-1); i++) {
         fl_show_object(fd_dsnetedit->dsed_top[i]);
	 fl_set_object_label(fd_dsnetedit->dsed_top[i],
			     fl_get_object_label(fd_network->node_label[i][level-1]));
         fl_show_object(fd_dsnetedit->dsed_top_grp[i]);
         fl_set_slider_bounds(fd_dsnetedit->dsed_top_b[i],  0.0, 1.0);
         fl_set_slider_bounds(fd_dsnetedit->dsed_top_d[i],  0.0, 1.0);
         fl_set_slider_bounds(fd_dsnetedit->dsed_top_u[i],  0.0, 1.0);
         fl_set_slider_value(fd_dsnetedit->dsed_top_b[i],   0.0);
         fl_set_slider_value(fd_dsnetedit->dsed_top_d[i],   0.0);
         fl_set_slider_value(fd_dsnetedit->dsed_top_u[i],   0.0);
	 DS_AlgoGetResult(i, level-1, results);
         fl_set_slider_value(fd_dsnetedit->dsed_top_b[i], (double)(results[1]));
         fl_set_slider_value(fd_dsnetedit->dsed_top_d[i], (double)(results[2]));
         fl_set_slider_value(fd_dsnetedit->dsed_top_u[i], (double)(results[3]));
       }
       //
       for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
         fl_show_object(fd_dsnetedit->dsed_lower[i]);
	 fl_set_object_label(fd_dsnetedit->dsed_lower[i],
			     fl_get_object_label(fd_network->node_label[i][level]));
         fl_show_object(fd_dsnetedit->dsed_lower_grp[i]);
         fl_set_slider_bounds(fd_dsnetedit->dsed_lower_b[i],  0.0, 1.0);
         fl_set_slider_bounds(fd_dsnetedit->dsed_lower_d[i],  0.0, 1.0);
         fl_set_slider_bounds(fd_dsnetedit->dsed_lower_u[i],  0.0, 1.0);
         fl_set_slider_value(fd_dsnetedit->dsed_lower_b[i],   0.0);
         fl_set_slider_value(fd_dsnetedit->dsed_lower_d[i],   0.0);
         fl_set_slider_value(fd_dsnetedit->dsed_lower_u[i],   0.0);
	 DS_AlgoGetResult(i, level, results);
         fl_set_slider_value(fd_dsnetedit->dsed_lower_b[i], (double)(results[1]));
         fl_set_slider_value(fd_dsnetedit->dsed_lower_d[i], (double)(results[2]));
         fl_set_slider_value(fd_dsnetedit->dsed_lower_u[i], (double)(results[3]));
	 //
         savwidth = fl_get_linewidth();
         lwidth = 2;
         lstyle = FL_SOLID;
         fl_get_object_geometry(fd_dsnetedit->dsed_lower[i], &HypX, &HypY, &HypW, &HypH);
         for (j=0; j<DS_AlgoGetLevelWidth(level-1); j++) {
           DS_AlgoGetMatrix(j, level-1, i, level,
			    In_String, Out_String, B_String, D_String, B_Weight, D_Weight);
           if (B_Weight > 0.0) {
             fl_get_object_geometry(fd_dsnetedit->dsed_top[j],
				    &Box1X, &Box1Y, &Box1W, &Box1H);
             fl_linewidth(lwidth);
             fl_linestyle(lstyle);
	     icolor = (int)(B_Weight*10.0);
	     if (icolor > 9) icolor = 9;
             fl_line((int)Box1X+(int)(Box1W/2), (int)Box1Y+(int)Box1H,
                 (int)(HypX+(int)(HypW/2)), (int)(HypY), lcolor[icolor]);
           }
         }
         fl_linewidth(savwidth);
	 fl_linestyle(FL_SOLID);
       }
   }
   return;
}

void filenameToTablename(char filename[], char tablename[])
{
  char            *ptr;

  // Strip the leading path.
  ptr = strrchr(filename, '/');

  if (ptr != NULL)
    strcpy(tablename, ptr + 1);
  else
    strcpy(tablename, filename);


  // Strip the file extension.
  ptr = strrchr(dsbtemp, '.');

  if (ptr != NULL)
    *ptr = '\0';
}

void saveNet()
{
  int  k;


  if (DSBMYSQL) {
    showStoryBrowser();

    if (storyName != NULL) {
      k = fl_get_menu(fd_network->menu_mission) - 1;
      DS_AlgoSaveAttributes(dsbtypes[k].filebase, storyName, TRUE);
    }
  } else {
    char  *fname;

    (const char*) fname =
       fl_show_fselector("Network File", "./DSBFiles", "*.dsbn", NULL);

    if (fname != NULL)
      DS_AlgoSaveAttributes(fname, "", FALSE);
  } // if DSBMYSQL
}
/*                                                                             */
/* --------------------------------------------------------------------------- */
/*         C A L L B A C K    R O U T I N E S    S T A R T    H E R E          */
/* --------------------------------------------------------------------------- */
/*                                                                             */
void dsbmenuCB(FL_OBJECT *object, long item_no)
{

   DSBclose(NULL, 0);

   return;
}

void dsbmissionCB(FL_OBJECT *object, long item_no)
{
FILE            *fp;
int             i, k, nstory, istory;
int             ret;
char            chstory[64];
//
int             nCols;
char            szSQL[32];
SQLINTEGER      nCol                            = 0;
SQLCHAR         szColumn[MAX_DATA_WIDTH+20]     = "";
SQLLEN          nIndicator                      = 0;
SQLCHAR         szColumnValue[MAX_DATA_WIDTH+1] = "";
SQLRETURN       nReturn;
SQLLEN          nRows = 0;
//
//   Clear all widgets
//
   fl_clear_browser(fd_dsevedit->input_browser);
   //
   fl_deactivate_object(fd_dsevedit->load_but);
   fl_deactivate_object(fd_dsevedit->save_but);
   fl_deactivate_object(fd_dsevedit->exec_but);
   fl_deactivate_object(fd_dsevedit->prop_but);
   fl_deactivate_object(fd_dsevedit->reset_but);
   fl_deactivate_object(fd_dsevedit->add_but);
   fl_deactivate_object(fd_dsevedit->mod_but);
   fl_deactivate_object(fd_dsevedit->del_but);
   fl_deactivate_object(fd_budgraph->prev_but);
//
//   Use selected mission
//
   fl_clear_choice(fd_network->dsb_story);
   k = fl_get_menu(fd_network->menu_mission) - 1;
   numStories = 0;

   if (DSBMYSQL) {
     sprintf(szSQL, "USE `%s`;", dsbtypes[k].filebase);
     ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter, bColumnNames,
                       bHTMLTable, nCols );
     SQLFreeStmt( hStmt, SQL_DROP );

     if (ret != 0) {
       fprintf(stderr, "Cannot access database (%s)\n", dsbtemp);
       } else {
       //
       //   Populate the Story menu with the mission's table names.
       //

       // See if a table with same name as database exists.
       sprintf(szSQL, "analyze table %s;", dsbtypes[k].filebase);
       if (ExecuteSQL( DSBhDbc, szSQL, cDelimiter, bColumnNames,
                         bHTMLTable, nCols) == 0) 
         i = 1;
       else
         i = 0;

       nRows = 0;
       strcpy(szSQL, "show tables;");
       ExecuteSQL( DSBhDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable, nCols);
       if ( nCols > 0 ) {
         nReturn = SQLFetch( hStmt );
         while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
	   for( nCol = 1; nCol <= nCols; nCol++ ) {
	     nReturn = SQLGetData( hStmt, nCol, SQL_C_CHAR,
                                   (SQLPOINTER)szColumnValue,
                                   sizeof(szColumnValue), &nIndicator );
	     szColumnValue[MAX_DATA_WIDTH] = '\0';

	     if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
	       sprintf(dsbtemp, "%s", szColumnValue);

	       if ( (strstr(dsbtemp, "Evidence") == NULL) &&
                    (strstr(dsbtemp, "Labels") == NULL) ) {
                 if (strcmp(dsbtemp, dsbtypes[k].filebase) == 0)
                   strcpy(stories[0], dsbtemp);
                 else
                   strcpy(stories[i++], dsbtemp);
                 numStories++;
               }
	     } else if ( nReturn == SQL_ERROR ) {
	       break;
	     }
	   } // for columns
	   nRows++;
	   nReturn = SQLFetch( hStmt );
         } // while rows
         for (i = 0; i < numStories; i++)
	   fl_addto_choice(fd_network->dsb_story, stories[i]);
       } else {
         fl_show_messages("Sorry, no stories available for this mission!!");
       }
       SQLFreeStmt( hStmt, SQL_DROP );
     }
   } else {
     //
     //   Populate the Story menu with the mission's story names.
     //
     //fprintf(stderr, "Loading story from %s.stry\n", dsbtypes[k].filebase);
     sprintf(dsbtemp, "DSBFiles/%s.stry", dsbtypes[k].filebase);
     if ((fp = fopen(dsbtemp, "r")) != NULL) {
       do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');
       sscanf(dsbtemp, "%d", &nstory);
       if (nstory > 0) {
         for (istory=0; istory<nstory; istory++) {
           do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');
           sscanf(dsbtemp, "%s %*f %*f %*f %s",
                  stories[numStories], storyPaths[numStories]);
           fl_addto_choice(fd_network->dsb_story, stories[numStories]);
           numStories++;
         }
       }
     }
   }

   sprintf(dsbtemp, "%d", numStories);
   fl_set_input(fd_dsstory->story_count, dsbtemp);
   fl_set_input(fd_dsstory->story_mission,
                fl_get_menu_text(fd_network->menu_mission));
   fl_set_input(fd_storygraph->story_count, dsbtemp);

   if (numStories == 1)
     input_menuCB(NULL, 0);

   return;
}
/*                                                                       */
/*                       Window Handling routines                        */
/* --------------------------------------------------------------------- */
/*                                                                       */
void optionCB(FL_OBJECT *object, long item_no) 
{
   fl_show_form(fd_options->options, FL_PLACE_CENTER,FL_FULLBORDER, "Options");
}

void optexitCB(FL_OBJECT *object, long item_no)
{
   fl_hide_form(fd_options->options);
}

void input_displayCB(FL_OBJECT *object, long item_no)
{
FILE            *fp;
int             i, j, k, nstory, istory, item = 0;
float           nonbelief, results[6];
float           StoryB[10], StoryD[10], StoryU[10];
const char      *ifield;
char            *fname;
char            chstory[12];
char            chtemp[64];
char            chfile[128];
Window          winid;

   switch (item_no)
   {
     case 0: // Show Operator screen
       fl_winposition(DSBwinX, DSBwinY);
       fl_initial_winsize(DSBwinW, DSBwinH);
       winid = fl_prepare_form_window(fd_dsevedit->dsevedit,
                                     FL_PLACE_POSITION,FL_NOBORDER, "Assess-Operator");
       fl_winreparent(winid, DSBwinid);
       fl_wintitle(winid, "Assess-Operator");
       fl_show_form_window(fd_dsevedit->dsevedit);
       StoreActiveEntry("Assess-Operator");
       break;

     case 1: // Old network editor
       fl_winposition(DSBwinX, DSBwinY);
       fl_initial_winsize(DSBwinW, DSBwinH);
       winid = fl_prepare_form_window(fd_dsnetedit->dsnetedit,
                                     FL_PLACE_POSITION,FL_NOBORDER, "Assess-NetEdit");
       fl_winreparent(winid, DSBwinid);
       fl_show_form_window(fd_dsnetedit->dsnetedit);
       fl_set_form_title(fd_dsnetedit->dsnetedit, "Assess-NetEdit");
       StoreActiveEntry("Assess-NetEdit");
       //
       RedrawEdit(1);
       break;

     case 2: // Show Network view
       fl_winposition(DSBwinX, DSBwinY);
       fl_initial_winsize(DSBwinW, DSBwinH);
       winid = fl_prepare_form_window(fd_network->network,
                                     FL_PLACE_POSITION,FL_NOBORDER, "Assess-Network");
       fl_winreparent(winid, DSBwinid);
       fl_wintitle(winid, "Assess-Network");
       fl_show_form_window(fd_network->network);
       StoreActiveEntry("Assess-Network");
       if (fl_get_button(fd_dsb_opt_ctl->prune) == 0) netdrawCB(NULL, 0);
       NetVisible = TRUE;
       break;

     case 3: // Show explanation screen
       fl_winposition(DSBwinX, DSBwinY);
       fl_initial_winsize(DSBwinW, DSBwinH);
       winid = fl_prepare_form_window(fd_dsexplain->dsexplain,
                                     FL_PLACE_POSITION,FL_NOBORDER, "Assess-Explain");
       fl_winreparent(winid, DSBwinid);
       fl_wintitle(winid, "Assess-Explain");
       fl_show_form_window(fd_dsexplain->dsexplain);
       StoreActiveEntry("Assess-Explain");
       break;

     case 4: // Show stories screen
       k = fl_get_menu(fd_network->menu_mission) - 1;

       fl_clear_browser(fd_dsstory->story_details);
       sprintf(dsbtemp, "%-12s %s",
           "Story ID", "      Belief     Disbelief    Unknown   Net Container");
       fl_set_object_label(fd_dsstory->story_header, dsbtemp);
       //
       if (DSBMYSQL) {
         for (i = 0; i < numStories; i++)
         {
           // Write the story data to a new line in the browser.
           // Will call assess_storyCB() to provide the bud values,
           // so just put 0, 0, 1 for now.
//correct?  assess_storyCB moves vals to oldB, etc --------------------------ttt
	   sprintf(dsbtemp,
                  "%-12s      %8.2f (%5.2f)%8.2f (%5.2f)%8.2f (%5.2f)  %-64s",
		   stories[i], 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, stories[i]);
//are using stories param; are sure is kept up to date
//  (eg when story is added or deleted?)
//  else need to get list of tables from db -------------------------ttt
	   fl_add_browser_line(fd_dsstory->story_details, dsbtemp);
         }

         nstory = numStories;
       } else {
         // Open the user-selected or current story file.

         if (strcmp(dsbtypes[k].filebase, "*") == 0) {
	   (const char*) fname =
             fl_show_fselector("Story file", "./DSBFiles", "*.stry", NULL);
	   if (fname == NULL) return;
	   if ((fp = fopen(fname, "r")) == NULL) return;
         } else {
	   sprintf(dsbtemp, "DSBFiles/%s.stry", dsbtypes[k].filebase);
	   if ((fp = fopen(dsbtemp, "r")) == NULL) return;
         }
         //
         do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');
         sscanf(dsbtemp, "%d", &nstory);

         nstory = atoi(fl_get_input(fd_dsstory->story_count));
         if (nstory > 0) {
	   for (istory=0; istory<nstory; istory++) {
	     do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');

             // Get story data from the story file.
	     sscanf(dsbtemp, "%s %f %f %f %s", chstory,
		    &StoryB[istory], &StoryD[istory], &StoryU[istory],
		    chtemp);
             filenameToTablename(chtemp, chfile);

             // Write the story data to a new line in the browser.
             // Will call assess_storyCB() to provide the bud values,
             // the ones will write here are just place holders.
	     sprintf(dsbtemp,
                    "%-12s      %8.2f (%5.2f)%8.2f (%5.2f)%8.2f (%5.2f)  %-64s",
		     chstory, StoryB[istory], 0.0, StoryD[istory],
                     0.0, StoryU[istory], 1.0, chfile);
	     fl_add_browser_line(fd_dsstory->story_details, dsbtemp);
	   }
	 }

         fclose(fp);
       }

       if (nstory == 0) {
	 strcpy(dsbtemp, " ");
	 fl_addto_browser(fd_dsstory->story_details, dsbtemp);
	 strcpy(dsbtemp, "                   No stories to tell");
	 fl_addto_browser(fd_dsstory->story_details, dsbtemp);
       }

       fl_hide_object(fd_dsstory->story_done);

       fl_winposition(DSBwinX, DSBwinY);
       fl_initial_winsize(DSBwinW, DSBwinH);
       winid = fl_prepare_form_window(fd_dsstory->dsstory,
                                     FL_PLACE_POSITION,FL_NOBORDER, "Assess-Story");
       fl_winreparent(winid, DSBwinid);
       fl_wintitle(winid, "Assess-Story");
       fl_show_form_window(fd_dsstory->dsstory);
       fl_set_form_title(fd_dsstory->dsstory, "Assess-Story");
       StoreActiveEntry("Assess-Story");
       //
       assess_storyCB(NULL, 0);
       break;

     default:
       break;
   }
}

void exitCB(FL_OBJECT *object, long item_no)
{
int             item = 0;
const char      *fname;

   switch (item_no)
   {
     case 0:
       fl_hide_form(fd_dsevedit->dsevedit);
       EraseActiveEntry("Assess-Operator");
       netdrawCB(NULL, 0);
       break;

     case 1:  //  Net Editor
       fl_hide_form(fd_dsnetedit->dsnetedit);
       EraseActiveEntry("Assess-NetEdit");
       break;

     case 2:
       if (LinkChange == TRUE) {
	 LinkChange = fl_show_question("LINK weights changed. Save changes?", 0);
         if (LinkChange == TRUE)
           saveNet();
       }
       LinkChange = FALSE;

       NetVisible = FALSE;
       fl_hide_form(fd_network->network);
       EraseActiveEntry("Assess-Network");
       break;

     case 3:
       fl_hide_form(fd_dsexplain->dsexplain);
       EraseActiveEntry("Assess-Explain");
       break;

     case 4:
       fl_hide_form(fd_dsstory->dsstory);
       EraseActiveEntry("Assess-Story");
       break;

     case 10:
       fl_hide_form(fd_dshelpevid->dshelpevid);
       break;

     case 11:
       fl_hide_form(fd_dsbstats->dsbstats);
       //fl_trigger_object(fd_network->net_show);             // Force network re-draw
       break;

     case 12:
       fl_hide_form(fd_dsbstory->dsbstory);
       //fl_trigger_object(fd_network->net_show);             // Force network re-draw
       break;

     case 13:
       fl_hide_form(fd_netsnap->netsnap);
       break;

     case 14:
       fl_hide_form(fd_budgraph->budgraph);
       break;

     case 15:
       fl_hide_form(fd_dsbhelp->dsbhelp);
       break;

     case 16:
       fl_delete_formbrowser(fd_storygraph->story_browse, fd_storyBUDs);
       fl_free_form(fd_storyBUDs);
       fl_hide_form(fd_storygraph->storygraph);
       break;

     default:
       break;
   }

   return;
}

/*                                                                       */
/*                        Story Browser routines                         */
/* --------------------------------------------------------------------- */
/*                                                                       */

void showStoryBrowser()
{
   int          nCols;
   SQLLEN       nIndicator                      = 0;
   SQLCHAR      szStr[MAX_DATA_WIDTH+1] = "";
   SQLRETURN    nReturn;


   storyName = NULL;

   fl_set_input(fd_storyBrowser->input, "");
   fl_clear_browser(fd_storyBrowser->list);

   //
   //   Get all the tables
   //
   ExecuteSQL(DSBhDbc, "show tables;", cDelimiter,
              bColumnNames, bHTMLTable, nCols);

   if (nCols > 0) {
     nReturn = SQLFetch( hStmt );
     while ( nReturn == SQL_SUCCESS || nReturn == SQL_SUCCESS_WITH_INFO ) {
       nReturn = SQLGetData( hStmt, 1, SQL_C_CHAR, (SQLPOINTER)szStr,
                             sizeof(szStr), &nIndicator );

       if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA ) {
         szStr[MAX_DATA_WIDTH] = '\0';
         sprintf(dsbtemp, "%s", szStr);

         if ( (strstr(dsbtemp, "Evidence") == NULL) &&
              (strstr(dsbtemp, "Labels") == NULL) )
           fl_add_browser_line(fd_storyBrowser->list, dsbtemp);
       } else if ( nReturn == SQL_ERROR ) {
         break;
       }
       nReturn = SQLFetch( hStmt );
     } // while rows
   }

   SQLFreeStmt( hStmt, SQL_DROP );

   fl_deactivate_all_forms();
   fl_show_form(fd_storyBrowser->storyBrowser, FL_PLACE_CENTER, FL_TRANSIENT,
                "Story Browser");

   FL_OBJECT *obj;
   do
   {
     obj = fl_do_forms();

     if (obj == fd_storyBrowser->okBtn)
     {
       (const char*) storyName = fl_get_input(fd_storyBrowser->input);
     }
     else if (obj == fd_storyBrowser->cancelBtn)
     {
       fl_set_input(fd_storyBrowser->input, "");
       storyName = NULL;
     }
     else if (obj == fd_storyBrowser->list)
     {
       fl_set_input(fd_storyBrowser->input,
                    fl_get_browser_line(fd_storyBrowser->list,
                                        fl_get_browser(fd_storyBrowser->list)));
     }
   }
   while ((obj != fd_storyBrowser->okBtn) &&
          (obj != fd_storyBrowser->cancelBtn));

   fl_hide_form(fd_storyBrowser->storyBrowser);
   fl_activate_all_forms();
}

/*                                                                       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void input_menuCB(FL_OBJECT *object, long item_no)
{
int             k, l, item = 0;
char            chfile[128] = "";
char            story[128] = "";
char            *fname;


   k = fl_get_menu(fd_network->menu_mission) - 1;
   l = fl_get_choice(fd_network->dsb_story) - 1;

   if (DSBMYSQL)
   {
     DSinit(TRUE, dsbtypes[k].filebase, stories[l]);
   }
   else
   {
     // Set chfile to the flatfile path, or * for using the fileselector.
     // Pass NULL for the database table parameter.

     if (strcmp(dsbtypes[k].filebase, "*") == 0) {
       strcpy(chfile, "*");
     } else {
       strcpy(chfile, storyPaths[l]);
       strcat(chfile, ".dsbn");
     }

     DSinit(TRUE, chfile, NULL);
   }  // if MYSQL

   fl_activate_object(fd_dsevedit->load_but);
   input_loadCB(NULL, 1);
   netdrawCB(NULL, 0);
   executeCB(NULL, 0);
   StopLight();

   return;
}

void input_saveCB(FL_OBJECT *object, long item_no)
{
int        k, item = 0;
char       *otfilename;
char       chtemp[128];

   k = fl_get_menu(fd_network->menu_mission) - 1;

   if (DSBMYSQL) {
     otfilename = dsbtypes[k].filebase;
   } else {
     sprintf(chtemp, "DSBFiles/%s.evid", dsbtypes[k].filebase);

     (const char*) otfilename =
       fl_show_fselector("Save file", "DSBFiles/", "*.evid", chtemp);
   }

   if (otfilename != NULL) {
     ev.DS_saveEvidence(otfilename, DSBMYSQL);
   }
}

void input_loadCB(FL_OBJECT *object, long item_no)
{
FILE            *fp;
int             k, item = 0;
char            *fname;
char            chfile[128];
char            cfgName[128];

   k = fl_get_menu(fd_network->menu_mission) - 1;

   if (item_no == 0)
   {
     //
     //   Load the network
     //
     if (DSBMYSQL) {
       char  story[128];

       if (strcmp(dsbtypes[k].filebase, "*") == 0) {
         showStoryBrowser();

         if (storyName == NULL) return;

         strcpy(story, storyName);
       } else {
         strcpy(story, fl_get_choice_text(fd_network->dsb_story));
       }
       sprintf(cfgName, "./DSBFiles/%s.cfg", dsbtypes[k].filebase);
       DS_AlgoInit(dsbtypes[k].filebase, story, cfgName, 0, FALSE);
     } else {
       if (strcmp(dsbtypes[k].filebase, "*") == 0) {
         (const char*) fname =
           fl_show_fselector("Network file", "./DSBFiles", "*.dsbn", NULL);
         if (fname != NULL)
           strcpy(chfile, fname);
         else
           sprintf(chfile, "DSBFiles/%s.dsbn", dsbtypes[k].filebase);
       } else {
         sprintf(chfile, "DSBFiles/%s.dsbn", dsbtypes[k].filebase);
       }

       strcpy(cfgName, fname);
       char *ptr = strstr(cfgName, ".dsbn");
       if (ptr != NULL)
         sprintf(ptr, ".cfg");
       else
         strcat(cfgName, ".cfg");

       DS_AlgoInit(chfile, "", cfgName, 0, FALSE);
     }
   }
   //
   resetCB(NULL, 0);
   //
   //   Load the evidence
   //
   if (DSBMYSQL) {
     strcpy(chfile, dsbtypes[k].filebase);
   } else {
     if (NetMulti) {
       if (strcmp(dsbtypes[k].filebase, "*") == 0) {
	 (const char*) fname =
	   fl_show_fselector("Evidence file", "./DSBFiles", "*.evid", NULL);
	 if (fname == NULL) return;
	 strcpy(chfile, fname);
       } else {
	 strcpy(chfile, "DSBFiles/");
	 strcat(chfile, dsbtypes[k].filebase);
	 strcat(chfile, ".evid");
       }
     } else {
       sprintf(chfile, "DSBFiles/%s.evid", fl_get_menu_text(fd_network->menu_mission));
       strsub(chfile, ' ', '_'); 
     }
   }

   DSload(chfile);

/*
//------
//jjj  Use this to get dsbn and evid from flatfile to to db -- ttt
if (!DSBMYSQL)
{
//  strcpy(chfile, dsbtypes[k].filebase);
int k = fl_get_choice(fd_network->dsb_story) - 1;
char chfile[256];
filenameToTablename(storyPaths[k], chfile);

//printf("would save (%s).(%s)\n", "IW_DCI", chfile);
//DS_AlgoSaveAttributes("IW_DCI", chfile, TRUE);
  DS_AlgoSaveAttributes(chfile, chfile, TRUE);

//ev.DS_saveEvidence(chfile, TRUE);
}
//------
*/
}

void netdrawCB(FL_OBJECT *object, long item_no)
{
int             i, j, k, Hlev, item = 0;
int             lwidth, lstyle, icolor;
int             savwidth;
float           nonbelief;
float           ImpactWeight;
const char      *ifield;
char            In_String[64];   /* Out - Parent node name                            */
char            Out_String[64];  /* Out - Child node name                             */
char            B_String[64];    /* Out - String describing belief value              */
char            D_String[64];    /* Out - String describing belief value              */
float           B_Weight;        /* Out - Line-source belief value                    */
float           D_Weight;        /* Out - Line-source disbelief value                 */
Window          winid;
FL_Coord        xcoord, ycoord, width, height;
FL_COLOR        lcolor[10] = { FL_RED, FL_TOMATO, FL_MAGENTA, FL_ORCHID, FL_DARKGOLD,
                               FL_WHEAT, FL_YELLOW, FL_GREEN, FL_CYAN, FL_BLUE };

   if(!fl_form_is_visible(fd_network->network) ) return;
//
//   Draw the network topology
//
   savwidth = fl_get_linewidth();
   lwidth = 2;
   lstyle = FL_SOLID;

   k = DS_AlgoGetTreeDepth();

   for (Hlev=0; Hlev<k-1; Hlev++) {
     for (i=0; i<DS_AlgoGetLevelWidth(Hlev); i++) {
       for (j=0; j<DS_AlgoGetLevelWidth(Hlev+1); j++) {
         DS_AlgoGetMatrix(i, Hlev, j, Hlev+1, In_String, Out_String,
			  B_String, D_String, B_Weight, D_Weight);
         if ((B_Weight > 0.0) &&
	     (fd_network->node_label[i][Hlev]->visible == 1) &&
	     (fd_network->node_label[j][Hlev+1]->visible == 1)) {
	   fl_get_object_geometry(fd_network->node_label[i][Hlev],
				  &xcoord, &ycoord, &width, &height);
	   HypX[i] = xcoord; HypY[i] = ycoord; HypW[i] = width; HypH[i] = height;
	   fl_get_object_geometry(fd_network->node_label[j][Hlev+1],
				  &xcoord, &ycoord, &width, &height);
	   Box1X = xcoord; Box1Y = ycoord; Box1W = width; Box1H = height;
	   fl_linewidth(lwidth);
	   fl_linestyle(lstyle);
	   icolor = (int)(B_Weight*10.0);
	   if (icolor > 9) icolor = 9;
	   fl_line((int)(HypX[i]+(int)(HypW[i]/2)), (int)(HypY[i])+(int)HypH[i],
		   (int)Box1X+(int)(Box1W/2), (int)Box1Y, lcolor[icolor]);
         }
       }
       fl_linewidth(savwidth);
       fl_linestyle(FL_SOLID);
     }
   }
}
/*                       New Network edit routines                       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void UpdateBUD(int EditCol, int EditRow, float B, float D, int DoBP)
{
FILE            *fp;
int             i, j;
float           results[6];
char            chnode[16];
char            chnow[32];
char            chtemp[40];
char            mystr[64];
char            filename[128];
char            *str;
time_t          clock;
struct tm       *loctime;
Window          thiswin;


   thiswin = fl_winget();
   fl_set_cursor(thiswin, XC_watch);

   results[1] = B;
   results[2] = D;

   nodeinfo[EditCol][EditRow].old_belief = nodeinfo[EditCol][EditRow].belief;
   nodeinfo[EditCol][EditRow].old_disbelief = nodeinfo[EditCol][EditRow].disbelief;

   DS_AlgoOverrideNodeValues(EditCol,EditRow,B,D);      // Set the new values

   if (DoBP) {
     DS_AlgoPropogateOverrides(LinkConstrain, LinkLearn, LinkRange);    // Do the back propagation
     printf(" Link Constraints is %d\n", LinkConstrain);
     DS_AlgoGetResult(EditCol, EditRow, results);
     if (results[1] == 0.0)
       fl_show_messages("Desired Belief not achievable due to no input beliefs");
     else if ((LinkConstrain==DS_Existing_Bounded_Constraint) && (absf(B-results[1])>0.01))
       fl_show_messages("Desired Belief not achievable with current Constraint setting");
     if (results[2] == 0.0)
       fl_show_messages("Desired Disbelief not achievable due to no input disbeliefs");
     else if ((LinkConstrain==DS_Existing_Bounded_Constraint) && (absf(D-results[2])>0.01))
       fl_show_messages("Desired Disbelief not achievable with current Constraint setting");
     LinkChange = TRUE;                                 // BP will likely change weights
   }

   nodeinfo[EditCol][EditRow].belief = results[1];
   nodeinfo[EditCol][EditRow].disbelief = results[2];
   UpdateNet(0);

   fl_show_object(fd_network->override[EditCol][EditRow]);

   if (OverrideLog) {
     time(&clock);
     loctime = gmtime(&clock);
     str = asctime(loctime);
     strcpy(chnow, str);
     strsub(chnow, '\n', '\0');
     //
     //   Add the override to the evidence list
     //
     i = ev.DS_addEvidence(chnode,                            // name
                           chnow,                             // description
                           NULL,                              // lat
                           NULL,                              // lon
                           NULL,                              // alt
                           NULL,                              // subfname
                           0,                                 // caseId
                           TRUE,                              // valid
                           EditCol+1,                         // source
                           EditRow+1,                         // level
                           1,                                 // etype
                           ev.evidences[ev.numEvid-1]->Tin + 0.1, // timeStamp
                           ev.evidences[ev.numEvid-1]->duration,  // duration
                           results[1],                        // confidence
                           results[2]);                       // disbelief
     //
     //   Add the evidence to the input window
     //
     //GetLexical(B, char* desc);
     //GetLexical(D, char* desc);
     sprintf(chtemp, "%11s (%4.1f)  %11s (%4.1f)",
             "Actual", B*100.0, "Actual", D*100.0);
     strcpy(mystr,
            fl_get_object_label(fd_network->node_label[EditCol][EditRow]));
     strsub(mystr, '\n', ' ');
     strncpy(chnode, mystr, 15);
     chnode[15] = '\0';
     sprintf(dsbtemp, "@N@C1%-16s %-34s %8.2f %8.2f %30s",
             chnode, chtemp, ev.evidences[i]->Tin,
             ev.evidences[i]->duration, chnow);
     fl_addto_browser(fd_dsevedit->input_browser, dsbtemp);
     //
     //   One more piece of evidence to process
     //
     EvidModified = TRUE;
   }

   fl_reset_cursor(thiswin);
}

void UpdateLink(int EditCol, int EditRow, float B, float D)
{
char            chtemp[40];
char            *str;
time_t          clock;
struct tm       *loctime;

   LinkChange = TRUE;                                   // BP will likely change weights
   time(&clock);
   loctime = gmtime(&clock);
   str = asctime(loctime);
   strcpy(chtemp, " ");
   sprintf(dsbtemp, "@N@C1%-16s %-34s %8.2f %8.2f %30s",
           "Node Override", chtemp, atof(fl_get_object_label(fd_network->cdate_text)),
	   -1.0, str);
   fl_addto_browser(fd_dsevedit->input_browser, dsbtemp);
   resetCB(NULL, 0);                                    // Start over with updated data
   executeCB(NULL, 0);                                  // Re-do evidence fusion
}

int IMGclose(FL_FORM *form, void *data)
{
long            item;

   exitCB(NULL, 13);

   return(0);
}

void LinkEdit(FL_OBJECT *object, int icol, int irow)
{
int             i, j, k, item = 0;
int             ncols;
float           results[6];
char            mystr[32];
char            In_String[64];   /* Out - Parent node name                            */
char            Out_String[64];  /* Out - Child node name                             */
char            B_String[64];    /* Out - String describing belief value              */
char            D_String[64];    /* Out - String describing belief value              */
float           B_Weight;        /* Out - Line-source belief value                    */
float           D_Weight;        /* Out - Line-source disbelief value                 */

       fl_show_form(fd_netedlink->netedlink, FL_PLACE_CENTER,FL_FULLBORDER, "Link Editor");
       //
       //   Clear out any old data hanging around
       //
       for (i=0; i<6; i++) {
         fl_set_object_label(fd_netedlink->netedfrom[i+1], " ");
         fl_set_input(fd_netedlink->neted_bw_from[i+1], " ");
         fl_set_object_label(fd_netedlink->neted_i_link[i+1], " ");
         fl_set_object_label(fd_netedlink->netedto[i+1], " ");
         fl_set_input(fd_netedlink->neted_bw_to[i+1], " ");
         fl_set_object_label(fd_netedlink->neted_o_link[i+1], " ");
       }
       //
       //   Fill in the current data for the selected node
       //
       strcpy(mystr, object->label);
       strsub(mystr, '\n', ' ');
       fl_set_object_label(fd_netedlink->netednodename, mystr);
       //
       //   Fill in the data from previous and subsequent rows
       //
       if (irow > 0) {                                 // Input only available for rows 1...
         ncols = DS_AlgoGetLevelWidth(irow-1);         // Get node count of row above
         for (j=0; j<ncols; j++) {                     // List inputs from all nodes this row
           DS_AlgoGetMatrix(j, irow-1, icol, irow, In_String, Out_String,
			    B_String, D_String, B_Weight, D_Weight);
           if (B_Weight > 0.0) {
	     fl_set_object_label(fd_netedlink->neted_i_link[j+1], "+");
           } else {
             fl_set_object_label(fd_netedlink->neted_i_link[j+1], "-");
           }
           strcpy(mystr, fl_get_object_label(fd_network->node_label[j][irow-1]));
	   strsub(mystr, '\n', ' ');
           fl_set_object_label(fd_netedlink->netedfrom[j+1], mystr);
           sprintf(dsbtemp, "%4.2f", B_Weight);
           fl_set_input(fd_netedlink->neted_bw_from[j+1], dsbtemp);
           sprintf(dsbtemp, "%4.2f", D_Weight);
           fl_set_input(fd_netedlink->neted_dw_from[j+1], dsbtemp);
         }
       }
       if (irow < 5) {                                 // Output only available for rows ...4
         ncols = DS_AlgoGetLevelWidth(irow+1);         // Get node count of row below
         for (j=0; j<ncols; j++) {                     // List outputs to all nodes this row
           DS_AlgoGetMatrix(icol, irow, j, irow+1, In_String, Out_String,
			    B_String, D_String, B_Weight, D_Weight);
           if (B_Weight > 0.0) {
	     fl_set_object_label(fd_netedlink->neted_o_link[j+1], "+");
           } else {
             fl_set_object_label(fd_netedlink->neted_o_link[j+1], "-");
           }
           strcpy(mystr, fl_get_object_label(fd_network->node_label[j][irow+1]));
	   strsub(mystr, '\n', ' ');
           fl_set_object_label(fd_netedlink->netedto[j+1], mystr);
           sprintf(dsbtemp, "%4.2f", B_Weight);
           fl_set_input(fd_netedlink->neted_bw_to[j+1], dsbtemp);
           sprintf(dsbtemp, "%4.2f", D_Weight);
             fl_set_input(fd_netedlink->neted_dw_to[j+1], dsbtemp);
         }
       }
}

void neteditCB(FL_OBJECT *object, long item_no)
{
int             i, j, k, item = 0;
int             menu;
int             Hlev, irow, icol, ncols, level;
int             nstories = 25;
int             Xpos, Ypos, Wpos, Hpos; 
float           results[6];
float           Tu;
char            mystr[32];
char            chfile[64];
char            In_String[64];   /* Out - Parent node name                            */
char            Out_String[64];  /* Out - Child node name                             */
char            B_String[64];    /* Out - String describing belief value              */
char            D_String[64];    /* Out - String describing belief value              */
float           B_Weight;        /* Out - Line-source belief value                    */
float           D_Weight;        /* Out - Line-source disbelief value                 */
Window          thiswin;
FL_IMAGE        *image;
Window          winid;
FL_FORM         *form;
//
FL_OBJECT       *vobj[25], *obj;
float           xx[5], y1[5];
float           B[25] = { 80.0, 70.0, 60.0, 75.0, 40.0, 65.0, 85.0, 55.0, 35.0,
                          70.0, 60.0, 75.0, 45.0, 65.0, 80.0, 85.0, 90.0, 20.0,
                          80.0, 55.0, 30.0, 45.0, 70.0, 60.0, 80.0 };
float           U[25] = {  5.0,  5.0, 30.0, 15.0, 25.0, 10.0,  5.0, 25.0, 40.0,
                           5.0,  5.0, 15.0, 35.0, 15.0, 10.0,  5.0,  5.0, 40.0,
                           5.0, 15.0, 40.0,  5.0,  5.0, 20.0,  5.0 };
float           D[25] = { 15.0, 25.0, 10.0, 10.0, 35.0, 25.0, 10.0, 20.0, 25.0,
                          25.0, 35.0, 10.0, 20.0, 20.0, 10.0, 10.0,  5.0, 40.0,
                          15.0, 30.0, 30.0, 50.0, 25.0, 20.0, 15.0 };
char            S[25][8] = { "SA-1", "SA-2", "CM-1", "SA-3", "SC-1", "CM-2", "SC-2", "SC-3", "CM-3",
                             "SA-4", "SC-3", "SC-4", "SA-5", "SC-5", "SC-6", "SA-6", "SA-7", "CM-3",
                             "SA-8", "SC-7", "CM-4", "SA-9", "SC-8", "CM-5", "SC-9" };
//
//   Find out where we are
//
   icol = (item_no/10);                           // 'irow' is the tree depth at the node
   irow = item_no % 10;                           // 'icol' is the selected node at this depth
   EditRow = irow;                                // Save row # for other CBs
   EditCol = icol;                                // Save node # for other CBs

   if (fl_mouse_button() == FL_LEFTMOUSE) {
     //
     //   Find out which edit mode
     //
     menu = fl_defpup(fl_winget(),
"Edit %t|Assessment|Stories|Node Fusion|Node Override|Node Copy|Node Paste|Link Edit|Influence");
     //
     switch (fl_dopup(menu)-1) {
     case 0:  // Assessment Graph
       fl_show_form(fd_budgraph->budgraph, FL_PLACE_CENTER,FL_FULLBORDER, "BUD Graph");
       resetCB(NULL, 0);
       curr_assess = icol;
       executeCB(NULL, 0);
       break;

     case 1:  // Compare Story Assessments
       nstories = atoi(fl_get_input(fd_dsstory->story_count));
       nstories = 25;
       if (nstories < 2) break;
       strcpy(dsbtemp, fl_get_object_label(fd_network->node_label[EditCol][EditRow]));
       strsub(dsbtemp, '\n', ' ');
       fl_set_object_label(fd_storygraph->story_hypoth, dsbtemp);
       //
       j = -18;
       fd_storyBUDs = fl_bgn_form(FL_FLAT_BOX, 1000, 340);
         for (i=0; i<nstories; i++) {
	   j = j + 28;
	   vobj[i] = fl_add_stacked(FL_VERT_STACKED, j, 30, 23, 310, S[i]);
	   fl_set_object_lcolor(vobj[i], FL_CYAN);
	   fl_set_object_lsize(vobj[i],  FL_TINY_SIZE);
	   fl_set_object_lstyle(vobj[i], FL_BOLD_STYLE);
	   fl_set_object_lalign(vobj[i], FL_ALIGN_TOP|FL_ALIGN_CENTER);
	   //fl_set_object_callback(vobj, done, 0);
	   fl_add_stacked_value(vobj[i], B[i], "B", FL_BLUE);
	   fl_add_stacked_value(vobj[i], U[i], "B", FL_YELLOW);
	   fl_add_stacked_value(vobj[i], D[i], "B", FL_RED);
	   sprintf(dsbtemp, "%s Neutralized\nBelief = %d%%\nIgnorance = %d%%\nConflict = %d%%",
		   S[i], (int)B[i], (int)U[i], (int)D[i]); 
	   fl_set_object_helper(vobj[i], dsbtemp);
         }
         obj = fl_add_positioner(FL_NORMAL_POSITIONER,0,100,1000,10,"");
         fl_set_object_boxtype(obj,FL_NO_BOX);
       fl_end_form();
       //
       fl_addto_formbrowser(fd_storygraph->story_browse, fd_storyBUDs);
       //
       graph = fd_storygraph->story_plot; 
       fl_set_xyplot_xbounds(graph, 0.0, 1000.0);
       fl_set_xyplot_ybounds(graph, 0.0, 100.0);
       fl_set_xyplot_xtics(graph, -1, -1);
       fl_set_xyplot_ytics(graph, 6, 0);
       xx[0] = 0.0;
       y1[0] = 0.0;
       xx[1] = 1000.0;
       y1[1] = 75.0;
       fl_set_xyplot_data(graph, xx, y1, 1, "","","");
       //
       fl_winposition(DSBwinX, DSBwinY);
       fl_initial_winsize(DSBwinW, DSBwinH);
       winid = fl_prepare_form_window(fd_storygraph->storygraph,
                                  FL_PLACE_POSITION,FL_NOBORDER, "Story-Explain");
       fl_winreparent(winid, DSBwinid);
       fl_show_form_window(fd_storygraph->storygraph);
       StoreActiveEntry("Story-Explain");
       break;

     case 2: // Introduce New B&D
       fl_show_form(fd_netedfuse->netedfuse,FL_PLACE_CENTER,FL_FULLBORDER,"Node Fusion");
       //
       //   Fill in the current data for the selected node
       //
       strcpy(mystr, object->label);
       strsub(mystr, '\n', ' ');
       fl_set_object_label(fd_netedfuse->netedBnode, mystr);
       //
       DS_AlgoGetResult(icol, irow, results);
       EditBelief = results[1];
       //sprintf(dsbtemp, "%f", results[1]);
       fl_set_counter_value(fd_netedfuse->netedB_b_value, (double)EditBelief);
       k = GetLexical(EditBelief, dsbtemp);
       fl_set_choice(fd_netedfuse->netedfuseB, k);

       EditDisbelief = results[2];
       //sprintf(dsbtemp, "%f", results[2]);
       fl_set_counter_value(fd_netedfuse->netedB_d_value, (double)EditDisbelief);
       k = GetLexical(EditDisbelief, dsbtemp);
       fl_set_choice(fd_netedfuse->netedfuseD, k);

       EditTime = ev.Tlast+1.0; //results[4];
       sprintf(mystr, "%f", EditTime);
       fl_set_object_label(fd_netedfuse->netedB_t_value, mystr); 
       break;

     case 3: // Node Override
       fl_show_form(fd_netednode->netednode,FL_PLACE_CENTER,FL_FULLBORDER,"Node Editor");
       //
       //   Fill in the current data for the selected node
       //
       strcpy(mystr, object->label);
       strsub(mystr, '\n', ' ');
       fl_set_object_label(fd_netednode->netednodename, mystr);
       //
       DS_AlgoGetResult(icol, irow, results);
       EditBelief = results[1];
       //sprintf(dsbtemp, "%f", results[1]);
       fl_set_counter_value(fd_netednode->neted_b_value, (double)EditBelief);
       k = GetLexical(EditBelief, dsbtemp);
       fl_set_choice(fd_netednode->netedbelief, k);
       EditDisbelief = results[2];
       //sprintf(dsbtemp, "%f", results[2]);
       fl_set_counter_value(fd_netednode->neted_d_value, (double)EditDisbelief);
       k = GetLexical(EditDisbelief, dsbtemp);
       fl_set_choice(fd_netednode->neteddisbelief, k);
       break;

     case 4: // Save belief and disbelief for future paste
       DS_AlgoGetResult(icol, irow, results);
       CopyBelief = results[1];
       CopyDisbelief = results[2];
       CopySelected = TRUE;
       break;

     case 5: // Paste changes to a nodes's B & D and update the network using BP
       if (CopySelected) {
         UpdateBUD(icol, irow, CopyBelief, CopyDisbelief, 1);
       }
       break;

     case 6: // Link Edit
       LinkEdit(object, icol, irow);
       break;

     case 7: // Influence
       DS_AlgoReset();
       DS_AlgoReverseLinks();
       DS_Algo(icol, irow, 1.0, 0.0, 0.0);
       UpdateNet(0);
       DS_AlgoReverseLinks();

       for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
	 Tu = 0.0;
	 for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	   DS_AlgoGetResult(i, level, results);
	   //
	   //   Color-code the node depending on bestBelief
	   //	   
	   fl_set_object_color(fd_network->node_label[i][level], FL_GOLD, FL_GOLD);
	   if (results[1] >= Tu) { Tu = results[1]; j = i; }
	 }
	 fl_set_object_color(fd_network->node_label[j][level], FL_TOMATO, FL_TOMATO);
       }
       break;

     default:
       break;
     } //switch
   }

   if (fl_mouse_button() == FL_RIGHTMOUSE) {
     DSPshow(DSBx, DSBy, DSBw, DSBh, DSBwid);
   }

   fl_freepup(menu);
   //
   //   Just hang out waiting for user to make the next move,
   //   which should put us in the following done CB.
   //
   return;
}

void tctthreshCB(FL_OBJECT *object, long item_no)
{
  int Xpos, Ypos;

  fprintf(stderr, "Drawing threshold\n");
       fl_linewidth(2);
       fl_linestyle(FL_SOLID);
       Xpos = 0;
       Ypos = 0;
       fl_line(Xpos+60, Ypos+180, Xpos+60+700, Ypos+180, FL_BLACK);
}

void neteddoneCB(FL_OBJECT *object, long item_no)
{
FILE            *fp;
int             i, j, k, n, irow, icol, ncols, nrows;
int             level, lowest, outer, ipath, error;
int             J0 = 1;
int             Ke = 21;
int             n_nodes = -1;
int             nodes;
int             nodecols[50], noderows[50], nodelist[50];
int             nodefirst[10];
int             linktable[10];
float           B, D, T, B_W, D_W;
float           change;
float           ratio, table[36][36], coltable[36];
float           results[6];
float           Bsum, Dsum, Usum, Bavg, Davg, Uavg, Btot, Dtot, Utot, Average;
char            *BITMAPDIR;
const char      *fname;
const char      *str;
char            chline[32];
char            chtemp[64];
char            In_String[64];   /* Out - Parent node name                            */
char            Out_String[64];  /* Out - Child node name                             */
char            B_String[64];    /* Out - String describing belief value              */
char            D_String[64];    /* Out - String describing belief value              */
float           B_Weight;        /* Out - Line-source belief value                    */
float           D_Weight;        /* Out - Line-source disbelief value                 */
FL_COLOR        c;
FL_COLOR        pcolor[4] = { FL_BLACK, FL_DARKCYAN, FL_TOMATO, FL_DARKGOLD };
FL_OBJECT       *ipipm;
DS_BP_Run_Statistics_Type bpstats;
#ifndef NOGL
H3DINFO         h3dinfo;
#endif

   switch (item_no) {
     case 0: // Next> changes a node's B & D
       B = (float)fl_get_counter_value(fd_netednode->neted_b_value);  // Get changed belief
       D = (float)fl_get_counter_value(fd_netednode->neted_d_value);  // Get changed disbelief
       UpdateBUD(EditCol, EditRow, B, D, 0);                // Set the new values
       fl_hide_form(fd_netednode->netednode);               // Get rid of the edit dialog
       fl_trigger_object(fd_network->net_show);             // Force network re-draw
       break;

     case 1: // Save Link values
       if (LinkChange == FALSE)
         LinkChange = fl_show_question("No LINK changes made! Save anyway?", 0);
       if (LinkChange == TRUE)
         saveNet();
       LinkChange = FALSE;
       break;

     case 2: // Apply changes to a nodes's B & D and update the network using BP
       B = (float)fl_get_counter_value(fd_netednode->neted_b_value); // Get changed belief
       D = (float)fl_get_counter_value(fd_netednode->neted_d_value); // Get changed disbelief
       UpdateBUD(EditCol, EditRow, B, D, 1);
     case 4:
       fl_hide_form(fd_netednode->netednode);               // Get rid of the edit dialog
       fl_trigger_object(fd_network->net_show);             // Force network re-draw
       break;

     case 3: // Link Edit
       i = EditCol;
       irow = EditRow;
       ncols = DS_AlgoGetLevelWidth(irow-1);                // Get node count of row above
       for (j=0; j<ncols; j++) {                            // Do inputs from all nodes this row
	 B_W = atof(fl_get_input(fd_netedlink->neted_bw_from[j+1]));
	 D_W = atof(fl_get_input(fd_netedlink->neted_dw_from[j+1]));
	 DS_AlgoSetNodeImpact(j, irow-1, i, irow, B_W, D_W);
       }
       ncols = DS_AlgoGetLevelWidth(irow+1);                // Get node count of row below
       for (j=0; j<ncols; j++) {                            // Do outputs to all nodes this row
	 B_W = atof(fl_get_input(fd_netedlink->neted_bw_to[j+1]));
	 D_W = atof(fl_get_input(fd_netedlink->neted_dw_to[j+1]));
	 DS_AlgoSetNodeImpact(i, irow, j, irow-1, B_W, D_W);
       }
       LinkChange = TRUE;
       resetCB(NULL, 0);                                    // Start over with updated data
       executeCB(NULL, 0);                                  // Re-do evidence fusion
     case 5:
       fl_hide_form(fd_netedlink->netedlink);               // Get rid of the edit dialog
       fl_trigger_object(fd_network->net_show);             // Force network re-draw
       break;

     case 6: // 3D Histogram
       for (i=0; i<36; i++) {
         for (j=0; j<36; j++) {
	   table[i][j] = 0.01; //drand48()*10.0;
	 }
       }
       fl_show_form(fd_completion->completion,FL_PLACE_CENTER,FL_FULLBORDER,"Percent Complete");
       fl_set_object_label(fd_completion->message, "Determining Forward Effects");
       fl_check_forms();
       //
       //   Do all the nodes for the forward effects
       //
       change = 0.1;
       icol = -1;
       for (outer=0; outer<DS_AlgoGetTreeDepth(); outer++) {
         for (k=0; k<DS_AlgoGetLevelWidth(outer); k++) {
           DS_AlgoReset();
           DS_Algo(k, outer, change, 0.0, 1.0);
           n = 0;
	   icol++;
           for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
             for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	       DS_AlgoGetResult(i, level, results);
	       table[icol][n] = (results[1]/change);
	       n++;
	     }
           }
         }
       }
       fl_set_object_label(fd_completion->message, "Determining Backward Effects");
       fl_check_forms();
       ncols = icol;
       nrows = n;
       //
       //   Do all the nodes for the backward effects
       //
       if (DiffBP) {
       icol = n;
       for (outer=DS_AlgoGetTreeDepth()-1; outer>0; outer--) {
         for (k=DS_AlgoGetLevelWidth(outer)-1; k>=0; k--) {
	   //
	   //   Initialze all nodes to B = 1.0
	   //
           for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
             for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	       if (level == 0)
		 DS_Algo(i, level, 1.0, 0.0, 0.0);
	       else
		 DS_AlgoOverrideNodeValues(i, level, 1.0, 0.0);
	     }
	   }
	   //
	   DS_AlgoOverrideNodeValues(k, outer, 1.0-change, 0.0);
	   DS_AlgoPropogateOverrides(DS_Any_Bounded_Constraint, 0.5, 6); // Do the back propagation
           n = 0;
	   icol--;
	   printf("Putting node %d level %d in table col %d\n", k, outer, icol);
           for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
             for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	       DS_AlgoGetResult(i, level, results);
	       coltable[n] = (1.0-results[1])/change;
	       printf("%f  ", coltable[n] );
	       n++;
	     }
           }
	   printf("\n");
	   for (i=DS_AlgoGetLevelWidth(0); i<=icol; i++) {
	     table[icol][i] = coltable[i];
	   }
	   fl_set_slider_value(fd_completion->percent,
		      ((double)(ncols-icol)/(double)ncols)*100.0);
	   fl_check_forms();
         }
       }
       }  // End -- DiffBP
       fl_hide_form(fd_completion->completion);
       //
#ifndef NOGL
       h3dinfo.delta = change;
       h3dinfo.vscale = 10.0;
       h3dinfo.nlevels = DS_AlgoGetTreeDepth();
       for (level=0; level<h3dinfo.nlevels; level++) {
	 h3dinfo.nnodes[level] = DS_AlgoGetLevelWidth(level);
	 strcpy(h3dinfo.label[level], fl_get_object_label(fd_network->level_label[level]));
         strsub(h3dinfo.label[level], '\n', ' ');
       }
       H3Ddata(ncols, nrows, table, h3dinfo);
       H3Dshow(DSBx, DSBy, DSBw, DSBh, DSBwid);
#endif
       break;

     case 7: // Show delta belief
       for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
         for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	   c = FL_TOMATO;
	   if ((nodeinfo[i][level].belief - nodeinfo[i][level].old_belief) > -0.6) c = FL_DARKTOMATO;
	   if ((nodeinfo[i][level].belief - nodeinfo[i][level].old_belief) > -0.25) c = FL_WHEAT;
	   if ((nodeinfo[i][level].belief - nodeinfo[i][level].old_belief) > 0.25) c = FL_PALEGREEN;
	   if ((nodeinfo[i][level].belief - nodeinfo[i][level].old_belief) > 0.6) c = FL_CYAN;
	   fl_set_object_color(fd_network->node_label[i][level], c, c);
	   //nodeinfo[i][level].disbelief = results[2];
         }
       }
       break;

     case 8: // Network editor
       lowest = -1;
       fl_clear_choice(fd_neteditor->netedit_level);
       for (i=0; i<6; i++) {
	 //
	 //   Indicate which levels have room for more nodes
	 //
         ipipm = fd_network->net_toomany[i];
         fl_show_object(ipipm);
         fl_free_pixmap_pixmap(ipipm);
	 if (DS_AlgoGetLevelWidth(i) < 6) {
	   if (lowest < 0) lowest = i+1;
           sprintf (dsbtemp, "%s/%s", DSBitMaps, "led-green.xpm");
	 } else {
           sprintf (dsbtemp, "%s/%s", DSBitMaps, "led-red.xpm");
	 }
         fl_set_pixmap_file(ipipm, dsbtemp);
	 //
	 //   Set up for choice of level
	 //
	 strcpy(chtemp, fl_get_object_label(fd_network->level_label[i]));
	 strsub(chtemp, '\n', ' ');
	 fl_addto_choice(fd_neteditor->netedit_level, chtemp);
	 fl_set_choice(fd_neteditor->netedit_level, lowest);
       }
       //
       //   Set up for choice of node
       //
       fl_clear_choice(fd_neteditor->netedit_node);
       for (i=0; i<DS_AlgoGetLevelWidth(lowest-1); i++) {
	 strcpy(chtemp, fl_get_object_label(fd_network->node_label[i][lowest-1]));
	 strsub(chtemp, '\n', ' ');
	 fl_addto_choice(fd_neteditor->netedit_node, chtemp);
       }
       fl_set_choice(fd_neteditor->netedit_node, DS_AlgoGetLevelWidth(lowest-1));

       fl_clear_choice(fd_neteditor->netedit_belief);
       fl_clear_choice(fd_neteditor->netedit_disbelief);
       for (i=0; i<LexCount; i++) {
	 fl_addto_choice(fd_neteditor->netedit_belief, LexTable[i].desc);
	 fl_addto_choice(fd_neteditor->netedit_disbelief, LexTable[i].desc);
       }
       fl_set_choice(fd_neteditor->netedit_belief, 1);
       fl_set_choice(fd_neteditor->netedit_disbelief, 1);

       if ((fp = fopen("NewNode.lock", "r")) != NULL) {
	 //sprintf(dsbtemp, "%s/%s", DSBitMaps, "led-green.xpm");
	 fscanf(fp, "%s", dsbtemp);               // Get Mission Domain
	 fscanf(fp, "%s", dsbtemp);               // Get suggested node name
	 strsub(dsbtemp, '_', ' ');
	 fclose(fp);
       } else {
	 strcpy(dsbtemp, "New Concept Node");
       }
       fl_set_input(fd_neteditor->netedit_name, dsbtemp);

       fl_show_form(fd_neteditor->neteditor,FL_PLACE_CENTER,FL_FULLBORDER,"Network Editor");
       break;

     case 9: // Deltas Table
       fl_clear_browser(fd_budtable->bud_cur_belief);
       fl_clear_browser(fd_budtable->bud_old_belief);
       fl_clear_browser(fd_budtable->bud_del_belief);
       fl_clear_browser(fd_budtable->bud_cur_disbelief);
       fl_clear_browser(fd_budtable->bud_old_disbelief);
       fl_clear_browser(fd_budtable->bud_del_disbelief);
       for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
         for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
           sprintf(chline, "%f", nodeinfo[i][level].belief);
           fl_addto_browser(fd_budtable->bud_cur_belief, chline);
           sprintf(chline, "%f", nodeinfo[i][level].old_belief);
           fl_addto_browser(fd_budtable->bud_old_belief, chline);
           sprintf(chline, "%f", nodeinfo[i][level].belief-nodeinfo[i][level].old_belief);
           fl_addto_browser(fd_budtable->bud_del_belief, chline);

           sprintf(chline, "%f", nodeinfo[i][level].disbelief);
           fl_addto_browser(fd_budtable->bud_cur_disbelief, chline);
           sprintf(chline, "%f", nodeinfo[i][level].old_disbelief);
           fl_addto_browser(fd_budtable->bud_old_disbelief, chline);
           sprintf(chline, "%f", nodeinfo[i][level].disbelief-nodeinfo[i][level].old_disbelief);
           fl_addto_browser(fd_budtable->bud_del_disbelief, chline);
         }
       }
       fl_show_form(fd_budtable->budtable,FL_PLACE_CENTER,FL_FULLBORDER,"Table View");
       TableView = TRUE;
       break;

     case 10: // Statistics
       if (DS_AlgoGetBPRunStatistics(&bpstats) == 0) {
         sprintf(dsbtemp, "%d", bpstats.Num_Iterations);        /* # backpropogation iterations */
         fl_set_input(fd_dsbstats->dsb_stat_niters, dsbtemp);
         sprintf(dsbtemp, "%d", bpstats.Num_B_Weights_Changed); /* # links whose B weight changed */
         fl_set_input(fd_dsbstats->dsb_stat_bchange, dsbtemp);
         sprintf(dsbtemp, "%d", bpstats.Num_D_Weights_Changed); /* # links whose D weight changed */
         fl_set_input(fd_dsbstats->dsb_stat_dchange, dsbtemp);
         sprintf(dsbtemp, "%d", bpstats.Constraint_Level);      /* Constraint level for run */
         fl_set_input(fd_dsbstats->dsb_stat_constraint, dsbtemp);
         sprintf(dsbtemp, "%f", bpstats.Learning_Rate);         /* Learning rate for run */
         fl_set_input(fd_dsbstats->dsb_stat_learn, dsbtemp);
         sprintf(dsbtemp, "%f", bpstats.B_Error);               /* Max diff in desired & actual Bs */
         fl_set_input(fd_dsbstats->dsb_stat_berror, dsbtemp);
         sprintf(dsbtemp, "%f", bpstats.D_Error);               /* Max diff in desired & actual Ds */
         fl_set_input(fd_dsbstats->dsb_stat_derror, dsbtemp);
         sprintf(dsbtemp, "%f", bpstats.L);
         fl_set_input(fd_dsbstats->dsb_stat_l, dsbtemp);
         sprintf(dsbtemp, "%f", bpstats.meanL);
         fl_set_input(fd_dsbstats->dsb_stat_meanl, dsbtemp);
         fl_show_form(fd_dsbstats->dsbstats,FL_PLACE_CENTER,FL_FULLBORDER,"BP Statistics");
       } else {
	 fl_show_messages("No Back Propagation statistics available!");
       }
       break;

     case 11: // Path Analysis
       if ((fp = fopen("TempPath.out", "w")) == NULL) return;
       k = 0;
       nodefirst[0] = 1;
       nodefirst[1] = 2;
       for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
	 nodefirst[level+2] = nodefirst[level+1] + DS_AlgoGetLevelWidth(level);
         for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	   k++;
	 }
       }
       Ke = k+2;
       //
       //   Take care of the non-existent top-level node by connecting
       //   it up with each node on the 1st level of the network and
       //   set the "length" to 1-B (we want the shortest distance).
       //
       fprintf(fp, "%8d            # No. of nodes\n", Ke);
       fprintf(fp, "#    Node     Col     Row   # Links\n");
       fprintf(fp, "%8d%8d%8d%8d", 1, 1, 0, DS_AlgoGetLevelWidth(0));
       for (i=0; i<DS_AlgoGetLevelWidth(0); i++) fprintf(fp, "%8d", i+2);
       fprintf(fp, "\n");
       fprintf(fp, "        ");
       ipath = PathNum;
       for (i=0; i<DS_AlgoGetLevelWidth(0); i++) {
	 DS_AlgoGetResult(i, 0, results);
	 fprintf(fp, "%8d", (int)((1.0-results[ipath])*100.0));
       }
       fprintf(fp, "\n");
       //
       //   Take care of the nodes on levels 1..'depth' by connecting
       //   them with nodes on the next level of the network and set
       //   the "length" to 1-B (we want the shortest distance) of the
       //   node on that level.
       //
       k = 1;
       for (level=0; level<DS_AlgoGetTreeDepth()-1; level++) {
         for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	   k++;
           fprintf(fp, "%8d%8d%8d", k, i+1, level+1);
	   n = 0;
           for (j=0; j<DS_AlgoGetLevelWidth(level+1); j++) {
             DS_AlgoGetMatrix(i, level, j, level+1, In_String, Out_String,
			    B_String, D_String, B_Weight, D_Weight);
             if (B_Weight > 0.0) { linktable[n] = nodefirst[level+2]+j; n++; }
	   }
           fprintf(fp, "%8d", n);
           for (j=0; j<DS_AlgoGetLevelWidth(level+1); j++) {
             DS_AlgoGetMatrix(i, level, j, level+1, In_String, Out_String,
			    B_String, D_String, B_Weight, D_Weight);
             if (B_Weight > 0.0) fprintf(fp, "%8d", nodefirst[level+2]+j);
	   }
           fprintf(fp, "\n");
	   fprintf(fp, "        ");
           for (j=0; j<DS_AlgoGetLevelWidth(level+1); j++) {
             DS_AlgoGetMatrix(i, level, j, level+1, In_String, Out_String,
			    B_String, D_String, B_Weight, D_Weight);
             if (B_Weight > 0.0) {
	       DS_AlgoGetResult(j, level+1, results);
	       fprintf(fp, "%8d", (int)((1.0-results[ipath])*100.0));
	     }
	   }
           fprintf(fp, "\n");
	 }
       }
       for (i=0; i<DS_AlgoGetLevelWidth(DS_AlgoGetTreeDepth()-1); i++) {
	 k++;
	 fprintf(fp, "%8d%8d%8d%8d%8d\n", k, i+1, DS_AlgoGetTreeDepth(), 1, Ke);
	 fprintf(fp, "        ");
	 fprintf(fp, "%8d\n", 0);
       }
       fprintf(fp, "%8d%8d%8d%8d\n", Ke, 1, DS_AlgoGetTreeDepth()+1, 0);
       fclose(fp);
       //fl_show_object(fd_network->blueled);
       //
#ifndef NOGL
       spath_(J0, Ke, n_nodes, nodecols, noderows, nodelist);
       //fprintf(stderr, " Path length is %d nodes\n", n_nodes[0]);
       for (i=1; i<n_nodes-1; i++) {
	 j = nodecols[nodelist[i]-1]-1;
	 k = noderows[nodelist[i]-1]-1;
         //fprintf(stderr, " Node = %d, Col = %d, Row = %d\n", nodelist[i], j, k);
         //fl_set_object_color(fd_network->node_label[j][k], pcolor[ipath], pcolor[ipath]);
	 if (ipath == 1) fl_show_object(fd_network->bestpath[j][k]);
	 else if (ipath == 2) fl_show_object(fd_network->worstpath[j][k]);
	 else if (ipath == 3) fl_show_object(fd_network->leastpath[j][k]);
       }
#endif
       break;

     case 12: // Story Info
       Btot = 0.0; Dtot = 0.0; Utot = 0.0;
       for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
	 strcpy(dsbtemp, fl_get_object_label(fd_network->level_label[level]));
	 strsub(dsbtemp, '\n', ' ');
         fl_set_object_label(fd_dsbstory->story_label[level], dsbtemp);
	 Bsum = 0.0; Dsum = 0.0; Usum = 0.0;
	 nodes = DS_AlgoGetLevelWidth(level);
         for (i=0; i<nodes; i++) {
	   DS_AlgoGetResult(i, level, results);
	   Bsum = Bsum + results[1];
	   Dsum = Dsum + results[2];
	   Usum = Usum + results[3];
	 }
	 Bavg = Bsum/(float)nodes;
	 sprintf(dsbtemp, "%f", Bavg);
	 fl_set_object_label(fd_dsbstory->story_belief[level], dsbtemp);
	 Davg = Dsum/(float)nodes;
	 sprintf(dsbtemp, "%f", Davg);
	 fl_set_object_label(fd_dsbstory->story_disbelief[level], dsbtemp);
	 Uavg = Usum/(float)nodes;
	 sprintf(dsbtemp, "%f", Uavg);
	 fl_set_object_label(fd_dsbstory->story_unknown[level], dsbtemp);
	 Btot = Btot + Bavg;
	 Dtot = Dtot + Davg;
	 Utot = Utot + Uavg;
       }
//jjj - need copy of this for db somewhere
       sprintf(dsbtemp, "%f", Btot/(float)DS_AlgoGetTreeDepth());
       fl_set_object_label(fd_dsbstory->story_btotal, dsbtemp);
       sprintf(dsbtemp, "%f", Dtot/(float)DS_AlgoGetTreeDepth());
       fl_set_object_label(fd_dsbstory->story_dtotal, dsbtemp);
       sprintf(dsbtemp, "%f", Utot/(float)DS_AlgoGetTreeDepth());
       fl_set_object_label(fd_dsbstory->story_utotal, dsbtemp);
       /*
       Average = Btot/(float)DS_AlgoGetTreeDepth();
       sprintf(dsbtemp, "%f", Average);
       fl_set_input(fd_dsbstory->story_rating, dsbtemp);
       */
       fl_show_form(fd_dsbstory->dsbstory,FL_PLACE_CENTER,FL_FULLBORDER,"Story Info");
       break;

     case 13: // Snapshot
       assess_ctlCB(NULL, 0);

       n = fl_get_menu(fd_network->menu_mission) - 1;
       sprintf(dsbtemp, "xwd -name %s | convert - gif:./DSBFiles/%s.gif",
               "Assess-Network", dsbtypes[n].filebase);
       system(dsbtemp);
       break;

     case 14: // Save Net
       saveNet();
       break;

     case 15: // Intersection
       if (DSBMYSQL) {
return;   // DS_AlgoMergeTrees doesn't know DB yet ----------------ttt
         strcpy(dsbtemp, fl_get_object_label(fd_network->net_compare));
         sprintf(chtemp, "DSBFiles/%s.save", dsbtemp);
       } else {
         sprintf(dsbtemp, "DSBFiles/%s.dsbn",
                 fl_get_object_label(fd_network->net_compare));
         sprintf(chtemp, "DSBFiles/%s.save",
                 fl_get_object_label(fd_network->net_compare));
       }      

       if (DS_AlgoMergeTrees(DS_Intersection, dsbtemp, chtemp) != -1) {
         DSinit(FALSE, NULL, NULL);
         resetCB(NULL, 0);                                  // Start over with updated data
         executeCB(NULL, 0);                                // Re-do evidence fusion
       } else {
         fl_show_messages("Error performing operation!");
       }      
       break;

     case 16: // Union
       if (DSBMYSQL) {
return;   // DS_AlgoMergeTrees doesn't know DB yet ----------------ttt
         strcpy(dsbtemp, fl_get_object_label(fd_network->net_compare));
         sprintf(chtemp, "DSBFiles/%s.save", dsbtemp);
       } else {
         sprintf(dsbtemp, "DSBFiles/%s.dsbn",
                 fl_get_object_label(fd_network->net_compare));
         sprintf(chtemp, "DSBFiles/%s.save",
                 fl_get_object_label(fd_network->net_compare));
       }      

       DS_AlgoMergeTrees(DS_Union, dsbtemp, chtemp);
       DSinit(FALSE, NULL, NULL);
       //
//       error = savedNet_readFile(chtemp);   not used unless want to know if there is a .save ------------ttt
//       if (!error) {
	 for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
	   int NoNodes = DS_AlgoGetNumMergeNodesLost(level);
	   //
	   //   Turn on red LED if nodes have been dropped at this level
	   //
	   if (NoNodes > 0) {
	     fl_show_object(fd_network->net_toomany[level]);
	     fl_free_pixmap_pixmap(fd_network->net_toomany[level]);
             sprintf (dsbtemp, "%s/%s", DSBitMaps, "led-red.xpm");
	     fl_set_pixmap_file(fd_network->net_toomany[level], dsbtemp);
	   }
	   for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {  // For each node this level
	     switch (DS_AlgoGetMergeNodesOriginalTree(i, level)) {
	     case 0: // Both networks
	       fl_set_object_color(fd_network->node_label[i][level], FL_CYAN, FL_CYAN);
	       break;
	     case 1: // Second (current) network
	       fl_set_object_color(fd_network->node_label[i][level], FL_WHEAT, FL_WHEAT);
	       break;
	     case 2: // First (saved) network
	       fl_set_object_color(fd_network->node_label[i][level], FL_PALEGREEN, FL_PALEGREEN);
	       break;
	     }
	   }
	 }
//       }
       //
       netdrawCB(NULL, 0);                                  // Draw the links
       resetCB(NULL, 0);                                    // Start over with updated data
       executeCB(NULL, 0);                                  // Re-do evidence fusion
       break;

     case 17: // Apply changes to a nodes's B & D
       B = (float)fl_get_counter_value(fd_netedfuse->netedB_b_value); // Get changed belief
       D = (float)fl_get_counter_value(fd_netedfuse->netedB_d_value); // Get changed disbelief
       T = atof(fl_get_object_label(fd_netedfuse->netedB_t_value));
       DS_Algo(EditCol, EditRow, B, D, T);
       UpdateNet(0);
     case 18:
       fl_hide_form(fd_netedfuse->netedfuse);               // Get rid of the edit dialog
       fl_trigger_object(fd_network->net_show);             // Force network re-draw
       break;

     default: // Diagnostic
       DS_AlgoReset();
       DS_AlgoReverseLinks();
       //float belief = 1.0;
       //float disbelief = 0.0;
       DS_Algo(0, DS_AlgoGetTreeDepth()-1, 1.0, 0.0, 0.0);
       UpdateNet(0);
       DS_AlgoReverseLinks();
       break;
   }
}

void pathsCB(FL_OBJECT *object, long item_no)
{
int             i, j, k, item = 0;

   for (j=0; j<MAXLEVELS; j++)
     for (i=0; i<MAXNODES; i++) {
       fl_hide_object(fd_network->bestpath[i][j]);
       fl_hide_object(fd_network->worstpath[i][j]);
       fl_hide_object(fd_network->leastpath[i][j]);
     }

   PathNum = 1;
   neteddoneCB(NULL, 11);
   PathNum = 2;
   neteddoneCB(NULL, 11);
   PathNum = 3;
   neteddoneCB(NULL, 11);

   return;
}

void netchartCB(FL_OBJECT *object, long item_no)
{
int             i, j, k, item = 0;

   return;
}

void netedhelpCB(FL_OBJECT *object, long item_no)
{
int             k, i, xval, yval;
int             irow, icol;
float           results[6];
char            chline[16];
char            chline1[32];
char            chline4[32];
char            In_String[64];   /* Out - Parent node name                            */
char            Out_String[64];  /* Out - Child node name                             */
char            B_String[64];    /* Out - String describing belief value              */
char            D_String[64];    /* Out - String describing belief value              */
float           B_Weight;        /* Out - Line-source belief value                    */
float           D_Weight;        /* Out - Line-source disbelief value                 */

   irow = EditRow;
   icol = item_no % 10;

   switch (item_no)
   {
     case 1:
     case 2:
     case 3:
     case 4:
     case 5:
     case 6:
       sprintf(chline1, "%s", "For the Evidence that");
       sprintf(chline4, "%s", "in the Hypothesis that");
       DS_AlgoGetMatrix(icol-1, irow, EditCol, irow-1, In_String, Out_String,
			B_String, D_String, B_Weight, D_Weight);
       break;

     case 11:
     case 12:
     case 13:
     case 14:
     case 15:
     case 16:
       sprintf(chline1, "%s", "For the Hypothesis that");
       sprintf(chline4, "%s", "on the Conclusion that");
       DS_AlgoGetMatrix(EditCol, irow-1, icol-1, irow-2, In_String, Out_String,
			B_String, D_String, B_Weight, D_Weight);
       break;
   }

   DS_AlgoGetResult(EditCol, EditRow, results);
   sprintf(chline, "%d%%", (int)(results[1]*100.0));
   fl_set_object_label(fd_dshelpevid->explain_belief, chline);
   sprintf(chline, "%d%%", (int)(results[2]*100.0));
   fl_set_object_label(fd_dshelpevid->explain_disbelief, chline);
   sprintf(chline, "%d%%", (int)(results[3]*100.0));
   fl_set_object_label(fd_dshelpevid->explain_unknown, chline);

   fl_set_object_label(fd_dshelpevid->ds_help_line1, chline1);
   fl_set_object_label(fd_dshelpevid->evid_desc, In_String);
   fl_set_object_label(fd_dshelpevid->belief_desc, B_String);
   fl_set_object_label(fd_dshelpevid->ds_help_line4, chline4);
   fl_set_object_label(fd_dshelpevid->disbelief_desc, D_String);
   fl_set_object_label(fd_dshelpevid->out_desc, Out_String);
   fl_show_form(fd_dshelpevid->dshelpevid,FL_PLACE_CENTER,FL_FULLBORDER,
                    "Weighting Explanation");
}

void neteditorCB(FL_OBJECT *object, long item_no)
{
int             k, i, j, ix, xval, yval;
int             lowest, ncols;
int             irow, icol;
float           results[6];
char            chline[16];
char            chline1[32];
char            chline4[32];
char            In_String[64];   /* Out - Parent node name                            */
char            Out_String[64];  /* Out - Child node name                             */
char            B_String[64];    /* Out - String describing belief value              */
char            D_String[64];    /* Out - String describing belief value              */
float           B_Weight;        /* Out - Line-source belief value                    */
float           D_Weight;        /* Out - Line-source disbelief value                 */

   irow = EditRow;
   icol = item_no % 10;

   switch (item_no)
   {
     case 0: // Exit
     case 1: // Cancel
       fl_hide_form(fd_neteditor->neteditor);
       for (i=0; i<6; i++) {
         fl_hide_object(fd_network->net_toomany[i]);
       }
       fl_trigger_object(fd_network->net_show);             // Force network re-draw
       break;

     case 2: // Add
       irow = fl_get_choice(fd_neteditor->netedit_level)-1;
       icol = fl_get_choice(fd_neteditor->netedit_node)-1;
       if (irow < 6) {
         fl_set_object_color(fd_network->node_label[icol][irow], FL_TOMATO, FL_TOMATO);
         fl_show_object(fd_network->node_label[icol][irow]);
         fl_show_object(fd_network->node_chart[icol][irow]);
         strcpy(dsbtemp, fl_get_input(fd_neteditor->netedit_name));
	 fl_addto_choice(fd_neteditor->netedit_node, dsbtemp);
	 fl_set_choice(fd_neteditor->netedit_node, icol+1);
         strsub(dsbtemp, ' ', '\n');
         fl_set_object_label(fd_network->node_label[icol][irow], dsbtemp);
	 fl_add_chart_value(fd_network->node_chart[icol][irow], (double)0.0, "U", FL_YELLOW);
	 fl_add_chart_value(fd_network->node_chart[icol][irow], (double)0.0, "D", FL_RED);
	 fl_add_chart_value(fd_network->node_chart[icol][irow], (double)0.0, "B", FL_BLUE);
	 results[1] = (float)fl_get_counter_value(fd_neteditor->netedit_b);
	 results[2] = (float)fl_get_counter_value(fd_neteditor->netedit_d);
         strsub(dsbtemp, '\n', ' ');

         DS_AlgoAddNode
           (icol,     // index
            irow,     // level
            dsbtemp,  // name
            "?",      // description
            0.75,     // Bthreshold
            100.0);   // Tthreshold

	 UpdateBUD(icol, irow, results[1], results[2], NoBP);
	 remove("NewNode.lock");
       } else
	 fl_show_messages("No empty node slots available at this level");
       break;

     case 3: // Delete
       break;

     case 4: // Rename
       icol = fl_get_choice(fd_neteditor->netedit_level)-1;
       irow = fl_get_choice(fd_neteditor->netedit_node)-1;
       strcpy(dsbtemp, fl_get_input(fd_neteditor->netedit_name));
       DS_AlgoSetNodeName(icol, irow, dsbtemp);
       strsub(dsbtemp, ' ', '\n');
       fl_set_object_label(fd_network->node_label[icol][irow], dsbtemp);
       break;

     case 5: // Change level
       //
       //   Set up for choice of node
       //
       lowest = fl_get_choice(fd_neteditor->netedit_level);
       irow = lowest-1;
       icol = fl_get_choice(fd_neteditor->netedit_node)-1;
       fl_clear_choice(fd_neteditor->netedit_node);
       for (i=0; i<DS_AlgoGetLevelWidth(lowest-1); i++) {
	 strcpy(chtemp, fl_get_object_label(fd_network->node_label[i][lowest-1]));
	 strsub(chtemp, '\n', ' ');
	 fl_addto_choice(fd_neteditor->netedit_node, chtemp);
       }
       fl_set_choice(fd_neteditor->netedit_node, DS_AlgoGetLevelWidth(lowest-1));
       //
       fl_clear_choice(fd_neteditor->netedit_inodes);
       fl_clear_choice(fd_neteditor->netedit_onodes);
       if (irow > 0) {                                 // Input only available for rows 1...
         ncols = DS_AlgoGetLevelWidth(irow-1);         // Get node count of row above
         for (j=0; j<ncols; j++) {                     // List inputs from all nodes this row
           strcpy(dsbtemp, fl_get_object_label(fd_network->node_label[j][irow-1]));
	   strsub(dsbtemp, '\n', ' ');
	   fl_addto_choice(fd_neteditor->netedit_inodes, dsbtemp);
         }
	 fl_set_choice(fd_neteditor->netedit_inodes, DS_AlgoGetLevelWidth(irow-1));
       }
       if (irow < 5) {                                 // Output only available for rows ...4
         ncols = DS_AlgoGetLevelWidth(irow+1);         // Get node count of row below
         for (j=0; j<ncols; j++) {                     // List outputs to all nodes this row
           strcpy(dsbtemp, fl_get_object_label(fd_network->node_label[j][irow+1]));
	   strsub(dsbtemp, '\n', ' ');
	   fl_addto_choice(fd_neteditor->netedit_onodes, dsbtemp);
         }
	 fl_set_choice(fd_neteditor->netedit_onodes, DS_AlgoGetLevelWidth(irow+1));
       }
       break;

     case 6: // Apply link
       irow = fl_get_choice(fd_neteditor->netedit_level);
       icol = fl_get_choice(fd_neteditor->netedit_node);
       B_Weight = (float)fl_get_counter_value(fd_neteditor->netedit_bweight);
       D_Weight = (float)fl_get_counter_value(fd_neteditor->netedit_dweight);
       if (Winput) {
	 ix = fl_get_choice(fd_neteditor->netedit_inodes);
	 printf("Set input link from %d %d to %d %d\n", ix-1, irow-2, icol-1, irow-1);
         DS_AlgoSetNodeImpact(ix-1, irow-2, icol-1, irow-1, B_Weight, D_Weight);
       } else {
	 ix = fl_get_choice(fd_neteditor->netedit_onodes);
	 printf("Set output link from %d %d to %d %d\n", icol-1, irow-1, ix-1, irow);
         DS_AlgoSetNodeImpact(icol-1, irow-1, ix-1, irow, B_Weight, D_Weight);
       }
       break;

     case 10:
       Winput = TRUE;
       break;

     case 11:
       Winput = FALSE;
       break;

     default:
       break;
   }
}


void netedboxCB(FL_OBJECT *object, long item_no)
{
int             i, j;
float           B, D;

   switch (item_no)
   {
     case 1: // Get new belief
       i = fl_get_choice(fd_netednode->netedbelief) - 1;
       B = LexTable[i].lower + (LexTable[i].upper - LexTable[i].lower)/2.0;
       fl_set_counter_value(fd_netednode->neted_b_value, (double)B);
       break;

     case 2: // Get new disbelief
       i = fl_get_choice(fd_netednode->neteddisbelief) - 1;
       D = LexTable[i].lower + (LexTable[i].upper - LexTable[i].lower)/2.0;
       fl_set_counter_value(fd_netednode->neted_d_value, (double)D);
       break;

     case 3: // Get new belief
       //EditBelief = (float)fl_get_counter_value(fd_netednode->neted_b_value);
       break;

     case 4: // Get new disbelief
       //EditDisbelief = (float)fl_get_counter_value(fd_netednode->neted_d_value);
       break;

     case 5: // Get new belief
       i = fl_get_choice(fd_netedfuse->netedfuseB) - 1;
       B = LexTable[i].lower + (LexTable[i].upper - LexTable[i].lower)/2.0;
       fl_set_counter_value(fd_netedfuse->netedB_b_value, (double)B);
       break;

     case 6: // Get new disbelief
       i = fl_get_choice(fd_netedfuse->netedfuseD) - 1;
       D = LexTable[i].lower + (LexTable[i].upper - LexTable[i].lower)/2.0;
       fl_set_counter_value(fd_netedfuse->netedB_d_value, (double)D);
       break;

     case 11:
     case 12:
     case 13:
     case 14:
     case 15:
       break;

     case 21:
     case 22:
     case 23:
     case 24:
     case 25:
       break;

     case 53: // Set Node link constraints
     case 52:
       //fl_show_messages("CAUTION: This choice may give unexpected, undesired results");
     case 51:
     case 50:
       LinkConstrain = item_no % 10;
       printf(" link contrainst is %d\n", LinkConstrain);
       break;

     case 60: // Set Node Learning Factor
     case 61:
     case 62:
     case 63:
       break;

     case 70: // Set Node Range-of-Effects
     case 71:
     case 72:
       break;
   }
}

void budtableCB(FL_OBJECT *object, long item_no)
{
int             i, j;
float           B, D;

   fl_hide_form(fd_budtable->budtable);
   TableView = FALSE;
}

void net_menuCB(FL_OBJECT *object, long item_no)
{
FILE            *fp;
int             k, n, item = 0;
char            chfile[128];

   switch (item_no) {
     case 0: // Get 'File' selection
       k = fl_get_menu(fd_network->menu_file)-1;
       switch (k) {
         case 0:  break;                                    //- Load
         case 1:  neteddoneCB(NULL, 14); break;             //+ Save
         case 2:  executeCB(NULL, 0); break;                //+ Run
         case 3:  executeCB(NULL, 1); break;                //- Step
         case 4:  input_displayCB(NULL, 1); break;          //- Edit network
         case 5:  fl_show_form(fd_dsevedit->dsevedit,
                               FL_PLACE_CENTER, FL_FULLBORDER,
                               "Evidence Editor"); break;   //+ Edit Evidence
         case 6:  neteddoneCB(NULL, 13); break;             //+ Snap
         case 7:  DSBexitCB(NULL, 0); break;                //+ Exit
         default: break;
       }
       break;

     case 1: // Get 'Tab Info' selection
       k = fl_get_menu(fd_network->nettab_menu)-1;
       switch (k) {
         case 0:  neteddoneCB(NULL,  9); break;             //+ Deltas Table
         case 1:  neteddoneCB(NULL, 10); break;             //+ Statistics
         case 2:  neteddoneCB(NULL, 12); break;             //+ Story
         default: break;
       }
       break;

     case 2: // Get 'Operations' selection
       n = fl_get_menu(fd_network->menu_mission) - 1;
       sprintf(chfile, "./DSBFiles/%s.gif", dsbtypes[n].filebase);
       strsub(chfile, ' ', '_');
       sprintf(dsbtemp, "xwd -name %s | convert - gif:%s",
               "Assess-Network", chfile);
       system(dsbtemp);
       k = fl_get_menu(fd_network->netops_menu)-1;
       switch (k) {
         case 0:  neteddoneCB(NULL, 15); break;             //+ Intersection
         case 1:  neteddoneCB(NULL, 16); break;             //+ Union
	 case 2:  assess_ctlCB(NULL, 1); break;             //+ % Effectiveness
         default: break;
       }
       break;

     case 3: // Get 'Analyze' selection
       k = fl_get_menu(fd_network->netanal_menu)-1;
       switch (k) {
         case 0:  neteddoneCB(NULL,  7); break;             //+ Delta Beliefs
         case 1:  pathsCB(NULL,  0); break;                 //+ Integration (Path)
         case 2:  neteddoneCB(NULL,  6); break;             //+ Differentiation (3D Histo)
         case 3:  StopLight(); break;                       //+ Plan Status
         default: break;
       }
       break;

     case 9: // Get 'Help' selection
       k = fl_get_menu(fd_network->nethelp_menu)-1;
       fl_clear_browser(fd_dsbhelp->dsb_help);
       //
       if(!fl_form_is_visible(fd_dsbhelp->dsbhelp) ) {
	 fl_show_form(fd_dsbhelp->dsbhelp, FL_PLACE_CENTER, FL_FULLBORDER,
                    "Belief Network Help");
       }
       switch (k) {
         case 0:
	   if ((fp = fopen("DSBFiles/DSB.help", "r")) != 0) {
	     fclose(fp);
	     fl_load_browser(fd_dsbhelp->dsb_help, "DSBFiles/DSB.help");
	   }
	   break;
         default:
	   break;
       }
       break;
   }

   netdrawCB(NULL, 0);

   return;
}
/*                      Input Screen edit routines                       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void input_modCB(FL_OBJECT *object, long item_no)
{
int             item = 0;
int             browser_line;
float           disbelief, confidence, Tin;

   browser_line = fl_get_browser(fd_dsevedit->input_browser) - 2;
     //
     //   No choice of levels. Set to selection.
     //
     DS_AlgoGetLevelName(ev.evidences[browser_line]->level - 1, dsbtemp);
     fl_clear_choice(fd_assessadd->add_lvl_choice);
     fl_addto_choice(fd_assessadd->add_lvl_choice, dsbtemp);
     //
     //   No choice of nodes. Set to selection.
     //
     DS_AlgoGetNodeName(ev.evidences[browser_line]->source - 1,
			ev.evidences[browser_line]->level - 1, dsbtemp);
     fl_clear_choice(fd_assessadd->add_src_choice);
     fl_addto_choice(fd_assessadd->add_src_choice, dsbtemp);
     //
     //fl_set_input(fd_assessadd->add_source,
     //               ev.evidences[browser_line]->chsource);
     fl_set_input(fd_assessadd->add_desc,
                  ev.evidences[browser_line]->chdescript);
     //
     confidence = ev.evidences[browser_line]->confidence;
     sprintf(dsline, "%f", confidence);
     fl_set_input(fd_assessadd->add_confid, dsline);
     disbelief = ev.evidences[browser_line]->disbelief;
     sprintf(dsline, "%f", disbelief);
     fl_set_input(fd_assessadd->add_plause, dsline);
     Tin = ev.evidences[browser_line]->Tin;
     sprintf(dsline, "%f", Tin);
     fl_set_input(fd_assessadd->add_time, dsline);
     Tin = ev.evidences[browser_line]->duration;
     sprintf(dsline, "%f", Tin);
     fl_set_input(fd_assessadd->add_duration, dsline);
     fl_set_input(fd_assessadd->add_lat, ev.evidences[browser_line]->latitude);
     fl_set_input(fd_assessadd->add_long,ev.evidences[browser_line]->longitude);
     fl_set_input(fd_assessadd->add_alt, ev.evidences[browser_line]->altitude);

     browser_mod = 1;
     fl_show_form(fd_assessadd->assessadd, FL_PLACE_CENTER, FL_FULLBORDER,
                    "Modify Evidence");

     return;
}

void input_addCB(FL_OBJECT *object, long item_no)
{
int             i, irow, icol, item = 0;

   irow = 0;
   icol = 0;
   //
   //   Provide choice of level
   //
   fl_clear_choice(fd_assessadd->add_lvl_choice);
   for (i=0; i<DS_AlgoGetTreeDepth(); i++) {
     DS_AlgoGetLevelName(i, dsbtemp);
     fl_addto_choice(fd_assessadd->add_lvl_choice, dsbtemp);
   }
   fl_set_choice(fd_assessadd->add_lvl_choice, irow+1);
   //
   //   Provide choice of node of 1st level
   //
   fl_clear_choice(fd_assessadd->add_src_choice);
   for (i=0; i<DS_AlgoGetLevelWidth(irow); i++) {
     strcpy(dsbtemp, fl_get_object_label(fd_network->node_label[i][irow]));
     strsub(dsbtemp, '\n', ' ');
     fl_addto_choice(fd_assessadd->add_src_choice, dsbtemp);
   }
   fl_set_choice(fd_assessadd->add_src_choice, icol+1);

   fl_set_input(fd_assessadd->add_desc, " ");
   sprintf(dsline, "%f", 0.0);
   fl_set_input(fd_assessadd->add_confid, dsline);
   sprintf(dsline, "%f", 0.0);
   fl_set_input(fd_assessadd->add_plause, dsline);
   sprintf(dsline, "%f", 0.0);
   fl_set_input(fd_assessadd->add_time, dsline);
   sprintf(dsline, "%f", 99999.0);
   fl_set_input(fd_assessadd->add_duration, dsline);
   fl_set_input(fd_assessadd->add_lat, " ");
   fl_set_input(fd_assessadd->add_long, " ");
   fl_set_input(fd_assessadd->add_alt, " ");

   browser_mod = 0;
   fl_show_form(fd_assessadd->assessadd,FL_PLACE_CENTER,FL_FULLBORDER,
                "Add Evidence");
}

void input_delCB(FL_OBJECT *object, long item_no)
{
int             item = 0;

   item = fl_get_browser(fd_dsevedit->input_browser);
   //fl_delete_browser_line(fd_dsevedit->input_browser, item);
   ev.evidences[item-2]->valid = FALSE;
}

void add_acceptCB(FL_OBJECT *object, long item_no)
{
int             item = 0, i, irow, icol;
float           nonbelief, Tin;
char            chtemp[128];

   if (item_no == 1) {                            // Do 'Apply'

     if (atof(fl_get_input(fd_assessadd->add_confid)) +
         atof(fl_get_input(fd_assessadd->add_plause)) > 1.0) {
       fl_show_messages("Belief + Disbelief must be less than or equal to 1.0");
       return;
     }

     icol = fl_get_choice(fd_assessadd->add_src_choice)-1;
     irow = fl_get_choice(fd_assessadd->add_lvl_choice)-1;

     if (browser_mod) {                           // If modifying
       i = fl_get_browser(fd_dsevedit->input_browser) - 2;
     } else {                                     // If adding
       i = ev.DS_addEvidence();
     }

     i = ev.DS_setEvidence
           (i,
            (char*)fl_get_choice_text(fd_assessadd->add_src_choice),  // name
            (char*)fl_get_input(fd_assessadd->add_desc),       // description
            (char*)fl_get_input(fd_assessadd->add_lat),        // lat
            (char*)fl_get_input(fd_assessadd->add_long),       // lon
            (char*)fl_get_input(fd_assessadd->add_alt),        // alt
            NULL,                                              // subfname
            0,                                                 // caseId
            TRUE,                                              // valid
            icol+1,                                            // source
            irow+1,                                            // level
            0,                                                 // etype
            atof(fl_get_input(fd_assessadd->add_time)),        // timeStamp
            atof(fl_get_input(fd_assessadd->add_duration)),    // duration
            atof(fl_get_input(fd_assessadd->add_confid)),      // confidence
            atof(fl_get_input(fd_assessadd->add_plause)));     // disbelief

     sprintf(dsbtemp, "%11s (%4.1f)  %11s (%4.1f)",
             fl_get_choice_text(fd_assessadd->belief_by_name),
             ev.evidences[i]->confidence*100.0,
             fl_get_choice_text(fd_assessadd->disbelief_by_name),
             ev.evidences[i]->disbelief*100.0);
     sprintf(chtemp, "%-16s %-34s %8.2f %8.2f %10s %10s %10s",
             ev.evidences[i]->chsource, dsbtemp, ev.evidences[i]->Tin,
             ev.evidences[i]->duration,
	     ev.evidences[i]->latitude, ev.evidences[i]->longitude,
             ev.evidences[i]->altitude);

     if (!browser_mod) {
       fl_addto_browser(fd_dsevedit->input_browser, chtemp);
       fl_show_object(fd_network->directin[icol][irow]);
     } else {

// ttt -- need to reload browser if timestamp changed! ---------------------ttt

       fl_replace_browser_line(fd_dsevedit->input_browser,
                               fl_get_browser(fd_dsevedit->input_browser),
                               chtemp);
     }
     EvidModified = TRUE;

     resetCB(NULL, 0);
     executeCB(NULL, 0);
   }

   fl_hide_form(fd_assessadd->assessadd);
}

void addchoiceCB(FL_OBJECT *object, long item_no)
{
int             i, line, item = 0;

   switch (item_no) {
   case 0:
     line = fl_get_browser(fd_dsevedit->input_browser)-2;
     strcpy(ev.evidences[line-1]->chsource,
            fl_get_choice_text(fd_assessadd->add_src_choice));
     //fl_set_input(fd_assessadd->add_source, ev.evidences[line-1]->chsource);
     break;

   case 1:
     //
     //   Provide choice of node of 1st level
     //
     line = fl_get_choice(fd_assessadd->add_lvl_choice) - 1;
     fl_clear_choice(fd_assessadd->add_src_choice);
     for (i=0; i<DS_AlgoGetLevelWidth(line); i++) {
       strcpy(dsbtemp, fl_get_object_label(fd_network->node_label[i][line]));
       strsub(dsbtemp, '\n', ' ');
       fl_addto_choice(fd_assessadd->add_src_choice, dsbtemp);
     }
     fl_set_choice(fd_assessadd->add_src_choice, 1);
     break;
   }
}
/*                       Old Network edit routines                       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void dseditorCB(FL_OBJECT *object, long item_no)
{
int             i, j, k, item = 0;

   fl_set_object_color(fd_dsnetedit->dsed_lower[EdCol], FL_COL1, FL_COL1);

   switch (item_no)
   {
     case 10:
       if (EdRow > 1) {
	 EdRow--;
	 EdCol = 0;
	 RedrawEdit(EdRow);
       }
       break;

     case 11:
       if (EdRow < DS_AlgoGetTreeDepth()-1) {
	 EdRow++;
	 EdCol = 0;
	 RedrawEdit(EdRow);
       }
       break;

     case 12:
       if (EdCol > 0) {
	 EdCol--;
       }
       break;

     case 13:
       if (EdCol < DS_AlgoGetLevelWidth(EdRow)-1) {
	 EdCol++;
       }
       break;

     case 19:
       LinkEdit(fd_dsnetedit->dsed_lower[EdCol], EdCol, EdRow);
       break;

     default:
       break;
   }

   fl_set_object_color(fd_dsnetedit->dsed_lower[EdCol], FL_TOMATO, FL_TOMATO);

   return;
}
/*                      Assessment Screen routines                       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void assess_tunitCB(FL_OBJECT *object, long item_no)
{
int             item = 0;

   time_unit = item_no;
}

void assess_issueCB(FL_OBJECT *object, long item_no)
{
int             item = 0;
char            chmission[40];
char            chhypoth[40];
char            chtemp[128];
float           results[6];

   if (item_no != 0)
     item = item_no;
   else
     item = curr_assess;

   strcpy(chmission, fl_get_menu_text(fd_network->menu_mission));
   strsub(chmission, ' ', '_');
   //
   DS_AlgoGetNodeName(item, EditRow, chhypoth);
   strsub(chhypoth, ' ', '_');
   //
   DS_AlgoGetResult(item, EditRow, results);

   sprintf(chtemp, "%s %d%% %d%% %s",
           chhypoth,
           (int)(results[1] * 100),
           (int)(results[2] * 100),
           "2003:03:06:17:12:36");
   sprintf(dsbtemp, "%s %s %s %d %d %d %s", inmsg.dst, inmsg.src, chmission,
           M_ISSUE, strlen(chtemp), 0, chtemp);
   printf("Issue: %s\n", dsbtemp);

   exitCB(NULL, 14);

   return;
}

void assess_ctlCB(FL_OBJECT *object, long item_no)
{
FILE*           fp;
int             i, j, n, item = 0;
int             w, h;
int             nlevels, nodes;
float           belief, disbelief, time;
float           Rbelief;
float           results[6];
const char      *tempfn;
char            *snapdir;
char            *fname;
char            tplate[24] = "TempEvid.XXXXXX";
char            chcase[32];
char            chvalue[64];
char            infname[64];
char            otfname[64];
char            chfile[128];

FL_COLOR        boxcolor;
FL_IMAGE        *Limage, *Rimage;

   switch (item_no)
   {
     case 0: // Snapshot beliefs
       n = fl_get_menu(fd_network->menu_mission) - 1;
       sprintf(otfname, "DSBFiles/%s.save", dsbtypes[n].filebase);
       fl_set_object_label(fd_network->net_compare, dsbtypes[n].filebase);
       savedNet_numLevels = DS_AlgoGetTreeDepth();
       strcpy(savedNet_mission, fl_get_object_label(fd_network->assess_type));
       strsub(savedNet_mission, ' ', '_');

       savedNet_numLevels = DS_AlgoGetTreeDepth();

       for (j = 0; j < savedNet_numLevels; j++) {
	 savedNet_levels[j].numNodes = DS_AlgoGetLevelWidth(j);
	 strcpy(savedNet_levels[j].name,
                fl_get_object_label(fd_network->level_label[j]));
	 strsub(savedNet_levels[j].name, '\n', '_');

         for (i=0; i<savedNet_levels[j].numNodes; i++) {
           DS_AlgoGetResult(i, j, results);
           savedNet_nodes[j][i].belief = results[1];
           savedNet_nodes[j][i].disbelief = results[2];
           savedNet_nodes[j][i].time = results[4];
	   strcpy(savedNet_nodes[j][i].name,
                  fl_get_object_label(fd_network->node_label[i][j]));
	   strsub(savedNet_nodes[j][i].name, '\n', '_');
         }
       }
       savedNet_saveFile(otfname);
       break;

     case 1: // Determine Effectiveness
       sprintf(otfname, "DSBFiles/%s.save", fl_get_object_label(fd_network->net_compare));

       if (savedNet_readFile(otfname) == 0) {
	 if (savedNet_numLevels == 6) {

	   for (j=0; j<savedNet_numLevels; j++) {
	     n = MIN(savedNet_levels[j].numNodes, DS_AlgoGetLevelWidth(j));

	     for (i=0; i<n; i++) {
	       DS_AlgoGetResult(i, j, results);

	       if (results[1] == savedNet_nodes[j][i].belief)
		 Rbelief = 1.0;
	       else if (savedNet_nodes[j][i].belief != 0.0)
		 Rbelief = results[1]/savedNet_nodes[j][i].belief;
	       else
		 Rbelief = -99999.0;

	       sprintf(chvalue, "%s\nRatio = %f",
                       savedNet_nodes[j][i].name, Rbelief);
	       boxcolor = FL_PALEGREEN;
	       if (Rbelief < 10.0) boxcolor = FL_WHEAT;
	       if (Rbelief < 1.0) boxcolor = FL_SALMON;
	       if (Rbelief < 0.0) boxcolor = FL_WHITE;
	       fl_set_object_color(fd_network->node_label[i][j], boxcolor, boxcolor);
	       fl_set_object_helper(fd_network->node_label[i][j], chvalue);
	     }
	   }
	 } else {
	   fl_show_messages("Sorry, this option only works with 6 level networks!");
	 }
       } else {
         char  msg[256];
         sprintf(msg, "Save file (%s)does not exist!", otfname);
	 fl_show_messages(msg);
       }
       break;

     case 2: // Fog-of-War
       FOWinit();
       FOWshow(DSBx, DSBy, DSBw, DSBh, DSBwid);
       break;

     case 3: // Auto evidence update
       break;

     case 4: // Log operator overrides
       OverrideLog = !OverrideLog;
       break;

     case 5: // Auto Deltas
       AutoDeltas = !AutoDeltas;
       break;

     case 6: // Differential BP during 3D display of impact differentiation
       DiffBP = !DiffBP;
       break;

     case 9: // Evidence import
       break;

     case 10: // Determine Plan status
       int k, nlevels, nnodes, nplans, mode, flags;
       float Ts, Te, Weight, Tbelief, Tdisbelief;
       char Tu;
       /*
       if (fl_get_button(fd_network->net_plan) == TRUE) {
	 fl_set_button(fd_network->net_plan, FALSE);
	 //return;
       } else {
	 fl_set_button(fd_network->net_plan, TRUE);
       }
       */
       k = fl_get_menu(fd_network->menu_mission) - 1;
       strcpy(chfile, "DSBFiles/");
       strcat(chfile, dsbtypes[k].filebase);
       strcat(chfile, ".plan");
       //
       if ((fp = fopen(chfile, "r")) != NULL) {
	 //do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');
	 fscanf(fp, "%d %s %d %f %f %c\n", &nlevels, chcase, &mode, &Ts, &Te, &Tu);
         for (j=0; j<nlevels; j++) {
	   fscanf(fp, "%d %s", &nnodes, chcase);
           for (i=0; i<nnodes; i++) {
	     fscanf(fp, "%s %d %f %f %f %d\n",
		    chcase, &nplans, &Weight, &Tbelief, &Tdisbelief, &flags);
             DS_AlgoGetResult(i, j, results);
	     Rbelief = Weight*(Tbelief-results[1]) + (1.0-Weight)*(Tdisbelief-results[2]);
	     /*
	     fprintf(stderr, "  %s:  %f %f %f %f %f %f\n",
		     chcase, results[1], results[2], Weight, Tbelief, Tdisbelief, Rbelief);
	     */
	                         boxcolor = FL_TOMATO;
	     if (Rbelief > 0.0)  boxcolor = FL_DARKGOLD;
	     if (Rbelief > 0.4)  boxcolor = FL_PALEGREEN;
	     if (Rbelief > 0.7)  boxcolor = FL_CYAN;
	     fl_set_object_color(fd_network->node_label[i][j], boxcolor, boxcolor);
	     //fl_set_object_helper(fd_network->node_label[i][level], chvalue);
	     for (k=0; k<nplans; k++) {
	       fscanf(fp, "%s %s %s", chfile, chvalue, dsbtemp);
	       //fprintf(stderr, "Skipping:  %s %s %s\n", chfile, chvalue, dsbtemp);
	     }
           }
	 }
         fclose(fp);
       } else {
	 fl_show_messages("Sorry, no .plan file available!");
       }
       break;

     case 11: // Show Snapshot'ed network
       fl_set_object_label(fd_netsnap->net_rtext,
			   fl_get_object_label(fd_network->net_compare));
       fl_set_object_label(fd_netsnap->net_ltext,
			   fl_get_object_label(fd_network->assess_type));
       fl_set_form_position(fd_netsnap->netsnap, DSBwinX, 0);
       fl_show_form(fd_netsnap->netsnap, FL_PLACE_POSITION, FL_FULLBORDER, "External Net");
       fl_set_form_atclose(fd_netsnap->netsnap, IMGclose, 0);
#ifndef XFVER88
       mysetup.visual_cue    = NULL;
       mysetup.error_message = NULL;
       mysetup.app_data      = fd_netsnap;
       flimage_setup(&mysetup);
       //
       n = fl_get_menu(fd_network->menu_mission) - 1;
       sprintf(dsbtemp, "./DSBFiles/%s.gif", dsbtypes[n].filebase);
       strsub(dsbtemp, ' ', '_');
       if (Rimage = flimage_load(dsbtemp)) {
	 w = (int)(0.50 * Rimage->w);
	 h = (int)(0.50 * Rimage->h);
	 flimage_scale(Rimage, w, h, FLIMAGE_SUBPIXEL);
	 flimage_sdisplay(Rimage, FL_ObjWin(fd_netsnap->net_rimage));
       }
       //
       n = fl_get_menu(fd_network->menu_mission) - 1;
       sprintf(dsbtemp, "./DSBFiles/%s.gif", dsbtypes[n].filebase);
       strsub(dsbtemp, ' ', '_');
       if (Limage = flimage_load(dsbtemp)) {
	 w = (int)(0.50 * Limage->w);
	 h = (int)(0.50 * Limage->h);
	 flimage_scale(Limage, w, h, FLIMAGE_SUBPIXEL);
	 flimage_sdisplay(Limage, FL_ObjWin(fd_netsnap->net_limage));
       }
#endif

       fl_update_display(0);
       break;

     default:
       break;
   }
}
/*                          Plan Update routines                         */
/* --------------------------------------------------------------------- */
/*                                                                       */
void DSPshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
FILE            *fp;
FILE            *outfp;
int             i, j, k, m, n, item = 0;
int             nlevels, nnodes, nplans, mode, flags;
int             nCols;
float           results[6];
float           Ts, Te, Weight, Tbelief, Tdisbelief;
float           Rbelief;
char            Tu;
const char      *fname;
char            chname[64];
char            chclass[64];
char            chcase[32];
char            chvalue[64];
char            chfile[128];
char            chtemp[256]; 
Window          winid;
float           B = 0.6;
float           D = 0.2;
float           T = 0.1;

   if(!fl_form_is_visible(fd_dsbplan->dsbplan) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_dsbplan->dsbplan,
                                     FL_PLACE_POSITION,FL_TRANSIENT, "Plan-Update");
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_show_form_window(fd_dsbplan->dsbplan);
      fl_set_form_atclose(fd_dsbplan->dsbplan, DSBclose, 0);
      StoreActiveEntry("Plan-Update");
   }
   //
   fl_set_input(fd_dsbplan->pup_estimated, "");
   fl_set_input(fd_dsbplan->pup_actual, "");
   fl_set_input(fd_dsbplan->pup_worth, "");
   fl_set_input(fd_dsbplan->pup_shell, "");
   //
   fl_clear_choice(fd_dsbplan->pup_update);
   fl_clear_browser(fd_dsbplan->pup_strategy);
   sprintf(chtemp, "@N@_%-32s  %-5s  %-5s  %-5s  %-8s  %-48s",
	   "        Plan Name",
	   " Est.",
	   " Act.",
	   "Worth",
	   " Action",
	   "  Fix");
   fl_add_browser_line(fd_dsbplan->pup_strategy, chtemp);
   //
   fl_clear_choice(fd_dsbplan->pup_hypothesis);
   //
   k = fl_get_menu(fd_network->menu_mission) - 1;
   sprintf(dsbtemp, "DSBFiles/%s.plan", dsbtypes[k].filebase);
   if ((fp = fopen(dsbtemp, "r")) != NULL) {
     DS_AlgoGetNodeName(EditCol, EditRow, chname);          // Get selected node name
     strsub(chname, ' ', '_');
     while (!feof(fp)) {
       fscanf(fp, "%s %d %f %f %f %d\n",                    // Search file for match
	      chcase, &nplans, &Weight, &Tbelief, &Tdisbelief, &flags);
       if (strcmp(chcase, chname) == 0) {                   // If match found
	 fl_addto_choice(fd_dsbplan->pup_hypothesis, chcase);
         DS_AlgoGetResult(EditCol, EditRow, results);       //    Get current BUD
         B = results[1];
         D = results[2];
         T = results[4];
	 //
	 sprintf(dsbtemp, "%f", B);
	 fl_set_input(fd_dsbplan->pup_belief, dsbtemp);
	 sprintf(dsbtemp, "%f", D);
	 fl_set_input(fd_dsbplan->pup_disbelief, dsbtemp);
	 sprintf(dsbtemp, "%f", T);
	 fl_set_input(fd_dsbplan->pup_time, dsbtemp);
	 Rbelief = Weight*(Tbelief-B) + (1.0-Weight)*(Tdisbelief-D);
	 //
	 for (k=0; k<nplans; k++) {
	   fscanf(fp, "%s %s %s", chfile, chvalue, chtemp);
	   strsub(chfile, '_', ' ');
	   strsub(chtemp, '_', ' ');
	   fl_addto_choice(fd_dsbplan->pup_update, chfile);
	   //
	   DSBEstScore[k] = 0.45;
	   DSBActScore[k] = 0.61;
	   DSBWorth[k] = DSBEstScore[k]/DSBActScore[k];
	   //
	   sprintf(dsbtemp, "%-32s  %5.2f  %5.2f  %5.2f  %-8s  | %-48s",
		   chfile,
		   DSBEstScore[k], DSBActScore[k], DSBWorth[k],
		   chvalue,
		   chtemp);
	   fl_add_browser_line(fd_dsbplan->pup_strategy, dsbtemp);
	 }
	 goto done;
       } else {
	 for (k=0; k<nplans; k++) {
	   fscanf(fp, "%s %s %s", chfile, chvalue, dsbtemp);
	 }
       }
     }
done:
     sprintf(dsbtemp, "%f", Tbelief);
     fl_set_input(fd_dsbplan->pup_bthresh, dsbtemp);
     sprintf(dsbtemp, "%f", Tdisbelief);
     fl_set_input(fd_dsbplan->pup_dthresh, dsbtemp);
     sprintf(dsbtemp, "%f", Rbelief);
     fl_set_input(fd_dsbplan->pup_weight, dsbtemp);
     //
     fclose(fp);
   } else {
     fl_show_messages("Sorry, no .plan file available!");
   }

   return;
}

void DSPexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_dsbplan->dsbplan);
   EraseActiveEntry("Plan-Update");

   return;
}

void SetFields(int i)
{
char            *p;

   sprintf(dsbtemp, "%f", DSBEstScore[i-1]);
   fl_set_input(fd_dsbplan->pup_estimated, dsbtemp);
   sprintf(dsbtemp, "%f", DSBActScore[i-1]);
   fl_set_input(fd_dsbplan->pup_actual, dsbtemp);
   sprintf(dsbtemp, "%f", DSBWorth[i-1]);
   fl_set_input(fd_dsbplan->pup_worth, dsbtemp);

   p = strchr(fl_get_browser_line(fd_dsbplan->pup_strategy, i+1), '|');
   p++;
   p++;

   if (p != NULL) {
     strcpy(dsbtemp, p);
     strtrm(dsbtemp);
     fl_set_input(fd_dsbplan->pup_shell, dsbtemp);
   }

   p = p-12;
   sscanf(p, "%s", dsbtemp);
   fl_set_object_label(fd_dsbplan->pup_action, dsbtemp);

   return;
}

void dspexecCB(FL_OBJECT *ob, long data)
{
char            chtemp[64];

   strcpy(chtemp, fl_get_choice_text(fd_dsbplan->pup_update)); 
   strsub(chtemp, ' ', '_');
   sprintf(dsbtemp, "140900:17Jul02+%s  %s  '%s'  %f  %f  %f   #Commander#",
	   fl_get_input(fd_dsbplan->pup_time),
	   chtemp,
	   fl_get_choice_text(fd_dsbplan->pup_hypothesis),
	   0.76, 0.76, 0.76,
	   fl_get_choice_text(fd_dsbplan->pup_update));
   fl_addto_browser(fd_dsbplan->pup_log, dsbtemp);

   strcpy(dsbtemp, fl_get_object_label(fd_dsbplan->pup_action));

   if (strcmp(dsbtemp, "Operator") == 0) {
     //fl_show_messages("A manual action by an operator is required for this fix.");
   } else if (strcmp(dsbtemp, "Message") == 0) {
     fl_show_messages("Message action not yet implemented.");
   } else if (strcmp(dsbtemp, "Script") == 0) {
     system(fl_get_input(fd_dsbplan->pup_shell));
   }

   return;
}

void dspupdateCB(FL_OBJECT *ob, long data)
{
int             i, j, k, n;

   i = fl_get_choice(fd_dsbplan->pup_update);

   fl_select_browser_line(fd_dsbplan->pup_strategy, i+1);

   SetFields(i);

   return;
}

void dspstratCB(FL_OBJECT *ob, long data)
{
int             i, j, k, n;

   i = fl_get_browser(fd_dsbplan->pup_strategy)-1;    // Skip header line

   fl_set_choice(fd_dsbplan->pup_update, i);

   SetFields(i);

   return;
}

void dsphypothCB(FL_OBJECT *ob, long data)
{

   return;
}
/*                         Story Telling routines                        */
/* --------------------------------------------------------------------- */
/*                                                                       */

/***** NOT USED **
void StoryDummyCB(FL_OBJECT *object, long item_no)
{
FILE            *outfp;
int             i, j, k, l, hold, nodes, level, item;
int             hasExtern;
float           belief, disbelief, Tin, duration;
float           results[6], b, d, u, Bold, Dold, Uold, biggest, value;
const char      *fname;
char            chname[32];
char            chfile[128];
DSevidence      evtemp;

       item  = fl_get_menu(fd_network->menu_mission) - 1;

       if (DSBMYSQL) {
         strcpy(chfile, dsbtypes[k].filebase);
       } else {
         if (strcmp(dsbtypes[item].filebase, "*") == 0) {
	   fname = fl_show_fselector("Evidence file", "./DSBFiles",
                                     "*.evid", NULL);
	   if (fname == NULL) return;
	   strcpy(chfile, fname);
         } else {
	   strcpy(chfile, "DSBFiles/");
	   strcat(chfile, dsbtypes[item-1].filebase);
	   strcat(chfile, ".evid");
         }
       }

       duration = 120.0;

       belief = 0.56;
       disbelief = 0.27;

       strcpy(chname, "Expert");
       strsub(chname, ' ', '_');
       if (DS_AlgoGetNodeCoordinates(chname, k, l) == 0)
	 printf("Node %s at %d %d\n", chname, k, l);
       else
	 printf("Node %s unknown!\n", chname);

       Tin = (float)drand48()*100.0;


// This code could replace the commented out code below:

       evtemp.DS_loadEvidence(chfile, DSBMYSQL, FALSE, 0, hasExtern);
       i = evtemp.DS_addEvidence(chname,            // name
                                 "Intel_Processor", // description
                                 "_Demo_",          // param1
                                 "_Demo_",          // param2
                                 "_Demo_",          // param3
                                 NULL,              // subfname
                                 0,                 // caseId
                                 TRUE,              // valid
                                 k,                 // source
                                 l,                 // level
                                 0,                 // etype
                                 Tin,               // timeStamp
	                         duration,          // duration
	                         belief,            // confidence
	                         disbelief);        // disbelief
       evtemp.DS_saveEvidence(filename, DSBMYSQL);

// {*****
//       //
//       if ((outfp = fopen(chfile, "a")) == NULL) return;
//       //
//       //
//       fprintf(outfp, "%4d %-24s %6.3f %6.3f %10.1f %c  %5.1f %c  %s  %s  %s  %-20s %4d %4d %-64s\n",
//	       0,                                           // Evidence type
//	       chname,                                      // Node name
//	       belief,                                      // Belief
//	       disbelief,                                   // Disbelief
//	       Tin,                                         // Evidence time
//	       'M',                                         // Time units
//	       duration,                                    // Duration
//	       'M',                                         // Duration units
//	       "_Demo_",                                    // Comment 1
//	       "_Demo_",                                    // Comment 2
//	       "_Demo_",                                    // Comment 3
//	       "Intel_Processor",                           // User supplied description
//	       0,                                           // External node #
//	       0,                                           // External level #
//	       "None");                                     // External network file name
//       fclose(outfp);
//       //
//       //   Make sure evidence is in ascending time order
//       //
//       sprintf(dsbtemp, "mv %s unsorted.temp", chfile);
//       system(dsbtemp);
//       sprintf(dsbtemp, "sort -k 5 -g -o %s unsorted.temp", chfile);
//       system(dsbtemp);
//       sprintf(dsbtemp, "rm unsorted.temp");
//       system(dsbtemp);
// *****}

       assess_storyCB(NULL, 0);

       return;
}
*****/

void StorySort()
{
int             i, j, k, l, hold, nodes, level, curr_type;
float           results[6], b, d, u, Bold, Dold, Uold, biggest, value;
char            chstory[32];
char            chtemp[128];

   if (atoi(fl_get_input(fd_dsstory->story_count)) < 2) return;

   for (j=1; j<=fl_get_browser_maxline(fd_dsstory->story_details); j++) {
     strcpy(dsbtemp, fl_get_browser_line(fd_dsstory->story_details, j));
     if (fl_get_choice(fd_dsstory->story_sortorder) == 1)
       biggest = 0.000001;
     else
       biggest = 100000.0;
     //
     //   Sort on B, D, or U either Ascending or Descending
     //
     for (k=j; k<=fl_get_browser_maxline(fd_dsstory->story_details); k++) {
       strcpy(chtemp, fl_get_browser_line(fd_dsstory->story_details, k));
       sscanf(chtemp, "%s %f (%f) %f (%f) %f (%f)", chstory, &b, &Bold, &d, &Dold, &u, &Uold);
       fprintf(stderr, "%s %f (%f) %f (%f) %f (%f)\n", chstory, b, Bold, d, Dold, u, Uold);
       switch (fl_get_choice(fd_dsstory->story_sortby)) {
       case 1: value = b; break;
       case 2: value = d; break;
       case 3: value = u; break;
       }
       //value = b;
       if (fl_get_choice(fd_dsstory->story_sortorder) == 1) {
         if (value > biggest) {biggest = value; hold = k; }
       } else {
         if (value < biggest) {biggest = value; hold = k; }
       }
     }
     //
     fprintf(stderr, "%d %d %d\n", k, j, hold);
     fflush(stderr);
     fl_replace_browser_line(fd_dsstory->story_details, j,
			     fl_get_browser_line(fd_dsstory->story_details, hold));
     fl_replace_browser_line(fd_dsstory->story_details, hold, dsbtemp);
   }

   return;
}

void StoryExec(float bdu[], char chtemp[])
{
char            chfile[128];
int             i, j, k, l, nodes, level, curr_type;
float           Btot, Dtot, Utot;
float           Bsum, Dsum, Usum;
float           results[6];
 
     k = fl_get_menu(fd_network->menu_mission) - 1;
     //
     //   Execute the network
     //
     if (DSBMYSQL) {
       //fprintf(stderr, "Loading story from table: [%s]\n", dsbtypes[k].filebase);
       DSinit(TRUE, dsbtypes[k].filebase, chtemp);         // Load the network
     } else {
       sprintf(chfile, "DSBFiles/%s.dsbn", chtemp);
       //fprintf(stderr, "Loading story from file: [%s]\n",  chfile);
       DSinit(TRUE, chfile, NULL);                         // Load the network
     }
     resetCB(NULL, 0);                                     // Reset everything
     //
     if (DSBMYSQL)
       DSload(dsbtypes[k].filebase);                       // Load the evidence
     else {
       sprintf(chfile, "DSBFiles/%s.evid", dsbtypes[k].filebase);
       DSload(chfile);                                     // Load the evidence
     }
     executeCB(NULL, 0);                                   // Do evidence fusion
     //
     //   Get the story metrics
     //
     Btot = 0.0; Dtot = 0.0; Utot = 0.0;
     for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
       Bsum = 0.0; Dsum = 0.0; Usum = 0.0;
       nodes = DS_AlgoGetLevelWidth(level);
       for (i=0; i<nodes; i++) {
	 DS_AlgoGetResult(i, level, results);
	 Bsum = Bsum + results[1];
	 Dsum = Dsum + results[2];
	 Usum = Usum + results[3];
       }
       Btot = Btot + (Bsum/(float)nodes);
       Dtot = Dtot + (Dsum/(float)nodes);
       Utot = Utot + (Usum/(float)nodes);
     }
     //
     //   Return the answers
     //
     bdu[1] = Btot;
     bdu[2] = Dtot;
     bdu[3] = Utot;

     return;
}

void assess_storyCB(FL_OBJECT *object, long item_no)
{
int             nstory, istory, iselect;
float           StoryB[10], StoryD[10], StoryU[10];
float           results[6], b, d, u, Bold, Dold, Uold;
char            chstory[32];
char            chtemp[128];

   iselect = fl_get_browser(fd_dsstory->story_details);
   //
   //   If no selection, do all stories
   //
   if (iselect == 0) {
     nstory = atoi(fl_get_input(fd_dsstory->story_count));
     for (istory=0; istory<nstory; istory++) {
       strcpy(dsbtemp, fl_get_browser_line(fd_dsstory->story_details, istory+1));
       sscanf(dsbtemp, "%s %f (%f) %f (%f) %f (%f) %s",
	      chstory, &b, &Bold, &d, &Dold, &u, &Uold, chtemp);

       Bold = b;
       Dold = d;
       Uold = u;
       //
       //   Execute the network
       //
       StoryExec(results, chtemp);
       StoryB[istory] = results[1]/(float)DS_AlgoGetTreeDepth();
       StoryD[istory] = results[2]/(float)DS_AlgoGetTreeDepth();
       StoryU[istory] = results[3]/(float)DS_AlgoGetTreeDepth();
       sprintf(dsbtemp, "%-12s      %8.2f (%5.2f)%8.2f (%5.2f)%8.2f (%5.2f)  %-64s",
	       chstory, StoryB[istory], Bold, StoryD[istory], Dold, StoryU[istory], Uold, chtemp);
       fl_replace_browser_line(fd_dsstory->story_details, istory+1, dsbtemp);
     }
   } else {
     //
     //   If selection, do only selected story
     //
     istory = iselect-1;
     strcpy(dsbtemp, fl_get_browser_line(fd_dsstory->story_details, iselect));
     sscanf(dsbtemp, "%s %f (%f) %f (%f) %f (%f) %s",
	    chstory, &b, &Bold, &d, &Dold, &u, &Uold, chtemp);
     Bold = b;
     Dold = d;
     Uold = u;
     //
     StoryExec(results, chtemp);
     StoryB[istory] = results[1]/(float)DS_AlgoGetTreeDepth();
     StoryD[istory] = results[2]/(float)DS_AlgoGetTreeDepth();
     StoryU[istory] = results[3]/(float)DS_AlgoGetTreeDepth();
     sprintf(dsbtemp, "Story-%03d      %8.2f (%5.2f)  %8.2f (%5.2f)  %8.2f (%5.2f)    %-64s",
	     istory, StoryB[istory], Bold, StoryD[istory], Dold, StoryU[istory], Uold, chtemp);
     fl_replace_browser_line(fd_dsstory->story_details, iselect, dsbtemp);
     fl_deselect_browser(fd_dsstory->story_details);
   }

   StorySort();
   fl_show_object(fd_dsstory->story_sort);
   fl_show_object(fd_dsstory->story_done);

   return;
}

void storyfileCB(FL_OBJECT *object, long item_no)
{
const char      *fname;

   if (DSBMYSQL) {
     showStoryBrowser();
     fname = storyName;
   } else {    
     fname = fl_show_fselector("Network file", "./DSBFiles", "*.dsbn", NULL);
   }

   if (fname)
     fl_set_input(fd_dsstory->story_file, fname);

   return;
}

void storyeditCB(FL_OBJECT *object, long item_no)
{
FILE            *fp;
int             i, j, ret;
int             nCols;
float           b, d, u, Bold, Dold, Uold;
const char      *fname;
char            chstory[32];
char            chtemp[64];
char            szSQL[32];

   switch (item_no)
   {
     case 0: // Delete
       i = fl_get_browser(fd_dsstory->story_details);
       if (i > 0) fl_delete_browser_line(fd_dsstory->story_details, i);
       break;

     case 1: // Add
       if (strlen(fl_get_input(fd_dsstory->story_file)) == 0) {
         strncpy(chstory, fl_get_input(fd_dsstory->story_ident), 9);
         chstory[9] = '\0';
       } else {
         sprintf(chstory, "Story_%03d",
	         fl_get_browser_maxline(fd_dsstory->story_details));
       }

       ret = 0;

       if (DSBMYSQL) {
         i = fl_get_menu(fd_network->menu_mission) - 1;
         ret = DS_AlgoSaveAttributes(dsbtypes[i].filebase, chstory, TRUE);
       }

       // Add the new story to the browser.
       if (ret == 0) {
         sprintf(dsbtemp,
                 "%-12s      %8.2f (%5.2f)%8.2f (%5.2f)%8.2f (%5.2f)  %-64s",
                 chstory, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0,
                 fl_get_input(fd_dsstory->story_file));
         fl_addto_browser(fd_dsstory->story_details, dsbtemp);
         StorySort();
       } else {
         fprintf(stderr, "Cannot add story (%s)\n",
                 fl_get_input(fd_dsstory->story_ident));
       }

       break;

     case 2: // Save
       fname = fl_show_fselector("Story file", "./DSBFiles", "*.stry", NULL);
       if (fname == NULL) return;
       if ((fp = fopen(fname, "w")) == NULL) return;
       fprintf(fp, "    %d\n",
               fl_get_browser_maxline(fd_dsstory->story_details));
       for (j=1; j<=fl_get_browser_maxline(fd_dsstory->story_details); j++) {
         strcpy(dsbtemp, fl_get_browser_line(fd_dsstory->story_details, j));
         sscanf(dsbtemp, "%s %f (%f) %f (%f) %f (%f) %s",
		chstory, &b, &Bold, &d, &Dold, &u, &Uold, chtemp);
	 fprintf(fp, "%s  %f %f %f %s\n", chstory, b, d, u, chtemp);
       }
       fclose(fp);
       break;

     case 3: // Rename
       i = fl_get_browser(fd_dsstory->story_details);
       if ((i > 0) && (strlen(fl_get_input(fd_dsstory->story_ident)) > 0)) {
         strcpy(dsbtemp, fl_get_browser_line(fd_dsstory->story_details, i));
         sscanf(dsbtemp, "%s %f (%f) %f (%f) %f (%f) %s",
	        chstory, &b, &Bold, &d, &Dold, &u, &Uold, chtemp);
         ret = 0;

         if (DSBMYSQL) {
           sprintf(szSQL, "RENAME TABLE `%s` TO `%s`;",
                   chstory, fl_get_input(fd_dsstory->story_ident));
           ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter, bColumnNames,
                             bHTMLTable, nCols );
           SQLFreeStmt( hStmt, SQL_DROP );
	 }

         if (ret == 0) {
           sprintf(dsbtemp,
                   "%-12s      %8.2f (%5.2f)%8.2f (%5.2f)%8.2f (%5.2f)  %-64s",
	           fl_get_input(fd_dsstory->story_ident),
                   b, Bold, d, Dold, u, Uold, chtemp);
           fl_replace_browser_line(fd_dsstory->story_details, i, dsbtemp);
         } else {
           fprintf(stderr, "Cannot rename story (%s) to (%s)\n",
                   chstory, fl_get_input(fd_dsstory->story_ident));
	 }
	 fl_deselect_browser(fd_dsstory->story_details);
       }
       break;

     case 4: // Sort
       StorySort();
       break;

     default:
       break;
   }

   sprintf(dsbtemp, "%d", fl_get_browser_maxline(fd_dsstory->story_details));
   fl_set_input(fd_dsstory->story_count, dsbtemp);

   return;
}

void dsbynameCB(FL_OBJECT *object, long item_no)
{
int             i, item = 0;
float           B;

   switch (item_no)
   {
     case 0:
       i = fl_get_choice(fd_assessadd->belief_by_name) - 1;
       B = LexTable[i].lower + (LexTable[i].upper - LexTable[i].lower)/2.0;
       sprintf(dsbtemp, "%f", B);
       fl_set_input(fd_assessadd->add_confid, dsbtemp);
       break;

     case 1:
       i = fl_get_choice(fd_assessadd->disbelief_by_name) - 1;
       B = LexTable[i].lower + (LexTable[i].upper - LexTable[i].lower)/2.0;
       sprintf(dsbtemp, "%f", B);
       fl_set_input(fd_assessadd->add_plause, dsbtemp);
       break;

     default:
       break;
   }
}
/*                     Explanation Screen routines                       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void WxUpdate(int frameno, char* filename)
{
int             i, i1;
char            *BITMAPDIR;
char            pixname[128];
FL_IMAGE        *image;
FL_OBJECT       *ipipm;

   for (i=0; i<3; i++) {
     ipipm = fd_dsexplain->ds_frameno[i];
     fl_show_object(ipipm);
     fl_free_pixmap_pixmap(ipipm);
     i1 = (int)((double)frameno/pow(10,i));
     sprintf (pixname, "%s/%s", DSBitMaps, xpmfiles[i1]);
     fl_set_pixmap_file(ipipm, pixname);
   }

   //fprintf(stderr, "Loading image from %s\n", filename);
#ifndef XFVER88
   image = flimage_load(filename);
   flimage_sdisplay(image, FL_ObjWin(fd_dsexplain->ds_image));
#endif
   fl_update_display(0);
}

static void imerrorCB(FL_IMAGE *im, const char *s)
{
//FD_ibcanvas *fdui = im->app_data;

  //if(!s) s = " ";
  //fl_show_messages(s);
}

static int imstatusCB(FL_IMAGE *im, const char *s)
{
  /*
     FD_ibcanvas *fdui = im->app_data;
     char buf[512];

     if(im->completed < 0)
     {
        strcpy(buf,s);
     }
     else if(im->completed >= 0 && im->completed < im->total)
     {
        sprintf(buf,"%s %3.0f%% (%4d of %4d)", s,
                 (im->completed*100.0)/im->total,
                 im->completed, im->h);
     }
     else
        strcpy(buf,s);
  */
    fl_set_slider_value(fd_dsexplain->dsim_percent,
		      ((double)im->completed/(double)im->total)*100.0);
    //fl_set_object_label(fdui->status, buf);
    //fl_update_display(0);
    return 0;
}

void assess_explainCB(FL_OBJECT *object, long item_no)
{
FILE*           INITfp;
int             i, k, i1, item = 0;
int             etype;
const char      *fname;
char            filename[128];
Window          winid;

   exitCB(NULL, 14);

   k = fl_get_menu(fd_network->menu_mission) - 1;

   if (strcmp(dsbtypes[k].filebase, "*") == 0) {
     fname = fl_show_fselector("Explanation file", "./DSBFiles", "*.desc", NULL);
     if (fname == NULL) return;
     strcpy(filename, fname);
   } else {
     strcpy(filename, "DSBFiles/");
     strcat(filename, dsbtypes[k].filebase);
     strcat(filename, ".desc");
   }

   EXPLshow(DSBx, DSBy, DSBw, DSBh, DSBwid, 0, filename);
   /*
   if ((INITfp = fopen(filename, "r")) == NULL) return;

   do fgets(dsbtemp, 128, INITfp); while (dsbtemp[0] == '#'); // Read text explanation filename
   sscanf(dsbtemp, "%s %d", filename, &etype);

   if (etype == 0) {
     fl_load_browser(fd_dsexplain->ds_explain, filename);
     do fgets(dsbtemp, 128, INITfp); while (dsbtemp[0] == '#'); // Read visual explanation filename count
     sscanf(dsbtemp, "%d", &WxFiles);
     do fgets(dsbtemp, 128, INITfp); while (dsbtemp[0] == '#'); // Read filename or template
     sscanf(dsbtemp, "%s", WxFormat);
     //
     WxCount = WxFiles;
     if (WxFiles < 0) {
       WxCount = -WxCount;
     }
     fclose(INITfp);
   } else {
     fclose(INITfp);
     return;
   }

   fl_winposition(DSBwinX, DSBwinY);
   fl_initial_winsize(DSBwinW, DSBwinH);
   winid = fl_prepare_form_window(fd_dsexplain->dsexplain,
                                  FL_PLACE_POSITION,FL_NOBORDER, "Assess-Explain");
   fl_winreparent(winid, DSBwinid);
   fl_show_form_window(fd_dsexplain->dsexplain);
   StoreActiveEntry("Assess-Explain");

#ifndef XFVER88
   //memset(mysetup, 0, sizeof(mysetup));
   mysetup.visual_cue = imstatusCB;
   mysetup.error_message = imerrorCB;
   mysetup.app_data = fd_dsexplain;
   flimage_setup(&mysetup);
#endif

   fl_set_object_helper(fd_dsexplain->dsex_rew,  "Rewind");
   fl_set_object_helper(fd_dsexplain->dsex_rev,  "Reverse");
   fl_set_object_helper(fd_dsexplain->dsex_back, "Backward");
   fl_set_object_helper(fd_dsexplain->dsex_stop, "Stop");
   fl_set_object_helper(fd_dsexplain->dsex_step, "Forward");
   fl_set_object_helper(fd_dsexplain->dsex_play, "Play");
   fl_set_object_helper(fd_dsexplain->dsex_end,  "End");

   WxFrame = 0;

   strcpy(filename, WxFormat);
   for (i=0; i<WxCount; i++) {
     WxFrame++;
     if (WxFiles < 0) {
       //strcpy(WxFormat, "../Weather/WxPict-%03d.gif");
       sprintf(filename, WxFormat, WxFrame);
     }
     //fprintf(stderr, "Image %d (of %d) from %s\n", WxFrame, WxCount, filename);
     WxUpdate(WxFrame, filename);
     fl_msleep(WxDelay);
   }
   */
}

/*
void dsimdelayCB(FL_OBJECT *object, long item_no)
{
int             item = 0;

   WxDelay = (int)fl_get_dial_value(fd_dsexplain->dsim_delay);
   sprintf(dsbtemp, "Delay is\n%d ms", WxDelay);
   fl_set_object_label(fd_dsexplain->dsim_delay, dsbtemp);
}

void dscontrolCB(FL_OBJECT *object, long item_no)
{
int             i, item = 0;
char            filename[128];

   strcpy(filename, WxFormat);

   switch (item_no)
   {
     case 0:
       WxFrame = 1;
       if (WxFiles < 0) {
         sprintf(filename, WxFormat, WxFrame);
       }
       WxUpdate(WxFrame, filename);
       WxStop = TRUE;
       break;

     case 1:
       WxFrame = WxCount+1;
       for (i=0; i<WxCount; i++) {
         WxFrame--;
         if (WxFiles < 0) {
           sprintf(filename, WxFormat, WxFrame);
         }
         WxUpdate(WxFrame, filename);
         fl_msleep(WxDelay);
       }
       break;

     case 2:
       if (WxFrame > 1) {
         WxFrame--;
         if (WxFiles < 0) {
           sprintf(filename, WxFormat, WxFrame);
         }
         WxUpdate(WxFrame, filename);
       } else WxStop = TRUE;
       break;

     case 3:
       WxStop = TRUE;
       break;

     case 4:
       if (WxFrame < WxCount) {
         WxFrame++;
         if (WxFiles < 0) {
           sprintf(filename, WxFormat, WxFrame);
         }
         WxUpdate(WxFrame, filename);
       } else WxStop = TRUE;
       break;

     case 5:
       WxFrame = 0;
       for (i=0; i<WxCount; i++) {
         WxFrame++;
         if (WxFiles < 0) {
           sprintf(filename, WxFormat, WxFrame);
         }
         WxUpdate(WxFrame, filename);
         fl_msleep(WxDelay);
       }
       break;

     case 6:
       WxFrame = WxCount;
       if (WxFiles < 0) {
         sprintf(filename, WxFormat, WxFrame);
       }
       WxUpdate(WxFrame, filename);
       WxStop = TRUE;
       break;

     default:
       break;
   }
}

void dsimscrollCB(FL_OBJECT *object, long item_no)
{
int             item = 0;

   switch (item_no)
   {
     case 0:

       break;

     case 1:

       break;

     default:
       break;
   }
}
*/
void evidenceCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}
/*                       Graph & Network routines                        */
/* --------------------------------------------------------------------- */
/*                                                                       */
void assess_selectCB(FL_OBJECT *object, long item_no)
{
/*  Purpose:        Set network node to graph at a given level
**  Screen:         Assess
**  Invoked By:     User selecting a graph select button
**  Processing:     - Reset network
**                  - Get node (assessment) chosen
**                  - Execute network
*/
int             i, n, item = 0;

   resetCB(NULL, 0);
   curr_assess = item_no;
   executeCB(NULL, 0);
}

void plotolddataCB(FL_OBJECT *object, long item_no)
{
/*  Purpose:        Draw previous graph on top of current graph
**  Screen:         Assess
**  Invoked By:     User the 'Prev' graph select button
**  Processing:     - Copy previously saved data itno working array
**                  - Draw graph
*/
int             i, k;

   fl_add_xyplot_overlay(graph, 5, XtOld, YbOld, osteps, FL_SLATEBLUE);
   fl_add_xyplot_overlay(graph, 6, XtOld, YpOld, osteps, FL_INDIANRED);
}

void UpdateNet(int item)
{
int             i, j, k, level;
float           results[6];
float           inverse[6];
char            chline[32];
char            bigstring[64];

   if (TableView) {
     fl_clear_browser(fd_budtable->bud_cur_belief);
     fl_clear_browser(fd_budtable->bud_old_belief);
     fl_clear_browser(fd_budtable->bud_del_belief);
     fl_clear_browser(fd_budtable->bud_cur_disbelief);
     fl_clear_browser(fd_budtable->bud_old_disbelief);
     fl_clear_browser(fd_budtable->bud_del_disbelief);
   }

   for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
     for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
       DS_AlgoGetResult(i, level, results);
       DS_AlgoGetUnfuse(i, level, 0.95, inverse);
       //for (j=0; j<6; j++) inverse[j] = results[j];
       //
       //   Update network screen
       //
       if (NetMulti) {
	 fl_replace_chart_value(fd_network->node_chart[i][level], 1,
                              (double)(results[1]), "B", FL_BLUE);
	 fl_replace_chart_value(fd_network->node_chart[i][level], 2,
                              (double)(results[2]), "D", FL_RED);
	 fl_replace_chart_value(fd_network->node_chart[i][level], 3,
                              (double)(results[3]), "U", FL_YELLOW);
       }
       //
       if (TableView) {
         sprintf(chline, "%f", results[1]);
         fl_addto_browser(fd_budtable->bud_cur_belief, chline);
         sprintf(chline, "%f", nodeinfo[i][level].old_belief);
         fl_addto_browser(fd_budtable->bud_old_belief, chline);
         sprintf(chline, "%f", results[1]-nodeinfo[i][level].old_belief);
         fl_addto_browser(fd_budtable->bud_del_belief, chline);

         sprintf(chline, "%f", results[2]);
         fl_addto_browser(fd_budtable->bud_cur_disbelief, chline);
         sprintf(chline, "%f", nodeinfo[i][level].old_disbelief);
         fl_addto_browser(fd_budtable->bud_old_disbelief, chline);
         sprintf(chline, "%f", results[2]-nodeinfo[i][level].old_disbelief);
         fl_addto_browser(fd_budtable->bud_del_disbelief, chline);
       }

       if (AutoDeltas) neteddoneCB(NULL, 7);

       GetLexical(results[1], chline);
       sprintf(dsbtemp,   "Belief    = %5.2f\n",  results[1]);
       GetLexical(results[2], chline);
       sprintf(bigstring, "Disbelief = %5.2f\n",  results[2]);
       strcat(dsbtemp, bigstring);
       GetLexical(results[3], chline);
       sprintf(bigstring, "Unknown   = %5.2f\n",  results[3]);
       strcat(dsbtemp, bigstring);

       sprintf(bigstring, " -- Threshold -- \n");
       strcat(dsbtemp, bigstring);
       sprintf(bigstring, "Value     = %5.2f\n", 75.0);
       strcat(dsbtemp, bigstring);
       GetLexical(inverse[1], chline);
       sprintf(bigstring, "Belief    = %5.2f\n",  inverse[1]);
       strcat(dsbtemp, bigstring);
       GetLexical(inverse[2], chline);
       sprintf(bigstring, "Disbelief = %5.2f\n",  inverse[2]);
       strcat(dsbtemp, bigstring);
       GetLexical(inverse[3], chline);
       sprintf(bigstring, "Unknown   = %5.2f",  inverse[2]);
       strcat(dsbtemp, bigstring);

       fl_set_object_helper(fd_network->node_chart[i][level], dsbtemp);
     }
   }
}
/*                    Execute & Propagate routines                       */
/* --------------------------------------------------------------------- */
/*                                                                       */
float DegradeEvid(float Tevid, float Tnow, float Tdur)
{
int             ijump;
float           degrade;

   ijump = fl_get_choice(fd_dsevedit->degrade) - 1;
   DSevidence::DS_degradeEvidence(Tevid, Tnow, Tdur, ijump, degrade);

   return(degrade);
}

static void OneTimeStep(int ievid, int jevid, float Tnow, float Tend)
{
int             item = 0, i, j, k, n;
int             nevid, level;
int             dofusion;
int             NodeNo = 0;
float           Bmax = -1.0;
float           Dmin =  2.0;
float           Umin =  2.0;
float           U = 2.0;
float           belief, disbelief;
float           results[6];
float           fogfactors[2];
float           Tin, Tdur, degrade;
char            chline[24];

   i = ievid;
   j = jevid;


   return;
}
/*
void populateCB(FL_OBJECT *object, long item_no)
{
int             item = 0, i, j, k;
int             level;
int             prop_grayed = FALSE;
float           confidence, disbelief, Tin, Tdur, results[5];
float           degrade;
float           fogfactors[2];
const char      *chtemp;
char            chline[40];
char            chdum1[16], chdum2[16];
char            chsource[40];
char            chhypoth[40];

   executeCB(object, 0);           // If continuous, do 'Execute'

   return;
}
*/

void StopLight()
{
int             i, j, n, level, item = 0;
float           Tp, Tu;
float           results[6];

   for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
     Tp = 0.0; 
     for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
       DS_AlgoGetResult(i, level, results);
       //
       //   Color-code the node depending on Belief vis-a-vis threshold
       //
       Tu = 0.0;
       fl_set_object_color(fd_network->node_label[i][level], FL_GOLD, FL_GOLD);
       if (results[1] >= nodeinfo[i][level].threshold) {
	 Tu = 1.0;
	 fl_set_object_color(fd_network->node_label[i][level], FL_SPRING, FL_SPRING);
       }
       if (results[1] <= nodeinfo[i][level].dropdead) {
	 Tu = -1.0;
	 fl_set_object_color(fd_network->node_label[i][level], FL_SALMON, FL_SALMON);
       }
       Tp = Tp + Tu;
     }
     Tp = Tp/DS_AlgoGetLevelWidth(level);
     fl_set_object_color(fd_network->level_label[level], FL_GOLD, FL_GOLD);
     if (Tp > 0.5) {
       fl_set_object_color(fd_network->level_label[level], FL_SPRING, FL_SPRING);
     }
     if (Tp < 0.0) {
       fl_set_object_color(fd_network->level_label[level], FL_SALMON, FL_SALMON);
     }
   }
}

void executeCB(FL_OBJECT *object, long item_no)
{
/*  Purpose:        Execute the network
**  Screen:         Assess
**  Invoked By:     User selecting the 'Run' button
**  Processing:     -
**                  -
**                  -
*/
FILE            *typefp;
int             i, j, n, item = 0;
int             ievid, nevid, level;
int             dofusion;
int             numNodes = 0;
int             NodeNo = 0;
float           Bmax = -1.0;
float           Dmin =  2.0;
float           Umin =  2.0;
float           U = 2.0;
float           belief, disbelief, plause[50];
float           results[6];
float           fogfactors[2];
float           eTime;
float           Tend, Tnow, Tin, Tdur, Tx, degrade;
float           Tconvert[4] = { 60.0, 1.0, 1.0/60.0, 1.0/60.0/24.0 };
float           stepTime = 6.0;                   // Step time = 6 min
char            chline[240];
char            chtemp[128];
char            story[32];
char            mission[32];
int             nFusions = 0;
struct tms      cpustart;
struct tms      cpustop;
unsigned long long int Scycles;
unsigned long long int Ecycles;
unsigned long long int Dcycles;

int             stepNum = 0;                      // Step number
int             stepMode = FALSE;                 // Single-step mode
//
//   Determine step mode: No-step or Single-step
//
   //stepMode = item_no;
   //
   if (!discrete) {                               // If Continuous mode, force no Single-step
     stepNum = 0;
     stepMode = FALSE;
   }
//
//   Do the following setup only once each network evaluation
//
   if (stepNum == 0) {
     //
     //   Need end time for degrading evidence and for setting graph limits
     //
     eTime = (float) (stringToTime(endTime) - stringToTime(startTime)) / 60.0;
     if (eTime > 0.0)
       Tend = eTime;
     else
       Tend = ev.Tlast;

     //printf("Evaluating with Tend = %f and endTime = %s\n", Tend, endTime);
     //
     //   Setup the graph
     //
     Tx = Tend*Tconvert[time_unit];               // Convert to user specified units
     //
     level = EditRow;
     graph = fd_budgraph->bud_plot;
     //
     fl_set_xyplot_overlay_type(graph, 1, FL_NORMAL_XYPLOT);     // Belief curve
     fl_set_xyplot_overlay_type(graph, 2, FL_NORMAL_XYPLOT);     // Plausibility curve
     fl_set_xyplot_overlay_type(graph, 3, FL_NORMAL_XYPLOT);     // Threshold curves
     fl_set_xyplot_xbounds(graph, 0.0, Tx);
     fl_set_xyplot_ybounds(graph, 0.0, 100.0);
     fl_set_xyplot_ytics(graph, 10, 0);
     fl_set_xyplot_linewidth(graph, 1, 2);
     fl_set_xyplot_linewidth(graph, 2, 2);
     Xover2[0] = 0.0;
     Yover2[0] = 0.0;
     strcpy(dsbtemp, "Confidence in ");
     DS_AlgoGetNodeName(curr_assess, level, chline);
     strsub(chline, '\n', ' ');
     strcat(dsbtemp, chline);
     fl_set_xyplot_data(graph, Xover2, Yover2, 1, dsbtemp, "Time (Min)","%");
     //
     Xover2[0] = 0.0;
     Yover2[0] = nodeinfo[curr_assess][level].threshold*100.0;
     Xover2[1] = nodeinfo[curr_assess][level].cutoff*Tconvert[time_unit];
     Yover2[1] = nodeinfo[curr_assess][level].threshold*100.0;
     Xover2[2] = nodeinfo[curr_assess][level].cutoff*Tconvert[time_unit];
     Yover2[2] = 0.0;
     fl_add_xyplot_overlay(graph, 3, Xover2, Yover2, 3, FL_DARKGOLD);
     //
     //   Clear the network display if pruning nodes
     //
     if (fl_get_button(fd_dsb_opt_ctl->prune) == 1) {
       for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
	 for (i=0; i<DS_AlgoGetLevelWidth(level); i++) {
	   fl_hide_object(fd_network->node_label[i][level]);
	   fl_hide_object(fd_network->node_chart[i][level]);
	 }
       }
     }
   }                                              // if(stepNum==0)
//
//   If not in Single-step mode, do timing if requested
//
   if (!stepMode) {
     if (getenv("PLANTIME") != NULL) {
       nFusions = 0;
       DS_AlgoSetNumFusions (0);
       times(&cpustart);
       Scycles = rdtsc();
     }
   }
//
//   Set up steps depending on discrete or continuous mode
//   and No-step or Single-step
//
   n = -1;
   Tnow = -stepTime;
   if (discrete) {                                // Discrete mode
     if (!stepMode)                               // If NOT single-step
       nsteps = ev.numEvid;                       // No. of discrete evidence values
     else                                         // If Single-step
       nsteps = stepNum+1;                        // Do loop once
   } else {                                       // Continuous mode
     nsteps = int(Tend/stepTime);                 // No. of 6 min intervals
   }
//
//   Fuse the evidence by processing either number of pieces of evidence
//   (ev.numEvid) or number of 6 min. time intervals (depending on discrete
//   or continuous fusion) as defined by 'nsteps'.
//
   for (j=stepNum; j<nsteps; j++) {
     /*
     printf("Processing evidence[%d of %d] for %s with B of (%f,%f) at %f\n", j+1, nsteps,
	    ev.evidences[j]->chsource, ev.evidences[j]->confidence,
            ev.evidences[j]->disbelief,
	    ev.evidences[j]->Tin);
     */
     curr_evid = 0;                               // Start from beginning in either case
     DS_AlgoReset();                              // Reset the accumulation matrix for this pass
     //
     if (discrete) {
       Tnow = ev.evidences[j]->Tin;               // Time now = time of evidence
       nevid = j+1;                               // Process evidence 1..current
     } else {
       Tnow = Tnow+stepTime;                      // Time now = time of time step
       nevid = ev.numEvid;                        // Process evidence 1..ev.numEvid
     }
     //
     //   For discrete:
     //     Process all pieces of evidence from 1..current with degradation
     //     based on duration.
     //   For continuous:
     //     Process all pieces of evidence from 1..ev.numEvid with degradation
     //     based on duration.
     //
     for (i=0; i<nevid; i++) {
       //   i     = Current evidence to process
       //   j     = Current time step
       //   Tnow  = Time of current time step
       //   Tend  = Time of last evidence
       if (ev.evidences[i]->valid == FALSE) continue;                 // Evidence is to be skipped
       if ((ev.evidences[i]->Tin+ev.evidences[i]->duration < Tnow) || // Evidence has expired
	   (ev.evidences[i]->Tin > Tend) ||                           // Evidence past end time
	   (ev.evidences[i]->Tin > Tnow)) {                           // Evidence in future
	 belief    = 0.0;
	 disbelief = 0.0;
	 dofusion  = FALSE;
       } else {
	 //
	 //   Degrade belief and disbelief due to time
	 //          1.0 - ((Tend-Tin)/Tdur)
	 degrade   = 1.0;// - ((Tnow - ev.evidences[i]->Tin)/ev.evidences[i]->duration);
	 degrade   = DegradeEvid(ev.evidences[i]->Tin,
                                 Tnow, ev.evidences[i]->duration);
	 belief    = ev.evidences[i]->confidence * degrade;
	 disbelief = ev.evidences[i]->disbelief * degrade;
	 dofusion  = TRUE;
       }	
       //
       //   Apply FOW if enabled
       //
       if (FOWtest()) {
	 if (FOWget(FOW_LOSS, FOW_EVIDENCE, fogfactors) > 0) {
	   belief = belief - fogfactors[1]*belief;
	 }
       }	
       //
       //   Fuse all the (degraded) evidence for current time
       //
       sprintf(dsbtemp, "%f", ev.evidences[i]->Tin);
       fl_set_object_label(fd_network->cdate_text, dsbtemp);
       fl_select_browser_line(fd_dsevedit->input_browser, i+2);
       if (dofusion == TRUE) {
	 if (ev.evidences[i]->type == 2) {                           // Override
	   //
	 } else {                                                // Fuse
	   DS_Algo(ev.evidences[i]->source-1, ev.evidences[i]->level-1,
		   belief, disbelief, ev.evidences[i]->Tin);
	 }
       }
     } // for nevid
     //
     //   Get and save the final answer for each node at this time
     //
     if (DSBMYSQL) {
       strcpy(mission,
              dsbtypes[fl_get_menu(fd_network->menu_mission) - 1].filebase);
       strcpy(story, fl_get_choice_text(fd_network->dsb_story));
     } else {
       // Nothing for now.
     }

     for (level=0; level<DS_AlgoGetTreeDepth(); level++) {
       numNodes = DS_AlgoGetLevelWidth(level);

       for (i=0; i<numNodes; i++) {
	 //
	 nodeinfo[i][level].old_belief = nodeinfo[i][level].belief;
	 nodeinfo[i][level].old_disbelief = nodeinfo[i][level].disbelief;
	 DS_AlgoGetResult(i, level, results);
	 nodeinfo[i][level].belief = results[1];
	 nodeinfo[i][level].disbelief = results[2];

	 if ((fl_get_button(fd_dsb_opt_ctl->prune)) && (results[3] < 0.9999) &&
	     (fd_network->node_label[i][level]->visible == 0)) {
	   fl_show_object(fd_network->node_label[i][level]);
	   fl_show_object(fd_network->node_chart[i][level]);
	   netdrawCB(NULL, 0);
	 }

//         DS_AlgoSaveNodeValues(mission, story, i, level,
//                               nodeinfo[i][level].belief,
//                               nodeinfo[i][level].disbelief, DSBMYSQL);
	 //
	 Ybelief[level+1][i][j] = results[1];
	 Yplause[level+1][i][j] = 1.0 - results[2];
	 Xtime[level+1][i][j] = Tnow;
       }
     }
     //
     //   Update network view
     //
     UpdateNet(i);
     //
     //   Update graph
     //
     n++;
     level = EditRow;
     Yb[n] = (Ybelief[level+1][curr_assess][j])*98.0;
     Yp[n] = (Yplause[level+1][curr_assess][j]+0.01)*98.0;
     Xt[n] =    Xtime[level+1][curr_assess][j]*Tconvert[time_unit];
     fl_add_xyplot_overlay(graph, 1, Xt, Yb, n+1, FL_BLUE);
     fl_add_xyplot_overlay(graph, 2, Xt, Yp, n+1, FL_RED);
     //
     //   Find "Best" assessment
     //
     NodeNo = 0;
     Bmax = -1.0;
     Dmin =  2.0;
     Umin =  2.0;
     U = 2.0;
     numNodes = DS_AlgoGetLevelWidth(DS_treeDepth - 1);
     for (i=0; i < numNodes; i++) {
       DS_AlgoGetResult(i, DS_treeDepth-1, results);
       // switch (fl_get_choice(fd_assess->assess_best)) 
       switch (1) {
       case 1:
	 if (results[1] > Bmax) { Bmax = results[1]; NodeNo = i; }
	 break;
       case 2:
	 if (results[2] < Dmin) { Dmin = results[2]; NodeNo = i; }
	 break;
       case 3:
	 U = 1.0 - results[1] - results[2];
	 if (U < Umin) { Umin = U; NodeNo = i; }
	 break;
       }
     }
     //
     //   If recording, snapshot the display
     //
     if (fl_get_button(fd_dsb_opt_ctl->record)) {
       DSBframe++;
       fprintf(stderr, "Snapshoting frame %d\n", DSBframe);
       sprintf(dsbtemp, "/tmp/ScreenSnaps/DSBN-%d-%04d.gif", getpid(), DSBframe);
       sprintf(chtemp, "xwd -screen -root | convert - gif:%s",
		 /*fl_get_browser_line(fd_bmc3->act_windows, maxline),*/
		 /*"Assess-Network",*/
	       dsbtemp);
       system(chtemp);
     }
   } // for nsteps
   //
   BestNode = NodeNo;
//
//   If not in Single-step mode, do timing if requested
//
   if (!stepMode) {
     if (getenv("GPDATIME") != NULL) {
       Dcycles = rdtsc() - Scycles;
       times(&cpustop);
       double deltatime = (double)(cpustop.tms_utime-cpustart.tms_utime);
       nFusions = DS_AlgoGetNumFusions ();
       if ((typefp = fopen("GPDAinfo.log", "a")) != NULL) {
	 fprintf(typefp, "Data Fusion completed %d fusions in %f seconds.\n",
		 nFusions, (double)Dcycles/(double)Frequency/* deltatime */);
	 fclose(typefp);
       }
     }
   }
//
//   If batch mode, issue the best assessment and exit
//
   if (DSBbatch) {
     assess_issueCB(NULL, BestNode);
     DSBexitCB(NULL, 0);
   }
//
//   Show user details for selected node
//
   DS_AlgoGetResult(curr_assess, level, results);
   strcpy(chtemp, fl_get_object_label(fd_network->node_label[curr_assess][level]));
   strsub(chtemp, '\n', ' ');
   strcpy(chline, fl_get_object_label(fd_network->level_label[level]));
   strsub(chline, '\n', ' ');
   sprintf(dsbtemp,
	   "Assessment: %s\nAction: %s\nTime: %s\nBelief: %d%%\nContrary Evidence: %d%%",
	   chline,
	   chtemp,
	   "2002:06:06:14:34:37",
	   (int)(results[1]*100.0),
	   (int)(results[2]*100.0) );
   fl_set_object_label(fd_budgraph->summary, dsbtemp);

   stepNum++;

   return;
}

void resetCB(FL_OBJECT *object, long item_no)
{
/*  Purpose:        Reset network
**  Screen:         Assess
**  Invoked By:     User selecting 'Reset' button
**  Processing:     - Reset variables
**                  - Set Xforms objects to initial states
**                  - Reset DS algorithm
**                  - Save current graph data for future use
*/
int             i, j, item = 0;

   curr_assess = 0;
   curr_evid   = 0;

   browser_line = 0;
   browser_mod  = 0;            // 0=> 'add', 1=> 'modify'

   fl_activate_object(fd_dsevedit->prop_but);
   fl_set_object_color(fd_dsevedit->prop_but, FL_COL1, FL_INACTIVE_COL);
   //fl_activate_object(fd_network->prop_button);
   //fl_set_object_color(fd_network->prop_button, FL_COL1, FL_INACTIVE_COL);

   DS_AlgoReset();

   fl_get_xyplot_overlay_data(fd_budgraph->bud_plot, 1, XtOld, YbOld, &osteps);
   fl_get_xyplot_overlay_data(fd_budgraph->bud_plot, 2, XtOld, YpOld, &osteps);

   fl_activate_object(fd_budgraph->prev_but);
}

//int buttonpressCB(FL_OBJECT *ob, int event, FL_Coord mx, FL_Coord my, int key, void *raw)
int DSBbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
   netdrawCB(NULL, 0);

   return(0);
}

//<------------------------expose-------------------------------------->

//Run when the screen needs to be refreshed 'cause it is "exposed"

int DSBexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
   if (NetVisible) netdrawCB(NULL, 0);

   return(0);
}

void create_form_dsbuform1(void)
{
  FL_OBJECT *obj;
  //FL_FORM *fdui = (FL_FORM *) fl_calloc(1, sizeof(*fdui));

  fd_dsbuform1 = fl_bgn_form(FL_UP_BOX, 851, 461);

  obj = fl_add_lightbutton(FL_RADIO_BUTTON,30,90,170,30,"sin(2x)*cos(x)");
    fl_set_object_boxtype(obj,FL_BORDER_BOX);

  fl_end_form();

  return;
}

