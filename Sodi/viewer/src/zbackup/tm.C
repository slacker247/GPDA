/************************************************************
 *  tm.C is a Time management statistics viewer.
 *
 *  As of Nov. 7, 1997, the following items need implementing:
 *
 *	1) Move time bars from GISP to this module
 *	2) Implement strip chart
 *	3) Support up to 64 gauges in a scrollable window
 *	4) Fix date display
 *	5) Add support for ETA statistics
 *	6) Add dynamic gauge ranges
 *
 ***********************************************************/

#include <math.h>
#include <time.h>
#include <sys/types.h>
#include <malloc.h>

#include "GR_Shell.H"
#include "GR_Model.H"
#include "speedes_graphics.H"
#include "StatsMessage.H"
#include "DataParser.H"
#include "gd.H"

#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/PanedW.h>
#include <Xm/DrawingA.h>

#ifdef Linux
#include "Xfwf/Hdial.h"
#include "Xfwf/PcBar.h"
#endif

struct TMStruct {
int             NodeId;
int             Cycle;
SIMTIME         GVT;
double          Cpu;
double          Wall;
int             Phase1;
int             Phase2;
int             Events;
int             EperC;
int             EvtGVT;
int             Rolls;
int             Msgs;
int             Anti;
int             Cancels;
int             ExtMsg;
};

Widget		tm_shell;
GR_Window	*tmwindow;
Boolean		tmfirst = TRUE;
int             tmwindW=720, tmwindH=640;
int		GLtmwindW=200, GLtmwindH=420;
XtIntervalId	timeoutid;
int             tmdigitX, tmdigitY;
char            *DATADIR;
 
int		CycleMax = 200;
Boolean		TMVisible = FALSE;
int		TMMenuItem = 0;
int		TMNodes;
float           TMAhead = 0.0;
int		TMNgvt, TMNrisk, TMNopt, TMSpin;
int		TMEvtCount = 0;
long		TMGvt;
long		TMEvents;
long		TMRollbacks;
long		TMAntiMsg;
long		TMMessages;
long		TMCycle;
long		*TMSmsg;
Widget		TMBars[64];
static Widget		TMEvtDial[64];
char            *TMMode;

char		chtime[10];
char		chtoday[12];
char		chgvt[10];
char		chroll[10];
char		chanti[10];
char		chmsgs[10];
char		chevent[10];
char		chcycle[10];

void tm_update (long gvt, StatsMessage *Smsg, Boolean UseRandom);
void time_viewerCB (Widget);
void tmdraw ();
void tm_doneCB ();
void timeoutCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void cyclesCB (Widget menuitem, XtPointer itemno, XtPointer call_data);
   
void
tm_update (long gvt, StatsMessage *Smsg, Boolean UseRandom)
{
extern FILE	*STATfp;
extern char	*STATS;
int		i, inode, irandom;
Arg		TMEvtArgs[10];
int		nargs;

static double  	OldCPU[64];
static double  	OldWall = 0.0;
static double   OldNode0CPU = -1.0;
static int	SVRolls[64];		// Save Rollbacks last cycle
static int	SVAnti[64];		// Save Anti-messages last cycle
static int	SVMsgs[64];		// Save Messages last cycle

int             ST_NodeId;
static int      ST_Cycle=0;
SIMTIME         ST_GVT;
double          ST_Cpu;
double          ST_Wall;
int             ST_Phase1;
int             ST_Phase2;
int             ST_Events;
int             ST_EperC;
int             ST_EvtGVT;
int             ST_Rolls;
int             ST_Msgs;
int             ST_Anti;
int             ST_Cancels;
int             ST_ExtMsg;
/*
 *	Re-draw the SPEEDES statistics stuff
 *	------------------------------------
 */
     /*if (!TMVisible) return;  */
     TMGvt = gvt;
 
     if (!UseRandom) {
       ST_Cycle  = Smsg[0].GetWhichCycle();
       ST_Wall   = Smsg[0].GetWall();
       ST_Events = 0;
       ST_Rolls  = 0;
       ST_Anti   = 0;
       ST_Msgs   = 0;
       for (inode=0; inode<TMNodes; inode++) {
//       inode      = Smsg[i].GetNode();
//       ST_Cpu     = Smsg[inode].GetNodeCPU();
//       ST_Phase1  = Smsg[inode].GetPhase1();
//       ST_Phase2  = Smsg[inode].GetPhase2();
//       ST_Events  = Smsg[inode].GetEvents();
//       ST_EvtGVT  = Smsg[inode].GetEvtGVT();
//       ST_Cancels = Smsg[inode].GetNumCancels();
//       ST_ExtMsg  = Smsg[inode].GetNumExternalMessages();
         ST_Events  = ST_Events + Smsg[inode].GetEvents();
         ST_Rolls   = ST_Rolls + Smsg[inode].GetNumRollBacks();
         ST_Msgs    = ST_Msgs  + Smsg[inode].GetNumMessages();
         ST_Anti    = ST_Anti  + Smsg[inode].GetNumAntiMessages();
       }
     } else {
       ST_Cycle  = ST_Cycle + 1;
       ST_Wall   = OldWall + (int)( (rand()/(RAND_MAX+1.0))*100.0 );
       ST_Events = (int)( (rand()/(RAND_MAX+1.0))*100.0 );
       ST_Rolls  = (int)( (rand()/(RAND_MAX+1.0))*100.0 );
       ST_Anti   = (int)( (rand()/(RAND_MAX+1.0))*100.0 );
       ST_Msgs   = (int)( (rand()/(RAND_MAX+1.0))*100.0 );
     }
     TMEvents    = (long)ST_Events;		// Total events all nodes
     TMRollbacks = (long)ST_Rolls;		// Total rollbacks all nodes
     TMMessages  = (long)ST_Msgs;		// Total messages all nodes
     TMAntiMsg   = (long)ST_Anti;		// Total anti-msgs all nodes
     TMCycle     = (long)ST_Cycle;		// Current cycle
//
// Update bar values
//
     for (i=0; i<TMNodes; i++) {		
       if (!UseRandom) {
	 ST_Cpu  = Smsg[i].GetNodeCPU();
	 irandom = (int)( (ST_Cpu-OldCPU[i]) / (ST_Wall - OldWall) * 100.0);
       } else {
	 irandom = (int)(50.0 + (rand()/(RAND_MAX+1.0))*45.0 );
	 ST_Cpu = irandom;
       }
#ifdef Linux       
       XfwfPcBarSetPercentage(TMBars[i], irandom);
#endif
       OldCPU[i] = ST_Cpu;
     }
//
// Update gauge values
//
     for (i=0; i<TMNodes; i++) {
       if (!UseRandom) {
	 switch (TMMenuItem) {
	    case 0:
               ST_EperC   = Smsg[i].GetEventsPerCycle();
	       break;
            case 1:
               ST_EperC   = Smsg[i].GetNumRollBacks() - SVRolls[i];
               break;
            case 2:
               ST_EperC   = Smsg[i].GetNumMessages() - SVMsgs[i];
               break;
            case 3:
               ST_EperC   = Smsg[i].GetNumAntiMessages() - SVAnti[i];
               break;
            default:
               ST_EperC   = 0;
               break;
         } /*switch*/
       } else {
	 ST_EperC = (int)((float)CycleMax * (rand()/(RAND_MAX+1.0)) );
       }
#ifdef Linux
       nargs = 0;
       if (ST_EperC > CycleMax) {
	   CycleMax = (int)(ST_EperC*1.2);
	   XtSetArg(TMEvtArgs[nargs], XtNmaximum, CycleMax); nargs++;
	 }
       fprintf(stderr, "   Setting gauge %d to value %d\n", i, ST_EperC);
       XtSetArg(TMEvtArgs[nargs], XtNvalue, ST_EperC); nargs++;
       //XtSetValues(TMEvtDial[i], TMEvtArgs, nargs);
#endif
       if (!UseRandom) {
	 SVRolls[i] = Smsg[i].GetNumRollBacks();
	 SVMsgs[i]  = Smsg[i].GetNumMessages();
	 SVAnti[i]  = Smsg[i].GetNumAntiMessages();
       }
     }
     OldWall = ST_Wall;

   if (TMVisible) tmwindow->draw();

   if (STATS  != NULL) {
       fprintf(STATfp, " GVT = %d\t\tNodes = %d\n", TMGvt, TMNodes);
       for (i=0; i<TMNodes; i++) {
           fprintf(STATfp, " GVT = %d\t\tNode = %d\n", TMGvt, i);
           fprintf(STATfp, " CPU =    %f\tWall =   %f",
                     Smsg[i].GetNodeCPU(), Smsg[i].GetWall() );
           fprintf(STATfp, " Phase1 = %d\tPhase2 = %d\n",
                     Smsg[i].GetPhase1(), Smsg[i].GetPhase2() );
         }
     }
}

Widget
makeTpanel (char* name, Widget parent)
{
   Widget       t_panel;
   Widget	sw, sw_form;
   Widget	tmpsep, nodeidstr00, nodeidstr01, nodeidstr02, nodeidstr03;
   Widget       TMEvtDial[64];
   XmString     title;
   XmString     nodenoid;
   int          rpos, pcent, i, iloop, inode;
   int		CycleMax = 200;
   char		line[80];
   XColor       color, unused;
   Pixel        bg_color, fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
   Colormap     cmap;

   XtVaGetValues(parent, XmNcolormap, &cmap, NULL);
   XAllocNamedColor(XtDisplay(parent), cmap, "red", &color, &unused);
   bg_color = color.pixel;
   XmGetColors(XtScreen(parent), cmap, bg_color, &fg_ret, &top_shadow,
               &bottom_shadow, &select_color);
   XAllocNamedColor(XtDisplay(parent), cmap, "blue", &color, &unused);
   fg_color = color.pixel;
/*
 *      Do the Gauges
 *      -------------
 */
   sw = XtVaCreateManagedWidget("Scrolled", xmScrolledWindowWidgetClass, parent,
                 XmNwidth,            280,
                 XmNheight,           260,
                 XmNscrollingPolicy,  XmAUTOMATIC,
                 XmNvisualPolicy,     XmCONSTANT,
                 XmNscrollBarDisplayPolicy, XmSTATIC,
                 NULL);
   sw_form = XtVaCreateWidget("Gauges", xmFormWidgetClass, sw,
                 XmNwidth,            260,
                 XmNheight,           520,
                 NULL);
   tmpsep = XtVaCreateManagedWidget("TmpSep", xmSeparatorWidgetClass, sw_form,
                 XmNwidth,            260,
                 XmNheight,           2,
                 XmNorientation,      XmHORIZONTAL,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_FORM,
                 NULL);
#ifdef Linux
   iloop = (TMNodes/5) + 1;
   nodeidstr00 = tmpsep;
   nodeidstr01 = tmpsep;
   nodeidstr02 = tmpsep;
   nodeidstr03 = tmpsep;
   inode = -1;
   inode = inode+1;
   printf("Building %d Gauges\n", TMNodes);
   for (i=0; i<iloop; i++) {
     sprintf(line, "Node%2.2d", inode);
     nodenoid     = XmStringCreateSimple((char *)line);
     TMEvtDial[inode] = XtVaCreateManagedWidget("Dial00", hdialWidgetClass, sw_form,
                   XmNwidth,            60,
                   XmNheight,           60,
                   XmNmarginLeft,       8,
                   XtNminimum,          0,
                   XtNmaximum,          CycleMax,
                   XtNvalue,            0,
                   XtNforeground,       fg_color,
                   XtNlabelForeground,  fg_color,
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        nodeidstr00,
                   NULL);
     nodeidstr00  = XtVaCreateManagedWidget((String)line,xmLabelWidgetClass,sw_form,
                   XmNwidth,            60,
                   XmNheight,           20,
                   XmNmarginLeft,       8,
                   XmNalignment,        XmALIGNMENT_CENTER,
                   XmNlabelType,        XmSTRING,
                   XmNlabelString,      nodenoid,
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        TMEvtDial[inode],
                   NULL);
     XmStringFree(nodenoid);
     printf("   Gauge %d built....\n", inode);
     inode = inode+1;
     if (inode == TMNodes) goto nodedone;
     sprintf(line, "Node%2.2d", inode);
     nodenoid     = XmStringCreateSimple((char *)line);
     TMEvtDial[inode] = XtVaCreateManagedWidget("Dial01", hdialWidgetClass, sw_form,
                   XmNwidth,            60,
                   XmNheight,           60,
                   XtNminimum,          0,
                   XtNmaximum,          CycleMax,
                   XtNvalue,            0,
                   XtNforeground,       fg_color,
                   XtNlabelForeground,  fg_color,
                   XmNleftAttachment,   XmATTACH_WIDGET,
                   XmNleftWidget,       TMEvtDial[inode-1],
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        nodeidstr01,
                   NULL);
     nodeidstr01  = XtVaCreateManagedWidget((String)line,xmLabelWidgetClass,sw_form,
                   XmNwidth,            60,
                   XmNheight,           20,
                   XmNalignment,        XmALIGNMENT_CENTER,
                   XmNlabelType,        XmSTRING,
                   XmNlabelString,      nodenoid,
                   XmNleftAttachment,   XmATTACH_WIDGET,
                   XmNleftWidget,       nodeidstr00,
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        TMEvtDial[inode],
                   NULL);
     XmStringFree(nodenoid);
     printf("   Gauge %d built....\n", inode);
     inode = inode+1;
     if (inode == TMNodes) goto nodedone;
     sprintf(line, "Node%2.2d", inode);
     nodenoid     = XmStringCreateSimple((char *)line);
     TMEvtDial[inode] = XtVaCreateManagedWidget("Dial02", hdialWidgetClass, sw_form,
                   XmNwidth,            60,
                   XmNheight,           60,
                   XtNminimum,          0,
                   XtNmaximum,          CycleMax,
                   XtNvalue,            0,
                   XtNforeground,       fg_color,
                   XtNlabelForeground,  fg_color,
                   XmNleftAttachment,   XmATTACH_WIDGET,
                   XmNleftWidget,       TMEvtDial[inode-1],
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        nodeidstr02,
                   NULL);
     nodeidstr02  = XtVaCreateManagedWidget((String)line,xmLabelWidgetClass,sw_form,
                   XmNwidth,            60,
                   XmNheight,           20,
                   XmNalignment,        XmALIGNMENT_CENTER,
                   XmNlabelType,        XmSTRING,
                   XmNlabelString,      nodenoid,
                   XmNleftAttachment,   XmATTACH_WIDGET,
                   XmNleftWidget,       nodeidstr01,
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        TMEvtDial[inode],
                   NULL);
     XmStringFree(nodenoid);
     printf("   Gauge %d built....\n", inode);
     inode = inode+1;
     if (inode == TMNodes) goto nodedone;
     sprintf(line, "Node%2.2d", inode);
     nodenoid     = XmStringCreateSimple((char *)line);
     TMEvtDial[inode] = XtVaCreateManagedWidget("Dial03", hdialWidgetClass, sw_form,
                   XmNwidth,            60,
                   XmNheight,           60,
                   XtNminimum,          0,
                   XtNmaximum,          CycleMax,
                   XtNvalue,            0,
                   XtNforeground,       fg_color,
                   XtNlabelForeground,  fg_color,
                   XmNleftAttachment,   XmATTACH_WIDGET,
                   XmNleftWidget,       TMEvtDial[inode-1],
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        nodeidstr03,
                   NULL);
     nodeidstr03  = XtVaCreateManagedWidget((String)line,xmLabelWidgetClass,sw_form,
                   XmNwidth,            60,
                   XmNheight,           20,
                   XmNalignment,        XmALIGNMENT_CENTER,
                   XmNlabelType,        XmSTRING,
                   XmNlabelString,      nodenoid,
                   XmNleftAttachment,   XmATTACH_WIDGET,
                   XmNleftWidget,       nodeidstr02,
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        TMEvtDial[inode],
                   NULL);
     XmStringFree(nodenoid);
     printf("   Gauge %d built....\n", inode);
     inode = inode+1;
     if (inode == TMNodes) goto nodedone;
   }

nodedone:
#endif
   XtManageChild(sw_form);

   return(sw);
}

void
tm_init ()
{
Widget       tm_form;
Widget       tm_list; 
Widget       tm_frame;
Widget       sw, sw_form;
Widget       vertsep, titlewidget, topsep, trailwidget, botsep, tmpsep;
Widget       tm_control, done_button, eventtitle;
Widget       nodeidstr00, nodeidstr01, nodeidstr02, nodeidstr03;
Widget       nonetitle, vertsep2;
Widget       tpanel, drawing, cyclemenu;
Widget       datetitle, timetitle, gvttitle, rolltitle, msgtitle, antititle;
Widget       evttitle,  cycltitle, looktitle;
Widget       ngvttitle, risktitle, nopttitle, spintitle;
XmString     *xstr, title, classtitle;
XmString     title0, title1, title2, title3, title4;
XmString     nodenoid;
XColor       color, unused;
Pixel        bg_color, fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
Pixel        bg_yellow;
Colormap     cmap;
Display      *dpy;
XFontStruct  *font;
XmFontList   fontlist;
Arg	     args[10];
int	     nargs;
int	     i, iloop;
int	     count; 
int	     inode;
char	     line[80]; 
float	     px, py, pz;

   DATA_PARSER parser("speedes.par");
   parser.GoTo("parameters", NULL);
   TMNodes = parser.GetInt("n_nodes");
   TMMode  = parser.GetString("mode");
   //TMSpin  = (int)(parser.GetFloat("spin") * 1000.0);
   TMAhead = parser.GetFloat("lookahead");
   if (TMNodes > 64) TMNodes = 64;
   fprintf(stderr, "\n ---> Using %d Nodes for simulation <---\n", TMNodes);
   parser.GoTo("gvt_parameters", NULL);
   TMNgvt  = (int)parser.GetInt("Ngvt");
   TMNrisk = (int)parser.GetInt("Nrisk");
   TMNopt  = (int)parser.GetInt("Nopt");
   //fprintf(INFOfp, "Using %d Nodes for simulation.\n", TMNodes);
/*
 *      Create the Motif window
 *      -----------------------
 */
   tm_shell = XtCreatePopupShell("TimeViewer", topLevelShellWidgetClass,
                                GR_toplevel, 0, 0);
   tm_form = XmCreateForm (tm_shell, "TMForm", NULL, 0);
   XtVaSetValues(tm_form,
               XmNwidth,             tmwindW,
               XmNheight,            tmwindH,
               NULL);
   dpy = XtDisplay(GR_toplevel);
   font = XLoadQueryFont(dpy,
          "-adobe-courier-bold-*-*-*-20-*-*-*-*-*-*-*");
   fontlist = XmFontListCreate(font, "charset1");

   XtVaGetValues(tm_shell, XmNcolormap, &cmap, NULL);
   XAllocNamedColor(XtDisplay(tm_shell), cmap, "red", &color, &unused);
   bg_color = color.pixel;
   XmGetColors(XtScreen(tm_shell), cmap, bg_color, &fg_ret, &top_shadow,
               &bottom_shadow, &select_color);
   XAllocNamedColor(XtDisplay(tm_shell), cmap, "blue", &color, &unused);
   fg_color = color.pixel;
   XAllocNamedColor(XtDisplay(tm_shell), cmap, "yellow", &color, &unused);
   bg_yellow = color.pixel;

   classtitle   = XmStringCreateLtoR("Unclassified", "charset1");
   titlewidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,tm_form,
                 XmNwidth,            tmwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNshadowThickness,  2,
                 XmNtopAttachment,    XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 NULL);
   topsep = XtVaCreateManagedWidget("TopSep", xmSeparatorWidgetClass, tm_form,
                 XmNwidth,            tmwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        titlewidget,
                 NULL);
   trailwidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,tm_form,
                 XmNwidth,            tmwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 NULL);
   botsep = XtVaCreateManagedWidget("BotSep", xmSeparatorWidgetClass, tm_form,
                 XmNwidth,            tmwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     trailwidget,
                 NULL);
   XmStringFree(classtitle);

   title      = XmStringCreateLtoR(TMMode, "charset1");
   msgtitle   = XtVaCreateManagedWidget("Algor", xmLabelWidgetClass, tm_form,
                 XmNwidth,            280,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
		 XmNrightAttachment,  XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   XmStringFree(title);
/*
 *      Setup the Gauges
 *      ----------------
 */
   title0       = XmStringCreateSimple("Gauge:");
   title1       = XmStringCreateSimple("Events/Cycle");
   title2       = XmStringCreateSimple("Rollbacks/Cycle");
   title3       = XmStringCreateSimple("Messages/Cycle");
   title4       = XmStringCreateSimple("Anti-Messages/Cycle");
   eventtitle = XtVaCreateManagedWidget("rowcol", xmRowColumnWidgetClass, tm_form,
                 XmNwidth,            280,
                 XmNheight,           50,
                 XmNisAligned,        True,
                 XmNentryAlignment,   XmALIGNMENT_END,
                 XmNlabelString,      title0,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        msgtitle,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   cyclemenu = XmVaCreateSimpleOptionMenu(eventtitle, "CyclesMenu",
                 title0, 'G',    0, cyclesCB,
                 XmVaPUSHBUTTON, title1, 'R', NULL, NULL,
                 XmVaPUSHBUTTON, title2, 'R', NULL, NULL,
                 XmVaPUSHBUTTON, title3, 'M', NULL, NULL,
                 XmVaPUSHBUTTON, title4, 'A', NULL, NULL,
                 NULL);
   XmStringFree(title0);
   XmStringFree(title1);
   XmStringFree(title2);
   XmStringFree(title3);
   XmStringFree(title4);
   XtManageChild(cyclemenu);

   tpanel = makeTpanel("Gauges", tm_form);
   XtVaSetValues(tpanel,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        eventtitle,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNtopOffset,        2,
                 NULL);
/*
 *      Setup the Strip Chart
 *      ---------------------
 */
   drawing = XtVaCreateManagedWidget("Drawing", xmFrameWidgetClass, tm_form,
                 XmNheight,           40,
                 XmNwidth,            280,
                 XmNbackground,       bg_yellow,
                 XmNshadowThickness,  3,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
		 XmNtopWidget,        tpanel,
                 NULL);
   XtManageChild(drawing);
/*
 *      Setup the various buttons
 *      -------------------------
 */
   done_button = XtVaCreateManagedWidget ("Quit",
                 xmPushButtonWidgetClass, tm_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)tm_doneCB, NULL);
   vertsep2 = XtVaCreateManagedWidget("VertSep", xmSeparatorWidgetClass, tm_form,
                 XmNwidth,            10,
                 XmNorientation,      XmVERTICAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       done_button,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);
/*
 *      Setup the parameter value displays
 *      ----------------------------------
 */
   title       = XmStringCreateSimple(" ");
   nonetitle   = XtVaCreateManagedWidget("None", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           12,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Date", "charset1");
   datetitle   = XtVaCreateManagedWidget("Date", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        nonetitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Wall Time", "charset1");
   timetitle   = XtVaCreateManagedWidget("Time", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        datetitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("GVT", "charset1");
   gvttitle   = XtVaCreateManagedWidget("GVT", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        timetitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Rollbacks", "charset1");
   rolltitle   = XtVaCreateManagedWidget("Roll", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        gvttitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Messages", "charset1");
   msgtitle   = XtVaCreateManagedWidget("Msgs", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        rolltitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Anti-Msgs", "charset1");
   antititle   = XtVaCreateManagedWidget("Anti", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        msgtitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Events", "charset1");
   evttitle    = XtVaCreateManagedWidget("Event", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        antititle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Cycle No.", "charset1");
   cycltitle   = XtVaCreateManagedWidget("Cycle", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        evttitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Lookahead", "charset1");
   looktitle   = XtVaCreateManagedWidget("Look", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        cycltitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Events/GVT", "charset1");
   ngvttitle   = XtVaCreateManagedWidget("Look", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        looktitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Risky", "charset1");
   risktitle   = XtVaCreateManagedWidget("Look", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        ngvttitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Optimistic", "charset1");
   nopttitle   = XtVaCreateManagedWidget("Look", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        risktitle,
                 NULL);
   XmStringFree(title);
   title       = XmStringCreateLtoR("Spin (Sec)", "charset1");
   spintitle   = XtVaCreateManagedWidget("Look", xmLabelWidgetClass, tm_form,
                 XmNwidth,            120,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      title,
                 XmNfontList,         fontlist,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        nopttitle,
                 NULL);
   XmStringFree(title);

   Widget bar_form = XmCreateForm (tm_form, "BarForm", NULL, 0);
   XtVaSetValues(bar_form,
                 XmNwidth,            400,
                 XmNheight,           100,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep2,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);
   XtManageChild(bar_form);

#ifdef Linux
   int rpos  = 55;
   int pcent = 0;
   printf("Building %d Bars\n", TMNodes);
   for (i=0; i<TMNodes; i++) {
      TMBars[i] = XtVaCreateManagedWidget("bar", xfwfPcBarWidgetClass, bar_form,
                 XtNforeground,       fg_color,
                 XtNpercentage,       pcent,
                 XtNdisplaypc,        FALSE,
                 XtNvertical,         TRUE,
                 NULL);
      XtVaSetValues(TMBars[i],
                 XmNbackground,       bg_color,
                 XmNrightAttachment,  XmATTACH_POSITION,
                 XmNrightPosition,    rpos,
                 XmNtopAttachment,    XmATTACH_POSITION,
                 XmNtopPosition,      4,
                 XtNabs_width,        10,
                 XtNabs_height,       100,
                 NULL);
      XtManageChild(TMBars[i]);
      XfwfPcBarSetPercentage(TMBars[i], 0);
      rpos  = rpos+3;
   }
#endif
/*
 *	Do the OpenGL window piece
 *	--------------------------
 */
   tm_frame = XtVaCreateManagedWidget ("TMFrame", xmFrameWidgetClass, tm_form,
               XmNwidth,          GLtmwindW,
               XmNheight,         GLtmwindH,
               XmNshadowType,     XmSHADOW_IN,
               XmNtopAttachment,  XmATTACH_WIDGET,
               XmNtopWidget,      topsep,
               XmNleftAttachment, XmATTACH_WIDGET,
               XmNleftWidget,     gvttitle,
               NULL);
   tmwindow = new GR_Window ();
   tmwindow->doublebuffer ();
   tmwindow->rgbmode ();
   tmwindow->GR_Widget::createWidget ("TMWindow", tm_frame);
   tmwindow->set_viewmode (GR_ORTHO2);
   tmwindow->left (-1.0);
   tmwindow->right (+1.0);
   tmwindow->bottom (-1.0);
   tmwindow->top (+1.0);
   tmwindow->set_drawfunction(tmdraw);
 
   XtManageChild (tm_form);
   XtPopup(tm_shell, XtGrabNone);
   timeoutid = XtAppAddTimeOut(GR_appcontext, 1000,
                    (XtTimerCallbackProc)timeoutCB, NULL);
   TMEvtCount  = TMNodes;
   TMRollbacks = 0;
   TMAntiMsg   = 0;
   TMMessages  = 0;
   
   if ((DATADIR=getenv("DATADIR")) == NULL)
        DATADIR = "./RSD_Data";
   GR_LcdMakeFont(LcdYELLOW, "./RSD_Data");
   tmdigitX = (int)GR_LcdGetWidth(); tmdigitY = (int)GR_LcdGetHeight(); 
}

void 
time_viewerCB (Widget toplevel)
{
   if (tmfirst)
   {
      GR_toplevel = toplevel;
      tmfirst = FALSE;
      tm_init ();
   }
   else
   {
      XtPopup(tm_shell, XtGrabNone);
      timeoutid = XtAppAddTimeOut(GR_appcontext, 1000,
                    (XtTimerCallbackProc)timeoutCB, NULL);
   }
   TMVisible = TRUE;
}

void
tm_doneCB ()
{
  XtPopdown (tm_shell);
  XtRemoveTimeOut(timeoutid);
  TMVisible = FALSE;
//  tmwindow->set_awake (FALSE);
}

void
tm_listCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
   char *text;

}

void
timeoutCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
char	  *text;
time_t    clock;
struct tm *ltime;
char      *str;
int       i, imon;

   time(&clock);
   ltime = localtime(&clock);
   str = asctime(ltime);           // str = 'Fri Aug 14 10:39:03 1998'
                                   //        012345678901234567890123
   imon = ltime->tm_mon;

   for (i=0; i<8; i++)             // Copy ASCII time
      chtime[i] = str[i+11];

   sprintf(chtoday, "%02.2d%s", i, "/29/1998");
   chtoday[3] = str[8]; if (str[8] == ' ') chtoday[3] = '0';
   chtoday[4] = str[9];
   for (i=0; i<4; i++)             // Copy ASCII year
      chtoday[i+6] = str[i+20];

   timeoutid = XtAppAddTimeOut(GR_appcontext, 1000,
                    (XtTimerCallbackProc)timeoutCB, NULL);
   tmwindow->draw();
}

void
cyclesCB (Widget menuitem, XtPointer itemno, XtPointer call_data)
{
   TMMenuItem = (int) itemno;
}

void tmdraw ()
{
float spoint[2], epoint[2];
int xsize, ysize;
int tmwindW, tmwindH;
unsigned long *lptr;
unsigned short *base, *ibuf, *abuf;
int y, x, i, j, irandom;
char c;
float point[3], deltaY = 2.0;
GLfloat white[3] = { 1.0, 1.0, 1.0 };
/*
 *	Setup the OpenGL Window
 *	-----------------------
 */
   tmwindW = GLtmwindW;
   tmwindH = GLtmwindH;
   glViewport(0, 0, tmwindW, tmwindH);
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho(0.0, tmwindW, 0.0, tmwindH, -1.0, 1.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   glClear(GL_COLOR_BUFFER_BIT);
   glColor3fv(white);
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
/*
 *	Re-draw the date and time strings
 *	---------------------------------
 */ 
   deltaY = 2.0;

   point[0] = (tmwindW) - (tmdigitX * 11);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chtoday, 10, LcdGREEN, 0);

   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chtime, 8, LcdGREEN, 0);

   sprintf(chgvt,  "%8.8d", TMGvt);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chgvt, 8, LcdGREEN, 0);

   sprintf(chroll, "%8.8d", TMRollbacks);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chroll, 8, LcdGREEN, 0);

   sprintf(chmsgs, "%8.8d", TMMessages);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chmsgs, 8, LcdGREEN, 0);

   sprintf(chanti, "%8.8d", TMAntiMsg);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chanti, 8, LcdGREEN, 0);

   sprintf(chevent, "%8.8d", TMEvents);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chevent, 8, LcdGREEN, 0);

   sprintf(chcycle, "%8.8d", TMCycle);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chcycle, 8, LcdGREEN, 0);

   sprintf(chcycle, "%8.8d", TMAhead);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chcycle, 8, LcdGREEN, 0);

   sprintf(chcycle, "%8.8d", TMNgvt);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chcycle, 8, LcdGREEN, 0);

   sprintf(chcycle, "%8.8d", TMNrisk);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chcycle, 8, LcdGREEN, 0);

   sprintf(chcycle, "%8.8d", TMNopt);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chcycle, 8, LcdGREEN, 0);

   sprintf(chcycle, "%8.8f", (float)TMSpin*1000.0);
   point[0] = (tmwindW) - (tmdigitX * 9);
   point[1] = (tmwindH) - (tmdigitY * deltaY); deltaY = deltaY+1.5;
   point[2] = 0;
   GR_LcdPrint(point, chcycle, 8, LcdGREEN, 0);

   glFlush();
}

