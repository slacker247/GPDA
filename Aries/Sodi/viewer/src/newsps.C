/******************************************************************

  Graphics -- Interactive Speedes interface
  -- 06/04/93: created from sps.C made 11/24/92, by Tung;
  -- 08/24/93: 
    
*******************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
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
//#include <rpc/xdr.h>
#define SIGCHILD SIGCLD 

#include "convert.H"
#include "newsps.H"
#include "speedes_graphics.H"
#include "StatsMessage.H"
#include "DataParser.H"	
#include "GR_Spslinks.H"
#include "GISP_Obj.H"
#include "GR_Trail.H"
#include "GR_Sensor.H"
#include "GR_AirObj.H"
#include "GR_String.H"
#include "GR_Impact.H"
#include "GR_TrackErr.H"
#include "GISP_Globals.H"

#ifndef Linux
  typedef int socklen_t;
#endif

//
//   TADILJ stuff
//   ------------
//
int                n_sensors;
int                id_sensor;
extern struct SENSOR  *Assets;

int                n_gispobjs = 0;
int                n_dispobjs = 0;
int                n_records  = 0;
int                n_dropped  = 0;
int                n_launchs  = 0;
int                id_found   = FALSE;
int                trkindex   = -1;
int                n_rtns     = 0;
struct TRACK       trackobj[100];
struct TRAIL       rtninfo[MAXRTNS];
static GISP_Obj    *sensorobj=NULL;
static GISP_Obj    *sensorlbl[10];
static GISP_Obj    *anobj=NULL;

int                SHOWSENSOR;
int                DELAYSECS;                   // Seconds to delay after drawing
Boolean            TADILJINPUT;
char               *TADILJFILE = NULL;
char               *GISPINPUT;
int                eof_encountered = 0;

int                no_pdu;
int                no_j22;
int                no_j30;
int                no_j32;
int                no_j36;
int                no_j70;
int                no_j102;

C_CONVERT          conversion;
//
//   PROX stuff
//   ----------
//
static int         entered = FALSE;
FILE               *OBJSfp;
FILE               *infile;
float              DT;
long               T0;
struct tms         Tbuf;
char               filetype[16];

GR_Shell           *sps_panel;

C_SPEEDES_GRAPHICS *speedes_graphics;
C_SPEEDES_STATE    *speedes_state;
double             speedes_time;
double             speedes_gvt;
double             speedes_slider_time;
double             speedes_start_time = 0.0;
double             speedes_end_time = 99999.0;
double             speedes_delta_t = 20.0;
int                speedes_direction = 1;
Boolean            sps_singlestep_mode = FALSE;
GR_Spslinks        *spslinks=NULL;
short              sensor_type = 0x0001;
short              LMASK = 0xffff, SMASK = 0xffff;
//
//   External data (mostly in gisp)
//   ------------------------------
//
extern GR_DispList *displist;
extern GR_DispList *sps_displist;
extern GR_DispList *sps_links_displist;
extern GR_DispList *trail_displist;
extern GR_DispList *sensor_displist;
extern GR_Window   *gwindow;
extern GR_Window   *mapwindow;
extern float       get_scale (long type);
//extern int         TMNodes;
extern int         FlatMapVisible;
extern int         FlatMapInited;
extern int         PLAYBACK, AUTORUN, AUTOEXIT;
extern double      TIMESTEP;
extern Boolean     SHOWTRACK, SHOWTRAIL, SHOWIMPACT, SHOWERROR;
extern Boolean     SHOWR2;                      // Show R2 sensor duels
extern Boolean     KEEPLINK;                    // Keep sensor link lines
extern Boolean     DROPTRACK;                   // Process 'Drop track' messages if TRUE
extern Boolean     LABELTRACK;                  // Label tracks if TRUE
extern int         PROX, FILEINPUT, SOCKINPUT;
extern char        *INFILE;
extern char        *DEBUG;
extern GR_Window   *gwindow;
extern Widget      modestat, execstat;
extern Pixmap      redledpix, grnledpix, yelledpix, bluledpix;
//
//   Motif stuff
//   -----------
//
XtWorkProcId       spsId = 0;
XtIntervalId       sps_timeoutid = 0;
Boolean            paused = FALSE;
Widget             sps_form;
Widget             sps_progress_slider, sps_progress_text_w;
Widget             sps_timestep_text_w;
//
//   Network socket stuff
//
int                msgsock;
int                sock;
extern int         portid;
extern char        *hostid;
struct sockaddr_in server; //, client;
socklen_t          server_len;//, client_len;
//
//   Callbacks and Work Proc templates
//   ---------------------------------
//
void sps_progressCB (Widget, XtPointer, XmScaleCallbackStruct*);
void sps_timestepCB (Widget menuitem, XtPointer itemno, XtPointer call_data);
void sps_textCB (Widget, int);
void sps_fplayCB ();
void sps_rplayCB ();
void sps_pauseCB ();
void sps_waitCB ();
void sps_stepCB ();
void sps_resetCB ();
Boolean spsWP (XtPointer);
Boolean proxWP (XtPointer);
Boolean inputWP (XtPointer);
// next two are called within main program:
Widget makeSpanel (char* name, Widget parent);
void speedes_init ();
void input_init ();
void input_finish ();
void sps_linksCB (Widget, XtPointer client_data, XtPointer);
void sps_com_linksCB (Widget, XtPointer client_data, XtPointer);
void sps_sen_linksCB (Widget, XtPointer client_data, XtPointer);
extern void makeRasterFont(void);
extern void logoCB (Widget, XtPointer, XtPointer);

extern void TrackAdd(int pos, TRACK *trackobj);
extern void TrackUpdate(double speedes_time, TRACK *trackobj, SENSOR *Assets);
extern void TrackReplace(int pos, TRACK *Tsave);
extern void TrackClear();

extern void FlatMapAdd(int id, int icontype, float x, float y, float z,
                       short r, short g, short b);
extern void FlatMapDraw();

extern char *FileSelector(Widget parent);

extern "C" int   llasec_(float *site, float *sensor, float *azi, float *elev, float *dist);

void OpenInputFile(char *infilename);
void CloseInputFile();

void Net_init(int port, char *host);
void Net_write(char *buf, int bytes, struct sockaddr_in *client, int length);
int  Net_read(char *buf, int bufsize, struct sockaddr_in *client, int *length);
void Net_close();


/* ================================================================================= */

void OpenInputFile(char *infilename)
{
char            *filename;

   if ((strcmp(infilename, "*") == 0) || (infilename == NULL)) {
      if ((filename = FileSelector(GR_toplevel)) == NULL) {
         fprintf(stderr, "No input file selected!!\n");
	 return;
      }
   } else {
      filename = infilename;
   }

   if (infile) fclose(infile);

   if ((infile = fopen(filename, "r+")) == NULL) {
      perror(filename);
   }

   printf("Displaying data from file: %s\n", filename);
}

void CloseInputFile()
{
   fclose(infile);

   return;
}

Widget makeSpanel (char* name, Widget parent, Widget botattach)
{
Widget spanel;
Widget progress_area;
Widget progress_label;  //Widget progress_slider, progress_text_w;
Widget timestep_area;
Widget timestep_label, timestep_buttons; //Widget timestep_text_w;
Widget timestep_b;
Widget button_area;
Widget fplay_b, rplay_b, pause_b, step_b, reset_b;
XmString title, title0, title1, title2, title3, title4, title5, title6, title7;
XmString titleT, title8, title9, title10, title20, title50, title100;
Pixel fg, bg;
Pixmap pixmap;
char *BITMAPDIR;
char rplayfile[80], fplayfile[80], pausefile[80];

   if (TIMESTEP != 0.0) speedes_delta_t = TIMESTEP;
 
   if ((BITMAPDIR=getenv("BITMAPDIR"))==NULL)
     BITMAPDIR = "./BitMaps";
   sprintf (rplayfile, "%s%s", BITMAPDIR, "/rplay.bit");
   sprintf (pausefile, "%s%s", BITMAPDIR, "/pause.bit");
   sprintf (fplayfile, "%s%s", BITMAPDIR, "/fplay.bit");

   spanel = parent;
/*   
   spanel = XmCreateForm (parent, name, NULL, 0);
   XtManageChild (spanel);
   XtVaSetValues (spanel,
		  XmNfractionBase, 100,
                  NULL);
*/
//
//   Build the Progress control area
//   -------------------------------
//
   title = XmStringCreateSimple("Sim Time");
   sps_progress_slider = XtVaCreateManagedWidget("ProgressSlider",
      xmScaleWidgetClass,  spanel,
      XmNheight,           35,
      XmNwidth,            205,
      XmNorientation,      XmHORIZONTAL,
      XmNshowValue,        True,
      XmNdecimalPoints,    1,
      XmNscaleMultiple,    100,
      XmNtitleString,      title,
      XmNbottomAttachment, XmATTACH_WIDGET,
      XmNbottomWidget,     botattach,
      XmNleftAttachment,   XmATTACH_FORM,
      XmNleftOffset,       10,
      NULL);
   XtAddCallback (sps_progress_slider, XmNvalueChangedCallback,
		     (XtCallbackProc)sps_progressCB, NULL);
   XtAddCallback (sps_progress_slider, XmNdragCallback,
		  (XtCallbackProc)sps_progressCB, NULL);
   XtVaSetValues (sps_progress_slider,
		  XmNminimum, (int)(-1.0*10),
		//XmNminimum, (int)(speedes_start_time*10),
		  XmNmaximum, (int)(speedes_end_time*10),
		  NULL);

   sps_progress_text_w = XtVaCreateManagedWidget("ProgressText",
      xmTextFieldWidgetClass, spanel,
      XmNheight,           35,
      XmNwidth,            100,
      XmNtraversalOn,      True,
      XmNbottomAttachment, XmATTACH_WIDGET,
      XmNbottomWidget,     botattach,      
      XmNleftAttachment,   XmATTACH_WIDGET,
      XmNleftWidget,       sps_progress_slider,
      XmNleftOffset,       5,
      XmNshadowThickness,  3,
      NULL);
   XtAddCallback (sps_progress_text_w, XmNactivateCallback,
		  (XtCallbackProc)sps_textCB,
		  (XtPointer)0);                     // 0 for setting sim time;
   char time_string[10];
   sprintf (time_string, "%.1f", speedes_start_time);
   XtVaSetValues (sps_progress_text_w,
		  XmNvalue, time_string,
		  NULL);
   XmStringFree (title);
//
//   Build the Time Step control area
//   --------------------------------
//
   titleT       = XmStringCreateSimple("Tstep:");
   title0       = XmStringCreateSimple("0");
   title1       = XmStringCreateSimple("1");
   title2       = XmStringCreateSimple("2");
   title3       = XmStringCreateSimple("3");
   title4       = XmStringCreateSimple("4");
   title5       = XmStringCreateSimple("5");
   title6       = XmStringCreateSimple("6");
   title7       = XmStringCreateSimple("7");
   title8       = XmStringCreateSimple("8");
   title9       = XmStringCreateSimple("9");
   title10      = XmStringCreateSimple("10");
   title20      = XmStringCreateSimple("20");
   title50      = XmStringCreateSimple("50");
   title100     = XmStringCreateSimple("100");
   timestep_buttons = XtVaCreateManagedWidget("rowcol", xmRowColumnWidgetClass, spanel,
      XmNheight,           35,
      XmNwidth,            180,
      XmNmarginHeight,     0,
      XmNspacing,          0,
      XmNisAligned,        True,
      XmNadjustLast,       True,
      XmNentryAlignment,   XmALIGNMENT_CENTER,
      XmNlabelString,      titleT,
      XmNbottomAttachment, XmATTACH_WIDGET,
      XmNbottomWidget,     botattach,
      XmNleftAttachment,   XmATTACH_WIDGET,
      XmNleftWidget,       sps_progress_text_w,
      NULL);
   Widget summenu = XmVaCreateSimpleOptionMenu(timestep_buttons, "SumMenu",
                                 titleT, 'S',    0, sps_timestepCB,
                 XmVaPUSHBUTTON, title0, '0', NULL, NULL,
                 XmVaPUSHBUTTON, title1, '1', NULL, NULL,
                 XmVaPUSHBUTTON, title2, '2', NULL, NULL,
                 XmVaPUSHBUTTON, title3, '3', NULL, NULL,
                 XmVaPUSHBUTTON, title4, '4', NULL, NULL,
                 XmVaPUSHBUTTON, title5, '5', NULL, NULL,
                 XmVaPUSHBUTTON, title6, '6', NULL, NULL,
                 XmVaPUSHBUTTON, title7, '7', NULL, NULL,
                 XmVaPUSHBUTTON, title8, '8', NULL, NULL,
                 XmVaPUSHBUTTON, title9, '9', NULL, NULL,
                 XmVaPUSHBUTTON, title10, 't', NULL, NULL,
                 XmVaPUSHBUTTON, title20, 'w', NULL, NULL,
                 XmVaPUSHBUTTON, title50, 'f', NULL, NULL,
                 XmVaPUSHBUTTON, title100, 'h', NULL, NULL,
                 NULL);
   XtVaSetValues(summenu,
                 XmNmarginHeight,     0,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       sps_progress_text_w,
                 //XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   XmStringFree(titleT);
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
   XmStringFree(title10);
   XmStringFree(title20);
   XmStringFree(title50);
   XmStringFree(title100);

   XtManageChild(summenu);

   sps_timestep_text_w = XtVaCreateManagedWidget("TimestepText",
      xmTextFieldWidgetClass, spanel,
      XmNheight,           35,
      XmNwidth,            100,
      XmNtraversalOn,      True,
      XmNbottomAttachment, XmATTACH_WIDGET,
      XmNbottomWidget,     botattach,
      XmNleftAttachment,   XmATTACH_WIDGET,
      XmNleftWidget,       timestep_buttons,
      XmNshadowThickness,  3,
      NULL);
   XtAddCallback (sps_timestep_text_w, XmNactivateCallback,
		  (XtCallbackProc)sps_textCB,
		  (XtPointer)1);   // 1 for setting time step;

   char delta_t_string[10];
   sprintf (delta_t_string, "%.1f", speedes_delta_t);
   XtVaSetValues (sps_timestep_text_w,
		  XmNvalue, delta_t_string,
		  NULL);
//
//   Build the control button area
//   -----------------------------
//  
   reset_b = XtVaCreateManagedWidget("Reset", xmPushButtonWidgetClass, spanel,
      XmNheight,           35,
      XmNwidth,            50,
      XmNshadowThickness,  2,
      XmNbottomAttachment, XmATTACH_WIDGET,
      XmNbottomWidget,     botattach,
      XmNrightAttachment,  XmATTACH_FORM,
      NULL);
   XtAddCallback(reset_b, XmNactivateCallback, (XtCallbackProc)sps_resetCB, (XtPointer)0);

   step_b = XtVaCreateManagedWidget("Step", xmPushButtonWidgetClass, spanel,
      XmNheight,           35,
      XmNwidth,            50,
      XmNshadowThickness,  2,
      XmNbottomAttachment, XmATTACH_WIDGET,
      XmNbottomWidget,     botattach,
      XmNrightAttachment,  XmATTACH_WIDGET,
      XmNrightWidget,      reset_b,
      NULL);
   XtAddCallback (step_b, XmNactivateCallback, (XtCallbackProc)sps_stepCB, (XtPointer)0);

   fplay_b = XtVaCreateManagedWidget(">", xmPushButtonWidgetClass, spanel,
      XmNheight,           35,
      XmNwidth,            50,
      XmNshadowThickness,  2,
      XmNbottomAttachment, XmATTACH_WIDGET,
      XmNbottomWidget,     botattach,
      XmNrightAttachment,  XmATTACH_WIDGET,
      XmNrightWidget,      step_b,
      NULL);
   XtAddCallback (fplay_b, XmNactivateCallback, (XtCallbackProc)sps_fplayCB, (XtPointer)0);
   XtVaGetValues (fplay_b, XmNforeground, &fg, XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap(XtScreen(fplay_b), fplayfile, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf("   Couldn't load file %s\n", fplayfile);
   else
     XtVaSetValues (fplay_b,
		    XmNlabelType,   XmPIXMAP,
		    XmNlabelPixmap, pixmap,
		    NULL);
  
   pause_b = XtVaCreateManagedWidget("||", xmPushButtonWidgetClass, spanel,
      XmNheight,           35,
      XmNwidth,            50,
      XmNshadowThickness,  2,
      XmNbottomAttachment, XmATTACH_WIDGET,
      XmNbottomWidget,     botattach,
      XmNrightAttachment,  XmATTACH_WIDGET,
      XmNrightWidget,      fplay_b,
      NULL);
   XtAddCallback (pause_b, XmNactivateCallback, (XtCallbackProc)sps_pauseCB, (XtPointer)0);
   XtVaGetValues (pause_b, XmNforeground, &fg, XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap(XtScreen(pause_b), pausefile, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf("   Couldn't load file %s\n", pausefile);
   else
     XtVaSetValues (pause_b,
		    XmNlabelType,   XmPIXMAP,
		    XmNlabelPixmap, pixmap,
		    NULL);

   rplay_b = XtVaCreateManagedWidget("<", xmPushButtonWidgetClass, spanel,
      XmNheight,           35,
      XmNwidth,            50,
      XmNshadowThickness,  2,
      XmNbottomAttachment, XmATTACH_WIDGET,
      XmNbottomWidget,     botattach,
      XmNrightAttachment,  XmATTACH_WIDGET,
      XmNrightWidget,      pause_b,
      NULL);
   XtAddCallback (rplay_b, XmNactivateCallback, (XtCallbackProc)sps_rplayCB, (XtPointer)0);
   XtVaGetValues (rplay_b, XmNforeground, &fg, XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap(XtScreen(rplay_b), rplayfile, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf("   Couldn't load file %s\n", rplayfile);
   else
     XtVaSetValues (rplay_b,
		    XmNlabelType,   XmPIXMAP,
		    XmNlabelPixmap, pixmap,
		    NULL);
   
   return (rplay_b);
}

/* ================================================================================= */

void
sps_progressCB (Widget, XtPointer, XmScaleCallbackStruct *cbs)
{
  int scale_value = cbs->value;
  char time_string[20];

  speedes_slider_time = scale_value/10.0;
  speedes_time = speedes_slider_time;
  sprintf (time_string, "%.1f", speedes_time);

  //XtVaSetValues (sps_progress_text_w, XmNvalue, time_string, NULL);

  if (spsId) {
     XtRemoveWorkProc (spsId);
     spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
  }
  if (sps_displist) {
     sps_displist->delete_objects ();
  }
}

void
sps_timestepCB(Widget menuitem, XtPointer itemno, XtPointer call_data)
{
int  value;
char delta_t_string[20];

   switch ((int)itemno) {
      case 0:
	//if (!FILEINPUT) value = 20;
         value = 0;
         break;
      case 1:
      case 2:
      case 3:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case 9:
      case 10:
         value = (int)itemno;
         break;
      case 11:
         value = 20;
         break;
      case 12:
         value = 50;
         break;
      case 13:
         value = 100;
         break;
      default:
         value = 20;
         break;
   }   
   speedes_delta_t = value;
   sprintf (delta_t_string, "%.1f", speedes_delta_t);
   XtVaSetValues (sps_timestep_text_w, XmNvalue, delta_t_string, NULL);
   /*if (FILEINPUT)*/ DELAYSECS = value;

   if (spsId) {
      XtRemoveWorkProc (spsId);
      spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
   }
}

void
sps_textCB (Widget text_w, int code)
{
  double value;
  char *text_w_str = XmTextFieldGetString (text_w);

  if (code==0) // Sim Time Progress:
  {
     if (sscanf (text_w_str, "%lf", &value) == 1)
     {
	speedes_time = value;
	if (spsId)
	{
	   XtRemoveWorkProc (spsId);
	   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
	}
     }  
     else
       printf ("   invalid value entered.\n");
  }
  else if (code==1)   // Time Step:
  {
     if (sscanf (text_w_str, "%lf", &value) == 1)
     {
	speedes_delta_t = value; 
	//printf ("   speedes_delta_t is now set to: %f\n", speedes_delta_t);
	if (spsId)
	{  
	   XtRemoveWorkProc (spsId);
	   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
	}
     }    
     else
       printf ("   invalid value entered.\n");
  }
  XtFree (text_w_str);
}

void
sps_fplayCB ()
{
   if (spsId)
     XtRemoveWorkProc (spsId);
   speedes_direction = 1;
   paused = FALSE;
   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
   XtVaSetValues(execstat, XmNbackgroundPixmap, grnledpix, NULL);
   if (sps_displist)
     gwindow->addDispList (sps_displist, "sps_displist");
}

void
sps_rplayCB ()
{
   if (spsId)
      XtRemoveWorkProc (spsId);
   speedes_direction = -1;
   paused = FALSE;
   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
   XtVaSetValues(execstat, XmNbackgroundPixmap, bluledpix, NULL);
   if (sps_displist)
      gwindow->addDispList (sps_displist, "sps_displist");
}

void
sps_stepCB ()
{
   if (spsId)
      XtRemoveWorkProc (spsId); 
   sps_singlestep_mode = TRUE;
   spsId = XtAppAddWorkProc  (GR_appcontext, (XtWorkProc)spsWP, 0);
}

void
sps_pauseCB ()
{
   if (spsId) {
      XtRemoveWorkProc (spsId);
      spsId = 0;
   }
   if (sps_timeoutid) {
      XtRemoveTimeOut(sps_timeoutid);
      sps_timeoutid = 0;
   }
   paused = TRUE;
   XtVaSetValues(execstat, XmNbackgroundPixmap, yelledpix, NULL);
}

void
sps_waitCB ()
{
   if (spsId) {
      XtRemoveWorkProc (spsId);
      spsId = 0;
      if (!paused) sps_timeoutid = XtAppAddTimeOut(GR_appcontext, DELAYSECS, 
                      (XtTimerCallbackProc)sps_fplayCB, NULL);
   }
}

void
sps_resetCB ()
{
   int		i;

   if (spsId) {
      XtRemoveWorkProc (spsId);
      spsId = 0;
   }
   /*
   speedes_time = speedes_graphics->get_start_time () + 0.001;
   speedes_direction = 1;
   XmScaleSetValue (sps_progress_slider, (int)(speedes_time*10));
   */

   if (speedes_graphics) delete speedes_graphics;
//
//   Delete all traces of link lines
//
   if (spslinks != NULL) {
      delete spslinks;
      spslinks = NULL;
   }
   if (sps_links_displist) {
      sps_links_displist->delete_objects ();
   }
//
//   Delete all traces of display objects
//
   if (anobj != NULL) {
      delete[] anobj;
      anobj = NULL;
   }   
   if (sps_displist) {
      sps_displist->delete_objects ();
   }
//
//   Delete all traces of trail objects
//
   if (trail_displist) {
      trail_displist->delete_objects ();
   }
//
//   Clean up the Track Table
//
   for (i=0; i<n_gispobjs; i++) {
      if (trackobj[i].airobj != NULL) {
	 trackobj[i].airobj->drop_track();
	 delete trackobj[i].airobj;
	 trackobj[i].airobj = NULL;
      }
      trackobj[i].old_x = 0.0;
      trackobj[i].old_y = 0.0;
      trackobj[i].old_z = 0.0;

      if (trackobj[i].labels != NULL) {
	 delete trackobj[i].labels;
	 trackobj[i].labels = NULL;
      }

      if (trackobj[i].impactarea != NULL) {
	 delete trackobj[i].impactarea;
	 trackobj[i].impactarea = NULL;
      }

      trackobj[i].id = 0;
   }
   TrackClear();

   n_gispobjs = 0;

   if (FILEINPUT) {
      n_records = 0;
      n_launchs = 0;
      n_dropped = 0;
      if (infile) rewind(infile);
   }
//
//   Re-draw display without objects, links, or trails
//
   gwindow->draw ();
}

/* ================================================================================= */

void
speedes_init ()
{
int       i, j, id, icontype;

   speedes_graphics = new C_SPEEDES_GRAPHICS();
   speedes_state = speedes_graphics->get_speedes_state();
   speedes_start_time = speedes_graphics->get_start_time () + 0.001;
   speedes_end_time = speedes_graphics->get_end_time ();
   speedes_time = speedes_start_time;
   speedes_gvt = 0.0;

   XtVaSetValues (sps_progress_slider,
                   XmNminimum, (int)(-1.0*10),
                   //XmNminimum, (int)(speedes_start_time*10),
		   XmNmaximum, (int)(speedes_end_time*10),
		   NULL);
   
   spslinks = NULL;
   LMASK = 0xffff;

   OBJSfp = fopen("gispobjs.file", "w+");
}

void
input_init ()
{
FILE      *trakfile;
int       i, j, id, icontype, stn;
float     scale_factor;
float     values[10];
int       in_icon, in_rvicon;
short     in_r, in_g, in_b;
float     in_x, in_y, in_z, in_scale;
char      in_name[16];
double    X[3], lat, lon;
char      *filename;
/*
 *    Process the TADILJ Parameter file
 *    ---------------------------------
 */
      TADILJINPUT = FILEINPUT;
      DATA_PARSER tadiljparser("graphics.par");
                   tadiljparser.GoTo("input", NULL);
      TADILJFILE = tadiljparser.GetString("data_file");
      DELAYSECS  = tadiljparser.GetInt("delay_secs");
                   tadiljparser.GoTo("parameters", NULL);
      speedes_start_time = tadiljparser.GetFloat("start_time");
      speedes_end_time   = tadiljparser.GetFloat("tend");   

      // GISPINPUT environment variable always overrides .par file
      if ((GISPINPUT = getenv("GISPINPUT")) != NULL)
         TADILJFILE = GISPINPUT;
      // Input file name on command line overrides everything
      if (INFILE != NULL)
         TADILJFILE = INFILE;
/*
 *    Process the track color scheme file
 *    -----------------------------------
 */
   n_rtns = 0;
   if ((trakfile = fopen("scheme.dat", "r+")) != NULL) {
      fscanf(trakfile, "%d\n", &n_rtns);
      if (n_rtns > MAXRTNS) {
	fprintf(stderr, " Warning: Number of RTNs must be < %d\n", MAXRTNS+1);
        n_rtns = MAXRTNS;
      }
      for (i=0; i<n_rtns; i++) {
         fscanf(trakfile, "%d %hd %hd %hd\n", &rtninfo[i].icon,
                &rtninfo[i].r, &rtninfo[i].g, &rtninfo[i].b);
      }
      fclose(trakfile);
   } else fprintf(stderr, " Warning: No Color Scheme file loaded!!!\n");
/*
 *    Open the input data stream
 *    --------------------------
 */
   if (FILEINPUT) {
      OpenInputFile(TADILJFILE);
   } else {
      Net_init(portid, hostid);
   }

   XtVaSetValues (sps_progress_slider,
                   XmNminimum, (int)(-1.0*10),
                   //XmNminimum, (int)(speedes_start_time*10),
		   XmNmaximum, (int)(speedes_end_time*10),
		   NULL);
   
   spslinks = NULL;
   LMASK = 0xffff;

   OBJSfp = fopen("gispobjs.file", "w+");
}

/* ================================================================================= */

void
Net_init(int portid, char *host)
{
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
Net_write(char *buf, int bytes, struct sockaddr_in *client, int Lclient)
{
int i;
    
   if (sendto(sock, buf, bytes, 0, (struct sockaddr *)client, Lclient) < 0) {
     perror("writing on stream socket"); close(sock); exit(1);
   }
}

int
Net_read(char *buf, int bufsize, struct sockaddr_in *client,  int *Lclient)
{
static int iowait = FALSE, rval;
static socklen_t clientl;

   if (!iowait) {
     memset(buf, 0, bufsize);
     iowait = TRUE;
   }

   clientl = sizeof(struct sockaddr_in);
   rval = recvfrom(sock, buf, bufsize, 0, (struct sockaddr *)client, &clientl);
   if (rval < 0) {	
	if (errno == EWOULDBLOCK) {
          return(rval);
        } else { perror("reading stream message"); close(sock); exit(2); }
   }

   *Lclient = (int)clientl;
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

/* ================================================================================= */

void
input_finish ()
{
int     i, j;

   if (INFOfp != NULL) {
      fprintf(INFOfp, "  -- Objects processed ................... %d\n", n_gispobjs);
      fprintf(INFOfp, "  -- Display objects processed ........... %d\n", n_dispobjs);
      fprintf(INFOfp, "  -- Input file records read ............. %d\n", n_records);

      fprintf(INFOfp, "\n");
      fprintf(INFOfp, "  -- The following Tracks were processed:\n");
      fprintf(INFOfp, "        Track         # Drop       Track\n");
      fprintf(INFOfp, "          ID          Track        Marking\n");
      for (i=0; i<n_gispobjs; i++)
         fprintf(INFOfp, "        %5d           %2d          %s\n",
                 trackobj[i].id, trackobj[i].dropped, trackobj[i].chmarking);

      fprintf(INFOfp, "\n");
      fprintf(INFOfp, "  -- The following Messages were processed:\n");
      fprintf(INFOfp, "        %s (Truth) .......... %5d\n", "PDU ", no_pdu);
      fprintf(INFOfp, "        %s (STN Loc) ........ %5d\n", "J2.2", no_j22);
      fprintf(INFOfp, "        %s (Ellipse) ........ %5d\n", "J3.0", no_j30);
      fprintf(INFOfp, "        %s (Air Trk) ........ %5d\n", "J3.2", no_j32);
      fprintf(INFOfp, "        %s (Space Trk) ...... %5d\n", "J3.6", no_j36);
      fprintf(INFOfp, "        %s (Drop Trk) ....... %5d\n", "J7.0", no_j70);
   }
}
/*                                                                                   */
/* ================================================================================= */
/*                                                                                   */
/*            Following are the Work Procs where all the work gets done              */
/*                                                                                   */
/* ================================================================================= */
/*                                                                                   */
Boolean
spsWP (XtPointer wp_data)
{
//
//   Determine if we are done yet.
//
   if (eof_encountered) return FALSE;

   if (FILEINPUT || SOCKINPUT)
      inputWP(wp_data);
   else
      proxWP(wp_data);
//
//   Delay a while if requested
//
   if (DELAYSECS > 0) {
      sleep((unsigned)DELAYSECS);
      //sps_waitCB ();
   }
//
//   Should we keep going??
//
   if (sps_singlestep_mode) {
      sps_singlestep_mode = FALSE;
      return TRUE;                           // i.e., quit for now;
   } else
      return FALSE;                          // i.e., continue;
}

Boolean
inputWP (XtPointer)
{
C_QUEUE          *objects, *links;
C_LINK           *link;
C_SPEEDES_OBJECT *speedes_object, *sensor_object, *track_object;
int              sensor_id, n_speedes_objects, n_links;
int              i, icontype, id, irandom, indx;
int              attrib;
double           X[3], V[3];
double           Xs[3], Vs[3];
double           Xt[3], Vt[3];
double           Vx, Vy, Vz;
double           Px, Py, Pz;
float            Xlat, Xlon, Xalt;
float            tadilj_time;
double           sideral, re_cos_lat;
double           Rmax;
double           sloc[3], tloc[3];
double           lat, lon;
float            scale_factor, sfactor2, rx, ry, rz;
float            vmag2;
// static GR_Spslinks *spslinks = NULL;
short            rgb[3];
int              errcount = 0;
short            debug_link_type;
extern char      *TIMING;
extern FILE      *TIMEfp;
char             buffer[180];
char             outbuf[8];
char             chline[80];
char             chmsgno[8];
char             chmark[40];
char             chmsgtyp;
char             chident[40];
char             idtext[8];
char             chsrc[8], chdst[8];
int              hold1, hold2;
int              imsg, tgtid;
float            orient, major, minor, inalt, invel, inextra;
float            Radius = 1.0;
float            Length = 4.0;
int              drop, stn, incolor;
int              trkid_S, trkid_A, trkid_E;
int              Lclient, incount;
struct sockaddr_in client;
char             littlestr[40];
char             bigstring[160];
XmString         xstr;
extern Widget    textarea;
float            elev, dist, azi, sensor[3], site[3];

//
//   =========================
//   Get the next INPUT record
//   =========================
//
   if (FILEINPUT) {                             // -------- FILE INPUT --------
      do fgets(buffer, sizeof(buffer), infile); while (buffer[0]=='#');
      if (feof(infile)) {
         eof_encountered = TRUE;
         fprintf(stderr, "  --- End of data encountered reading input file. ---\n");
         rewind(infile);
         if (AUTOEXIT) logoCB(NULL, (XtPointer)8, NULL);
         XtVaSetValues(execstat, XmNbackgroundPixmap, redledpix, NULL);
         return TRUE;
      }
      // The data we are waiting for has arrived
      sscanf(buffer, "%f %c %s %d %d %d %d %f %f %f %s %d %f %f %f %f %f %f",
            &tadilj_time, &chmsgtyp, chmsgno,
            &trkid_S, &trkid_A, &trkid_E, &stn, &Xlat, &Xlon, &Xalt,
            chmark, &incolor, &inalt, &invel, &orient, &major, &minor, &inextra);
   } else {                                     // -------- SOCKET INPUT --------
      incount = Net_read(buffer, sizeof(buffer), &client, &Lclient); // Get the next message
      if (incount < 0) {                        // I/O is not yet complete
         return FALSE;                          // Continue until message arrives
      } else {                                  // I/O is complete
	 if (incount == 1) {                    // If only 1 byte, it must be an EOF
            eof_encountered = TRUE;
            fprintf(stderr, "  --- End of data encountered reading input socket. ---\n");
	    Net_close();
            if (AUTOEXIT) logoCB(NULL, (XtPointer)8, NULL);
            return TRUE;
	 }
	 // The data we are waiting for has arrived
         sscanf(buffer, "%s %s %d %d %f %c %s %d %d %d %d %f %f %f %s %d %f %f %f %f %f %f",
                chsrc, chdst, &hold1, &hold2,
                &tadilj_time, &chmsgtyp, chmsgno,
                &trkid_S, &trkid_A, &trkid_E, &stn, &Xlat, &Xlon, &Xalt,
                chmark, &incolor, &inalt, &invel, &orient, &major, &minor, &inextra);
	 }
   }                                            // -------- INPUT COMPLETE --------

   n_records = n_records+1;

   if (DEBUG != NULL) 
      fprintf(stderr, "[%d] %f %c %s %d %d %d %o %f %f %f %s %d %f %f %f\n",
               n_records, tadilj_time, chmsgtyp, chmsgno,
               trkid_S, trkid_A, trkid_E, stn, Xlat, Xlon, Xalt,
               chmark, incolor, orient, major, minor);
//
//   Optionally provide some timing statistics:
//      - Initialization time to get the display objects set up
//      - Initialization time to get the link objects set up
//      - Drawing time
//
   if (TIMING != NULL) {
      T0 = times(&Tbuf);
      fprintf(TIMEfp, ":> starting tick count is %d\n", T0);
   }

   if (fabs(Xlat) <= 90.0) {          // Input is lat/lon/alt
      lat = Xlat*M_PI/180.0;             // Convert input degrees latitude to radians
      lon = Xlon*M_PI/180.0;             // Convert input degrees longitude to radians
      Xalt = Xalt/1000.0;                // Convert input meters altitude to kilometers
      conversion.lla_to_xyz(lat, lon, Xalt, X);     // Get the (X,Y,Z) values
   } else {                           // Input is (X, Y, Z)
      X[0] = (Xlat/1000.0);              // Convert input meters X to kilometers
      X[1] = (Xlon/1000.0);              // Convert input meters Y to kilometers
      X[2] = (Xalt/1000.0);              // Convert input meters Z to kilometers
      conversion.xyz_to_latlon(X, lat, lon);          // Get the lat/lon values
      Xlat = lat*180.0/M_PI;
      Xlon = lon*180.0/M_PI;
      Xalt = sqrt(X[0]*X[0]+X[1]*X[1]+X[2]*X[2])-RE;  // Get the alt value
   }
//
//   When we get here, we have: lat/lon/alt in degrees/degrees/kilometers above sealevel
//                              X, Y and Z in kilometers from center of earth
//
   V[0] = 0.0;
   V[1] = 0.0;
   V[2] = 0.0;
//
//   Find the track index, if it exists in the table
//
   id = trkid_E;
   id_found = FALSE;
   for (i=0; i<n_gispobjs; i++) {
      if (id == trackobj[i].id) {
         id_found = TRUE;
         trkindex = i;
         break;
      }
   }
//
//   Track id not in table, put it in
//
   if (!id_found) {
     trkindex = n_gispobjs;
     if (INFOfp != NULL)
       fprintf(INFOfp, "TRACKS:  Putting track %d at index %d\n", id, trkindex);
     trackobj[trkindex].id = id;
     trackobj[trkindex].icon = rtninfo[incolor].icon;
     trackobj[trkindex].dropped = 0;
     trackobj[trkindex].labeled = FALSE;
     trackobj[trkindex].impactarea = NULL;
     trackobj[trkindex].iltime = 0.0;
     strncpy(trackobj[trkindex].weapon, "Unknown", 8);
     strncpy(trackobj[trkindex].engage, "Unknown", 8);
     strncpy(trackobj[trkindex].chmsg, chmsgno, 8);
     strncpy(trackobj[trkindex].chmarking, chmark, 15);
     strncpy(trackobj[trkindex].chstatus, "In-Flight", 15);
     trackobj[trkindex].airobj = new GR_AirObj (trackobj[trkindex].id, trackobj[trkindex].icon);
     trackobj[trkindex].airobj->init(trackobj[trkindex].id, trackobj[trkindex].icon);
     trackobj[trkindex].airobj->set_trackid(trackobj[trkindex].id);
     trackobj[trkindex].airobj->rotate_z (180);
     trackobj[trkindex].airobj->rotate_x (90);
     trackobj[trkindex].airobj->set_llah(Xlat, Xlon, Xalt, 0.0);
     trail_displist->add_object(trackobj[trkindex].airobj);
     //trackobj[trkindex].airobj->add_trail (trail_displist, 255, 255, 0);
     //trackobj[trkindex].airobj->trail_on ();

     if (LABELTRACK && (chmsgtyp != 'P')) {
        sprintf(idtext, "%s", trackobj[trkindex].chmarking);
        trackobj[trkindex].labels =
             new GR_String((float)X[1]/RE, (float)X[2]/RE, (float)X[0]/RE, idtext);
        sensor_displist->add_object(trackobj[trkindex].labels);
        trackobj[trkindex].labeled = TRUE;
     }
     /*
     trackobj[trkindex].error = new GR_TrackErr(1717, Xlat, Xlon, Radius, Length,
	          (float)X[1]/RE, (float)X[2]/RE, (float)X[0]/RE, id);
     trackobj[trkindex].error->set_rgb(0, 255, 0);
     sensor_displist->add_object(*trackobj[trkindex].error);
     */
     n_gispobjs = n_gispobjs + 1;
     n_launchs = n_launchs + 1;

     TrackAdd(trkindex, trackobj);
   }
//
//   Initialize some variables
//
    speedes_time = tadilj_time;
    n_speedes_objects = 1;
    n_links = 0;
    XmScaleSetValue (sps_progress_slider, (int)(speedes_time*10));
//
//   =========================
//   Process JDN message types
//   =========================
//
    if (chmsgtyp == 'J') {

       if (SHOWR2 && (stn > 0)) n_links = 1;

       imsg = (int)((atof(chmsgno)+0.005)*100.0);
       switch (imsg) {

       case 220:
       case 230:
       case 250:
          //
          //   Process 'STN location update' message (J2.2, J2.3, J2.5)
	  //        orient   = Azimuth
	  //        major    = Elevation
	  //        minor    = Range (Km)
	  //        inextra  = Field-of-View
	  //
	  no_j22 = no_j22 + 1;
	  Xalt = inalt;
          Assets[stn].gbix  = X[0];
	  Assets[stn].gbiy  = X[1];
	  Assets[stn].gbiz  = X[2];
	  Assets[stn].gbilatit = Xlat;
	  Assets[stn].gbilongi = Xlon;
	  Assets[stn].gbialt   = Xalt;
	  if (major > 0.0)   Assets[stn].gbielev = major;
	  if (orient > 0.0)  Assets[stn].gbiazi = orient;
	  if (minor > 0.0)   Assets[stn].gbirmax = minor;
	  if (inextra > 0.0) Assets[stn].gbifovhi = inextra;
	  if (Assets[stn].gbisensor != NULL) {
	    Assets[stn].gbisensor->set_lla(Xlat, Xlon, Xalt);
	    Assets[stn].gbisensor->set_elev(Assets[stn].gbielev);
	    Assets[stn].gbisensor->set_range(Assets[stn].gbirmax);
	    Assets[stn].gbisensor->set_azi(Assets[stn].gbiazi);
	    Assets[stn].gbisensor->set_fov(Assets[stn].gbifovhi);
	    Assets[stn].gbisensor->gen_sensor();
	  }
          //goto skip;
          break;

       case 300:
          //
          //  Process Impact area ellipse message (J3.0)
	  //        orient   = Orientation of Ellipse (0 => major axis points North)
	  //        major    = Major Axis length (Km)
	  //        minor    = Minor Axis length (Km)
	  //        inextra  = Not used
          //
          no_j30 = no_j30 + 1;
          if (SHOWIMPACT) {
             if (trackobj[trkindex].impactarea != NULL) {
	       sensor_displist->delete_object(trackobj[trkindex].impactarea);
	       trackobj[trkindex].impactarea = NULL;  
             }
             trackobj[trkindex].impactarea =
	       new GR_Impact (0, Xlat, Xlon, major/1000.0, minor/1000.0, orient);
             trackobj[trkindex].impactarea->set_rgb(rtninfo[incolor].r,
                                                    rtninfo[incolor].g,
                                                    rtninfo[incolor].b);
             sensor_displist->add_object(*trackobj[trkindex].impactarea);
	     }
	     trackobj[trkindex].impactarea->set_llah(Xlat, Xlon, Xalt, 0.0);
             trackobj[trkindex].impact_lat = Xlat;
             trackobj[trkindex].impact_lon = Xlon;
             trackobj[trkindex].impact_err = major/1000.0;
          goto skip;
          break;

       case 320:
          //
          //   Process 'Air Track' message (J3.2)
          //
	  no_j32 = no_j32 + 1;
	  Xalt = inalt;
	  if (strcmp(trackobj[trkindex].chmsg, chmsgno) != 0) {
            if (INFOfp != NULL)
	       fprintf(INFOfp, "WARNING: Unexpected message type %s for track %d at time %f\n",
                      chmsgno, id, tadilj_time);
            strcpy(trackobj[trkindex].chmsg, chmsgno);
	  }
          break;

       case 360:
          //
          //   Process 'Space Track' message (J3.6)
          //
	  no_j36 = no_j36 + 1;
	  if (strcmp(trackobj[trkindex].chmsg, chmsgno) != 0) {
            if (INFOfp != NULL)
	      fprintf(INFOfp, "WARNING: Unexpected message type %s for track %d at time %f\n",
                      chmsgno, id, tadilj_time);
            strcpy(trackobj[trkindex].chmsg, chmsgno);
	  }
	  /*
          if (SHOWERROR) {
            trackobj[trkindex].error->update((float)X[1]/RE, (float)X[2]/RE, (float)X[0]/RE);
	    				     //trkindex, trackobj
	    trackobj[trkindex].error->set_rgb(255, 255, 0);
	  }
	  */
          break;

       case 700:
          //
          //   Process 'drop track' message (J7.0)
          //
          no_j70 = no_j70 + 1;
	  if (DROPTRACK) {
             sps_displist->delete_track(id);
	     sps_links_displist->delete_track(id);
             trackobj[trkindex].airobj->drop_track();
             if (trackobj[trkindex].impactarea != NULL) {
	       sensor_displist->delete_object(trackobj[trkindex].impactarea);
	     }
	     if (trackobj[trkindex].labeled) {
	       sensor_displist->delete_object(trackobj[trkindex].labels);
	     }
	  }
	  if (trackobj[trkindex].iltime > 0.0) {
	     trackobj[trkindex].itime = tadilj_time;
	  }
          trackobj[trkindex].dropped = trackobj[trkindex].dropped + 1;
          strcpy(trackobj[trkindex].chstatus, "Dropped");
          n_dropped = n_dropped + 1;
          TrackReplace(trkindex, trackobj);
          goto skip;
          break;

       case 1020:
          //
          //   Process 'GBI Target' message (J10.2)
          //
	  no_j102 = no_j102 + 1;
          tgtid = (int)orient;
          for (i=0; i<n_gispobjs; i++) {
             if (tgtid == trackobj[i].id) {
	        trackobj[i].istn = stn;              // Save the Weapon System Asset ID
		strcpy(trackobj[i].engage, "Fire");  // Save Weapon Engagement Status
		trackobj[i].iltime = tadilj_time;    // Save Interceptor launch time
                goto skip;
             }
          }
	  goto skip;
          break;

       default:
	  fprintf(stderr, "Warning: J%s message handling not implemented!\n", chmsgno);
          fprintf(stderr, "Processing message type %d\n", imsg);
	  goto skip;
	  break;

       } /* switch */

    } //   --------------- End of JDN message processing --------------- 
    else {
       no_pdu = no_pdu + 1;
    }
//
//   Initialize some variables
//
    trackobj[trkindex].stn = stn;                        // Set tracking STN
    trackobj[trkindex].speed = invel;                    // Set Speed of track object
    strncpy(trackobj[trkindex].chmarking, chmark, 15);
//
//   ==================================================
//   Set up the display list for GISP object processing
//   ==================================================
//
    if (!SHOWTRACK) {
       //...... set up the sps_displist for GISP_Obj objects
       if (anobj != NULL) {
          delete[] anobj;
          anobj = NULL;
       }
       sps_displist->delete_objects();
    }

    if (n_speedes_objects) {
       anobj = new GISP_Obj[n_speedes_objects];
       n_dispobjs = n_dispobjs + n_speedes_objects;
    }

    trackobj[trkindex].X[0] = X[0];
    trackobj[trkindex].X[1] = X[1];
    trackobj[trkindex].X[2] = X[2];
    trackobj[trkindex].V[0] = V[0];
    trackobj[trkindex].V[1] = V[1];
    trackobj[trkindex].V[2] = V[2];

    if (SHOWERROR) {
       if (trackobj[trkindex].error != NULL) {
          delete trackobj[trkindex].error;
          trackobj[trkindex].error = NULL;    
       }
    }
 
   if (TIMING != NULL) {
      DT = (float)(times(&Tbuf) - T0);
      fprintf (TIMEfp, ":> speedes_time = %f\n", speedes_time);
      fprintf (TIMEfp, ":> pre-processing takes %f ticks\n", DT);
      T0 = times(&Tbuf);
   }
//
//   ================================================
//   Go through each object and fill the display list
//   ================================================
//
   for (i=0; i<n_speedes_objects; i++)
   {
      if (n_speedes_objects > 0) {
        icontype = rtninfo[incolor].icon;
        rgb[0] = rtninfo[incolor].r;
        rgb[1] = rtninfo[incolor].g;
        rgb[2] = rtninfo[incolor].b;
        //
        //   Connect the dots if wanted
        //
        if (SHOWTRAIL) {
           if (trackobj[trkindex].old_x != 0.0) {
              sloc[0] = trackobj[trkindex].old_y/RE;
              sloc[1] = trackobj[trkindex].old_z/RE;
              sloc[2] = trackobj[trkindex].old_x/RE;
              tloc[0] = X[1]/RE;
              tloc[1] = X[2]/RE;
              tloc[2] = X[0]/RE;
              trackobj[trkindex].airobj->add_link (0, sloc, tloc, rgb);
           }
        }
        //
        //   Save the current location for old time sake
        //
        trackobj[trkindex].old_x = X[0];
        trackobj[trkindex].old_y = X[1];
        trackobj[trkindex].old_z = X[2];

        //printf("Gisp Object display list setup: icon id is %d\n", icontype);
        if (icontype < 0) {
          errcount++;
          icontype = 199;  //  make sure it is valid............
        }
        else if (icontype ==   0) icontype = 76; // for debugging: ROCC
        else if (icontype == 105) icontype = 42; // for debugging: purple ball
        else if (icontype ==  37) icontype = 38; // for debugging: new_impact2

        anobj[i].init(id,icontype);
        anobj[i].set_trackid(id);
        sps_displist->add_object(&anobj[i]);
        sfactor2 = 1.0;
        Rmax = 1.0;
/*
 *              Do translation
 *              Determine scale factor
 *              Do rotation
 *              Do scaling
 */
        if (sfactor2 < 0.01)
          anobj[i].set_visible_flag (FALSE);
        else {                                   //...... fill graphics stuff
          anobj[i].translate ((float)X[1]/RE, (float)X[2]/RE, (float)X[0]/RE);
	  //lat = Xlat*180.0/M_PI;
	  //lon = Xlon*180.0/M_PI;
          trackobj[trkindex].latitude  = Xlat;
	  trackobj[trkindex].longitude = Xlon;
	  trackobj[trkindex].altitude  = Xalt;
          //printf("newsps: type %d lat %f lon %f\n", icontype, lat, lon);
          anobj[i].set_xyz ((float)X[0]/RE, (float)X[1]/RE, (float)X[2]/RE);
	  anobj[i].set_llah(lat, lon, Xalt, 0.0);
	  anobj[i].set_time(speedes_time);
          //
          //   Scale the icon to the "right" size and rescale selected models
          //
          scale_factor = get_scale (icontype);
          if (icontype == 24)                           // ssts
             scale_factor *= 0.5;
          else if (icontype == 2  || icontype == 17 ||  // stage 1 or 2
                   icontype == 68 || icontype == 70)    // AWACS & Mig
             scale_factor *= 5;
          else if (icontype == 150 || icontype == 151)  // gbi
             scale_factor *= 20.0;
          else if (icontype == 6 || icontype == 34)
             scale_factor *= 5.0;
          else if (icontype == 300 || icontype == 301 ||
                   icontype == 302 || icontype == 303 ||
                   icontype == 304)
             scale_factor *= 5.0;
          else if (icontype == 18 || icontype == 409)   // rv or bus
             scale_factor *= 10;
          else if (icontype == 127)                     // dome
             scale_factor *= Rmax/RE;
          scale_factor *= sfactor2;
          //
          //   Rotate the icon to the "right" orientation
          //
          anobj[i].rotate_z (180);
          vmag2 = V[0]*V[0] + V[1]*V[1] + V[2]*V[2];

          if (vmag2 != 0) {
             if (icontype==199 || icontype==90)
               printf ("Type: %d has non-zero velocity: %f\n", icontype, vmag2);
             Vx = V[1];
             Vy = V[2];
             Vz = V[0];
             rx = -atan2(Vy,sqrt(Vz*Vz+Vx*Vx))*180.0/M_PI;
             ry = atan2(Vx,Vz)*180.0/M_PI;
             if (icontype==55)                  // or any aircraft model.......
             {
               anobj[i].rotate_y (180);         // ??
             }
             else if (icontype==24)             // ssts or similar (sensor?) model
             {
               //anobj[i].rotate_z (-90);
             }
             anobj[i].rotate_x (rx);            // added roll around the body axis, 7/29/93:
             anobj[i].rotate_y (ry);
          } else {
             Px = X[1];
             Py = X[2];
             Pz = X[0];
             rx = -atan2(Py,sqrt(Pz*Pz+Px*Px))*180.0/M_PI;
             ry = atan2(Px,Pz)*180.0/M_PI;
             if (icontype!=127)                 // if it is not the ground sensor:
               anobj[i].rotate_x (90);
             anobj[i].rotate_x (rx);
             anobj[i].rotate_y (ry);
          }

          anobj[i].scale (scale_factor, scale_factor, scale_factor);
          anobj[i].finish();                    // DRE

          if (SHOWERROR && (chmsgtyp == 'J') && (imsg == 360)) {
	    trackobj[trkindex].error = new GR_TrackErr(1717, Xlat, Xlon, Radius, Length,
	          (float)X[1]/RE, (float)X[2]/RE, (float)X[0]/RE, id);
	    trackobj[trkindex].error->set_rgb(0, 255, 0);
	    sensor_displist->add_object(*trackobj[trkindex].error);
	  }    
        }  //...... end of fill graphics stuff (if visible)
     }
  }
//
//   ===========================
//   Update the flat map objects
//   ===========================
//
   if (FlatMapInited)
      FlatMapAdd(icontype, id, Xlat, Xlon, Xalt, rgb[0], rgb[1], rgb[2]);

   if (errcount>0)
      printf ("Warning: %d icontypes not set.\n", errcount);

   if (TIMING != NULL) {
      DT = (float)(times(&Tbuf) - T0);
      fprintf (TIMEfp, ":> n_speedes_objects = %d, DT = %f, average DT/n = %f ticks\n",
              n_speedes_objects, DT, DT/n_speedes_objects);
   }
//
//   =====================================
//   Update the sensor coverages and links
//   =====================================
//
   if (!KEEPLINK) {
      //...... set up the link display list
      if (spslinks != NULL) {
         delete spslinks;
         spslinks = NULL;
      }
      sps_links_displist->delete_objects();
   } /* KEEPLINK */

   if (n_links > 0) {
      spslinks = new GR_Spslinks (n_links);
      spslinks->set_id(id);
      spslinks->set_type(stn);
      spslinks->set_trackid(trackobj[trkindex].id);
      sps_links_displist->add_object (spslinks);
   }
	  
   if (Assets[stn].gbiazi < 0.0) {
     //   Calculate Azimuth and Elevation from this sensor [stn] to object
     //   Set sensor parameters
     //   Redraw sensor coverage
   }

   if (TIMING != NULL)
      T0 = times(&Tbuf);

   for (i=0; i<n_links; i++) {                //...... fill graphics link stuff
     id_sensor = stn;

     //sensor_object->get_pos_vel(speedes_time,Xs,Vs);
     //track_object->get_pos_vel(speedes_time,Xt,Vt);track_id = id;

     if (n_links > 0) {
       debug_link_type = 0x0002;
       if (debug_link_type & LMASK) {
         sloc[0] = (float)Assets[id_sensor].gbiy/RE;
         sloc[1] = (float)Assets[id_sensor].gbiz/RE;
         sloc[2] = (float)Assets[id_sensor].gbix/RE;
         tloc[0] = X[1]/RE;
         tloc[1] = X[2]/RE;
         tloc[2] = X[0]/RE;
         rgb[0] = Assets[id_sensor].gbir;
         rgb[1] = Assets[id_sensor].gbig;
         rgb[2] = Assets[id_sensor].gbib;
         spslinks->add_link (i, sloc, tloc, rgb);

	 if (Assets[stn].gbisensor != NULL) {
           site[0]  = trackobj[trkindex].latitude;
           site[1]  = trackobj[trkindex].longitude;
           site[2]  = RE + trackobj[trkindex].altitude;
           sensor[0] = Assets[id_sensor].gbilatit;
           sensor[1] = Assets[id_sensor].gbilongi;
           sensor[2] = RE + Assets[id_sensor].gbialt; 
           //llasec_(site, sensor, &azi, &elev, &dist);
	   elev = atan(Assets[id_sensor].gbiz/sqrt(Assets[id_sensor].gbix*Assets[id_sensor].gbix
					      + Assets[id_sensor].gbiy*Assets[id_sensor].gbiy));
	   elev = elev*180.0/M_PI;
	   azi = M_PI - atan2(Assets[id_sensor].gbiy, Assets[id_sensor].gbix);
	   azi = azi*180.0/M_PI;
	   fprintf(stderr, " From sensor %d at (%f %f) to object at (%f %f): azi=%f el=%f d=%f\n",
		 id_sensor, sensor[0], sensor[1], site[0], site[1], azi, elev, dist);
	   /*
	    Assets[stn].gbielev = elev;
	    Assets[stn].gbiazi  = azi;
	    Assets[stn].gbisensor->set_elev(Assets[stn].gbielev);
	    Assets[stn].gbisensor->set_azi(Assets[stn].gbiazi);
	    Assets[stn].gbisensor->gen_sensor();
	   */
	 }
       }
     }
   }

   if (TIMING != NULL) {
      DT = (float)(times(&Tbuf) - T0);
      fprintf (TIMEfp, ":> n_links = %d, DT = %f, average DT/n = %f ticks\n",
              n_links, DT, DT/n_links);
      T0 = times(&Tbuf);
   }
//
//   ===================
//   Re-render the scene
//   ===================
//
   //fprintf(stderr, "-----> Drawing %d newsps objects...\n", sps_displist->length() );
   gwindow->draw();
   if (FlatMapVisible) mapwindow->draw();

   if (TIMING != NULL) {
      DT = (float)(times(&Tbuf) - T0);
      fprintf (TIMEfp, ":> length of sps_displist = %d, ", sps_displist->length());
      fprintf (TIMEfp, ":> length of sps_links_displist = %d.\n", sps_links_displist->length());
      fprintf (TIMEfp, ":> gwindow->draw time = %f ticks\n", DT);
   }
//
//   =============================================
//   Finish-up this pass and prepare for next pass
//   =============================================
//
skip:
   TrackUpdate(speedes_time, trackobj, Assets);

   sprintf(bigstring, "\n");
   sprintf(littlestr, "%5d\n", n_records);
   strcat (bigstring, littlestr);
   sprintf(littlestr, "%5d\n", n_launchs);
   strcat (bigstring, littlestr);
   sprintf(littlestr, "%5d\n", n_dropped);
   strcat (bigstring, littlestr);
   sprintf(littlestr, "%5d\n", trackobj[trkindex].id);
   strcat (bigstring, littlestr);
   sprintf(littlestr, "%5c\n", chmsgtyp);
   strcat (bigstring, littlestr);
   sprintf(littlestr, "%5s\n", chmsgno);
   strcat (bigstring, littlestr);
   strcat (bigstring, "\0");
   xstr = XmStringCreateLtoR(bigstring, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(textarea, XmNlabelString, xstr, NULL);

   if (TIMING != NULL)
      fprintf (TIMEfp, ":> ending tick count is %d\n\n", times(&Tbuf));

   if (SOCKINPUT) {                           // Send the sender an ACK
      sprintf(outbuf, "%5d", 0);
      //Net_write(outbuf, 5, &client, Lclient);
   }
}


Boolean
proxWP (XtPointer)
{
C_QUEUE         *objects, *links;
C_LINK          *link;
C_SPEEDES_OBJECT *speedes_object, *sensor_object, *track_object;
C_EOM           *eom;
int             sensor_id, track_id, n_speedes_objects, n_links;
int             i, icontype, id, irandom, objtype;
double          X[3], V[3];
double          Xs[3], Vs[3];
double          Xt[3], Vt[3];
double          Vx, Vy, Vz;
double          Px, Py, Pz;
double          Rmax, Rmin;
double          sloc[3], tloc[3];
double          lat, lon;
float           scale_factor, sfactor2, rx, ry;
// float rz;
float           vmag2;
// static GR_Spslinks *spslinks = NULL;
short           rgb[3];
int             errcount = 0;
short           debug_link_type;
extern char     *TIMING;
extern FILE     *TIMEfp;
int             impacted = FALSE;
static int      sps_inited = FALSE;

/*
   if (sps_inited == FALSE) {
      fprintf(stderr, "---> Initializing SPEEDES interface\n");
      speedes_init();
      sps_inited = TRUE;
   }
*/

   while (speedes_gvt < speedes_time) {
      if (speedes_graphics->messages() == 0) break;
      speedes_gvt = speedes_graphics->get_gvt();
   }
   if (speedes_gvt < speedes_time) return FALSE;

   if (TIMING != NULL) {
      T0 = times(&Tbuf);
      fprintf(TIMEfp, ":> starting tick count is %d\n", T0);
   }

   speedes_graphics->process(speedes_time);
   objects = speedes_state->get_objects();
   links = speedes_state->get_links();
   n_speedes_objects = objects->get_length();
   n_links = links->get_length();

  //...... set up the sps_displist for GISP_Obj objects

   if (anobj != NULL) {
     delete[] anobj;
     anobj = NULL;
   }
   if (n_speedes_objects) {
      anobj = new GISP_Obj[n_speedes_objects];
   }
   sps_displist->delete_objects();

   if (TIMING != NULL) {
      DT = (float)(times(&Tbuf) - T0);
      fprintf (TIMEfp, ":> speedes_time = %f\n", speedes_time);
      fprintf (TIMEfp, ":> pre-processing takes %f ticks\n", DT);
      T0 = times(&Tbuf);
   }

  //...... go through each speedes object and fill the display list

  fprintf(stderr, "GISP object dump at time %f with %d objects.\n",
                  speedes_time, n_speedes_objects);

  speedes_object = (C_SPEEDES_OBJECT *)objects->get_top();
  for (i=0; i<n_speedes_objects; i++)
  {
     //eom = speedes_object->get_eom();
     if (speedes_object->get_alive() )
     {
	speedes_object->get_pos_vel(speedes_time,X,V);
	icontype = speedes_object->get_icon();
	if (icontype < 0)
        {
          errcount++; 
	  icontype = 199;  //  make sure it is valid............
        }
        else if (icontype ==   0) icontype = 76; // for debugging: ROCC
        else if (icontype == 105) icontype = 42; // for debugging: purple ball
        else if (icontype ==  37) icontype = 38; // for debugging: new_impact2
 
 	id = speedes_object->get_unique_id();

        if (icontype == 6 || icontype == 18) {	                    // rv or bus
	   fprintf(OBJSfp, "%f %c %s %d %d %d %d %f %f %f %s %d %f %f %f\n",
               speedes_time, 'P', "ES",
               0, 0, id, 10, X[0]*1000.0, X[1]*1000.0, X[2]*1000.0,
               "RV", id-2131, 90.0, 50.0, 30.0);
	}
        if (icontype == 151) {	                    // gbi
	   fprintf(OBJSfp, "%f %c %s %d %d %d %d %f %f %f %s %d %f %f %f\n",
               speedes_time, 'P', "ES",
               0, 0, id, 11, X[0]*1000.0, X[1]*1000.0, X[2]*1000.0,
               "GBI", 10, 90.0, 50.0, 30.0);
	}
        if (icontype == 37 || icontype == 90 || icontype == 91 ||
            icontype == 92 || icontype == 93) {	                    // intercept
	   fprintf(OBJSfp, "%f %c %s %d %d %d %d %f %f %f %s %d %f %f %f\n",
               speedes_time, 'J', "7.0",
               0, 0, id, 11, X[0]*1000.0, X[1]*1000.0, X[2]*1000.0,
               "GBI", 10, 90.0, 50.0, 30.0);
	}

	anobj[i].init(id,icontype);
	sps_displist->add_object(&anobj[i]);
        sfactor2 = speedes_object->get_scale ();
        Rmax = speedes_object->get_Rmax ();
        Rmin = speedes_object->get_Rmin ();
        objtype = anobj[i].get_type();
/*
 *		Do translation
 *		Determine scale factor
 *		Do rotation
 *		Do scaling
 */
        if (sfactor2 < 0.01)
          anobj[i].set_visible_flag (FALSE);
        else					 //...... fill graphics stuff
        {
	  anobj[i].translate ((float)X[1]/RE, (float)X[2]/RE, (float)X[0]/RE);
          conversion.xyz_to_latlon(X, lat, lon);
	  lat = lat*180.0/M_PI;
	  lon = lon*180.0/M_PI;
          //printf("newsps: type %d lat %f lon %f\n", icontype, lat, lon);
          anobj[i].set_xyz ((float)X[0]/RE, (float)X[1]/RE, (float)X[2]/RE);
	  anobj[i].set_llah(lon, lat, 1600.0, 0.0);
	  anobj[i].set_time(speedes_time);

	  scale_factor = get_scale (icontype);
	  // Re-scale selected models
          if (icontype == 24)				                    // ssts
	     scale_factor *= 0.5;
          else if (icontype ==   2 || icontype ==  17)		            // stage 1 or 2
	     scale_factor *= 2;
	  else if ( icontype == 150 || icontype == 151 || icontype == 152 || // gbi
                   icontype ==  68 || icontype == 29  || icontype ==  97)   // AWACS, Mig, carrier
             scale_factor *= 50;
          else if (icontype == 6 || icontype == 18)	                    // rv or bus
             scale_factor *= 40; 
          else if (icontype == 127)			                    // dome
             scale_factor *= Rmax/RE;
	  scale_factor *= sfactor2;

	  anobj[i].rotate_z (180);

	  vmag2 = V[0]*V[0] + V[1]*V[1] + V[2]*V[2];
	  if (vmag2 != 0)
          {
	   if (icontype==199 || icontype==90)
	     printf ("Type: %d has non-zero velocity: %f\n", icontype, vmag2); 
	   Vx = V[1];
	   Vy = V[2];
	   Vz = V[0];
	   rx = -atan2(Vy,sqrt(Vz*Vz+Vx*Vx))*180.0/M_PI;
	   ry = atan2(Vx,Vz)*180.0/M_PI;
           if (icontype==55)  // or any aircraft model.......
           {
             anobj[i].rotate_y (180);  // ??
	   }
           else if (icontype==24)  // ssts or similar (sensor?) model .....
           {
             //anobj[i].rotate_z (-90);
           }

// added roll around the body axis, 7/29/93:
           anobj[i].rotate_x (rx);
	   anobj[i].rotate_y (ry);
 	  } else {
	   Px = X[1];
	   Py = X[2];
	   Pz = X[0];
	   rx = -atan2(Py,sqrt(Pz*Pz+Px*Px))*180.0/M_PI;
	   ry = atan2(Px,Pz)*180.0/M_PI;
	   if (icontype!=127)   // if it is not the ground sensor:
             anobj[i].rotate_x (90);
	   anobj[i].rotate_x (rx);
	   anobj[i].rotate_y (ry);
	  }

	  anobj[i].scale (scale_factor, scale_factor, scale_factor);
	  anobj[i].finish();			// DRE
       }  //...... end of fill graphics stuff (if visible)
     }

     fprintf(stderr, "  %5d %5d  S=%f  R=(%12f, %12f)  X=(%f, %f, %f)  V=(%f, %f, %f)\n",
             id, objtype, scale_factor, Rmin, Rmax, Px, Py, Pz, Vx, Vy, Vz);

     speedes_object = (C_SPEEDES_OBJECT *)speedes_object->get_link();
  }

   if (errcount>0)
      printf ("Warning: %d icontypes not set.\n", errcount);

   if (TIMING != NULL) {
       DT = (float)(times(&Tbuf) - T0); 
       fprintf (TIMEfp, ":> n_speedes_objects = %d, DT = %f, average DT/n = %f ticks\n",
              n_speedes_objects, DT, DT/n_speedes_objects);
   }

   //...... set up the link display list
   if (spslinks != NULL) {
      delete spslinks;
      spslinks = NULL;
   }
   sps_links_displist->delete_objects();
   if (n_links > 0) {
      spslinks = new GR_Spslinks (n_links);
      sps_links_displist->add_object (spslinks);
   }

   if (TIMING != NULL)
       T0 = times(&Tbuf);
  /*
  fprintf(OBJSfp, "GISP link dump at time %f with %d links.\n", speedes_time, n_links);
  */
  link = (C_LINK *)links->get_top();
  for (i=0; i<n_links; i++) {              //...... fill graphics link stuff
     sensor_id = link->get_sensor_id();
     track_id = link->get_track_id();
     sensor_object = speedes_state->get_object(sensor_id);
     track_object  = speedes_state->get_object(track_id);
     if ((sensor_object != NULL)
	 && (track_object != NULL)
	 && sensor_object->get_alive()
	 && track_object->get_alive())
     {
       if (sensor_object->get_icon () == track_object->get_icon ())
	 debug_link_type = 0x0001;
       else
	 debug_link_type = 0x0002;
       if (debug_link_type & LMASK)
       //if (link->get_type () & LMASK)
       {
	 sensor_type = 0x0001;
	 //printf("Processing sensor type %d\n",sensor_object->get_icon()); 
	 if (sensor_object->get_icon() == 44) sensor_type = 0x0010;  // SBIRS Sensor
	 if (sensor_object->get_icon() == 45) sensor_type = 0x0020;  // DSP Sensor
	 if (sensor_object->get_icon() == 199) sensor_type = 0x0040; // GBR Sensor
	 if (sensor_object->get_icon() == 152) sensor_type = 0x0080; // GBI Sensor
	 if (sensor_type & SMASK) {
	   sensor_object->get_pos_vel(speedes_time,Xs,Vs);
	   track_object->get_pos_vel(speedes_time,Xt,Vt);
	   sloc[0] = (float)Xs[1]/RE;
	   sloc[1] = (float)Xs[2]/RE;
	   sloc[2] = (float)Xs[0]/RE;
	   tloc[0] = (float)Xt[1]/RE;
	   tloc[1] = (float)Xt[2]/RE;
	   tloc[2] = (float)Xt[0]/RE;
	   rgb[0] = (short)link->get_red ();
	   rgb[1] = (short)link->get_green ();
	   rgb[2] = (short)link->get_blue ();
	   spslinks->add_link (i, sloc, tloc, rgb);
	 }
       }
     }
     /*
     fprintf(OBJSfp, "  Sid=%5d Tid=%5d  Sicon=%5d  Ticon=%5d  Sx=(%f %f %f)  Tx=(%f %f %f)\n",
             sensor_id, track_id, sensor_object->get_icon(), track_object->get_icon(),
             sloc[0], sloc[1], sloc[2], tloc[0], tloc[1], tloc[2] );
     */
     link = (C_LINK *)link->get_link();
   } //...... end of fill graphics link stuff

   if (TIMING != NULL) {
      DT = (float)(times(&Tbuf) - T0);
      fprintf (TIMEfp, ":> n_links = %d, DT = %f, average DT/n = %f ticks\n",
              n_links, DT, DT/n_links);
   }
  
   if (PLAYBACK) {
      if (speedes_time<(speedes_start_time+speedes_delta_t)
          && speedes_direction<0)
         printf ("Warning: trying to go beyond simulation start time limit.\n");
      else if (speedes_time>(speedes_end_time-speedes_delta_t)
  	   && speedes_direction>0)
        printf ("Warning: trying to go beyond simulation end time limit.\n");
      else
        speedes_time += speedes_direction * speedes_delta_t;
   } else {
      speedes_time = speedes_gvt+1.0;
   }
  
skipall:
   XmScaleSetValue (sps_progress_slider, (int)(speedes_time*10));

   if (TIMING != NULL)
      T0 = times(&Tbuf);

   gwindow->draw();

   if (TIMING != NULL) {
      DT = (float)(times(&Tbuf) - T0);
      fprintf (TIMEfp, ":> length of sps_displist = %d, ", sps_displist->length());
      fprintf (TIMEfp, ":> length of sps_links_displist = %d.\n", sps_links_displist->length());
      fprintf (TIMEfp, ":> gwindow->draw time = %f ticks\n", DT); 
   }

   if (TIMING != NULL)
     fprintf (TIMEfp, ":> ending tick count is %d\n\n", times(&Tbuf));
}

void 
reprocess_links ()
{
  C_QUEUE *links;
  C_LINK *link;
  C_SPEEDES_OBJECT *sensor_object, *track_object;
  int sensor_id, track_id, n_links;
  int i;
  double Xs[3], Vs[3];
  double Xt[3], Vt[3];
  double sloc[3], tloc[3];
  short rgb[3];
  short debug_link_type;

  //  printf ("LMASK is %x\n", LMASK); // debugging....

  links = speedes_state->get_links();
  n_links = links->get_length();

  if (spslinks != NULL)
  {
     delete spslinks;
     spslinks = NULL;
  }
  sps_links_displist->delete_objects();
  if (n_links > 0)
  {
     spslinks = new GR_Spslinks (n_links);
     sps_links_displist->add_object (spslinks);
     spslinks->reset();
     spslinks->finish();			// DRE
  }

  link = (C_LINK *)links->get_top();
  for (i=0; i<n_links; i++)
  {
     sensor_id = link->get_sensor_id();
     track_id = link->get_track_id();
     sensor_object = speedes_state->get_object(sensor_id);
     track_object = speedes_state->get_object(track_id);
     if ((sensor_object != NULL) 
	 && (track_object != NULL)
	 && sensor_object->get_alive() 
	 && track_object->get_alive())
     { 
       if (sensor_object->get_icon () == track_object->get_icon ())
	 debug_link_type = 0x0001;
       else
	 debug_link_type = 0x0002;
       if (debug_link_type & LMASK)
       //if (link->get_type () & LMASK)
       {
	 sensor_type = 0x0001;
	 if (sensor_object->get_icon() == 44) sensor_type = 0x0010;  // SBIRS Sensor
	 if (sensor_object->get_icon() == 45) sensor_type = 0x0020;  // DSP Sensor
	 if (sensor_object->get_icon() == 152) sensor_type = 0x0040; // GBR Sensor
	 if (sensor_object->get_icon() == 151) sensor_type = 0x0080; // GBI Sensor
	 if (sensor_type & SMASK) {
	   sensor_object->get_pos_vel(speedes_time,Xs,Vs);
	   track_object->get_pos_vel(speedes_time,Xt,Vt);
	   sloc[0] = (float)Xs[1]/RE;
	   sloc[1] = (float)Xs[2]/RE;
	   sloc[2] = (float)Xs[0]/RE;
	   tloc[0] = (float)Xt[1]/RE;
	   tloc[1] = (float)Xt[2]/RE;
	   tloc[2] = (float)Xt[0]/RE;
	   rgb[0] = (short)link->get_red ();
	   rgb[1] = (short)link->get_green ();
	   rgb[2] = (short)link->get_blue ();
	   spslinks->add_link (i, sloc, tloc, rgb);
	 }
       }
     }
     link = (C_LINK *)link->get_link();
  }
  gwindow->draw();

}

void
sps_linksCB (Widget, XtPointer client_data, XtPointer)
{
  int item_no = (int) client_data;

  switch (item_no)
    {
    case 0:
      printf ("Turning all links on.\n");
      LMASK = 0xffff;
      break;
    case 1:
      printf ("Turning all links off.\n");
      LMASK = 0x0000;
      break;
    case 2:
      printf ("Turning COM links on.\n");
      LMASK = LMASK | 0x0001;
      break;
    case 3:
      printf ("Turning COM links off.\n");
      LMASK = LMASK & 0xfffe;
      break;
    case 4:
      printf ("Turning SEN links on.\n");
      LMASK = LMASK | 0x0002;
      SMASK = 0xffff;
      break;
    case 5:
      printf ("Turning SEN links off.\n");
      LMASK = LMASK & 0xfffd;
      SMASK = 0x0000;
      break;
    case 6:
      printf ("Turning DSP links on.\n");
      SMASK = SMASK | 0x0020;
      break;
    case 7:
      printf ("Turning DSP links off.\n");
      SMASK = SMASK & 0xffdf;
      break;
    case 8:
      printf ("Turning SBIRS links on.\n");
      SMASK = SMASK | 0x0010;
      break;
    case 9:
      printf ("Turning SBIRS links off.\n");
      SMASK = SMASK & 0xffef;
      break;
    case 10:
      printf ("Turning GBR links on.\n");
      SMASK = SMASK | 0x0040;
      break;
    case 11:
      printf ("Turning GBR links off.\n");
      SMASK = SMASK & 0xffbf;
      break;
    case 12:
      printf ("Turning GBI links on.\n");
      SMASK = SMASK | 0x0080;
      break;
    case 13:
      printf ("Turning GBI links off.\n");
      SMASK = SMASK & 0xff7f;
      break;
    default:
      break;
    }

  if (!FILEINPUT && !SOCKINPUT) reprocess_links ();
}


void
sps_com_linksCB (Widget, XtPointer client_data, XtPointer)
{
  int item_no = (int) client_data;

  switch (item_no)
    {
    case 0:
      printf ("Turning COM links on.\n");
      LMASK = LMASK | 0x0001;
      break;
    case 1:
      printf ("Turning COM links off.\n");
      LMASK = LMASK & 0xfffe;
      break;
    default:
      break;
    }
  reprocess_links ();
}

void
sps_sen_linksCB (Widget, XtPointer client_data, XtPointer)
{
  int item_no = (int) client_data;

  switch (item_no)
    {
    case 0:
      printf ("Turning SEN links on.\n");
      LMASK = LMASK | 0x0002;
      break;
    case 1:
      printf ("Turning SEN links off.\n");
      LMASK = LMASK & 0xfffd;
      break;
    default:
      break;
    }
  reprocess_links ();
}
