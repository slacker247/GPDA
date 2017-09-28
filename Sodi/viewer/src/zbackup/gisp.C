/***********************************************************************
New things for Version 0.7a:

         1) File editor for 'scheme.dat'
	 2) Data input from socket connection (optional)
	 3) Variable display area size
	 4) Multiple defended areas per GBI farm
	 5) Automatic processing of assets at startup (optional)
	 6) Persistent Save/Recall of views using QuickView buttons
	 7) File Selection dialog box if 'input_file' = '*'
	 8) Sensor file (sensors.dat) replaced by 'assets.par'
	 9) Display of R2 asset from Track Viewer
	10) Selection and view of asset details from Asset Viewer
 
........................................................................

Things to do:

	 1) Text display on screen
	 2) Picking
 	 3) Weather, clouds, blending
	 4) Curves, arcs, cones, etc
	 5) Demo menu item                                DRE - Removed
	 6) Sensor coverage patterns			  DRE - 10/13/99
	 7) Flat maps
	 8) GIF image display
	 9) Link control                                  DRE - 08/28/98
	10) Socket connections                            DRE - 12/07/99
	11) JTMDP Requirements Scrubber display
	12) -----  Battle Planner  -----
	13) Simulated Commander testbed
	14) Implement gisp.par for setable parameters     DRE - 08/28/98
        15) Armys, Navys, etc.                            DRE - 03/10/99
	16) Air Objects with trails
	17) Stroke text
	18) Ability to specify Air starting positions	  DRE - 03/08/99
	19) Interceptor flyouts and tracking              DRE - 03/11/99
	20) Realtime instrumentation			  DRE - 03/08/99
	21) Fix Startup textured earth lighting		  DRE - 03/08/99
	22) Use Polygon Offset for grids & boundaries
	23) Put clouds on transparent overlay globe
	24) Implement SAVE, RECALL, and RESET
	25) Implement file load from menu
	26) Implement Movie making capabilities
	27) Provide tracks.dat editor                     DRE - 12/07/99
	28) Allow multiple defended areas per GBI farm    DRE - 11/30/99
        29) Misc. cleanup
	      1) Allow FOV < 1.0
	      2) Include defended areas in GBI status window
	      3) Have NEWSPS use gbi.par instead of sensors.dat
	      4) Implement picking of track objects, popup track viewer
	      5) Fix user view from sensor
	      6) Load texture image only once for jtamv and ViewFinder
	      7) Use MBV boundaries & gridlines

************************************************************************/ 

#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <osfcn.h>
#include <sys/types.h>
//#include <malloc.h>

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
#include <Xm/TextF.h>
#include <Xm/DrawingA.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <GL/GLwDrawA.h>

#include "DataParser.H"			// 'speedes.par' file parser
#include "GR_DispList.H"
#include "GR_Window.H"
#include "GR_DispObj.H"
#include "GR_Shell.H"
#include "GR_Model.H"
#include "GR_Tearth.H"
#include "GR_Tearth2.H"
#include "GR_Tearth3.H"
#include "GR_Lines.H"
#include "GR_Area.H"
#include "GR_Army.H"
#include "GR_Bearth.H"
#include "GR_Carib.H"
#include "GR_Cloud.H"
#include "GR_Stars.H"
#include "GR_work.H"
#include "GISP_Globals.H"
#include "parsman.H"

#define RE           6378.145
#define descfile     "Models.desc"
#define YES          1
#define NO           0

FILE                 *INFOfp;
FILE                 *TIMEfp;
FILE                 *DBUGfp;
FILE                 *STATfp;
FILE                 *VIEWfp;
char                 *DEBUG;
char                 *SPEEDES;
char                 *TIMING;
char                 *STATS;
char                 *CLASS;
int                  PLAYBACK, AUTORUN;
double               TIMESTEP = 0.0;
int                  REGION;
Boolean              TEXTURED, GRIDLINE, BOUNDARY, SENSDOME;
Boolean              SHOWTRACK, SHOWTRAIL;
Boolean              ASSETS;
Boolean              RECORDING;
int                  PROX, FILEINPUT, SOCKINPUT;
char                 *INPUTSRC;
int                  YESNO;

int                  process_id;
int                  frame_no;
int                  image_no;
int                  run_no;

char                 shorefile[80];
char                 polifile[80];
char                 texturefile[80];
char                 rgbfile[80];
char                 starfile[80];
char                 elevdir[80];
char                 caribdir[80];
char                 cloudfile[80];
char                 earthdir[80];
char                 *texfile;

GR_DispList	     *displist;
GR_DispList          *sps_displist;
GR_DispList          *sps_links_displist;
GR_DispList          *trail_displist;
GR_DispList          *sensor_displist;
GR_DispList          *cloud_displist;
GR_Cloud             *cloud;

GR_Model             *models, *Searth33, *Adiz222;
GR_Window            *gwindow;

GR_Blueearth         *Bearth200;
GR_Tearth  	     *Tearth300;
GR_Tearth2  	     *Tearth301;
GR_Tearth3  	     *Tearth302;

GR_Lines             *ShoreLines320, *PoliLines321;
GR_Gridlines         *Grid330;
GR_Sensor            *dome_sensor, *cone_sensor, *GBRs[16];
GR_Stars	     *Star800;
GR_Army              *ARMYs[16];

long                 current_earth_type = 0;
long		     star_type = 800;

Boolean              first_Area[36][18];
GR_Area              *Area[36][18];

Carib                *carib;

int                  using_speedes = TRUE;

int                  v_LAT     = 0;
int                  v_LON     = 0;
int                  v_ALT     = 2500;
int                  v_FOV     = 132;
int                  v_FOVDIV  = 1;
int                  v_AZI     = 0;
int                  v_LOOKLAT = 0;
int                  v_LOOKLON = 0;
int                  v_LOOKALT = 0;
int                  v_LOOKAZI = 0;
int                  VMODE     = 0; // 0: normal; 1: tangential;
static int           MODE_1=0,LAT_1=0,LON_1=0,ALT_1=6700,FOV_1=62,AZI_1=0;
static int           MODE_2=0,LAT_2=0,LON_2=0,ALT_2=6700,FOV_2=62,AZI_2=0;
int                  saved_vu = FALSE;
//
//   Motif stuff
//
Widget               execstat, modestat;
Widget               scaleLAT, scaleLON, scaleALT, scaleFOV, scaleAZI;
Widget               textarea;
Pixel                fg_red, fg_yellow, bg_grey, fg_green, fg_blue;
Pixmap               redledpix, grnledpix, yelledpix, bluledpix;
static String        fallback_resources [] = {
   "*background: lightsteelblue",
   "*XmScale*foreground: black",
   "*XmScale*resizable: TRUE",
   "*XmScale*XmNhighlightOnEnter: True",
   "*.a.minimum: -90",
   "*.a.maximum: 90",
   "*.a.value: 0",
   "*.o.minimum: -180",
   "*.o.maximum: 180",
   "*.o.value: 0",
   "*.l.minimum: 0",
   "*.l.maximum: 60000",
   "*.l.value: 6700",
   "*.v.minimum: 1",
   "*.v.maximum: 180",
   "*.v.value: 90",
   "*XmMessageBox*foreground: white",
   "*XmMessageBox*background: steelblue",
   "*Logo.foreground: steelblue4",
   NULL };
//
//   Network socket stuff
//
int                  portid;
char                 *hostid;

/* ---------------------------------------------------------------------- */

Widget makeMenuBar (char*, Widget);
Widget makeViewBar (char*, Widget);
Widget makePanel (char*, Widget, Widget);
extern Widget makeSpanel (char*, Widget, Widget); // SPEEDES panel
Widget makeList (char*, Widget);
Widget createOneScale (Widget, char, XmString, XtCallbackProc);
int    GetYesNo(char *str);
void   toggleCB(Widget toggle_box, XtPointer n, XmToggleButtonCallbackStruct* cbs);
void   responseCB(Widget w, int *answer, XmAnyCallbackStruct *cbs);
/*
extern void f90_init(int argc, char *argv[]);
extern void f90_finish(int status);
*/
extern void view_finderCB ();
extern GR_Window *vfwindow;
extern void model_viewerCB (Widget);
extern void asset_init ();
extern void asset_viewerCB (Widget);
extern void time_viewerCB (Widget);
extern void map_viewerCB (Widget);
extern void track_editCB (Widget);
extern void track_viewCB (Widget);
extern GR_Window *mvwindow;
extern GR_Window *tmwindow;

extern void speedes_init ();
extern void input_init();
extern void input_finish ();
extern void TrackInit(Widget);
void logoCB (Widget, XtPointer, XtPointer);
void focusCB (Widget, XtPointer, XtPointer);
void earthCB (Widget, XtPointer, XtPointer);
void lightingCB (Widget, XtPointer, XtPointer);
void backgroundCB (Widget, XtPointer, XtPointer);
void weatherCB (Widget, XtPointer, XtPointer);
void optionCB (Widget, XtPointer, XtPointer);
extern void cyclesCB (Widget menuitem, XtPointer itemno, XmAnyCallbackStruct* call_data);
void helpCB (Widget, XtPointer, XtPointer);
void help_doneCB (Widget dialog, XtPointer, XtPointer);

void earthtypeCB  (Widget, XtPointer, XtPointer);
void earthareaCB  (Widget, XtPointer, XtPointer);
void earthlinesCB (Widget, XtPointer, XtPointer);
void earthadizCB  (Widget, XtPointer, XtPointer);
void earthgridCB  (Widget, XtPointer, XtPointer);
void fixedsensorCB(Widget, XtPointer, XtPointer);
void unitsCB      (Widget, XtPointer, XtPointer);
void cloudCB ();

void forceCB (Widget, XtPointer client_data, XtPointer);
void viewCB (Widget, XtPointer client_data, XtPointer);
void memoryCB (Widget, XtPointer client_data, XtPointer);
void flipCB (Widget, XtPointer client_data, XtPointer);
void changeScaleCB (Widget, char, XmScaleCallbackStruct*);

void setvparams (GR_Window* win, int vmode,
		 int lat, int lon, int alt, int fov, int azi);

//extern void spsCB ();
extern XtWorkProcId spsId;
extern Boolean spsWP (XtPointer);

extern void demoCB ();
extern XtWorkProcId demoId;
extern Boolean demoWP (XtPointer);
extern void sps_fplayCB ();
extern void sps_linksCB (Widget, XtPointer, XtPointer);
extern void sps_com_linksCB (Widget, XtPointer, XtPointer);
extern void sps_sen_linksCB (Widget, XtPointer, XtPointer);

extern void makeRasterFont(void);

/* ---------------------------------------------------------------------- */

void setvparams (GR_Window* win, int vmode,
		 int lat, int lon, int alt, int fov, int azi)		 
{
float vx, vy, vz;
float lx, ly, lz;
float p_lat, p_lon, p_alt, p_azi;
float l_lat, l_lon, l_alt, l_azi;
float Kt = 2.50; // tangential view constant;
   
   p_lat = lat*M_PI/180.0;
   p_lon = lon*M_PI/180.0;
   p_alt = 1 + alt/RE;
   p_azi = azi*M_PI/180.0;

   l_lat = v_LOOKLAT*M_PI/180.0;
   l_lon = v_LOOKLON*M_PI/180.0;
   l_alt = 1.0 + v_LOOKALT/RE;
   l_azi = v_LOOKAZI*M_PI/180.0;
   
   win->twist (0);
   if (vmode == 0) // normal view:
   {
      vx = cos (p_lat) * sin (p_lon) * p_alt;
      vy = sin (p_lat) * p_alt;
      vz = cos (p_lat) * cos (p_lon) * p_alt;
      lx = 0.0;
      ly = 0.0;
      lz = 0.0;
   }
   else // tangential view:
   {
      p_alt = 2 - exp (-alt/RE);

      // for now fix the alt:

      vx = -Kt * sin (p_azi) * cos (p_lon)
	   + Kt * cos (p_azi) * sin (p_lat) * sin (p_lon)
	   + cos (p_lat) * sin (p_lon);
      vy = -Kt * cos(p_azi) * cos(p_lat)
	   + sin (p_lat);
      vz = Kt * sin (p_azi) * sin (p_lon)
	   + Kt * cos (p_azi) * sin (p_lat) * cos (p_lon)
	   + cos (p_lat) * cos (p_lon);
      
      lx = cos (p_lat) * sin (p_lon);
      ly = sin (p_lat);
      lz = cos (p_lat) * cos (p_lon);
      if (p_lat < 0)
	win->twist (180);      
   }
   win->field_of_view ((float)fov/(float)v_FOVDIV);
   win->view_position(vx, vy, vz);
   win->look_vector(lx, ly, lz);
}

/*----------------------------- MAIN -------------------------------------*/

void GR_initialize (int argc, char *argv[]);

void
main (int argc, char *argv[])
{
time_t   clock;
struct tm *ltime;
char * str;

   printf("\n\n");
   printf("  -- Joint Theater Air and Missile Visualizer (JTAMV) --\n");
   printf("                   (Version 0.7a)");
   printf("\n\n");

#ifdef Linux
   f90_init(argc, argv);
#endif

   process_id = getpid();
   frame_no   = 1;
   image_no   = 1;
   run_no     = 1;

   FILEINPUT = FALSE;
   SOCKINPUT = FALSE;

   DATA_PARSER gispparser("jtamv.par");
   gispparser.GoTo("parameters", NULL);
   CLASS     = gispparser.GetString("class");
   TIMESTEP  = (double)gispparser.GetFloat("step_time");
   AUTORUN   = gispparser.GetLogical("autorun");
   REGION    = gispparser.GetInt("region");
   TEXTURED  = gispparser.GetLogical("textured");
   GRIDLINE  = gispparser.GetLogical("gridline");
   BOUNDARY  = gispparser.GetLogical("boundary");
   SENSDOME  = gispparser.GetLogical("coverage");
   ASSETS    = gispparser.GetLogical("assets");
   SHOWTRACK = gispparser.GetLogical("tracks");
   SHOWTRAIL = gispparser.GetLogical("trails");
   texfile   = gispparser.GetString("tex_file");
   INPUTSRC  = gispparser.GetString("input_source");
   if (strcmp(INPUTSRC, "file") == 0) FILEINPUT = TRUE;
   if (strcmp(INPUTSRC, "socket") == 0) SOCKINPUT = TRUE;
   if (SOCKINPUT) {
      portid    = gispparser.GetInt("portid");
      hostid    = gispparser.GetString("hostid");
   }

   if ((DEBUG = getenv("JTAMVDEBUG")) != NULL)
      DBUGfp = fopen("jtamvdbug.file", "w+");
   if ((TIMING = getenv("JTAMVTIMING")) != NULL)
      TIMEfp = fopen("jtamvtime.file", "w+");
   if ((STATS = getenv("JTAMVSTATS")) != NULL)
      STATfp = fopen("jtamvstat.file", "w+");
   INFOfp = fopen("jtamvinfo.file", "w+");

   DATA_PARSER grafparser("graphics.par");
   grafparser.GoTo("parameters", NULL);
   PLAYBACK = (int)grafparser.GetLogical("input_file");

   time(&clock);
   ltime = localtime(&clock);
   str = asctime(ltime);
   fprintf(INFOfp, "JTAMV Information output file for %s\n", str);
   fprintf(INFOfp, "The ID of this process is %d\n", process_id);
   fprintf(INFOfp, "Timing is %s\n", (TIMING==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Debugging is %s\n", (DEBUG==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Statistics is %s\n", (STATS==NULL ? "disabled" : "enabled"));
   fprintf(INFOfp, "Run Mode is %s\n", (PLAYBACK ? "playback" : "realtime"));
   fprintf(INFOfp, "\n");

   GR_fallback_resources = fallback_resources; 
   GR_startup (argc, argv);
   GR_initialize (argc, argv);

   XtAppMainLoop (GR_appcontext);

#ifdef Linux
   f90_finish(0);
#endif
}

/* --------------------------------------------------------------------- */

void
GR_initialize (int argc, char *argv[])
{
   Display	*dpy;
   GR_Shell	*shell;
   Widget 	form, menubar, panel, frame, glframe;
   Widget       viewbar, msgtitle, botsep, vertsep;
   Widget       spanel, tpanel, drawing;
   Widget	eventtitle, cyclemenu;
   XmString     title0, title1, title2, title3, title4, title, xstr;
   XFontStruct  *font;
   XmFontList   fontlist;
   XColor       color, unused;
   Pixel        fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
   Colormap     cmap;
   char		*DATADIR;
   char         *BITMAPDIR;
   char		*FWDIR;
   char         littlestr[40];
   char         bigstring[160];
   char         filename[120];
   
   if ((DATADIR=getenv("DATADIR")) == NULL)
	DATADIR = "./RSD_Data";
   sprintf (caribdir, "%s", DATADIR);
   sprintf (earthdir, "%s", DATADIR);
   sprintf (elevdir,  "%s", DATADIR);
   sprintf (shorefile, "%s%s", DATADIR, "/World.asc");
   sprintf (polifile, "%s%s", DATADIR, "/WorldPoli.asc");
   //sprintf (texturefile, "%s%s", DATADIR, "/jpl_earth.rgb");
   //sprintf (texturefile, "%s%s", DATADIR, "/earth-hires.rgb");
   sprintf (texturefile, "%s/%s", DATADIR, texfile);
   sprintf (starfile, "%s%s", DATADIR, "/stars.dat");
   sprintf (rgbfile, "%s%s", DATADIR, "/4320earth.rgb");
   sprintf (cloudfile, "%s%s", DATADIR, "/clouds.bw");

   /*
   for (i=0; i<36; i++)
   {
      for (j=0; j<18; j++)
      {
         first_Area[i][j] = TRUE;
      }
   }
   */

//	Set up the program's widget instance hierarchy. A Motif OpenGL
//	drawing area widget is nested in a frame widget that is nested
//	in the applications's top-level widget. In addition, several
//	menu and control widgets are nested in the frame widget.
//
//				GR_toplevel
//				    |
//				  frame
//				    |
//				   form
//             |----------|---------+----------|---------|
//	    menubar    viewbar     Panel     Spanel     Gframe
//							  |
//						       Gwindow
//						       (OpenGL)
//

   GR_toplevel = XtOpenApplication(&GR_appcontext, "Jtamv", NULL, 0, &argc, argv,
                 GR_fallback_resources, applicationShellWidgetClass, NULL, 0);
   dpy = XtDisplay(GR_toplevel);

   frame = XmCreateFrame(GR_toplevel, "frame", NULL, 0);
   XtManageChild(frame);

//   shell = new GR_Shell;
//   shell->createWidget ("XRSD"); // Note, name is still argv[0];
//   form = XmCreateForm (shell->widget(), "Form", NULL, 0);

   form = XmCreateForm(frame, "Form", NULL, 0);
   XtVaSetValues (form,
		  XmNshadowThickness,  0,
		  NULL);
   XtManageChild (form);

   XtVaGetValues(frame, XmNcolormap, &cmap, NULL);
   XAllocNamedColor(XtDisplay(frame), cmap, "red",    &color, &unused);
   fg_red = color.pixel;
   XAllocNamedColor(XtDisplay(frame), cmap, "blue",   &color, &unused);
   fg_blue = color.pixel;
   XAllocNamedColor(XtDisplay(frame), cmap, "yellow", &color, &unused);
   fg_yellow = color.pixel;
   XAllocNamedColor(XtDisplay(frame), cmap, "green",  &color, &unused);
   fg_green = color.pixel;
   XAllocNamedColor(XtDisplay(frame), cmap, "grey",   &color, &unused);
   bg_grey = color.pixel;
   XmGetColors(XtScreen(frame), cmap, fg_red, &fg_ret, &top_shadow,
               &bottom_shadow, &select_color);

   if ((BITMAPDIR=getenv("BITMAPDIR")) == NULL) {
        BITMAPDIR = "./BitMaps";
   }
   Boolean pixerror = False;
   sprintf (filename, "%s%s", BITMAPDIR, "/led-yellow.xpm");
   yelledpix = XmGetPixmap(XtScreen(GR_toplevel), filename,
                   BlackPixelOfScreen(XtScreen(GR_toplevel)),
                   WhitePixelOfScreen(XtScreen(GR_toplevel)));
   if (yelledpix == XmUNSPECIFIED_PIXMAP) {
       printf("Can't create Yellow LED pixmap\n"); pixerror = True;
   }
   sprintf (filename, "%s%s", BITMAPDIR, "/led-green.xpm");
   grnledpix = XmGetPixmap(XtScreen(GR_toplevel), filename,
                   BlackPixelOfScreen(XtScreen(GR_toplevel)),
                   WhitePixelOfScreen(XtScreen(GR_toplevel)));
   if (grnledpix == XmUNSPECIFIED_PIXMAP) {
       printf("Can't create Green LED pixmap\n"); pixerror = True;
   }
   sprintf (filename, "%s%s", BITMAPDIR, "/led-red.xpm");
   redledpix = XmGetPixmap(XtScreen(GR_toplevel), filename,
                   BlackPixelOfScreen(XtScreen(GR_toplevel)),
                   WhitePixelOfScreen(XtScreen(GR_toplevel)));
   if (redledpix == XmUNSPECIFIED_PIXMAP) {
       printf("Can't create Red LED pixmap\n"); pixerror = True;
   }
   sprintf (filename, "%s%s", BITMAPDIR, "/led-blue.xpm");
   bluledpix = XmGetPixmap(XtScreen(GR_toplevel), filename,
                   BlackPixelOfScreen(XtScreen(GR_toplevel)),
                   WhitePixelOfScreen(XtScreen(GR_toplevel)));
   if (bluledpix == XmUNSPECIFIED_PIXMAP) {
       printf("Can't create Blue LED pixmap\n"); pixerror = True;
   }

   font = XLoadQueryFont(dpy,
          "-adobe-courier-bold-*-*-*-20-*-*-*-*-*-*-*");
   fontlist = XmFontListCreate(font, "charset1");

   menubar = makeMenuBar("Menubar", form);
   XtVaSetValues (menubar,
		  XmNtopAttachment,    XmATTACH_FORM,
		  XmNleftAttachment,   XmATTACH_FORM,
		  XmNtopOffset,        2,
		  XmNleftOffset,       5,
		  NULL);

   viewbar = makeViewBar("Viewbar", form);
   XtVaSetValues (viewbar,
		  XmNtopAttachment,    XmATTACH_FORM,
		  XmNrightAttachment,  XmATTACH_FORM,
		  XmNtopOffset,        2,
		  XmNrightOffset,      10,
		  NULL);
//
//      Add speedes interface buttons...
//
   spanel = makeSpanel("Spanel", form, viewbar);

   botsep = XtVaCreateManagedWidget("BotSep", xmSeparatorWidgetClass, form,
                  XmNheight,           20,
                  XmNorientation,      XmHORIZONTAL,
                  XmNseparatorType,    XmSHADOW_ETCHED_IN,
                  XmNleftAttachment,   XmATTACH_FORM,
                  XmNrightAttachment,  XmATTACH_FORM,
                  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget,     spanel,
                  NULL);
//
//      Add view control buttons...
//
   panel = makePanel("Panel", form, botsep);

   textarea = XtVaCreateManagedWidget("MainText", xmLabelWidgetClass, form,
                 XmNwidth,            200,
                 XmNheight,           200,
                 XmNmarginHeight,     2,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
		 XmNshadowThickness,  3,
	       //XmNborderWidth       1,
                 XmNlabelType,        XmSTRING,
                 XmNrightAttachment,  XmATTACH_FORM,
		 XmNrightOffset,      5,
                 XmNbottomAttachment, XmATTACH_WIDGET,
		 XmNbottomWidget,     botsep,
                 NULL);
   sprintf(bigstring, "\n");
   sprintf(littlestr, " Tracks Processed  %5d\n", 0);
   strcat (bigstring, littlestr);
   sprintf(littlestr, " Tracks Dropped    %5d\n\n", 0);
   strcat (bigstring, littlestr);
   sprintf(littlestr, " Records Processed %5d\n", 0);
   strcat (bigstring, littlestr);
   strcat (bigstring, "\0");
   xstr = XmStringCreateLtoR(bigstring, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(textarea, XmNlabelString, xstr, NULL);

   modestat = XtVaCreateManagedWidget("MovLed", xmDrawingAreaWidgetClass, form,
                 XmNwidth,            16,
                 XmNheight,           16,
                 XmNmarginHeight,     2,
                 XmNspacing,          2,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     textarea,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   XtVaSetValues(modestat, XmNbackgroundPixmap, redledpix, NULL);
   XmString recstr = XmStringCreateLtoR("Rec. Status", "charset1");
   Widget movlabel = XtVaCreateManagedWidget("Header", xmLabelWidgetClass, form,
                 XmNwidth,            100,
                 XmNheight,           16,
                 XmNmarginHeight,     2,
                 XmNmarginWidth,      2,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      recstr,
                 XmNforeground,       fg_red,
                 XmNrightAttachment,  XmATTACH_WIDGET,
                 XmNrightWidget,      modestat,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     textarea,
                 NULL);
   XmStringFree(recstr);

   execstat = XtVaCreateManagedWidget("RunLed", xmDrawingAreaWidgetClass, form,
                 XmNwidth,            16,
                 XmNheight,           16,
                 XmNmarginHeight,     2,
                 XmNspacing,          2,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     textarea,
                 XmNrightAttachment,  XmATTACH_WIDGET,
                 XmNrightWidget,      movlabel,
                 NULL);
   if (SOCKINPUT)
      XtVaSetValues(execstat, XmNbackgroundPixmap, bluledpix, NULL);
   else
      XtVaSetValues(execstat, XmNbackgroundPixmap, redledpix, NULL);
   recstr = XmStringCreateLtoR("Run Status", "charset1");
   Widget runlabel = XtVaCreateManagedWidget("Header", xmLabelWidgetClass, form,
                 XmNwidth,            100,
                 XmNheight,           16,
                 XmNmarginHeight,     2,
                 XmNmarginWidth,      2,
                 XmNalignment,        XmALIGNMENT_END,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      recstr,
                 XmNforeground,       fg_red,
                 XmNrightAttachment,  XmATTACH_WIDGET,
                 XmNrightWidget,      execstat,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     textarea,
                 NULL);
   XmStringFree(recstr);

   glframe = XtVaCreateManagedWidget ("Gframe", xmFrameWidgetClass, form,
		  XmNwidth,            480,
		//XmNheight,           380,
                  XmNshadowType,       XmSHADOW_ETCHED_IN,
                  XmNshadowThickness,  3,
		  XmNtopAttachment,    XmATTACH_WIDGET,
		  XmNtopWidget,        menubar,
		  XmNtopOffset,        2,
                  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget,     panel,
		  XmNleftAttachment,   XmATTACH_FORM,
		  XmNleftOffset,       2,
		  XmNrightAttachment,  XmATTACH_FORM,
		  XmNrightOffset,      2,
		  NULL);
//
//	Check for an OpenGL capable visual, open it, and link OpenGL
//	to it.
//
   gwindow = new GR_Window();
   gwindow->doublebuffer ();
   gwindow->rgbmode ();
   gwindow->settop(GR_toplevel, GR_appcontext, glframe);

   gwindow->GR_Widget::createWidget("Gwindow", glframe);

   gwindow->set_viewmode (GR_PERSPECTIVE);
   gwindow->aspect(1);
   gwindow->near(0.1);
   gwindow->far(100.0);
//
//   Build all the object display lists
//
   displist = new GR_DispList;
   gwindow->addDispList (displist);
   sps_displist = new GR_DispList;
   gwindow->addDispList (sps_displist, "sps_displist");
   sps_links_displist = new GR_DispList;
   gwindow->addDispList (sps_links_displist, "sps_links_displist");
   trail_displist = new GR_DispList;
   gwindow->addDispList (trail_displist, "trail_displist");
   sensor_displist = new GR_DispList;
   gwindow->addDispList (sensor_displist, "sensor_displist");
//
//   shell->realize ();
//
   VMODE = 0;
   v_LAT = 0;
   v_LON = 0;
   v_ALT = 6700;
   v_FOV = 62;
   v_AZI = 0;
   setvparams (gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
   XmScaleSetValue (scaleLAT, v_LAT);
   XmScaleSetValue (scaleLON, v_LON);
   XmScaleSetValue (scaleALT, v_ALT);
   XmScaleSetValue (scaleFOV, v_FOV);
   XmScaleSetValue (scaleAZI, v_AZI);

   if ( (VIEWfp = fopen("viewsave.dat", "r+")) != NULL) {
      fscanf(VIEWfp, "%d %d %d %d %d %d\n",
             &MODE_1, &LAT_1, &LON_1, &ALT_1, &FOV_1, &AZI_1);
      fscanf(VIEWfp, "%d %d %d %d %d %d\n",
             &MODE_2, &LAT_2, &LON_2, &ALT_2, &FOV_2, &AZI_2);
      fclose(VIEWfp);
   }

   models = new GR_Model (descfile);

   Searth33 = new GR_Model (0, 33);
   if (Searth33)
   {
     displist->add_object (*Searth33);
     current_earth_type = 33;
   }

   focusCB (NULL, (XtPointer)REGION, NULL);	// Start with whatever is in .par file
   earthtypeCB(NULL, (XtPointer)4, NULL);	//   and Blue Earth
   if (BOUNDARY)
     earthlinesCB(NULL, (XtPointer)1, NULL);	// Shorelines & boundaries
   if (GRIDLINE)
     earthgridCB(NULL, (XtPointer)1, NULL);	// Grid lines
   lightingCB(NULL, (XtPointer)1, NULL);	// Moon light
   if (TEXTURED) 
       earthtypeCB(NULL, (XtPointer)0, NULL);	// If textured earth wanted
   if(ASSETS)
       asset_init();                           // If asset display wanted
   if (SENSDOME)
     fixedsensorCB(NULL, (XtPointer)1, NULL);   // Force the Fixed sensors to be
                                                //   displayed last

   if (FILEINPUT || SOCKINPUT) {
      PROX = FALSE;
      input_init();
      PLAYBACK = FALSE;
   } else {
      PROX = TRUE;
      speedes_init ();				// Initialize SPEEDES interface
   }

   TrackInit(GR_toplevel);
   makeRasterFont();
   //GR_LcdMakeFont(LcdYELLOW, "./RSD_Data");

   if (AUTORUN) sps_fplayCB();		        // If PLAYBACK, start the display

   printf("   ----- JTAMV Initialization Complete -----\n"); 
}
/*                                                                             */
/* ---------------------------- major widget components ---------------------- */
/*                                                                             */
Widget
makeMenuBar (char* name, Widget parent)
{
   Widget v_menubar;
   Widget menupane;
   Widget submenu;
   Widget dummywidget;
   XmString on, off;
   XmString logo, finder, viewer, timer, shooter, mapper, tester, demo, reset, quit;
   XmString focus, afr, nam, mideast, fareast, cam_th, mideast_th, nes_th;
   XmString earth, type, boundarylines, area, grid, cover, fight;
   XmString lighting, sun, moon, lightoff;
   XmString background, original, black, darkred, grey;
   XmString weather, cloud;
   XmString links, all_links_on, all_links_off, com_links_on, com_links_off;
   XmString sen_links_on, dsp_links_on,  sbr_links_on,  gbr_links_on,  gbi_links_on;
   XmString sen_links_off,dsp_links_off, sbr_links_off, gbr_links_off, gbi_links_off;
   XmString options, redraw, normal, tangent, save1, restore1, save2, restore2;
   XmString help, mouse, panel_1, panel_2;
   char     initial_set[12][12] = { "button_0", "button_1", "button_2", "button_3",
                                    "button_4", "button_5", "button_6", "button_7",
                                    "button_8", "button_9", "button_10", "button_11" };

   // ----- menu bar itself -----
   
   logo       = XmStringCreateSimple("File");
// program    = XmStringCreateSimple("Program");
   options    = XmStringCreateSimple("Edit");
   focus      = XmStringCreateSimple("View");
   earth      = XmStringCreateSimple("Earth");
   lighting   = XmStringCreateSimple("Lighting");
   // background = XmStringCreateSimple("Background");
   weather    = XmStringCreateSimple("Weather");
   links      = XmStringCreateSimple("Links");
   help       = XmStringCreateSimple("Help");

   v_menubar = XmVaCreateSimpleMenuBar (parent, name,
		XmVaCASCADEBUTTON, logo,         'F',
                XmVaCASCADEBUTTON, options,      'E',
		XmVaCASCADEBUTTON, focus,        'V',
		XmVaCASCADEBUTTON, earth,        'A',
		XmVaCASCADEBUTTON, lighting,     'L',
	      //XmVaCASCADEBUTTON, background,   'B',
		XmVaCASCADEBUTTON, weather,      'W',
		XmVaCASCADEBUTTON, links,        'K',
                XmVaCASCADEBUTTON, help,         'H',
		NULL);
   XtVaSetValues (v_menubar,
                  XmNspacing,      5,
                  NULL);  

   // ----- File menu -----;
   finder  = XmStringCreateSimple ("View Finder");
   viewer  = XmStringCreateSimple ("Model  Viewer");
   timer   = XmStringCreateSimple ("Time   Viewer");
   shooter = XmStringCreateSimple ("Asset  Viewer");
   mapper  = XmStringCreateSimple ("Map    Viewer");
   tester  = XmStringCreateSimple ("Track  Editor");
   demo    = XmStringCreateSimple ("Track  Viewer");
   reset   = XmStringCreateSimple ("Reset");
   quit    = XmStringCreateSimple ("Quit");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "File", 0, logoCB,
              XmVaPUSHBUTTON, finder, NULL, NULL, NULL,
              XmVaPUSHBUTTON, viewer, NULL, NULL, NULL,
              XmVaPUSHBUTTON, timer, NULL, NULL, NULL,
              XmVaPUSHBUTTON, shooter, NULL, NULL, NULL,
              XmVaPUSHBUTTON, mapper, NULL, NULL, NULL,
              XmVaPUSHBUTTON, tester, NULL, NULL, NULL,
              XmVaPUSHBUTTON, demo, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, reset, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, quit, NULL, NULL, NULL,
              NULL);
   XmStringFree (finder);
   XmStringFree (viewer);
   XmStringFree (timer);
   XmStringFree (shooter);
   XmStringFree (mapper);
   XmStringFree (tester);
   XmStringFree (demo);
   XmStringFree (reset);
   XmStringFree (quit);

   // ----- Edit menu -----;
   redraw   = XmStringCreateSimple ("Redraw");
   normal   = XmStringCreateSimple ("Normal View");
   tangent  = XmStringCreateSimple ("Tangential");
   save1    = XmStringCreateSimple ("Save 1");
   restore1 = XmStringCreateSimple ("Recall 1");
   save2    = XmStringCreateSimple ("Save 2");
   restore2 = XmStringCreateSimple ("Recall 2");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "options", 1, optionCB,
              XmVaPUSHBUTTON, redraw, NULL, NULL, NULL,
              XmVaPUSHBUTTON, normal, NULL, NULL, NULL,
              XmVaPUSHBUTTON, tangent, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, save1, NULL, NULL, NULL,
              XmVaPUSHBUTTON, restore1, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, save2, NULL, NULL, NULL,
              XmVaPUSHBUTTON, restore2, NULL, NULL, NULL,
              NULL);
   XmStringFree (redraw);
   XmStringFree (normal);
   XmStringFree (tangent);
   XmStringFree (save1);
   XmStringFree (restore1);
   XmStringFree (save2);
   XmStringFree (restore2); 
/*
   Widget logo_b =   
   XtVaCreateManagedWidget ("Logo", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, logo,
                            XmNmnemonic, 'A',
                            XmNsubMenuId, menupane,
                            NULL);

   Pixel fg, bg;
   XtVaGetValues (logo_b, XmNforeground, &fg, XmNbackground, &bg, NULL);

   char *BITMAPDIR;
   char logofile[80];
     
   if ((BITMAPDIR=getenv("BITMAPDIR"))==NULL)
   {
	BITMAPDIR = "./BitMaps";
   }
	
   sprintf (logofile, "%s%s", BITMAPDIR, "/gisp.bit");
   
   Pixmap pixmap =
     XmGetPixmap(XtScreen(logo_b), logofile, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf("   Couldn't load bitmap file %s.\n", logofile);
   else
     XtVaSetValues (logo_b,
		    XmNlabelType, XmPIXMAP,
		    XmNlabelPixmap, pixmap,
		    NULL);
*/
   // ----- focus menu ------;
   afr        = XmStringCreateSimple ("Africa, (0,0)");
   nam        = XmStringCreateSimple ("N. America");
   mideast    = XmStringCreateSimple ("Middle East");
   fareast    = XmStringCreateSimple ("Korea");
   cam_th     = XmStringCreateSimple ("Cent. Am Theater");
   mideast_th = XmStringCreateSimple ("Mid. East Theater");
   nes_th     = XmStringCreateSimple ("N.E. Siberia Theater");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "focus", 2, focusCB,
              XmVaRADIOBUTTON, afr, NULL, NULL, NULL,
              XmVaRADIOBUTTON, nam, NULL, NULL, NULL,
              XmVaRADIOBUTTON, mideast, NULL, NULL, NULL,
              XmVaRADIOBUTTON, fareast, NULL, NULL, NULL,
              XmVaRADIOBUTTON, cam_th, NULL, NULL, NULL,
              XmVaRADIOBUTTON, mideast_th, NULL, NULL, NULL,
              XmVaRADIOBUTTON, nes_th, NULL, NULL, NULL,
              XmNradioBehavior, True,
              XmNradioAlwaysOne, True,
              NULL);
   if (dummywidget = XtNameToWidget (menupane, initial_set[REGION]))
      XtVaSetValues (dummywidget, XmNset, True, NULL);
   XmStringFree (afr);
   XmStringFree (nam);
   XmStringFree (mideast);
   XmStringFree (fareast);
   XmStringFree (mideast_th);
   XmStringFree (cam_th);
   XmStringFree (nes_th);

  // ----- earth menu ------;
   type          = XmStringCreateSimple ("Type");
   area          = XmStringCreateSimple ("Area Earth");
   boundarylines = XmStringCreateSimple ("Boundary");
   grid          = XmStringCreateSimple ("Grid");
   cover         = XmStringCreateSimple ("Assets");
   fight         = XmStringCreateSimple ("Units");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "earth", 3, earthCB,
              XmVaCASCADEBUTTON, type, NULL, 
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, area, NULL, 
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, boundarylines, NULL,
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, grid, NULL,
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, cover, NULL,
              XmVaCASCADEBUTTON, fight, NULL,
              NULL);
   XmStringFree (type);
   XmStringFree (area);
   XmStringFree (boundarylines);
   XmStringFree (grid);
   XmStringFree (cover);
   XmStringFree (fight);

   // ..... start earth's submenus ................
   XmString texture, texture2, texture3, simple, blue, none;
   texture  = XmStringCreateSimple ("Texture -- Low");  
   texture2 = XmStringCreateSimple ("Texture -- Med");  
   texture3 = XmStringCreateSimple ("Texture -- Hi");  
   simple   = XmStringCreateSimple ("Polygon");  
   blue     = XmStringCreateSimple ("Blue Earth");  
   none     = XmStringCreateSimple ("None");  
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu0", 0,
             earthtypeCB,
             XmVaRADIOBUTTON, texture, NULL, NULL, NULL,
             XmVaRADIOBUTTON, texture2, NULL, NULL, NULL,
             XmVaRADIOBUTTON, texture3, NULL, NULL, NULL,
             XmVaRADIOBUTTON, simple, NULL, NULL, NULL,
             XmVaRADIOBUTTON, blue, NULL, NULL, NULL,
             XmVaRADIOBUTTON, none, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   XmStringFree (texture);
   XmStringFree (texture2);
   XmStringFree (texture3);
   XmStringFree (simple);
   XmStringFree (blue);
   XmStringFree (none);
   if (dummywidget = XtNameToWidget (submenu, "button_3"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   XmString load_carib, unload_carib, load, unload;
   load_carib   = XmStringCreateSimple ("Load Caribbean");
   unload_carib = XmStringCreateSimple ("Unload Caribbean");
   load         = XmStringCreateSimple ("Load");
   unload       = XmStringCreateSimple ("Unload");
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu1", 1,
             earthareaCB,
             XmVaRADIOBUTTON, load_carib, NULL, NULL, NULL,
             XmVaRADIOBUTTON, unload_carib, NULL, NULL, NULL,
             XmVaRADIOBUTTON, load, NULL, NULL, NULL,
             XmVaRADIOBUTTON, unload, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   XmStringFree (load_carib);
   XmStringFree (unload_carib);
   XmStringFree (load);
   XmStringFree (unload);
   if (dummywidget = XtNameToWidget (submenu, "button_0"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   on  = XmStringCreateSimple ("On");
   off = XmStringCreateSimple ("Off");

   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu2", 2, earthlinesCB,
             XmVaRADIOBUTTON, off, NULL, NULL, NULL,
             XmVaRADIOBUTTON, on, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   if (dummywidget = XtNameToWidget (submenu, "button_0"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);
  
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu3", 3, earthgridCB,
             XmVaRADIOBUTTON, off, NULL, NULL, NULL,
             XmVaRADIOBUTTON, on, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   if (dummywidget = XtNameToWidget (submenu, "button_1"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu4", 4, fixedsensorCB,
             XmVaRADIOBUTTON, off, NULL, NULL, NULL,
             XmVaRADIOBUTTON, on, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   if (dummywidget = XtNameToWidget (submenu, "button_0"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu5", 5, unitsCB,
             XmVaRADIOBUTTON, off, NULL, NULL, NULL,
             XmVaRADIOBUTTON, on, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   if (dummywidget = XtNameToWidget (submenu, "button_0"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   XmStringFree (on);
   XmStringFree (off);

   // ----- Lighting menu ------;
   sun      = XmStringCreateSimple ("Sun Light");
   moon     = XmStringCreateSimple ("Moon Light");
   lightoff = XmStringCreateSimple ("Light Off");
   original = XmStringCreateSimple ("Original");
   black    = XmStringCreateSimple ("Black");
   darkred  = XmStringCreateSimple ("Dark Red");
   grey     = XmStringCreateSimple ("Grey");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "lighting", 4, lightingCB,
              XmVaRADIOBUTTON, sun, NULL, NULL, NULL,
              XmVaRADIOBUTTON, moon, NULL, NULL, NULL,
              XmVaRADIOBUTTON, lightoff, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaRADIOBUTTON, original, NULL, NULL, NULL,
              XmVaRADIOBUTTON, black, NULL, NULL, NULL,
              XmVaRADIOBUTTON, darkred, NULL, NULL, NULL,
              XmVaRADIOBUTTON, grey, NULL, NULL, NULL,
              XmNradioBehavior, True,
              XmNradioAlwaysOne, True,
              NULL);
   /*
   if (dummywidget = XtNameToWidget (menupane, "button_0"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   // ----- background menu ------;
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "background", 5, backgroundCB,
              XmVaRADIOBUTTON, original, NULL, NULL, NULL,
              XmVaRADIOBUTTON, black, NULL, NULL, NULL,
              XmVaRADIOBUTTON, darkred, NULL, NULL, NULL,
              XmVaRADIOBUTTON, grey, NULL, NULL, NULL,
              XmNradioBehavior, True,
              XmNradioAlwaysOne, True,
              NULL);
   */
   XmStringFree (sun);
   XmStringFree (moon);
   XmStringFree (lightoff);
   XmStringFree (original);
   XmStringFree (black);
   XmStringFree (darkred);
   XmStringFree (grey);
   if (dummywidget = XtNameToWidget (menupane, "button_1"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   // -----Weather menu ----
   cloud = XmStringCreateSimple ("Clouds");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "weather", 5, 
              weatherCB,
              XmVaPUSHBUTTON, cloud, NULL, NULL, NULL,
              NULL);
   XmStringFree (cloud);

   // ----- Links menu ----
   all_links_on  = XmStringCreateSimple ("All Links On");
   all_links_off = XmStringCreateSimple ("All Links Off");
   com_links_on  = XmStringCreateSimple ("Com Links On");
   com_links_off = XmStringCreateSimple ("Com Links Off");
   sen_links_on  = XmStringCreateSimple ("Sensor Links On");
   sen_links_off = XmStringCreateSimple ("Sensor Links Off");
   dsp_links_on  = XmStringCreateSimple ("DSP Links On");
   dsp_links_off = XmStringCreateSimple ("DSP Links Off");
   sbr_links_on  = XmStringCreateSimple ("SBIRS Links On");
   sbr_links_off = XmStringCreateSimple ("SBIRS Links Off");
   gbr_links_on  = XmStringCreateSimple ("GBR Links On");
   gbr_links_off = XmStringCreateSimple ("GBR Links Off");
   gbi_links_on  = XmStringCreateSimple ("GBI Links On");
   gbi_links_off = XmStringCreateSimple ("GBI Links Off");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "links", 6,
	       sps_linksCB,
	       XmVaPUSHBUTTON, all_links_on, NULL, NULL, NULL,
	       XmVaPUSHBUTTON, all_links_off, NULL, NULL, NULL,
	       XmVaSEPARATOR,
	       XmVaPUSHBUTTON, com_links_on, NULL, NULL, NULL,
	       XmVaPUSHBUTTON, com_links_off, NULL, NULL, NULL,
	       XmVaSEPARATOR,
	       XmVaPUSHBUTTON, sen_links_on, NULL, NULL, NULL,
	       XmVaPUSHBUTTON, sen_links_off, NULL, NULL, NULL,
	       XmVaSEPARATOR,
	       XmVaPUSHBUTTON, dsp_links_on, NULL, NULL, NULL,
	       XmVaPUSHBUTTON, dsp_links_off, NULL, NULL, NULL,
	       XmVaSEPARATOR,
	       XmVaPUSHBUTTON, sbr_links_on, NULL, NULL, NULL,
	       XmVaPUSHBUTTON, sbr_links_off, NULL, NULL, NULL,
	       XmVaSEPARATOR,
	       XmVaPUSHBUTTON, gbr_links_on, NULL, NULL, NULL,
	       XmVaPUSHBUTTON, gbr_links_off, NULL, NULL, NULL,
	       XmVaSEPARATOR,
	       XmVaPUSHBUTTON, gbi_links_on, NULL, NULL, NULL,
	       XmVaPUSHBUTTON, gbi_links_off, NULL, NULL, NULL,
	       NULL);
   XmStringFree (all_links_on);
   XmStringFree (all_links_off);
   XmStringFree (com_links_on);
   XmStringFree (com_links_off);
   XmStringFree (sen_links_on);
   XmStringFree (sen_links_off);
   XmStringFree (dsp_links_on);
   XmStringFree (dsp_links_off);
   XmStringFree (sbr_links_on);
   XmStringFree (sbr_links_off);
   XmStringFree (gbr_links_on);
   XmStringFree (gbr_links_off);
   XmStringFree (gbi_links_on);
   XmStringFree (gbi_links_off);
   /*
   // ..... start Links' submenus ................
   on = XmStringCreateSimple ("On");
   off = XmStringCreateSimple ("Off");
   submenu = XmVaCreateSimplePulldownMenu (menupane, "LinksSubmenu2", 2,
             sps_com_linksCB,
             XmVaPUSHBUTTON, on, NULL, NULL, NULL,
             XmVaPUSHBUTTON, off, NULL, NULL, NULL,
             NULL);
   submenu = XmVaCreateSimplePulldownMenu (menupane, "LinksSubmenu3", 4,
             sps_sen_linksCB,
             XmVaPUSHBUTTON, on, NULL, NULL, NULL,
             XmVaPUSHBUTTON, off, NULL, NULL, NULL,
             NULL);
   */
   XmStringFree (restore2);

   // ----- Help menu ----
   mouse   = XmStringCreateSimple ("Mouse Operations");
   panel_1 = XmStringCreateSimple ("View Control");
   panel_2 = XmStringCreateSimple ("Simulation Control");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "help", 7,
              helpCB,
              XmVaPUSHBUTTON, mouse, NULL, NULL, NULL,
	      //XmVaPUSHBUTTON, panel_1, NULL, NULL, NULL,
              //XmVaPUSHBUTTON, panel_2, NULL, NULL, NULL,
              NULL);
   XmStringFree (mouse);
   XmStringFree (panel_1);
   XmStringFree (panel_2);
   
   XmStringFree (logo);
   XmStringFree (focus);
   XmStringFree (earth);
   XmStringFree (lighting);
   XmStringFree (background);
   XmStringFree (weather);
   XmStringFree (links);
   XmStringFree (options);
   XmStringFree (help);
   
   XtManageChild(v_menubar);

   return (v_menubar);
}

/* ------ */


Widget
makeViewBar (char* name, Widget parent)
{
   Widget v_viewbar;
   Widget v_force, v_normal, v_tangent, v_sav1, v_sav2, v_rec1, v_rec2;
   Widget v_option, v_flip;

   Pixel fg, bg;
   Pixmap pixmap;
   char *BITMAPDIR;
   char *FWDIR;
   char normalbit[80], tangentbit[80];
   char sav1bit[80], rec1bit[80], sav2bit[80], rec2bit[80];
   char optionbit[80];
   char flipbit[80];
 
   if ((BITMAPDIR=getenv("BITMAPDIR"))==NULL)
	BITMAPDIR = "./BitMaps";
   
   sprintf (normalbit, "%s%s", BITMAPDIR, "/normal.bit");
   sprintf (tangentbit, "%s%s", BITMAPDIR, "/tangent.bit"); 
   sprintf (sav1bit, "%s%s", BITMAPDIR, "/sav1.bit"); 
   sprintf (rec1bit, "%s%s", BITMAPDIR, "/rec1.bit"); 
   sprintf (sav2bit, "%s%s", BITMAPDIR, "/sav2.bit"); 
   sprintf (rec2bit, "%s%s", BITMAPDIR, "/rec2.bit"); 
   sprintf (optionbit, "%s%s", BITMAPDIR, "/option.bit"); 
   sprintf (flipbit, "%s%s", BITMAPDIR, "/flip.bit"); 

   v_viewbar = XmCreateRowColumn (parent, name, NULL, 0);
   XtManageChild (v_viewbar);
   XtVaSetValues (v_viewbar,
                  XmNorientation,  XmHORIZONTAL, 
                  XmNspacing,      5,
                  XmNmarginHeight, 0,
                  NULL);

   v_force = XtVaCreateManagedWidget
     ("Redraw", xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtAddCallback (v_force, XmNactivateCallback, forceCB, (XtPointer)0);

   v_normal = XtVaCreateManagedWidget
     ("Normal", xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtAddCallback (v_normal, XmNactivateCallback, viewCB, (XtPointer)0);
   XtVaGetValues (v_normal, XmNforeground, &fg, XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_normal), normalbit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf ("  Cannot find bitmap file %s\n", normalbit);
   else
     XtVaSetValues (v_normal,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);
   
   v_tangent = XtVaCreateManagedWidget
     ("Tangent",
      xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtAddCallback (v_tangent, XmNactivateCallback, viewCB, (XtPointer)1);
   XtVaGetValues (v_normal, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_normal), tangentbit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf ("  Cannot find bitmap file %s\n", tangentbit);
   else
     XtVaSetValues (v_tangent,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);

   v_sav1 = XtVaCreateManagedWidget
     ("SV1",
      xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtAddCallback (v_sav1, XmNactivateCallback, memoryCB, (XtPointer)1);
   XtVaGetValues (v_sav1, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_sav1), sav1bit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf ("  Cannot find bitmap file %s\n", sav1bit);
   else
     XtVaSetValues (v_sav1,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);
   
   v_rec1 = XtVaCreateManagedWidget
     ("RC1",
      xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtAddCallback (v_rec1, XmNactivateCallback, memoryCB, (XtPointer)101);
   XtVaGetValues (v_rec1, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_rec1), rec1bit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf ("  Cannot find bitmap file %s\n", rec1bit);
   else
     XtVaSetValues (v_rec1,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);
   
   v_sav2 = XtVaCreateManagedWidget
     ("SV2",
      xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtAddCallback (v_sav2, XmNactivateCallback, memoryCB, (XtPointer)2);
   XtVaGetValues (v_sav2, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_sav2), sav2bit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf ("  Cannot find bitmap file %s\n", sav2bit);
   else
     XtVaSetValues (v_sav2,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);

   v_rec2 = XtVaCreateManagedWidget
     ("RC2",
      xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtAddCallback (v_rec2, XmNactivateCallback, memoryCB, (XtPointer)102);
   XtVaGetValues (v_rec2, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_rec2), rec2bit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf ("  Cannot find bitmap file %s\n", rec2bit);
   else
     XtVaSetValues (v_rec2,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);
   
   v_option = XtVaCreateManagedWidget
     ("OPT",
      xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtVaGetValues (v_option, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_option), optionbit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf ("  Cannot find bitmap file %s\n", optionbit);
   else
     XtVaSetValues (v_option,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);
   
   v_flip = XtVaCreateManagedWidget
     ("FLIP",
      xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtAddCallback (v_flip, XmNactivateCallback, flipCB, NULL);
   XtVaGetValues (v_flip, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_flip), flipbit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf ("  Cannot find bitmap file %s\n", flipbit);
   else
     XtVaSetValues (v_flip,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);
   
   return (v_viewbar);
}

/* ------ */
Widget
makePanel (char* name, Widget parent, Widget botattach)
{
   Widget	v_panel;
   XmString	title;
   int          rpos, pcent, i;
   XColor	color, unused;
   Pixel	bg_color, fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
   Colormap	cmap;

   v_panel = parent;

   title = XmStringCreateSimple("Longitude");
   scaleLON = createOneScale (v_panel, 'o', title, (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleLON,
                  XmNleftAttachment,     XmATTACH_POSITION,
                  XmNleftPosition,       1,
                  XmNrightAttachment,    XmATTACH_POSITION,
                  XmNrightPosition,      24,
                  XmNbottomAttachment,   XmATTACH_WIDGET,
                  XmNbottomWidget,       botattach,
                  NULL);

   title = XmStringCreateSimple("Latitude");
   scaleLAT = createOneScale (v_panel, 'a', title, (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleLAT,
                  XmNleftAttachment,     XmATTACH_POSITION,
                  XmNleftPosition,       1,
                  XmNrightAttachment,    XmATTACH_POSITION,
                  XmNrightPosition,      24,
                  XmNbottomAttachment,   XmATTACH_WIDGET,
                  XmNbottomWidget,       scaleLON,
                  NULL);

   title = XmStringCreateSimple("Field of View");
   scaleFOV = createOneScale (v_panel, 'v', title, (XtCallbackProc)changeScaleCB);
   XmStringFree (title);
   XtVaSetValues (scaleFOV,
                  XmNleftAttachment,     XmATTACH_POSITION,
                  XmNleftPosition,       26,
                  XmNrightAttachment,    XmATTACH_POSITION,
                  XmNrightPosition,      49,
                  XmNbottomAttachment,   XmATTACH_WIDGET,
                  XmNbottomWidget,       botattach,
                  NULL);
 
   title = XmStringCreateSimple("Altitude");
   scaleALT = createOneScale (v_panel, 'l', title, (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleALT,
                  XmNleftAttachment,     XmATTACH_POSITION,
                  XmNleftPosition,       26,
                  XmNrightAttachment,    XmATTACH_POSITION,
                  XmNrightPosition,      49,
                  XmNbottomAttachment,   XmATTACH_WIDGET,
                  XmNbottomWidget,       scaleFOV,
		  XmNscaleMultiple,      100,
                  NULL);

   title = XmStringCreateSimple("Azimuth");
   scaleAZI = createOneScale (v_panel, 'z', title, (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleAZI,
                  XmNleftAttachment,     XmATTACH_POSITION,
                  XmNleftPosition,       51,
                  XmNrightAttachment,    XmATTACH_POSITION,
                  XmNrightPosition,      74,
                  XmNbottomAttachment,   XmATTACH_WIDGET,
                  XmNbottomWidget,       botattach,
                  NULL);

   XmStringFree (title);
   /*
   rpos  = 55;
   pcent = 0;
   for (i=0; i<TMNodes; i++) {
   TMBars[i] = XtVaCreateManagedWidget("bar", xfwfPcBarWidgetClass, v_panel,
                  XtNforeground, fg_color,
                  XtNpercentage, pcent,
                  XtNdisplaypc,  FALSE,
                  XtNvertical,   TRUE,
                  NULL);
   XtVaSetValues(TMBars[i],
                  XmNbackground,      bg_color,
                  XmNrightAttachment, XmATTACH_POSITION,
                  XmNrightPosition,   rpos,
                  XmNtopAttachment,   XmATTACH_POSITION,
                  XmNtopPosition,     4,
                  XtNabs_width,       10,
                  XtNabs_height,      100,
                  NULL);
   XtManageChild(TMBars[i]);
   rpos  = rpos+3;
   }
   */  
   return (scaleLAT /*v_panel*/);  /* Return a widget for top attachment */
}

Widget
createOneScale(Widget parent, char code, XmString title, XtCallbackProc CB)
{
   Widget scale;
   Arg args[10];
   register int n;
   char name[2];

   name[0] = code;
   name[1] = '\0';

   n = 0;
   XtSetArg (args[n], XmNorientation,   XmHORIZONTAL); n++;
   XtSetArg (args[n], XmNshowValue,     True);         n++;
   XtSetArg (args[n], XmNtitleString,   title);        n++;
   XtSetArg (args[n], XmNscaleMultiple, 1);            n++;
   XtSetArg (args[n], XmNtopOffset,     5);            n++;
   scale = XmCreateScale (parent, name, args, n);

   XtAddCallback (scale, XmNvalueChangedCallback, CB, (XtPointer)code);
   XtAddCallback (scale, XmNdragCallback, CB, (XtPointer)code);
   
   XtManageChild (scale);

   return (scale);
}
/*                                                                             */
/* ----------------------------   Main Menu Callbacks   ---------------------- */
/*                                                                             */
void
logoCB (Widget, XtPointer client_data, XtPointer)
{
int    answer;
int    item_no = (int) client_data;

   switch (item_no)
   {
     case 0:					// View finder
       view_finderCB ();
       break;

     case 1:					// Model viewer
       model_viewerCB(GR_toplevel);
       break;

     case 2:
#ifdef Linux					// Time management viewer
       time_viewerCB(GR_toplevel);
#endif
       break;

     case 3:                                    // Shooter
       asset_viewerCB(GR_toplevel);
       break;

     case 4:
#ifdef Linux					// Flat Map
       map_viewerCB(GR_toplevel);
#endif
       break;

     case 5:					// Test routine
       track_editCB(GR_toplevel);
       break;

     case 6:					// Track Viewer
       track_viewCB(GR_toplevel);
       break;

     case 7:					// reset
       /*
       delete displist;
       displist = new GR_DispList;
       gwindow->addDispList (displist);
       */
       gwindow->rem_all_namedDispLists ();
       break;

     case 8:					// quit
       if (saved_vu) {
           answer = GetYesNo("View Registers Changed.\nSave New Values?");
           if (answer == YES) {
              if ( (VIEWfp = fopen("viewsave.dat", "w+")) != NULL) {
                 fprintf(VIEWfp, "%d %d %d %d %d %d\n",
                        MODE_1, LAT_1, LON_1, ALT_1, FOV_1, AZI_1);
                 fprintf(VIEWfp, "%d %d %d %d %d %d\n",
                        MODE_2, LAT_2, LON_2, ALT_2, FOV_2, AZI_2);
                 fclose(VIEWfp);
              }
	   }
       }
       input_finish();
       exit (0);
       break;

     default:
       break;
   } 
}

int
GetYesNo(char *str)
{
Widget     dialog, toggle;
Arg        arg[10];
static int answer;
int        scn, response, pad1, pad2, pad3, pad4;
XmString   xstr, defval, YESstr, NOstr;
int        i, nargs, xferbytes;

      answer = -1;
      xstr   = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
      defval = XmStringCreateLtoR("User Verify", XmSTRING_DEFAULT_CHARSET);
      YESstr = XmStringCreateLtoR("Yes", XmSTRING_DEFAULT_CHARSET);
      NOstr  = XmStringCreateLtoR("No", XmSTRING_DEFAULT_CHARSET); 
      nargs = 0;
      XtSetArg (arg[0], XmNmessageString,     xstr); nargs++;
      XtSetArg (arg[1], XmNautoUnmanage,      True); nargs++;
      XtSetArg (arg[2], XmNdialogStyle,       XmDIALOG_FULL_APPLICATION_MODAL); nargs++;
      XtSetArg (arg[3], XmNuserData,          0); nargs++;
      XtSetArg (arg[4], XmNdialogTitle,       defval); nargs++;
      XtSetArg (arg[5], XmNokLabelString,     YESstr); nargs++;
      XtSetArg (arg[6], XmNcancelLabelString, NOstr); nargs++;
      dialog = XmCreateWarningDialog (GR_toplevel, "Dialog", arg, nargs);
      XmStringFree (xstr);
      XmStringFree (defval);
      XmStringFree (YESstr);
      XmStringFree (NOstr);
      XtAddCallback(dialog, XmNcancelCallback, responseCB, &answer);
      XtAddCallback(dialog, XmNokCallback,     responseCB, &answer);
      XtManageChild (dialog);
      XtPopup(XtParent(dialog), XtGrabNone);
      while (answer < 0) XtAppProcessEvent(GR_appcontext, XtIMAll);
      XtDestroyWidget(dialog);
      return(answer);
}
/*
void
toggleCB(Widget toggle, XtPointer n, XmToggleButtonCallbackStruct *cbs)
{
  if (cbs->set)
    YESNO = (int) n;
  else
    YESNO = NO;
}
*/
void
responseCB(Widget w, int *answer, XmAnyCallbackStruct *cbs)
{
   switch (cbs->reason) {
   case XmCR_OK:
     *answer = YES;
     break;
   case XmCR_CANCEL:
     *answer = NO;
     break;
   }
}

void
focusCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;

   VMODE    = 0;                  // normal
   v_ALT    = 6700;
   v_FOV    = 42;
   v_FOVDIV = 1;

   switch (item_no)
   {
    case 0:                       // AFR, (0,0):
      v_LAT = 0;
      v_LON = 0;
      v_FOV = 62;
      break;
    case 1:                       // N.AM:   
      v_LAT = 37;
      v_LON = -102;
      v_FOV = 62;
      break;
    case 2:                       // M.East:
      v_LAT = 33;
      v_LON = 45;
      v_FOV = 62;
      break;
    case 3:                       // Korea:
      v_LAT = 45;
      v_LON = 127;
      v_FOV = 1;
      v_AZI = 0;
      VMODE = 2;
      break;
    case 4:                       // C.AM Theater:
      v_LAT = 23;
      v_LON = -85;
      break;
    case 5:                       // M.East Theater:
      v_LAT = 29;
      v_LON = 47;
      v_FOVDIV = 10;
      break;
    case 6:                       // NE.Siberia Theater:
      v_LAT = 57;
      v_LON = 165;
      break;
    default:
      break;
   }
   XmScaleSetValue (scaleLAT, v_LAT);
   XmScaleSetValue (scaleLON, v_LON);
   XmScaleSetValue (scaleALT, v_ALT);
   XmScaleSetValue (scaleFOV, v_FOV);
   XmScaleSetValue (scaleAZI, v_AZI);
   
   setvparams (gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
   
   if (vfwindow && vfwindow->get_awake())
     vfwindow->draw ();
}

void
earthCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   printf ("..... Warning: earthCB is called....\n");
}

void
earthtypeCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   static Boolean first_300 = TRUE;
   static Boolean first_301 = TRUE;
   static Boolean first_302 = TRUE;
   static Boolean first_310 = TRUE;
   static Boolean first_33  = FALSE;
   static Boolean first_200 = TRUE;
   static Boolean first_800 = TRUE;

   if (first_800)
   {
      first_800 = FALSE;
      Star800   = new GR_Stars(255, 255, 255, starfile, 800);
   }
   star_type = 800;

   if (displist->inlist_by_type (star_type))
       displist->delete_object_by_type (star_type);
   displist->add_object (*Star800);

   switch (item_no)
   {
     case 0:  // texture -- low
     case 1:  // texture -- med
     case 2:  // texture -- high
       if (current_earth_type!=300)
       {
          if (first_300)
          {
             first_300 = FALSE;
             Tearth300 = new GR_Tearth (texturefile, 4, 0, 0);
          }
          if (displist->inlist_by_type (current_earth_type))
             displist->delete_object_by_type (current_earth_type);
          displist->add_object (*Tearth300);
          current_earth_type = 300;
       }
       break;
/*
     case 1:  // texture -- med
       if (current_earth_type!=301)
       {
          if (first_301)
          {
             first_301 = FALSE;
             Tearth301 = new GR_Tearth2 (earthdir, 301);
          }
          if (displist->inlist_by_type (current_earth_type))
             displist->delete_object_by_type (current_earth_type);
          displist->add_object (*Tearth301);
          current_earth_type = 301;
       }
       break;

     case 2:  // texture -- high
       if (current_earth_type!=302)
       {
          if (first_302)
          {
             first_302 = FALSE;
             Tearth302 = new GR_Tearth3 (earthdir, 302);
          }
          if (displist->inlist_by_type (current_earth_type))
             displist->delete_object_by_type (current_earth_type);
          displist->add_object (*Tearth302);
          current_earth_type = 302;
       }
*/
       break;

     case 3:   // simple
       if (current_earth_type!=33)
       {
          if (first_33)
          {
             first_33 = FALSE;
             Searth33 = new GR_Model (0, 33);
          }
          if (displist->inlist_by_type (current_earth_type))
             displist->delete_object_by_type (current_earth_type);
          displist->add_object (*Searth33);
          current_earth_type = 33;
       }
       break;

     case 4:   // blue 
       if (current_earth_type!=200)
       {
          if (first_200)
          {
             first_200= FALSE;
             Bearth200 = new GR_Blueearth (200);
          }
          if (displist->inlist_by_type (current_earth_type))
             displist->delete_object_by_type (current_earth_type);
          displist->add_object (*Bearth200);
          current_earth_type = 200;
       }
       break;

     case 5:   // none
       if (current_earth_type!=0)
       {
          displist->delete_object_by_type (current_earth_type);
          current_earth_type = 0;
       }
       break;

     default:
       break;
   }
}
 
void
earthareaCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   int x, y;
   long type;
   static first_Caribbean = TRUE;

   x = ((v_LON + 180)/10)%36;          // 0 to 35;
   y = ((v_LAT + 90)/10)%18;           // 0 to 17;
   type = y*36+x+1000;

   switch (item_no)
   {
    case 0: // load Caribbean
      if (first_Caribbean)
      {
	 first_Caribbean = FALSE;
	 carib = new Carib (caribdir, 201);
      }
      displist->delete_object_by_type (201); // hard code type for now
      displist->add_object (carib);
      break;
    case 1: // unload Caribbean
      displist->delete_object_by_type (201);
      break;
    case 2: // load
      /*
      if (first_Area[x][y])
      {
	 first_Area[x][y] = FALSE;
	 Area[x][y] = new GR_Area (rgbfile, elevdir, type, v_LON, v_LAT);
      }
      if (!displist->inlist_by_type (type))
	displist->add_object(*Area[x][y]);
	*/
      break;
    case 3:  // unload
      /*
      if (displist->inlist_by_type (type))
	displist->delete_object_by_type(type);
	*/
      break;
    default:
      break;
   }
}

void
earthlinesCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   static Boolean first_320 = TRUE;

   switch (item_no)
   {
     case 0:  // off
       if (displist->inlist_by_type (320))
       {
          displist->delete_object_by_type (320);
          displist->delete_object_by_type (321);
       }
       break;

     case 1:  // on
       if (first_320)
       {
          first_320 = FALSE;
          work_progress (0, "Reading boundary lines");
          PoliLines321  = new GR_Lines (255,255,255,polifile,321); 
          ShoreLines320 = new GR_Lines (255,255,255,shorefile,320); 
          work_progress (2, NULL);
          work_progress (3, NULL); 
       }
       if (!displist->inlist_by_type (320))
       {
          displist->add_object (*ShoreLines320);
          displist->add_object (*PoliLines321);
       }
       break;

     default:
       break;
   }
}

void
earthgridCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   static Boolean first_330 = TRUE;
   float lat, lon, alt, x0, y0, z0, x, y, z;
   float rad = 1.005;

   switch (item_no)
   {
     case 0:  // off
       if (displist->inlist_by_type (330))
          displist->delete_object_by_type (330);
       break;

     case 1:  // on
       if (first_330)
       {
          first_330 = FALSE;
          Grid330 = new GR_Gridlines (155,155,0,10,330);
       }
       if (!displist->inlist_by_type (330))
          displist->add_object (*Grid330);
       break;

     default:
       break;
   }
}

extern unsigned long WidgetBackgroundToGlRgb (Widget);

void
lightingCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   long org_background;
   static Boolean first_sun = TRUE;
   static Boolean first_moon= TRUE;
//
//	Define the Lighting Model parameters
//
   static float mat_AMBIENT[]      = { 0.3, 0.3, 0.3, 1.0 };
   static float mat_DIFFUSE[]      = { 0.8, 0.8, 0.8, 1.0 };
   static float mat_EMISSION[]     = { 0.3, 0.3, 0.3, 1.0 };
   static float mat_SPECULAR[]     = { 0.8, 0.8, 0.8, 1.0 };
   static float mat_SHININESS[]    = { 10.0 };
   static float dullmat_AMBIENT[]  = { 0.2, 0.2, 0.2, 1.0 };
   static float dullmat_DIFFUSE[]  = { 0.5, 0.5, 0.5, 1.0 };
   static float dullmat_EMISSION[] = { 0.3, 0.3, 0.3, 1.0 };
   static float dullmat_SPECULAR[] = { 0.4, 0.4, 0.4, 1.0 };
   static float dullmat_SHININESS[] = { 5.0 };
   static float sunlt_LCOLOR[]    = { 0.2, 0.8, 0.5, 0.0 };
   static float sunlt_POSITION[]  = {-1.0, 1.0, 2.0, 0.0 };
   static float sunlt_AMBIENT[]   = { 0.1, 0.1, 0.1, 1.0 };
   static float sunlt_DIFFUSE[]   = { 1.0, 1.0, 1.0, 1.0 };
   static float sunlt_SPECULAR[]  = { 1.0, 1.0, 1.0, 1.0 };
   static float light_POSITION[]  = {-1.0,-1.0, 2.0, 0.0 };
   static float moonlt_LCOLOR[]   = { 0.1, 0.1, 0.8, 0.0 };
   static float moonlt_POSITION[] = {-1.0, 1.0, 2.0, 0.0 };
   static float nonelt_LCOLOR[]   = { 0.1, 0.1, 0.6, 0.0 };
   static float nonelt_POSITION[] = { 0.0, 0.0, 1.0, 0.0 };
   static float lm_LMAMBIENT[] = { 0.5, 0.5, 0.5, 1.0 };
   static float lm_LOCALVIEWER[] = { 1.0 };
   static float dulllm_LMAMBIENT[] = { 0.2, 0.2, 0.2, 1.0 };
   static float dulllm_LOCALVIEWER[] = { 1.0 };

   switch (item_no)
   {
     case 0:            // sun light:
       //glMaterialfv(GL_FRONT, GL_AMBIENT, mat_AMBIENT);
       //glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_DIFFUSE);
       //glMaterialfv(GL_FRONT, GL_EMISSION, mat_EMISSION);
       //glMaterialfv(GL_FRONT, GL_SPECULAR, mat_SPECULAR);
       //glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_SHININESS);
       glLightfv(GL_LIGHT0,  GL_POSITION, sunlt_POSITION);
       glLightfv(GL_LIGHT0,  GL_DIFFUSE,  sunlt_LCOLOR);
       glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lm_LMAMBIENT);
       glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lm_LOCALVIEWER);
       glShadeModel(GL_SMOOTH);
       glEnable(GL_LIGHT0);
       glEnable(GL_LIGHTING);
       //glEnable(GL_COLOR_MATERIAL);
       break;

     case 1:             // moon light:
       //glMaterialfv(GL_FRONT, GL_AMBIENT, dullmat_AMBIENT);
       //glMaterialfv(GL_FRONT, GL_DIFFUSE, dullmat_DIFFUSE);
       //glMaterialfv(GL_FRONT, GL_EMISSION, dullmat_EMISSION);
       //glMaterialfv(GL_FRONT, GL_SPECULAR, dullmat_SPECULAR);
       //glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, dullmat_SHININESS);
       glLightfv(GL_LIGHT0,  GL_POSITION, moonlt_POSITION);
       glLightfv(GL_LIGHT0,  GL_DIFFUSE,  moonlt_LCOLOR);
       glLightModelfv(GL_LIGHT_MODEL_AMBIENT, dulllm_LMAMBIENT);
       glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, dulllm_LOCALVIEWER);
       glShadeModel(GL_SMOOTH);
       glEnable(GL_LIGHT0);
       glEnable(GL_LIGHTING);
       //glEnable(GL_COLOR_MATERIAL);
       break;

     case 2:             // light off: 
       glDisable(GL_LIGHT0);
       glDisable(GL_LIGHTING);
       break;

     case 3:             // original:
       if (gwindow) {
          org_background = WidgetBackgroundToGlRgb (gwindow->widget());
          gwindow->color(org_background);
       }
       break;

     case 4:             // black:
          if (gwindow) gwindow->color(0,0,0); 
       break;

     case 5:             // dark red:
          if (gwindow) gwindow->color(110,0,4);
       break;

     case 6:             // grey:
          if (gwindow) gwindow->color(32,28,26);
       break;

     default:	         // All others, light off
          glLightfv(GL_LIGHT0,  GL_POSITION, nonelt_POSITION);
          glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lm_LMAMBIENT);
          glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lm_LOCALVIEWER);
       break;
    }
}


void
backgroundCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   long org_background;

   if (gwindow) {
   switch (item_no) {
     case 0:       // original:
        org_background = WidgetBackgroundToGlRgb (gwindow->widget());
        gwindow->color(org_background);
        break;
     case 1:        // black:
        gwindow->color(0,0,0); 
        break;
     case 2:        // dark red:
        gwindow->color(110,0,4);
        break;
     case 3:        // grey:
        gwindow->color(32,28,26);
        break;
     default:
        break;
   }
   }
}

void
weatherCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
 
   switch (item_no)
   {
      case 0: // for testing cloud only:
	cloudCB ();
	break;
      default:
        break;
   }
}

void
fixedsensorCB (Widget, XtPointer client_data, XtPointer)
{
int            i, item_no = (int) client_data;
static Boolean first_sen = TRUE;

   switch (item_no)
   {
     case 0:  // off
       /*
       for (i=0; i<gbrcount; i++)
         if (sensor_displist->inlist_by_type (gbrbase+i))
             sensor_displist->delete_object_by_type (gbrbase+i);
       */
       break;

     case 1:  // on
       if (first_sen) {
	 first_sen = FALSE;                        // Don't do this again
          asset_init ();                           // Load and initialize all assets
       }
       /*
       for (i=0; i<gbrcount; i++)
         if (!sensor_displist->inlist_by_type(gbrbase+i))
             sensor_displist->add_object (*GBRs[i]);
       */
       break;

     default:
       break;
   }
}

void
unitsCB (Widget, XtPointer client_data, XtPointer)
{
int            i, item_no = (int) client_data;
static Boolean first_unit = TRUE;
float          lat, lon, alt, x0, y0, z0, x, y, z, size;
double         latitude, longitude;
double         lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec;
static int     armycount = 0, armybase = 1995;
char           *PARMDIR;
char           armyfile[80];
int            armytype;

static C_PARSER *army_parser = NULL;
C_BASETYPE     *army_names, *army_name;

   switch (item_no)
   {
     case 0:  // off
       for (i=0; i<armycount; i++)
         if (displist->inlist_by_type (armybase+i))
             displist->delete_object_by_type (armybase+i);
       break;

     case 1:  // on
       if (first_unit)
       {
          first_unit = FALSE;
/*
 *      Parse the Ground Units parameter file
 *      -------------------------------------
 */
          if ((PARMDIR=getenv("PARMDIR")) == NULL)
               PARMDIR = "./ParFiles";
          sprintf (armyfile, "%s%s", PARMDIR, "/units.par");
          army_parser = new C_PARSER(armyfile);
          army_names = army_parser->get_basetype("units");
          armycount  = army_names->get_ntypes();
          army_name  = army_names->get_first_type();
          for (i=0; i<armycount; i++) {
	    //printf("Processing Army %s\n", army_name->get_name() );
            lat_deg = army_name->get_float("lat_deg");
            lat_min = army_name->get_float("lat_min");
            lat_sec = army_name->get_float("lat_sec");
            lon_deg = army_name->get_float("lon_deg");
            lon_min = army_name->get_float("lon_min");
            lon_sec = army_name->get_float("lon_sec");
            latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
            longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
            if (!army_name->get_logical("north")) latitude = -latitude;
            if (!army_name->get_logical("east")) longitude = -longitude;
	    armytype = army_name->get_int("type");
	    size = army_name->get_float("size");
            ARMYs[i] = new GR_Army(armytype, latitude, longitude, size);
            army_name = army_names->get_next_type();
          }
       }
       for (i=0; i<armycount; i++)
         if (!displist->inlist_by_type(armybase+i))
             displist->add_object (*ARMYs[i]);
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
       gwindow->draw();
       break;

     case 1:
       viewCB(NULL, (XtPointer)0, NULL);
       break;

     case 2:
       viewCB(NULL, (XtPointer)1, NULL);
       break;

     case 4:  // save 1
       memoryCB(NULL, (XtPointer)1, NULL);
       break;

     case 7: // save 2
       memoryCB(NULL, (XtPointer)2, NULL);
       break;

     case 5:  // recall 1
       memoryCB(NULL, (XtPointer)101, NULL);
       break;

     case 8: // recall 2
       memoryCB(NULL, (XtPointer)102, NULL);
       break;

     default:
       break;
  }
}

void
helpCB (Widget w, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   Widget idialog;
   XmString helptext;
   Arg args[2];
   char* helpstr;
   Widget theform = XtParent(XtParent(XtParent(XtParent(w))));
   // Level 1. Menupane7; 2. popup_Menupane0; 3. Menubar; 4. Form. 
 
   switch (item_no)
   {
      case 0:   // mouse operations:
        helpstr = "Mouse Operations:\n Left: picking;\n Shift_Left: track ball action.";
        helptext = XmStringCreateLtoR (helpstr, XmSTRING_DEFAULT_CHARSET); 
        XtSetArg (args[0], XmNmessageString, helptext);
        XtSetArg (args[1], XmNautoUnmanage, False);
        idialog = XmCreateInformationDialog (gwindow->widget(),"helpd",args,2);
        XmStringFree (helptext);
        XtUnmanageChild (XmMessageBoxGetChild(idialog,XmDIALOG_CANCEL_BUTTON));
        XtUnmanageChild (XmMessageBoxGetChild(idialog,XmDIALOG_HELP_BUTTON));
        //XtSetSensitive (XmMessageBoxGetChild(idialog,XmDIALOG_CANCEL_BUTTON), False);

        //XtSetSensitive (XmMessageBoxGetChild(idialog,XmDIALOG_HELP_BUTTON), False);
        XtAddCallback (idialog, XmNokCallback, help_doneCB, NULL);
        XtManageChild (idialog);
        XtPopup (XtParent(idialog), XtGrabNone); 
        break;

      case 1:
	XtUnmapWidget (XtNameToWidget (theform, "Spanel"));
	XtMapWidget (XtNameToWidget (theform, "Panel"));
	break;

      case 2:
	XtUnmapWidget (XtNameToWidget (theform, "Panel"));
	XtMapWidget (XtNameToWidget (theform, "Spanel"));
	break;	

      default:
        break;
   }
}

void
help_doneCB (Widget dialog, XtPointer, XtPointer)
{
   XtDestroyWidget (dialog);
}


/* ---------------- control panel CB's ----------------- */

void
forceCB(Widget, XtPointer client_data, XtPointer)
{
   gwindow->draw();
}

void
viewCB (Widget, XtPointer client_data, XtPointer)
{
   int new_vmode = (int)client_data; // 0: normal; 1: tangential;

   if (VMODE != new_vmode)
   {
      if ((VMODE == 0) && (new_vmode == 1)) // from normal to tangent:
      {
	 v_FOV = v_FOV*(15 + v_ALT/1000)/30;
	 // v_ALT /= 10;
	 XmScaleSetValue (scaleFOV, v_FOV);
	 XmScaleSetValue (scaleALT, v_ALT);
      }
      else if ((VMODE == 1) && (new_vmode == 0)) // from tangent to normal:
      {
	 v_FOV = v_FOV*30/(15 + v_ALT/1000);
	 // v_ALT *= 10;
	 XmScaleSetValue (scaleFOV, v_FOV);
	 XmScaleSetValue (scaleALT, v_ALT);
      }
      VMODE = new_vmode;
      setvparams (gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
      gwindow->draw();
   }
}

void
memoryCB (Widget, XtPointer client_data, XtPointer)
{
  int memorycode = (int)client_data; // #: save; 100+# recall;
  Boolean recall = FALSE;

  switch (memorycode)
  {  
     case 1:  // save 1
       MODE_1 = VMODE;
       LAT_1 = v_LAT;
       LON_1 = v_LON;
       ALT_1 = v_ALT;  
       FOV_1 = v_FOV;
       AZI_1 = v_AZI;
       saved_vu = TRUE;
       break;

     case 2: // save 2
       MODE_2 = VMODE;
       LAT_2 = v_LAT;
       LON_2 = v_LON;
       ALT_2 = v_ALT; 
       FOV_2 = v_FOV;
       AZI_2 = v_AZI;
       saved_vu = TRUE;
       break;

     case 101:  // recall 1
       VMODE = MODE_1; 
       v_LAT = LAT_1;
       v_LON = LON_1;
       v_ALT = ALT_1;
       v_FOV = FOV_1;
       v_AZI = AZI_1;
       recall = TRUE;
       break;

     case 102: // recall 2
       VMODE = MODE_2; 
       v_LAT = LAT_2;
       v_LON = LON_2;
       v_ALT = ALT_2;
       v_FOV = FOV_2;
       v_AZI = AZI_2;
       recall = TRUE;
       break;

     default:
       break;
  }

  if (recall)
  {
     setvparams (gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);

     XmScaleSetValue (scaleLAT, v_LAT);
     XmScaleSetValue (scaleLON, v_LON);
     XmScaleSetValue (scaleALT, v_ALT);
     XmScaleSetValue (scaleFOV, v_FOV);
     XmScaleSetValue (scaleAZI, v_AZI);
     
     gwindow->draw();
 
     if (vfwindow && vfwindow->get_awake())
        vfwindow->draw();
  }
}


void
changeScaleCB (Widget, char code, XmScaleCallbackStruct* call_data)
{
   switch (code)
   {
    case 'a':
      v_LAT = call_data->value;
      break;
    case 'o':
      v_LON = call_data->value;
      break;
    case 'l':
      v_ALT = call_data->value;
      break;
    case 'v':
      v_FOV = call_data->value;
      gwindow->field_of_view((float)v_FOV);
      break;
    case 'z':
      v_AZI = call_data->value;
      break;
    default:
      XtWarning ("Unknown code in changeScaleCB function.");
      return;
   }

   setvparams (gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
   gwindow->request_draw(100);

   if (vfwindow && vfwindow->get_awake())
     vfwindow->draw ();
}

void
flipCB (Widget, XtPointer, XtPointer)
{
   /*
   static Boolean default_panel = TRUE;
   
   Widget theform = XtParent(XtParent(w));
   // Level 1. Viewbar; 2. Form.

   default_panel = !default_panel;
   if (default_panel)
   {
      XtUnmapWidget (XtNameToWidget (theform, "Panel"));
      XtMapWidget (XtNameToWidget (theform, "Spanel"));
   }
   else
   {
      XtUnmapWidget (XtNameToWidget (theform, "Spanel"));
      XtMapWidget (XtNameToWidget (theform, "Panel"));
   }
   
   */
}


/* === */

void
cloudCB ()
{
   static Boolean first_cloud = TRUE;
   static Boolean on_flag = FALSE;
   
   if (first_cloud)
   {
      printf ("cloudCB is called, will use file %s...\n", cloudfile);
      cloud = new GR_Cloud (202, cloudfile);
      cloud_displist = new GR_DispList;
      gwindow->addDispList (cloud_displist, "cloud_displist");
      cloud_displist->add_object (*cloud);
      printf ("....will draw the clouds...\n");
      first_cloud = FALSE;
   }
   else
   {
      if (!on_flag)
	 cloud_displist->delete_objects ();
      else
      {
         gwindow->addDispList (cloud_displist, "cloud_displiat");
	 // to ensure the "cloud_displist" is always attached even after 
         // the main reset button is pressed -- added 07/09/93 by Y.Tung;
 
         cloud_displist->add_object (*cloud);
         printf ("....will draw the clouds...\n");
      }
      on_flag = !on_flag;
   }
   gwindow->draw ();
}

