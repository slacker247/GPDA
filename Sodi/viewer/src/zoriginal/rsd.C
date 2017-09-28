/***********************************************************************

 Testing New RSD (caution: this is an unstable version);
 -- 10/15/92 created, Y. Tung;
 -- 01/14/93: experimented with new window settings; 
 -- 01/18/93: use jbl's setViewTrans and do w.o. p_change_lookat;
              vfwindow now works, but picking doesn't, and localdraw
              untested;
 -- 03/05/93: add army icons...;
 -- 03/25/93: rebuild ASF and add new ASF-EOM interfaces;
              note that this part may not always compile because ASF is buggy;
 -- 04/06/93: put clouds in;
 -- 04/12/93: rewrote view params and take out Kn/Kt to view weather, etc;
 -- 04/29/93: move major external files out;
 -- 06/08/93: take out SPEEDES and old interfaces;
 -- 06/22/93: use FRAMEWORKHOME env var if  DATADIR, BITMAPDIR, MODELDIR
              are missing;
 -- 07/20/93: tried a higher resolution earth using GR_Tearth2;
	       
************************************************************************/ 

#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <osfcn.h>
#include <sys/types.h>
#include <malloc.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/SeparatoG.h>

#include "GR_DispList.H"
#include "GR_Window.H"
#include "GR_DispObj.H"
#include "GR_Shell.H"
#include "GR_Model.H"
#include "GR_Tearth.H"
#include "GR_Tearth2.H"
#include "GR_Lines.H"
#include "GR_Eearth.H"
#include "GR_Area.H"
#include "GR_Bearth.H"
#include "GR_Carib.H"
#include "GR_Cloud.H"
#include "GR_work.H"
  
//#include "army.C"
extern void armyCB ();
//#include "test.C"
extern void test1CB ();
extern void test2CB ();
extern void test3CB ();


extern void get_aircraft_message(void*, char*, int);
extern void get_sensor_message(void*, char*, int);
extern void get_track_message(void*, char*, int);
extern void work_progress (int workstate, char* msgstr, Widget parent = GR_toplevel);

extern void view_finderCB ();
extern GR_Window *vfwindow;

extern void model_viewerCB ();
extern GR_Window *mvwindow;

#define RE 6378.145
#define descfile "rsdModels.desc"
char shorefile[80];
char polifile[80];
char texturefile[80];
char rgbfile[80];
char elevdir[80];
char caribdir[80];
char cloudfile[80];
char earthdir[80];

GR_DispList *cloud_displist;
Cloud *cloud;
void cloudCB ();

Widget makeMenuBar (char*, Widget);
Widget makePanel (char*, Widget);
Widget makeList (char*, Widget);
Widget createOneScale (Widget, char, XmString, XtCallbackProc);
Widget scaleLAT, scaleLON, scaleALT, scaleFOV;

void alphaCB (Widget, XtPointer, XtPointer);
void programCB (Widget, XtPointer, XtPointer);
void focusCB (Widget, XtPointer, XtPointer);
void earthCB (Widget, XtPointer, XtPointer);
void lightingCB (Widget, XtPointer, XtPointer);
void backgroundCB (Widget, XtPointer, XtPointer);
void weatherCB (Widget, XtPointer, XtPointer);
void helpCB (Widget, XtPointer, XtPointer);
void help_doneCB (Widget dialog, XtPointer, XtPointer);

void earthtypeCB  (Widget, XtPointer, XtPointer);
void earthareaCB  (Widget, XtPointer, XtPointer);
void earthlinesCB (Widget, XtPointer, XtPointer);
void earthadizCB  (Widget, XtPointer, XtPointer);
void earthgridCB  (Widget, XtPointer, XtPointer);

void viewCB (Widget, XtPointer client_data, XtPointer);
void memoryCB (Widget, XtPointer client_data, XtPointer);
void changeScaleCB (Widget, char, XmScaleCallbackStruct*);

int v_LAT=0;
int v_LON=0;
int v_ALT=2500;
int v_FOV=132;
int v_AZI=0;
int VMODE=0; // 0: normal; 1: tangential; 

void setvparams (GR_Window* win, int vmode,
		 int lat, int lon, int alt, int fov, int azi);

extern GR_DispList *offlinelist;

extern void asf_CB ();
extern void asfeom_CB ();
extern XtWorkProcId asf_Id;
extern XtWorkProcId asfeom_Id;
extern Boolean asf_WP (XtPointer);
extern Boolean asfeom_WP (XtPointer);

/*
  extern void oldCB ();
  extern XtWorkProcId oldId;
  extern Boolean oldWP (XtPointer);

  extern void spsCB ();
  extern XtWorkProcId spsId;
  extern Boolean spsWP (XtPointer);
*/
  
extern void demoCB ();
extern XtWorkProcId demoId;
extern Boolean demoWP (XtPointer);
 

void setvparams (GR_Window* win, int vmode,
		 int lat, int lon, int alt, int fov, int azi)
		 
{
   float vx, vy, vz;
   float lx, ly, lz;
   float p_lat, p_lon, p_alt, p_azi;
   float Kt = 2.50; // tangential view constant;
   
   p_lat = lat*M_PI/180.0;
   p_lon = lon*M_PI/180.0;
   p_alt = 1 + alt/RE;
   p_azi = azi*M_PI/180.0;
   
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
      p_alt = 2 - fexp (-alt/RE);
      vx = -Kt * sin (p_azi) * cos (p_lon)
	   + Kt * cos (p_azi) * sin (p_lat) * sin (p_lon)
	   + cos (p_lat) * sin (p_lon) * p_alt;
      vy = -Kt * cos(p_azi) * cos(p_lat)
	   + sin (p_lat) * p_alt;
      vz = Kt * sin (p_azi) * sin (p_lon)
	   + Kt * cos (p_azi) * sin (p_lat) * cos (p_lon)
	   + cos (p_lat) * cos (p_lon) * p_alt;
      
      lx = cos (p_lat) * sin (p_lon) * p_alt;
      ly = sin (p_lat) * p_alt;
      lz = cos (p_lat) * cos (p_lon) * p_alt;
      if (p_lat < 0)
	win->twist (180);      
   }
   win->field_of_view ((float)fov);
   win->view_position(vx, vy, vz);
   win->look_vector(lx, ly, lz);
   
}



void
fyi (char *str)
{
   printf ("  FYI: doing %s soon...\n", str);
}

/* -------------------------- */

Matrix idmat={{1,0,0,0}, {0,1,0,0}, {0,0,1,0}, {0,0,0,1},};	      

static float mat[] =
{
        ALPHA, 1.0,
        AMBIENT, 0.3, 0.3, 0.3,
        DIFFUSE, 0.8, 0.8, 0.8,
        EMISSION, 0.3, 0.3, 0.3,
        SPECULAR, 0.8, 0.8, 0.8,
        SHININESS, 30,
        LMNULL,
};

static float dullmat[] =
{
        ALPHA, 0.1,
        AMBIENT, 0.2, 0.2, 0.2,
        DIFFUSE, 0.2, 0.2, 0.2,
        EMISSION, 0.0, 0.0, 0.0,
        SPECULAR, 0.1, 0.1, 0.1,
        SHININESS, 1,
        LMNULL,
};

static float sunlt[] =
{
        LCOLOR, 1, 1, 1,
        POSITION, 1, 1, 2, 0,
        LMNULL
};

static float moonlt[] =
{
        LCOLOR, 0.1, 0.1, 0.6,
        POSITION, 1, 1, 2, 0,
        LMNULL
};

static float lm[] =
{
        AMBIENT, .5, .5, .5,
        LOCALVIEWER, 1,
        LMNULL
};

static float dulllm[] =
{
        AMBIENT, .075, .075, .075,
        LOCALVIEWER, 1,
        LMNULL
};

/* -------------------------- */
static 
String fallback_resources [] =
{
   "*background: wheat",
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
   "*.l.maximum: 12800",
   "*.l.value: 6400",
   "*.v.minimum: 1",
   "*.v.maximum: 180",
   "*.v.value: 62",
   "*XmMessageBox*foreground: white",
   "*XmMessageBox*background: steelblue",
   "*Alpha.foreground: firebrick4",
   NULL
};


/* -------------------------- */

GR_Model                *models, *Searth33, *Adiz222;
GR_DispList		*displist;
GR_Window               *gwindow;

GR_Blueearth            *Bearth200;
GR_Tearth  		*Tearth300;
GR_Tearth2  		*Tearth301;
GR_Eearth               *Eearth310;
GR_Lines                *ShoreLines320, *PoliLines321;
GR_Gridlines            *Grid330;

long                    current_earth_type = 0;

Boolean                 first_Area[36][18];
GR_Area                 *Area[36][18];

Carib                   *carib;

//------------------------ MAIN -------------------------------------

void GR_initialize ();

void
main (int argc, char *argv[])
{
   GR_fallback_resources = fallback_resources; 
   GR_startup (argc, argv);
   GR_initialize ();
   XtAppMainLoop (GR_appcontext);
}


/* ---------------------------------- */

void
GR_initialize ()
{
   GR_Shell		*shell;
   Widget 		form, menubar, panel, frame;
   int i, j;
   char *DATADIR;
   char *FWDIR;

   if ((DATADIR = getenv("DATADIR")) == NULL)
   {
      if ((FWDIR = getenv("FRAMEWORKHOME")) == NULL)
	DATADIR = "../../data";
      else
      {
	 DATADIR = (char*)malloc(80);
	 sprintf (DATADIR, "%s/data", FWDIR);
      }
   }
       
   sprintf (shorefile, "%s%s", DATADIR, "/World.bin");
   sprintf (polifile, "%s%s", DATADIR, "/WorldPoli.bin");
   sprintf (texturefile, "%s%s", DATADIR, "/jpl_earth.rgb");
   sprintf (rgbfile, "%s%s", DATADIR, "/4320earth.rgb");
   sprintf (caribdir, "%s", DATADIR);
   sprintf (cloudfile, "%s%s", DATADIR, "/cloud.bw");
   sprintf (earthdir, "%s", DATADIR);

   //sprintf (elevdir, "/usr/esd/tung/earth/data/");

   for (i=0; i<36; i++)
   {
      for (j=0; j<18; j++)
      {
         first_Area[i][j] = TRUE;
      }
   }

   shell = new GR_Shell;
   shell->createWidget ("XRSD"); // Note, name is still argv[0];

   form = XmCreateForm (shell->widget(), "Form", NULL, 0);
   XtManageChild (form);
    
   menubar = makeMenuBar("Menubar", form);
   XtVaSetValues (menubar,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNtopOffset, 10,
		  XmNleftOffset, 30,
		  NULL);

   panel = makePanel("Panel", form);
   XtVaSetValues (panel,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomOffset, 8,
		  XmNleftOffset, 20,
		  NULL);
  
   frame = XtVaCreateManagedWidget ("Frame",
                  xmFrameWidgetClass, form,
                  XmNshadowType, XmSHADOW_ETCHED_IN,
                  XmNshadowThickness, 3,
                  NULL);

   gwindow = new GR_Window();
   gwindow->doublebuffer ();
   gwindow->rgbmode ();

   //gwindow->createWidget("Gwindow", frame);
   gwindow->GR_Widget::createWidget("Gwindow", frame);

   XtVaSetValues (frame,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, menubar,
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget, panel,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopOffset, 10,
		  XmNbottomOffset, 10,
		  XmNleftOffset, 25,
		  XmNrightOffset, 25,
		  NULL);

   gwindow->set_viewmode (GR_PERSPECTIVE);
   gwindow->aspect(1);
   gwindow->near(0.1);
   gwindow->far(100.0);

   displist = new GR_DispList;
   gwindow->addDispList (displist);

   shell->realize ();

   VMODE = 0;
   v_LAT = 0;
   v_LON = 0;
   v_ALT = 6400;
   v_FOV = 62;
   v_AZI = 0;
   setvparams (gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
   XmScaleSetValue (scaleLAT, v_LAT);
   XmScaleSetValue (scaleLON, v_LON);
   XmScaleSetValue (scaleALT, v_ALT);
   XmScaleSetValue (scaleFOV, v_FOV);

   models = new GR_Model (descfile);

   Searth33 = new GR_Model (0, 33);
  
   if (Searth33)
   {
     displist->add_object (*Searth33);
     current_earth_type = 33;
   }
   focusCB (NULL, (XtPointer)4, NULL); // C.AM

   offlinelist = new GR_DispList;
}
 


/* ------ major widget components ------ */

Widget
makeMenuBar (char* name, Widget parent)
{
   Widget v_menubar;
   Widget menupane;
   Widget submenu;
   Widget dummywidget;
   XmString alpha, finder, modv, reset, quit;
   XmString program, asf, asfeom, army, demo;
   //XmString  old, sps;
   XmString focus, afr, nam, mideast, fareast, cam_th, mideast_th, nes_th;
   XmString earth, type, boundarylines, adiz, terrain, grid;
   XmString lighting, sun, moon, lightoff;
   XmString background, original, black, darkred, grey;
   XmString weather, cloud;
   XmString help, mouse, test1, test2, test3;

   // ----- menu bar itself -----
   
   alpha = XmStringCreateSimple("Alpha");
   program = XmStringCreateSimple("Program");
   focus = XmStringCreateSimple("Focus");
   earth = XmStringCreateSimple("Earth");
   lighting = XmStringCreateSimple("Lighting");
   background = XmStringCreateSimple("Background");
   weather = XmStringCreateSimple("Weather");
   help = XmStringCreateSimple("Help");

   /*
   v_menubar = XmVaCreateSimpleMenuBar(parent, name,
               XmVaCASCADEBUTTON, alpha, 'A',
               XmVaCASCADEBUTTON, program, 'P',
               XmVaCASCADEBUTTON, focus, 'F',
               XmVaCASCADEBUTTON, earth, 'E',
               XmVaCASCADEBUTTON, lighting, 'L',
               XmVaCASCADEBUTTON, background, 'B',
               XmVaCASCADEBUTTON, help, 'H',
               NULL);
   */

   v_menubar = XmCreateMenuBar (parent, name, NULL, 0);
   XtVaSetValues (v_menubar,
                  XmNspacing, 10,
                  NULL);  

   XtManageChild (v_menubar);
 
   // ----- alpha menu -----;
   finder = XmStringCreateSimple ("View Finder");
   modv = XmStringCreateSimple ("Model Viewer");
   reset = XmStringCreateSimple ("Reset");
   quit = XmStringCreateSimple ("Quit");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane0", 0,
              alphaCB,
              XmVaPUSHBUTTON, finder, NULL, NULL, NULL,
              XmVaPUSHBUTTON, modv, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, reset, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, quit, NULL, NULL, NULL,
              NULL);
   XmStringFree (finder);
   XmStringFree (modv);
   XmStringFree (reset);
   XmStringFree (quit); 

   Widget alpha_b =   
   XtVaCreateManagedWidget ("Alpha", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, alpha,
                            XmNmnemonic, 'A',
                            XmNsubMenuId, menupane,
                            NULL);

   Pixel fg, bg;
   XtVaGetValues (alpha_b, XmNforeground, &fg, XmNbackground, &bg, NULL);
   
   char *BITMAPDIR;
   char *FWDIR;
   char alphafile[80];
     
   if ((BITMAPDIR = getenv("BITMAPDIR")) == NULL)
   {      
      if ((FWDIR = getenv("FRAMEWORKHOME")) == NULL)
	BITMAPDIR = "../../data/bitmaps";
      else
      {
	 BITMAPDIR = (char*)malloc(80);
	 sprintf (BITMAPDIR, "%s/data/bitmaps", FWDIR);
      }
   }
   
   sprintf (alphafile, "%s/jpl_alpha.bit", BITMAPDIR);
      
   Pixmap pixmap =
     XmGetPixmap(XtScreen(alpha_b), alphafile, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     printf("   Couldn't load bitmap file %s.\n", alphafile);
   else
     XtVaSetValues (alpha_b,
		    XmNlabelType, XmPIXMAP,
		    XmNlabelPixmap, pixmap,
		    NULL);


   // ----- program menu ------;
   asf = XmStringCreateSimple ("ASF");
   asfeom = XmStringCreateSimple ("ASF - EOM");
   //old = XmStringCreateSimple ("Old Sim");
   //sps = XmStringCreateSimple ("Speedes");
   army = XmStringCreateSimple ("Army");
   demo = XmStringCreateSimple ("Demo");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane1", 1,
              programCB,
              XmVaPUSHBUTTON, asf, NULL, NULL, NULL,
              XmVaPUSHBUTTON, asfeom, NULL, NULL, NULL,
	      //XmVaPUSHBUTTON, old, NULL, NULL, NULL,
              //XmVaPUSHBUTTON, sps, NULL, NULL, NULL,
              XmVaPUSHBUTTON, army, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, demo, NULL, NULL, NULL,
              NULL);
   XmStringFree (asf);
   XmStringFree (asfeom);
   //XmStringFree (old);
   //XmStringFree (sps);
   XmStringFree (army);
   XmStringFree (demo);

   XtVaCreateManagedWidget ("Program", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, program,
                            XmNmnemonic, 'P',
                            XmNsubMenuId, menupane,
                            NULL);

   // ----- focus menu ------;
   afr = XmStringCreateSimple ("Africa, (0,0)");
   nam = XmStringCreateSimple ("N. America");
   mideast = XmStringCreateSimple ("Middle East");
   fareast = XmStringCreateSimple ("Far East");
   cam_th = XmStringCreateSimple ("Cent. Am Theater");
   mideast_th = XmStringCreateSimple ("Mid. East Theater");
   nes_th = XmStringCreateSimple ("N.E. Siberia Theater");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane2", 2,
              focusCB,
              XmVaRADIOBUTTON, afr, NULL, NULL, NULL,
              XmVaRADIOBUTTON, nam, NULL, NULL, NULL,
              XmVaRADIOBUTTON, mideast, NULL, NULL, NULL,
              XmVaRADIOBUTTON, fareast, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaRADIOBUTTON, cam_th, NULL, NULL, NULL,
              XmVaRADIOBUTTON, mideast_th, NULL, NULL, NULL,
              XmVaRADIOBUTTON, nes_th, NULL, NULL, NULL,
              XmNradioBehavior, True,
              XmNradioAlwaysOne, True,
              NULL);
   XmStringFree (afr);
   XmStringFree (nam);
   XmStringFree (mideast);
   XmStringFree (fareast);
   XmStringFree (mideast_th);
   XmStringFree (cam_th);
   XtVaCreateManagedWidget ("Focus", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, focus,
                            XmNmnemonic, 'F',
                            XmNsubMenuId, menupane,
                            NULL);
   if (dummywidget = XtNameToWidget (menupane, "button_4"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

  // ----- earth menu ------;
   type = XmStringCreateSimple ("Type");
   terrain = XmStringCreateSimple ("Area Earth");
   boundarylines = XmStringCreateSimple ("Boundary");
   adiz = XmStringCreateSimple ("ADIZ");
   grid = XmStringCreateSimple ("Grid");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane3", 3,
              earthCB,
              XmVaCASCADEBUTTON, type, NULL, 
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, terrain, NULL,
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, boundarylines, NULL,
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, adiz, NULL,
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, grid, NULL,
              NULL);
   XmStringFree (type);
   XmStringFree (terrain);
   XmStringFree (boundarylines);
   XmStringFree (adiz);
   XmStringFree (grid);
 
   XtVaCreateManagedWidget ("Earth", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, earth,
                            XmNmnemonic, 'E',
                            XmNsubMenuId, menupane,
                            NULL);

   // ..... start earth's submenus ................
   XmString texture_low;
   XmString texture_med;
   //XmString elevation;
   XmString simple, blue, none;
   texture_low = XmStringCreateSimple ("Texture - low");  
   texture_med = XmStringCreateSimple ("Texture - med");  
   //elevation = XmStringCreateSimple ("Elevation");
   simple = XmStringCreateSimple ("Polygon");  
   blue = XmStringCreateSimple ("Blue Earth");  
   none = XmStringCreateSimple ("None");  
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu0", 0,
             earthtypeCB,
             XmVaRADIOBUTTON, texture_low, NULL, NULL, NULL,
             XmVaRADIOBUTTON, texture_med, NULL, NULL, NULL,
	     //XmVaRADIOBUTTON, elevation, NULL, NULL, NULL,
             XmVaRADIOBUTTON, simple, NULL, NULL, NULL,
             XmVaRADIOBUTTON, blue, NULL, NULL, NULL,
             XmVaRADIOBUTTON, none, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   XmStringFree (texture_low);
   XmStringFree (texture_med);
   //XmStringFree (elevation);
   XmStringFree (simple);
   XmStringFree (blue);
   XmStringFree (none);
   if (dummywidget = XtNameToWidget (submenu, "button_2"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   XmString load_carib, unload_carib, load, unload;
   load_carib = XmStringCreateSimple ("Load Caribbean");
   unload_carib = XmStringCreateSimple ("Unload Caribbean");
   load = XmStringCreateSimple ("Load");
   unload = XmStringCreateSimple ("Unload");
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu2", 2,
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

   XmString on, off;
   on = XmStringCreateSimple ("On");
   off = XmStringCreateSimple ("Off");
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu4", 4,
             earthlinesCB,
             XmVaRADIOBUTTON, on, NULL, NULL, NULL,
             XmVaRADIOBUTTON, off, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   if (dummywidget = XtNameToWidget (submenu, "button_1"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);
  
   on = XmStringCreateSimple ("ADIZ On");
   off = XmStringCreateSimple ("ADIZ Off");
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu6", 6,
             earthadizCB,
             XmVaRADIOBUTTON, on, NULL, NULL, NULL,
             XmVaRADIOBUTTON, off, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   if (dummywidget = XtNameToWidget (submenu, "button_0"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   on = XmStringCreateSimple ("Grid On");
   off = XmStringCreateSimple ("Grid Off");
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu8", 8,
             earthgridCB,
             XmVaRADIOBUTTON, on, NULL, NULL, NULL,
             XmVaRADIOBUTTON, off, NULL, NULL, NULL,
             XmNradioBehavior, True,
             XmNradioAlwaysOne, True,
             NULL);
   XmStringFree (on);
   XmStringFree (off);
   if (dummywidget = XtNameToWidget (submenu, "button_1"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   // ----- lighting menu ------;
   sun = XmStringCreateSimple ("Sun Light");
   moon = XmStringCreateSimple ("Moon Light");
   lightoff = XmStringCreateSimple ("Light Off");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane4", 4,
              lightingCB,
              XmVaRADIOBUTTON, sun, NULL, NULL, NULL,
              XmVaRADIOBUTTON, moon, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaRADIOBUTTON, lightoff, NULL, NULL, NULL,
              XmNradioBehavior, True,
              XmNradioAlwaysOne, True,
              NULL);
   XmStringFree (sun);
   XmStringFree (moon);
   XmStringFree (lightoff);

   XtVaCreateManagedWidget ("Lighting", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, lighting,
                            XmNmnemonic, 'L',
                            XmNsubMenuId, menupane,
                            NULL);
   if (dummywidget = XtNameToWidget (menupane, "button_2"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   // ----- background menu ------;
   original = XmStringCreateSimple ("Original");
   black = XmStringCreateSimple ("Black");
   darkred = XmStringCreateSimple ("Dark Red");
   grey = XmStringCreateSimple ("Grey");
   
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane5", 5,
              backgroundCB,
              XmVaRADIOBUTTON, original, NULL, NULL, NULL,
              XmVaRADIOBUTTON, black, NULL, NULL, NULL,
              XmVaRADIOBUTTON, darkred, NULL, NULL, NULL,
              XmVaRADIOBUTTON, grey, NULL, NULL, NULL,
              XmNradioBehavior, True,
              XmNradioAlwaysOne, True,
              NULL);
  
   XmStringFree (original);
   XmStringFree (black);
   XmStringFree (darkred);
   XmStringFree (grey);

   XtVaCreateManagedWidget ("Background", xmCascadeButtonWidgetClass,
			    v_menubar,
                            XmNlabelString, background,
                            XmNmnemonic, 'B',
                            XmNsubMenuId, menupane,
                            NULL);
   if (dummywidget = XtNameToWidget (menupane, "button_1"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

   // -----Weather menu ----
   cloud = XmStringCreateSimple ("Clouds");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane6", 6, 
              weatherCB,
              XmVaPUSHBUTTON, cloud, NULL, NULL, NULL,
              NULL);
   XmStringFree (cloud);
   XtVaCreateManagedWidget ("Weather", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, weather,
                            XmNmnemonic, 'W',
                            XmNsubMenuId, menupane,
                            NULL);

   // ----- Help menu ----
   mouse = XmStringCreateSimple ("Mouse Operations");
   test1 = XmStringCreateSimple ("test1");
   test2 = XmStringCreateSimple ("test2");
   test3 = XmStringCreateSimple ("test3");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane7", 7,
              helpCB,
              XmVaPUSHBUTTON, mouse, NULL, NULL, NULL,
	      //XmVaPUSHBUTTON, test1, NULL, NULL, NULL,
              //XmVaPUSHBUTTON, test2, NULL, NULL, NULL,
              //XmVaPUSHBUTTON, test3, NULL, NULL, NULL,
              NULL);
   XmStringFree (mouse);
   XmStringFree (test1);
   XmStringFree (test2);
   XmStringFree (test3);
   XtVaCreateManagedWidget ("Help", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, help,
                            XmNmnemonic, 'H',
                            XmNsubMenuId, menupane,
                            NULL);

   // ===========================

   XmStringFree (alpha);
   XmStringFree (program);
   XmStringFree (focus);
   XmStringFree (earth);
   XmStringFree (lighting);
   XmStringFree (background);
   XmStringFree (weather);
   XmStringFree (help);
   
   return (v_menubar);
}


Widget
makePanel (char* name, Widget parent)
{
   Widget v_panel, v_view, v_memory;
   Widget v_normal, v_tangent, v_sav1, v_sav2, v_rec1, v_rec2;
   XmString title;

   Pixel fg, bg;
   Pixmap pixmap;
   char *BITMAPDIR;
   char *FWDIR;
   char normalbit[80], tangentbit[80];
   char sav1bit[80], rec1bit[80], sav2bit[80], rec2bit[80];
 
   if ((BITMAPDIR = getenv("BITMAPDIR")) == NULL)
   {
      if ((FWDIR = getenv("FRAMEWORKHOME")) == NULL)
        BITMAPDIR = "../../data/bitmaps";
      else
      {
	 BITMAPDIR = (char*)malloc(80);
	 sprintf (BITMAPDIR, "%s/data/bitmaps", FWDIR);
      }
   }
   
   sprintf (normalbit, "%s%s", BITMAPDIR, 
            "/normal.bit");
   sprintf (tangentbit, "%s%s", BITMAPDIR, 
            "/tangent.bit"); 
   sprintf (sav1bit, "%s%s", BITMAPDIR, 
            "/sav1.bit"); 
   sprintf (rec1bit, "%s%s", BITMAPDIR, 
            "/rec1.bit"); 
   sprintf (sav2bit, "%s%s", BITMAPDIR, 
            "/sav2.bit"); 
   sprintf (rec2bit, "%s%s", BITMAPDIR, 
            "/rec2.bit"); 

   v_panel = XmCreateForm (parent, name, NULL, 0);
   XtManageChild (v_panel);
   XtVaSetValues (v_panel,
		  XmNfractionBase, 100,
                  NULL);

   /*
     v_demo_button = XtVaCreateManagedWidget ("Demo",
                        xmPushButtonWidgetClass, v_panel,
                        //XmNwidth, 50,
                        XmNheight, 50,
                        //XmNleftAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        NULL);
   XtAddCallback (v_demo_button, XmNactivateCallback, demoCB, NULL);
   */
   
   v_view = XmCreateRowColumn (v_panel, "View", NULL, 0);
   XtManageChild (v_view);
   XtVaSetValues (v_view,
		  XmNleftAttachment, XmATTACH_FORM,
		  //XmNleftAttachment, XmATTACH_POSITION,
                  //XmNleftPosition, 0,
		  //XmNrightAttachment, XmATTACH_POSITION,
		  //XmNrightPosition, 8,
                  XmNorientation, XmVERTICAL, 
                  XmNspacing, 3,
                  NULL);


   v_normal = XtVaCreateManagedWidget ("Normal",
                        xmPushButtonWidgetClass, v_view,
                        XmNwidth, 20,
                        XmNheight, 20,
                        NULL);
   XtAddCallback (v_normal, XmNactivateCallback, viewCB, (XtPointer)0);
   XtVaGetValues (v_normal, XmNforeground, &fg,
                            XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_normal), normalbit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     //printf ("  Cannot find bitmap file %s\n", normalbit);
     ;
   else
     XtVaSetValues (v_normal,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);
   
   v_tangent = XtVaCreateManagedWidget ("Tangent",
                        xmPushButtonWidgetClass, v_view,
                        XmNwidth, 20,
                        XmNheight, 20,
                        NULL);
   XtAddCallback (v_tangent, XmNactivateCallback, viewCB, (XtPointer)1);
   XtVaGetValues (v_normal, XmNforeground, &fg,
                            XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_normal), tangentbit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     //printf ("  Cannot find bitmap file %s\n", tangentbit);
     ;
   else
     XtVaSetValues (v_tangent,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);

   v_memory = XmCreateRowColumn (v_panel, "Memory", NULL, 0);
   XtManageChild (v_memory);
   XtVaSetValues (v_memory,
                  XmNleftAttachment, XmATTACH_WIDGET,
                  XmNleftWidget, v_view,
                  //XmNleftAttachment, XmATTACH_POSITION,
                  //XmNleftPosition, 9,
		  //XmNrightAttachment, XmATTACH_POSITION,
		  //XmNrightPosition, 15,
                  XmNpacking, XmPACK_COLUMN, 
                  XmNnumColumns, 2,
                  XmNorientation, XmVERTICAL,
                  XmNspacing, 3,
                  NULL);

   v_sav1 = XtVaCreateManagedWidget ("SV1",
                        xmPushButtonWidgetClass, v_memory,
                        XmNwidth, 20,
                        XmNheight, 20,
                        NULL);
   XtAddCallback (v_sav1, XmNactivateCallback, memoryCB, (XtPointer)1);
   XtVaGetValues (v_sav1, XmNforeground, &fg,
                          XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_sav1), sav1bit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     //printf ("  Cannot find bitmap file %s\n", sav1bit);
     ;
   else
     XtVaSetValues (v_sav1,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);

   v_rec1 = XtVaCreateManagedWidget ("RC1",
                        xmPushButtonWidgetClass, v_memory,
                        XmNwidth, 20,
                        XmNheight, 20,
                        NULL);
   XtAddCallback (v_rec1, XmNactivateCallback, memoryCB, (XtPointer)101);
   XtVaGetValues (v_rec1, XmNforeground, &fg,
                          XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_rec1), rec1bit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     //printf ("  Cannot find bitmap file %s\n", rec1bit);
     ;
   else
     XtVaSetValues (v_rec1,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);

   v_sav2 = XtVaCreateManagedWidget ("SV2",
                        xmPushButtonWidgetClass, v_memory,
                        XmNwidth, 20,
                        XmNheight, 20,
                        NULL);
   XtAddCallback (v_sav2, XmNactivateCallback, memoryCB, (XtPointer)2);
   XtVaGetValues (v_sav2, XmNforeground, &fg,
                          XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_sav2), sav2bit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     //printf ("  Cannot find bitmap file %s\n", sav2bit);
     ;
   else
     XtVaSetValues (v_sav2,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);

   v_rec2 = XtVaCreateManagedWidget ("RC2",
                        xmPushButtonWidgetClass, v_memory,
                        XmNwidth, 20,
                        XmNheight, 20,
                        NULL);
   XtAddCallback (v_rec2, XmNactivateCallback, memoryCB, (XtPointer)102);
   XtVaGetValues (v_rec2, XmNforeground, &fg,
                          XmNbackground, &bg, NULL);
   pixmap = XmGetPixmap (XtScreen (v_rec2), rec2bit, fg, bg);
   if (pixmap == XmUNSPECIFIED_PIXMAP)
     //printf ("  Cannot find bitmap file %s\n", rec2bit);
     ;
   else
     XtVaSetValues (v_rec2,
                    XmNlabelType, XmPIXMAP,
                    XmNlabelPixmap, pixmap,
                    NULL);


   title = XmStringCreateSimple("Latitude");
   scaleLAT = createOneScale (v_panel, 'a', title,
			    (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleLAT,
                  XmNleftAttachment, XmATTACH_POSITION,
                  XmNleftPosition, 15,
                  XmNrightAttachment, XmATTACH_POSITION,
                  XmNrightPosition, 35,
                  NULL);
 
   title = XmStringCreateSimple("Longitude");
   scaleLON = createOneScale (v_panel, 'o', title,
			    (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleLON,
                  XmNleftAttachment, XmATTACH_POSITION,
                  XmNleftPosition, 36,
                  XmNrightAttachment, XmATTACH_POSITION,
                  XmNrightPosition, 56,
                  NULL);

   title = XmStringCreateSimple("Altitude");
   scaleALT = createOneScale (v_panel, 'l', title,
			    (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleALT,
                  XmNleftAttachment, XmATTACH_POSITION,
                  XmNleftPosition, 57,
                  XmNrightAttachment, XmATTACH_POSITION,
                  XmNrightPosition, 77,
		  XmNscaleMultiple, 100,
                  NULL);

   title = XmStringCreateSimple("Field of View");
   scaleFOV= createOneScale (v_panel, 'v', title,
			   (XtCallbackProc)changeScaleCB);

   XmStringFree (title);
   XtVaSetValues (scaleFOV,
                  XmNleftAttachment, XmATTACH_POSITION,
                  XmNleftPosition, 78,
                  XmNrightAttachment, XmATTACH_POSITION,
                  XmNrightPosition, 98,
                  NULL);
  
   return (v_panel);
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
   XtSetArg (args[n], XmNorientation, XmHORIZONTAL); n++;
   XtSetArg (args[n], XmNshowValue, True); n++;
   XtSetArg (args[n], XmNtitleString, title); n++;
   XtSetArg (args[n], XmNscaleMultiple, 5); n++;
   scale = XmCreateScale (parent, name, args, n);

   XtAddCallback (scale, XmNvalueChangedCallback, CB, (XtPointer)code);
   XtAddCallback (scale, XmNdragCallback, CB, (XtPointer)code);
   
   XtManageChild (scale);

   return (scale);
}


/* ---------------- main menu CB's ----------------- */
void
alphaCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;

   switch (item_no)
   {
     case 0:    // view finder
       view_finderCB ();
       break;
     case 1:    // model viewer
       model_viewerCB ();
       break;
     case 2:    // reset
       /*
       delete displist;
       displist = new GR_DispList;
       gwindow->addDispList (displist);
       */
       gwindow->rem_all_namedDispLists ();
       break;
     case 3:   // quit
       exit (0);
       break;
     default:
       break;
   } 
}

void
programCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   
   switch (item_no)
   {
     case 0:   
       asf_CB ();
       break;
     case 1:
       asfeom_CB ();
       break;
     /*
       case 2: 
       oldCB (); // oldsim panel;
       break;
       case 3:    // Sps panel;
       spsCB ();  
       break;
       */
     case 2:   // army button;
       armyCB ();
       break;
     case 3:   // demo;
       demoCB ();
       break;
   }
}

void
focusCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;

   VMODE = 0; // normal
   if (item_no <= 3)
   {
      v_ALT = 6400;
      v_FOV = 62;
   }
   else if (item_no <=7)
   {
      v_ALT = 6400;
      v_FOV = 42;
   }
   switch (item_no)
   {
    case 0:        // AFR, (0,0):
      v_LAT = 0;
      v_LON = 0;
      break;
    case 1:        // N.AM:   
      v_LAT = 37;
      v_LON = -102;
      break;
    case 2:        // M.East:
      v_LAT = 33;
      v_LON = 45;
      break;
    case 3:        // F.East:
      v_LAT = 30;
      v_LON = 120;
      break;
    case 4:        // C.AM Theater:
      v_LAT = 23;
      v_LON = -85;
      break;
    case 5:        // M.East Theater:
      v_LAT = 29;
      v_LON = 47;
      break;
    case 6:        // NE.Siberia Theater:
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
   static Boolean first_310 = TRUE;
   static Boolean first_33  = FALSE;
   static Boolean first_200 = TRUE;

   switch (item_no)
   {
     case 0:  // texture
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
     case 1:   // texture2
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
       /*
     case 1:   // elevation
       if (current_earth_type!=310)
       {
          if (first_310)
          {
             first_310 = FALSE;
             Eearth310 = new GR_Eearth (elevdir, 310);
          }
          if (displist->inlist_by_type (current_earth_type))
             displist->delete_object_by_type (current_earth_type);
          displist->add_object (*Eearth310);
          current_earth_type = 310;
       }
       break;
       */
     case 2:   // simple
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
     case 3:   // blue 
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

     case 4:   // none
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
      break;
      */
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
     case 0:  // on
       if (first_320)
       {
          first_320 = FALSE;
	  //work_progress (0, "Reading boundary lines", gwindow->widget());
	  work_progress (0, "Reading boundary lines");
          PoliLines321  = new GR_Lines (255,255,255,polifile,321); 
             printf ("\n......\n\n"); 
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
     case 1:  // off
       if (displist->inlist_by_type (320))
       {
          displist->delete_object_by_type (320);
          displist->delete_object_by_type (321);
       }
       break;
     default:
       break;
   }
}

              
void
earthadizCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   static Boolean first_222 = TRUE;

   switch (item_no)
   {
     case 0:  // on
       if (first_222)
       {
          first_222 = FALSE;
          Adiz222 = new GR_Model (1, 222);
          Adiz222->rotate_x (270);
          Adiz222->rotate_y (270);
       }
       if (!displist->inlist_by_type (222))
          displist->add_object (*Adiz222);
       break;
     case 1:  // off
       if (displist->inlist_by_type (222))
          displist->delete_object_by_type (222);
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

   switch (item_no)
   {
     case 0:  // on
       if (first_330)
       {
          first_330 = FALSE;
          Grid330 = new GR_Gridlines (55,55,0,10,330);
       }
       if (!displist->inlist_by_type (330))
          displist->add_object (*Grid330);
       break;
     case 1:  // off
       if (displist->inlist_by_type (330))
          displist->delete_object_by_type (330);
       break;
     default:
       break;
   }
}




void 
lightingCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   static Boolean first_sun = TRUE;
   static Boolean first_moon= TRUE;

   switch (item_no)
   {
     case 0:            // sun light:
       if (first_sun)
       {
          first_sun = FALSE;
          lmdef (DEFMATERIAL, 10, 3, mat);
          lmdef (DEFLIGHT, 20, 3, sunlt);
          lmdef (DEFLMODEL, 30, 3, lm);
       }
       lmbind (MATERIAL, 10);
       lmbind (LIGHT0, 20);
       lmbind (LMODEL, 30);
       break;
     case 1:            // moon light:
       if (first_moon)
       {
          first_moon = FALSE;
          lmdef (DEFMATERIAL, 11, 3, dullmat);
          lmdef (DEFLIGHT, 21, 3, moonlt);
          lmdef (DEFLMODEL, 31, 3, dulllm);
          //lmdef (DEFLMODEL, 30, 3, lm);
       }
       lmbind (MATERIAL, 11);
       lmbind (LIGHT0, 21);
       lmbind (LMODEL, 31);
       break;
     case 2:             // light off: 
       lmbind (MATERIAL, 0);
       break;
     default:
       break;
    }
}

extern unsigned long WidgetBackgroundToGlRgb (Widget);

void
backgroundCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   long org_background;

   if (gwindow)
   {
   switch (item_no)
   {
     case 0:       // original:
          org_background = WidgetBackgroundToGlRgb (gwindow->widget());
          gwindow->color(org_background);
       break;
     case 1:        // black:
          gwindow->color(0,0,0); 
       break;
     case 2:        // dark red:
          gwindow->color(10,0,4);
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
helpCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;
   Widget idialog;
   XmString helptext;
   Arg args[2];
   char* helpstr;
 
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

      case 1: // for testing only:
	test1CB ();
	break;
      case 2: // for testing only:
	test2CB ();
	break;
      case 3: // for testing only:
	test3CB ();
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
viewCB (Widget, XtPointer client_data, XtPointer)
{
   int new_vmode = (int)client_data; // 0: normal; 1: tangential;

   if (VMODE != new_vmode)
   {
      if ((VMODE == 0) && (new_vmode == 1))
      {
	 v_FOV = v_FOV*2/3;
	 v_ALT /= 10;
	 XmScaleSetValue (scaleFOV, v_FOV);
	 XmScaleSetValue (scaleALT, v_ALT);
      }
      else if ((VMODE == 1) && (new_vmode == 0))
      {
	 v_FOV = v_FOV*3/2;
	 v_ALT *= 10;
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
  static int MODE_1=0,LAT_1=0,LON_1=0,ALT_1=6400,FOV_1=62,AZI_1=0;
  static int MODE_2=0,LAT_2=0,LON_2=0,ALT_2=6400,FOV_2=62,AZI_2=0;
  Boolean recall=FALSE;

  switch (memorycode)
  {  
     case 1:  // save 1
       MODE_1 = VMODE;
       LAT_1 = v_LAT;
       LON_1 = v_LON;
       ALT_1 = v_ALT;  
       FOV_1 = v_FOV;
       AZI_1 = v_AZI;
       break;
     case 2: // save 2
       MODE_2 = VMODE;
       LAT_2 = v_LAT;
       LON_2 = v_LON;
       ALT_2 = v_ALT; 
       FOV_2 = v_FOV;
       AZI_2 = v_AZI;
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
     
     gwindow->draw();
 
     if (vfwindow && vfwindow->get_awake())
        vfwindow->draw();
  }
}


void
changeScaleCB (Widget, char client_data, XmScaleCallbackStruct* call_data)
{
   char code = client_data;

   /*
   if (call_data->reason == XmCR_DRAG)
     printf ("   changeScaleCB: dragging...\n");
   else
     printf ("   changeScaleCB: value change ...\n");
   */

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
    default:
      XtWarning ("Unknown code in changeScaleCB function.");
      return;
   }

   setvparams (gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
   gwindow->request_draw(100);

   if (vfwindow && vfwindow->get_awake())
     vfwindow->draw ();
}

/* ==== */

void
cloudCB ()
{
   static Boolean first_cloud = TRUE;
   static Boolean on_flag = FALSE;
   
   if (first_cloud)
   {
      printf ("cloudCB is called, will use file %s...\n", cloudfile);
      cloud = new Cloud (202, cloudfile);
      cloud_displist = new GR_DispList;
      gwindow->addDispList (cloud_displist, "cloud_displiat");
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
	 cloud_displist->add_object (*cloud);
         printf ("....will draw the clouds...\n");
      }
      on_flag = !on_flag;
   }
   gwindow->draw ();
}

