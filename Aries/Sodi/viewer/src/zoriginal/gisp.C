/***********************************************************************
 
 Graphics for Interactive SPEEDES
 -- 06/03/93: created, Y. Tung;
 -- 06/04/93: using overloaded speedes/view control panel;
 -- 06/22/93: used FRAMEWORKHOME env var, if DATADIR, BITMAPDIR, MODELDIR
              are missing;
 -- 07/06/93: added Host User interface;
 -- 07/12/93: tried GR_Tearth2;
 -- 07/12/93: added GR_Tearth3;
 -- 07/26/93: changed the panel look again;
 
************************************************************************/ 

#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <osfcn.h>
#include <sys/types.h>
//#include <malloc.h>

#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/SeparatoG.h>
#include <Xm/TextF.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <GL/GLwDrawA.h>

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
#include "GR_Bearth.H"
#include "GR_Carib.H"
#include "GR_Cloud.H"
#include "GR_work.H"

extern void view_finderCB ();
extern GR_Window *vfwindow;
extern void model_viewerCB ();
extern GR_Window *mvwindow;

#define RE 6378.145
#define descfile "gispModels.desc"

char *DEBUG;
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
Widget makeViewBar (char*, Widget);
Widget makePanel (char*, Widget);
extern Widget makeSpanel (char*, Widget); // SPEEDES panel
extern speedes_init ();

Widget makeList (char*, Widget);
Widget createOneScale (Widget, char, XmString, XtCallbackProc);
Widget scaleLAT, scaleLON, scaleALT, scaleFOV;

void logoCB (Widget, XtPointer, XtPointer);
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
void flipCB (Widget, XtPointer client_data, XtPointer);
void changeScaleCB (Widget, char, XmScaleCallbackStruct*);

int v_LAT=0;
int v_LON=0;
int v_ALT=2500;
int v_FOV=132;
int v_AZI=0;
int VMODE=0; // 0: normal; 1: tangential; 

   int		gridobjid;
   Display	*dpy;
   XtAppContext GR_sppcontext;
   Widget	/*GR_toplevel,*/ frame, glxwidget;
   Bool		direct;
   XVisualInfo	*visinfo;
   GLXContext	glxcontext;
   int		attribs[] = {GLX_RGBA, GLX_DOUBLEBUFFER, None};

void setvparams (GR_Window* win, int vmode,
		 int lat, int lon, int alt, int fov, int azi);

//extern void spsCB ();
extern XtWorkProcId spsId;
extern Boolean spsWP (XtPointer);

extern void demoCB ();
extern XtWorkProcId demoId;
extern Boolean demoWP (XtPointer);
extern void sps_linksCB (Widget, XtPointer, XtPointer);
extern void sps_com_linksCB (Widget, XtPointer, XtPointer);
extern void sps_sen_linksCB (Widget, XtPointer, XtPointer);
void expose(Widget w, XtPointer client_data, XtPointer call);
void resize(Widget w, XtPointer client_data, XtPointer call);

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
   win->field_of_view ((float)fov);
   win->view_position(vx, vy, vz);
   win->look_vector(lx, ly, lz);
   
}



/* -------------------------- */

GR_matrix idmat={{1,0,0,0}, {0,1,0,0}, {0,0,1,0}, {0,0,0,1},};	      

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
   "*.l.maximum: 60000",
   "*.l.value: 9200",
   "*.v.minimum: 1",
   "*.v.maximum: 180",
   "*.v.value: 62",
   "*XmMessageBox*foreground: white",
   "*XmMessageBox*background: steelblue",
   "*Logo.foreground: steelblue4",
   NULL
};


/* -------------------------- */

GR_Model                *models, *Searth33, *Adiz222;
GR_DispList		*displist;
GR_Window               *gwindow;

GR_Blueearth            *Bearth200;
GR_Tearth  		*Tearth300;
GR_Tearth2  		*Tearth301;
GR_Tearth3  		*Tearth302;

GR_Lines                *ShoreLines320, *PoliLines321;
GR_Gridlines            *Grid330;

long                    current_earth_type = 0;

Boolean                 first_Area[36][18];
GR_Area                 *Area[36][18];

Carib                   *carib;

//------------------------ MAIN -------------------------------------

void GR_initialize (int argc, char *argv[]);
void Gridlines(short R, short G, short B, int degree, long type);

void
main (int argc, char *argv[])
{
   printf("\n\n");
   printf("  -- Graphics for Interactive SPEEDES Processing (GISP) --\n");
   printf("                  (OpenGL Version 0.1)");
   printf("\n\n");

   DEBUG = getenv("GISPDEBUG");
   GR_fallback_resources = fallback_resources; 
/*
   GR_startup (argc, argv);
*/
   GR_initialize (argc, argv);
   XtAppMainLoop (GR_appcontext);
}

void resize(Widget w, XtPointer client_data, XtPointer call)
{
}
/* ---------------------------------- */

void
GR_initialize (int argc, char *argv[])
{
   GR_Shell		*shell;
   Widget 		form, menubar, panel, frame;
   Widget               viewbar;
   Widget               spanel;
   char			*DATADIR;
   char			*FWDIR;
   
   if ((DATADIR=getenv("DATADIR")) == NULL)
	DATADIR = "../RSD_Data";
   
   sprintf (caribdir, "%s", DATADIR);
   sprintf (earthdir, "%s", DATADIR);
   sprintf (elevdir,  "%s", DATADIR);
   sprintf (shorefile, "%s%s", DATADIR, "/World.bin");
   sprintf (polifile, "%s%s", DATADIR, "/WorldPoli.bin");
   sprintf (texturefile, "%s%s", DATADIR, "/jpl_earth.rgb");
   sprintf (rgbfile, "%s%s", DATADIR, "/4320earth.rgb");
   sprintf (cloudfile, "%s%s", DATADIR, "/cloud.bw");

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

   GR_toplevel = XtOpenApplication(&GR_appcontext, "gisp", NULL, 0, &argc, argv,
                 GR_fallback_resources, applicationShellWidgetClass, NULL, 0);
   dpy = XtDisplay(GR_toplevel);
   frame = XmCreateFrame(GR_toplevel, "frame", NULL, 0);
   XtManageChild(frame);

//   shell = new GR_Shell;
//   shell->createWidget ("XRSD"); // Note, name is still argv[0];

//   form = XmCreateForm (shell->widget(), "Form", NULL, 0);
   form = XmCreateForm(frame, "Form", NULL, 0);
   XtManageChild (form);
    
   menubar = makeMenuBar("Menubar", form);
   XtVaSetValues (menubar,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNtopOffset, 2,
		  XmNleftOffset, 5,
		  NULL);

   viewbar = makeViewBar("Viewbar", form);
   XtVaSetValues (viewbar,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopOffset, 2,
		  XmNrightOffset, 10,
		  NULL);

   // one panel for view control:
   panel = makePanel("Panel", form);
   XtVaSetValues (panel,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomOffset, 2,
		  XmNleftOffset, 10,
		  NULL);
 
   // another panel for speedes interface...
   spanel = makeSpanel("Spanel", form);
   XtVaSetValues (spanel,
                  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget, panel,
                  XmNleftAttachment, XmATTACH_FORM,
                  XmNrightAttachment, XmATTACH_FORM,
                  XmNbottomOffset, 2,
                  XmNleftOffset, 10,
                  NULL);
 
   frame = XtVaCreateManagedWidget ("Frame",
                  xmFrameWidgetClass, form,
                  XmNshadowType, XmSHADOW_ETCHED_IN,
                  XmNshadowThickness, 3,
                  NULL);
   XtVaSetValues (frame,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, menubar,
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget, spanel,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopOffset, 2,
		  XmNbottomOffset, 2,
		  XmNleftOffset, 25,
		  XmNrightOffset, 25,
		  NULL);

//	Check for an OpenGL capable visual, open it, and link OpenGL
//	to it.

   if (!(visinfo = glXChooseVisual(dpy, DefaultScreen(dpy), attribs)))
       XtAppError(GR_appcontext, " No suitable RGB visual");
   glxwidget = XtVaCreateManagedWidget("glxwidget", glwDrawingAreaWidgetClass,
               frame, GLwNvisualInfo, visinfo, XtNwidth, 620,
               XtNheight, 400, NULL);

   XtAddCallback(glxwidget, GLwNexposeCallback, expose, NULL);
   XtAddCallback(glxwidget, GLwNresizeCallback, resize, NULL);

   XtRealizeWidget(GR_toplevel);
   glxcontext = glXCreateContext(dpy, visinfo, 0, GL_TRUE);
   direct = glXIsDirect(dpy, glxcontext);
   GLwDrawingAreaMakeCurrent(glxwidget, glxcontext);
   glMatrixMode(GL_PROJECTION);
   glOrtho(0, 620, 0, 400, 0, 1);
   glMatrixMode(GL_MODELVIEW);
   glClearColor(0.5, 0.5, 0.5, 0.0);

   gwindow = new GR_Window();
   gwindow->doublebuffer ();
   gwindow->rgbmode ();

//   gwindow->GR_Widget::createWidget("Gwindow", frame);

   gwindow->set_viewmode (GR_PERSPECTIVE);
   gwindow->aspect(1);
   gwindow->near(0.1);
   gwindow->far(100.0);
   displist = new GR_DispList;
   gwindow->addDispList (displist);

//   shell->realize ();

   VMODE = 0;
   v_LAT = 0;
   v_LON = 0;
   v_ALT = 9200;
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

   focusCB (NULL, (XtPointer)1, NULL);		// North America 
   lightingCB(NULL, (XtPointer)1, NULL);	// Moon light
//   earthgridCB(NULL, (XtPointer)0, NULL);	// Grid lines
   Gridlines (55,55,0,10,330);

   //XtUnmapWidget (XtNameToWidget (form, "Panel"));

//   speedes_init ();		// Initialize SPEEDES

}

void
expose(Widget w, XtPointer client_data, XtPointer call)
{
   glClear(GL_COLOR_BUFFER_BIT);
   glPushMatrix();
   glTranslatef(310.0, 200.0, 0.0);
//  glScalef(200.0, 200.0, 1.0);
//  glTranslatef(0.0, 1.2, 0.0);
   glColor3f(1.0, 1.0, 0.0);
   glPushMatrix();
//  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
//  glRotatef(45.0, 0.0, 0.0, 1.0);
//  glRectf(-0.5, -0.5, 1.0, 1.0);
   GR_circ(0.0, 0.0, 100.0);
   glScalef(25.0, 25.0, 0.0);
//   gwindow->draw();
   GR_callobj (gridobjid);
   glPopMatrix();
   glPopMatrix();
   GLwDrawingAreaSwapBuffers(glxwidget);
   glFinish();
} 
 
void Gridlines(short R, short G, short B, int degree, long type)
{
   int i, j;
   int gridnumber;
   float theta, phi;
   float rad = 1.005;
   float v[3];
   long  p_type;

   if (degree < 5)
      degree = 5;
   else if (degree > 90)
      degree = 90;
   gridnumber = 180/degree;
   theta = degree * M_PI / 180;

   p_type = type;
   gridobjid = GR_genobj ();
   GR_makeobj (gridobjid);
   GR_color (R,G,B);
   for (i=0; i<gridnumber; i++)
   {
      GR_pushmatrix ();
      GR_rotate (i*degree*10, 'y');
      GR_circ (0.0, 0.0, rad);
      GR_popmatrix ();
   }

   GR_pushmatrix ();
   for (i=0; i<(gridnumber/2); i++)
   {
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18)
     {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18)
     {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=-sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();
   }
   GR_popmatrix ();
   GR_closeobj ();
}

/* ------ major widget components ------ */

Widget
makeMenuBar (char* name, Widget parent)
{
   Widget v_menubar;
   Widget menupane;
   Widget submenu;
   Widget dummywidget;
   XmString on, off;
   XmString logo, finder, demo, reset, quit;
   XmString focus, afr, nam, mideast, fareast, cam_th, mideast_th, nes_th;
   XmString earth, type, boundarylines, area, grid;
   XmString lighting, sun, moon, lightoff;
   XmString background, original, black, darkred, grey;
   XmString weather, cloud;
   //XmString help, mouse, panel_1, panel_2;
   XmString links, all_links_on, all_links_off;
   XmString com_links, sen_links;

   // ----- menu bar itself -----
   
   logo = XmStringCreateSimple("Gisp");
// program = XmStringCreateSimple("Program");
   focus = XmStringCreateSimple("Focus");
   earth = XmStringCreateSimple("Earth");
   lighting = XmStringCreateSimple("Lighting");
   background = XmStringCreateSimple("Background");
   weather = XmStringCreateSimple("Weather");
   links = XmStringCreateSimple("Links");
// help = XmStringCreateSimple("Help");

   v_menubar = XmCreateMenuBar (parent, name, NULL, 0);
   XtVaSetValues (v_menubar,
                  XmNspacing, 5,
                  NULL);  

   XtManageChild (v_menubar);
 
   // ----- logo menu -----;
   finder = XmStringCreateSimple ("View Finder");
   demo   = XmStringCreateSimple ("Demo");
   reset  = XmStringCreateSimple ("Reset");
   quit   = XmStringCreateSimple ("Quit");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane0", 0,
              logoCB,
              XmVaPUSHBUTTON, finder, NULL, NULL, NULL,
              XmVaPUSHBUTTON, demo, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, reset, NULL, NULL, NULL,
              XmVaSEPARATOR,
              XmVaPUSHBUTTON, quit, NULL, NULL, NULL,
              NULL);
   XmStringFree (finder);
   XmStringFree (demo);
   XmStringFree (reset);
   XmStringFree (quit); 

   Widget logo_b =   
   XtVaCreateManagedWidget ("Logo", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, logo,
                            XmNmnemonic, 'A',
                            XmNsubMenuId, menupane,
                            NULL);

   Pixel fg, bg;
   XtVaGetValues (logo_b, XmNforeground, &fg, XmNbackground, &bg, NULL);

   char *BITMAPDIR;
   char *FWDIR;
   char logofile[80];
     
   if ((BITMAPDIR=getenv("BITMAPDIR"))==NULL)
   {
      if ((FWDIR=getenv("FRAMEWORKHOME")) == NULL)
	BITMAPDIR = "../../data/bitmaps";
      else
      {
	 BITMAPDIR = new char[80];
	 sprintf (BITMAPDIR, "%s/data/bitmaps", FWDIR);
      }
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
   if (dummywidget = XtNameToWidget (menupane, "button_1"))
      XtVaSetValues (dummywidget, XmNset, True, NULL);

  // ----- earth menu ------;
   type = XmStringCreateSimple ("Type");
   area = XmStringCreateSimple ("Area Earth");
   boundarylines = XmStringCreateSimple ("Boundary");
   grid = XmStringCreateSimple ("Grid");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane3", 3,
              earthCB,
              XmVaCASCADEBUTTON, type, NULL, 
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, area, NULL, 
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, boundarylines, NULL,
              XmVaSEPARATOR,
              XmVaCASCADEBUTTON, grid, NULL,
              NULL);
   XmStringFree (type);
   XmStringFree (area);
   XmStringFree (boundarylines);
   XmStringFree (grid);
 
   XtVaCreateManagedWidget ("Earth", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, earth,
                            XmNmnemonic, 'E',
                            XmNsubMenuId, menupane,
                            NULL);

   // ..... start earth's submenus ................
   XmString texture, texture2, texture3, simple, blue, none;
   texture = XmStringCreateSimple ("Texture -- Low");  
   texture2 = XmStringCreateSimple ("Texture -- Med");  
   texture3 = XmStringCreateSimple ("Texture -- Hi");  
   simple = XmStringCreateSimple ("Polygon");  
   blue = XmStringCreateSimple ("Blue Earth");  
   none = XmStringCreateSimple ("None");  
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

   //XmString on, off;          // defined in header;
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
  
   on = XmStringCreateSimple ("Grid On");
   off = XmStringCreateSimple ("Grid Off");
   submenu = XmVaCreateSimplePulldownMenu (menupane, "Submenu6", 6,
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
  
   /*
   // ----- Help menu ----
   mouse = XmStringCreateSimple ("Mouse Operations");
   panel_1 = XmStringCreateSimple ("View Control");
   panel_2 = XmStringCreateSimple ("Simulation Control");
   menupane = XmVaCreateSimplePulldownMenu (v_menubar, "Menupane7", 7,
              helpCB,
              XmVaPUSHBUTTON, mouse, NULL, NULL, NULL,
	      //XmVaPUSHBUTTON, panel_1, NULL, NULL, NULL,
              //XmVaPUSHBUTTON, panel_2, NULL, NULL, NULL,
              NULL);
   XmStringFree (mouse);
   XmStringFree (panel_1);
   XmStringFree (panel_2);
   XtVaCreateManagedWidget ("Help", xmCascadeButtonWidgetClass, v_menubar,
                            XmNlabelString, help,
                            XmNmnemonic, 'H',
                            XmNsubMenuId, menupane,
                            NULL);
   */
   
   // ----- Links menu ----
   all_links_on = XmStringCreateSimple ("All Links On");
   all_links_off = XmStringCreateSimple ("All Links Off");
   com_links = XmStringCreateSimple ("Com Links");
   sen_links = XmStringCreateSimple ("Sensor Links");
   menupane = XmVaCreateSimplePulldownMenu 
              (v_menubar, "Menupane7",7,
	       sps_linksCB,
	       XmVaPUSHBUTTON, all_links_on, NULL, NULL, NULL,
	       XmVaPUSHBUTTON, all_links_off, NULL, NULL, NULL,
	       XmVaSEPARATOR,
	       XmVaCASCADEBUTTON, com_links, NULL,
	       XmVaCASCADEBUTTON, sen_links, NULL,
	       NULL);
   XmStringFree (all_links_on);
   XmStringFree (all_links_off);
   XmStringFree (com_links);
   XmStringFree (sen_links);
   XtVaCreateManagedWidget ("Links", xmCascadeButtonWidgetClass, v_menubar,
			    XmNlabelString, links,
			    XmNmnemonic, 'L',
			    XmNsubMenuId, menupane,
			    NULL);
  
   // ..... start Links' submenus ................
   on = XmStringCreateSimple ("On");
   off = XmStringCreateSimple ("Off");
   submenu = XmVaCreateSimplePulldownMenu (menupane, "LinksSubmenu3", 3,
             sps_com_linksCB,
             XmVaPUSHBUTTON, on, NULL, NULL, NULL,
             XmVaPUSHBUTTON, off, NULL, NULL, NULL,
             NULL);
   submenu = XmVaCreateSimplePulldownMenu (menupane, "LinksSubmenu4", 4,
             sps_sen_linksCB,
             XmVaPUSHBUTTON, on, NULL, NULL, NULL,
             XmVaPUSHBUTTON, off, NULL, NULL, NULL,
             NULL);
   XmStringFree (on);
   XmStringFree (off); 
  
   // =========================== //

   XmStringFree (logo);
   XmStringFree (focus);
   XmStringFree (earth);
   XmStringFree (lighting);
   XmStringFree (background);
   XmStringFree (weather);
   XmStringFree (links);
   
   return (v_menubar);
}

/* ------ */


Widget
makeViewBar (char* name, Widget parent)
{
   Widget v_viewbar;
   Widget v_normal, v_tangent, v_sav1, v_sav2, v_rec1, v_rec2;
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
   {
      if ((FWDIR=getenv("FRAMEWORKHOME")) == NULL)
	BITMAPDIR = "../../data/bitmaps";
      else
      {
	 BITMAPDIR = new char[80];
         sprintf (BITMAPDIR, "%s/data/bitmaps", FWDIR);
      }
   }
   
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
                  XmNorientation, XmHORIZONTAL, 
                  XmNspacing, 10,
                  NULL);

   v_normal = XtVaCreateManagedWidget
     ("Normal",
      xmPushButtonWidgetClass, v_viewbar,
      XmNwidth, 20,
      XmNheight, 20,
      NULL);
   XtAddCallback (v_normal, XmNactivateCallback, viewCB, (XtPointer)0);
   XtVaGetValues (v_normal, XmNforeground, &fg,
		  XmNbackground, &bg, NULL);
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
makePanel (char* name, Widget parent)
{
   Widget v_panel;
   XmString title;

   v_panel = XmCreateForm (parent, name, NULL, 0);
   XtManageChild (v_panel);
   XtVaSetValues (v_panel,
		  XmNfractionBase, 100,
                  NULL);

   title = XmStringCreateSimple("Latitude");
   scaleLAT = createOneScale (v_panel, 'a', title,
			    (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleLAT,
                  XmNleftAttachment, XmATTACH_POSITION,
                  XmNleftPosition, 1,
                  XmNrightAttachment, XmATTACH_POSITION,
                  XmNrightPosition, 24,
                  NULL);
 
   title = XmStringCreateSimple("Longitude");
   scaleLON = createOneScale (v_panel, 'o', title,
			    (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleLON,
                  XmNleftAttachment, XmATTACH_POSITION,
                  XmNleftPosition, 26,
                  XmNrightAttachment, XmATTACH_POSITION,
                  XmNrightPosition, 49,
                  NULL);

   title = XmStringCreateSimple("Altitude");
   scaleALT = createOneScale (v_panel, 'l', title,
			    (XtCallbackProc)changeScaleCB);
   XtVaSetValues (scaleALT,
                  XmNleftAttachment, XmATTACH_POSITION,
                  XmNleftPosition, 51,
                  XmNrightAttachment, XmATTACH_POSITION,
                  XmNrightPosition, 74,
		  XmNscaleMultiple, 100,
                  NULL);

   title = XmStringCreateSimple("Field of View");
   scaleFOV= createOneScale (v_panel, 'v', title,
			   (XtCallbackProc)changeScaleCB);

   XmStringFree (title);
   XtVaSetValues (scaleFOV,
                  XmNleftAttachment, XmATTACH_POSITION,
                  XmNleftPosition, 76,
                  XmNrightAttachment, XmATTACH_POSITION,
                  XmNrightPosition, 99,
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
logoCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;

   switch (item_no)
   {
     case 0:    // view finder
       view_finderCB ();
       break;
     case 1:    // demo
       demoCB ();
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
focusCB (Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;

   VMODE = 0; // normal
   if (item_no <= 3)
   {
      v_ALT = 19200;
      v_FOV = 62;
   }
   else if (item_no <=7)
   {
      v_ALT = 19200;
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
   static Boolean first_302 = TRUE;
   static Boolean first_310 = TRUE;
   static Boolean first_33  = FALSE;
   static Boolean first_200 = TRUE;

   switch (item_no)
   {
     case 0:  // texture -- low
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
     case 0:  // on
       if (first_320)
       {
          first_320 = FALSE;
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

#define ALPHA		0.0
#define	AMBIENT		0.0
#define	DIFFUSE		0.0
#define EMISSION	0.0
#define SPECULAR	0.0
#define SHININESS	0.0
#define LMNULL		0.0
#define POSITION	0.0
#define LMAMBIENT	0.0
#define LOCALVIEWER	0.0
#define LCOLOR		0.0

static float mat_AMBIENT[] = { 0.3, 0.3, 0.3, 1.0 };
static float mat_DIFFUSE[] = { 0.8, 0.8, 0.8, 1.0 };
static float mat_EMISSION[] = { 0.3, 0.3, 0.3, 1.0 };
static float mat_SPECULAR[] = { 0.8, 0.8, 0.8, 1.0 };
static float mat_SHININESS[] = { 30.0 };
static float dullmat_AMBIENT[] = { 0.2, 0.2, 0.2, 1.0 };
static float dullmat_DIFFUSE[] = { 0.2, 0.2, 0.2, 0.1 };
static float dullmat_EMISSION[] = { 0.0, 0.0, 0.0, 1.0 };
static float dullmat_SPECULAR[] = { 0.1, 0.1, 0.1, 1.0 };
static float dullmat_SHININESS[] = { 1.0 };

static float sunlt_LCOLOR[] = { 1.0, 1.0, 1.0, 0.0 };
static float sunlt_POSITION[] = { 1.0, 1.0, 2.0, 0.0 };
static float moonlt_LCOLOR[] = { 0.1, 0.1, 0.6, 0.0 };
static float moonlt_POSITION[] = { 1.0, 1.0, 2.0, 0.0 };
static float nonelt_LCOLOR[] = { 0.1, 0.1, 0.6, 0.0 };
static float nonelt_POSITION[] = { 0.0, 0.0, 1.0, 0.0 };

static float lm_LMAMBIENT[] = { 0.5, 0.5, 0.5, 1.0 };
static float lm_LOCALVIEWER[] = { 1.0 };

static float dulllm_LMAMBIENT[] = { 0.075, 0.075, 0.075, 1.0 };
static float dulllm_LOCALVIEWER[] = { 1.0 };

   switch (item_no)
   {
     case 0:            // sun light:
//       if (first_sun)
//       {
//          first_sun = FALSE;
//          lmdef (DEFMATERIAL, 10, 3, mat);
//          lmdef (DEFLIGHT, 20, 3, sunlt);
//          lmdef (DEFLMODEL, 30, 3, lm);
//       }
//       lmbind (MATERIAL, 10);
//       lmbind (LIGHT0, 20);
//       lmbind (LMODEL, 30);
       glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat_AMBIENT);
       glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mat_DIFFUSE);
       glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_EMISSION);
       glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_SPECULAR);
       glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_SHININESS);
       glLightfv(GL_LIGHT0,  GL_POSITION, sunlt_POSITION);
       glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lm_LMAMBIENT);
       glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lm_LOCALVIEWER);
       break;
     case 1:            // moon light:
//       if (first_moon)
//       {
//          first_moon = FALSE;
//          lmdef (DEFMATERIAL, 11, 3, dullmat);
//          lmdef (DEFLIGHT, 21, 3, moonlt);
//          lmdef (DEFLMODEL, 31, 3, dulllm);
//          lmdef (DEFLMODEL, 30, 3, lm);
//       }
//       lmbind (MATERIAL, 11);
//       lmbind (LIGHT0, 21);
//       lmbind (LMODEL, 31);
       glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, dullmat_AMBIENT);
       glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, dullmat_DIFFUSE);
       glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, dullmat_EMISSION);
       glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, dullmat_SPECULAR);
       glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, dullmat_SHININESS);
       glLightfv(GL_LIGHT0,  GL_POSITION, moonlt_POSITION);
       glLightModelfv(GL_LIGHT_MODEL_AMBIENT, dulllm_LMAMBIENT);
       glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, dulllm_LOCALVIEWER);
       break;
//     case 2:             // light off: 
//       lmbind (MATERIAL, 0);
//       break;
     default:		   // All others, light off
       glLightfv(GL_LIGHT0,  GL_POSITION, nonelt_POSITION);
       glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lm_LMAMBIENT);
       glLightModelfv(GL_LIGHT_MODEL_LOCAL_VIEWER, lm_LOCALVIEWER);
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
  static int MODE_1=0,LAT_1=0,LON_1=0,ALT_1=19200,FOV_1=62,AZI_1=0;
  static int MODE_2=0,LAT_2=0,LON_2=0,ALT_2=19200,FOV_2=62,AZI_2=0;
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

