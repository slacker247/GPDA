/************************************************************
  vf.C is for view finder used in rsd.C.
  
  -- 12/07/92: created by Tung; 
  -- 02/18/93: basic features including "view box" work;
               adding drawing area for azimuth, ...;
  -- 07/09/93: change map file path from NULL (using hard-coded default)
               to one that depends on the env vars DATADIR and
               FRAMEWORKHOME.

************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "GR_Shell.H"
#include "GR_Lines.H"			// Was GR_2Dlines.C
#include "GISP_Obj.H"
#include "GR_work.H"
#include "GR_Map2D.H"
#include "GR_MapPoint.H"
#include "GR_Image.H"
#include "gltk.h"

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

#define RE      6378.145
#define DEGRAD  57.29577951308232
#define RADDEG  0.0174532925199432958

//#define MW_XMIN                 (-1.0)
//#define MW_XMAX                 (1.0)
//#define MW_YMIN                 (-1.0)
//#define MW_YMAX                 (1.0)

/* ---------------------------------------------------------------------- */

typedef struct {
  int           id;
  float         x;
  float         y;
} TRACKPT;

typedef struct {
  int           id;
  int           count;
  short         r;
  short         g;
  short         b;
  GR_MapPoint   *trail;
  GR_Image      *object;
} TRACKID;                

/* ---------------------------------------------------------------------- */

int             mapwindW=640, mapwindH=480;
int             FlatMapVisible = FALSE;
int             FlatMapInited = FALSE;
float           MAP_XMIN, MAP_XMAX, MAP_XDEL;
float           MAP_YMIN, MAP_YMAX, MAP_YDEL;

int             map_ids = 0;
int             map_pnts = 0;
TRACKPT         mappoints[500];
TRACKID         mapids[100];

TK_RGBImageRec  *mapimage;
IMAGE           *mapsized;

TK_RGBImageRec  *aegisimage;
IMAGE           *aegis;
float           aegisLon, aegisLat = -10.0;

GR_Window	*mapwindow;
GR_Map2D        *flat_map;
GR_DispList     *back_displist;
GR_DispList	*map_displist;

Widget		map_shell;
Widget		map_form;
Widget		map_frame;
Widget		map_area, map_reset;

extern GR_Window *gwindow;
extern Widget	scaleLAT, scaleLON, scaleFOV, scaleALT;
extern int	VMODE, v_LAT, v_LON, v_FOV, v_ALT, v_AZI;
extern void setvparams (GR_Window* win, int vmode,
			int lat, int lon, int alt, int fov, int azi);

/* ---------------------------------------------------------------------- */

void FlatMap(char* texfile, int areano, float lat, float lon);
void FlatMapInit();
void FlatMapAdd(int type, int icon, float x, float y, float z,
                short r, short g, short b);
void FlatMapDraw();

void map_zoominCB ();
void map_zoomoutCB ();
void map_doneCB (Widget, XtPointer, XtPointer);
void map_viewCB (Widget, XtPointer, XtPointer);
void map_areaCB (Widget, XtPointer, XmDrawingAreaCallbackStruct*);

void recpol(float x, float y, float *r, float *a)
{

}

void recpol3d(float x, float y, float z, float *r, float *az, float *ax)
{

}

void rot3d(int axis, float x1, float y1, float z1, float ang,
           float *x2, float *y2, float *z2)
{

}

void polrec(float r, float a, float *x, float *y)
{
  *x = r*cos(a);
  *y = r*sin(a);
}

void polrec3d(float r, float az, float ax, float *x, float *y, float *z)
{
float           zz, xx, yy, rxy;

  polrec(r, az, &zz, &rxy);
  *z = zz;
  polrec(rxy, ax, &xx, &yy);
  *x = xx;
  *y = yy;
}

void ll2rb(float lng1, float lat1, float lng2, float lat2, float *dist, float *azi)
{
float           raddeg = M_PI/180.0;
float           x1, x2, x3;
float           y1, y2, y3;
float           z1, z2, z3;
float           d, r, ax;

  polrec3d(1.0, (90.0-lat2)/raddeg, lng2/raddeg, &x1, &y1, &z1);
  rot3d(3, x1, y1, z1, -(180.0-lng1)/raddeg, &x2, &y2, &z2);
  rot3d(2, x2, y2, z2, -(90.0-lat1)/raddeg, &x3, &y3, &z3);
  recpol3d(x3, y3, z3, &r, &d, &ax);

  *azi = fmod((360.0 - ax*raddeg), 360.0);
  *dist = d;
}

/* ---------------------------------------------------------------------- */

void  FlatMap(char* texfile, int areano, float lat, float lon)
{
FILE*           MAPfp;
int             i, j, found, n_areas;
float           latNW, lonNW, latSE, lonSE;
float           theta, phi;
float           rad = 1.000;
float           v[3];
float           degree = 10.0;
float           d2r = M_PI/180.0;
char            *DATADIR;
char            *FWDIR;
char            mapfile[128];
char            fileName[64];
char            chname[64];

   if (!FlatMapInited) {
      FlatMapInit();
   }

   lat = 39.45;
   lon = 126.07;

   if ((MAPfp = fopen("AreaMaps.dat", "r")) == NULL) {
      perror("AreaMaps.dat");
      return;
   }

   found = FALSE;
   fscanf(MAPfp, "%d\n", &n_areas);

   for (j=0; j<n_areas; j++) {
     fscanf(MAPfp, "%s %f %f %f %f %s\n", chname, &lonNW, &latNW, &lonSE, &latSE, fileName);
     if (areano == 0) {                 // Look for area containing (lat,lon)
       if ((lat <= latNW) && (lat >= latSE) && (lon >= lonNW) && (lon <= lonSE)) {
         found = TRUE;
         break;
       }
     } else {                           // Look for area number 'areano'
       if (j+1 == areano) {
         found = TRUE;
         break;
       }
     }
   }
   fclose(MAPfp);

   if (!found) {
     fprintf(stderr, "Requested area(=%d) not in database\n", areano);
     return;
   }
   fprintf(stderr, "%s %f %f %f %f %s\n", chname, lonNW, latNW, lonSE, latSE, fileName);
//
//   Found correct area for map, generate the flat map background object
//
   if ((DATADIR=getenv("DATADIR")) == NULL) {
        DATADIR = "../RSD_Data";
   }
   sprintf (mapfile, "%s/%s", DATADIR, fileName);
   flat_map = new GR_Map2D(mapfile, 329, mapwindW, mapwindH, lat, lon);
   flat_map->set_corners(lonNW, latNW, lonSE, latSE);
   back_displist->add_object (*flat_map);

   mapwindow->set_viewmode (GR_ORTHO2);
   mapwindow->left (lonNW*RADDEG);
   mapwindow->right (lonSE*RADDEG);
   mapwindow->bottom (latSE*RADDEG);
   mapwindow->top (latNW*RADDEG);
   mapwindow->near (-1.0);
   mapwindow->far (1.0);
   mapwindow->set_awake (TRUE);

   aegisimage = tkRGBImageLoad("Aegis.rgb");
   aegis = (IMAGE *)malloc(sizeof(IMAGE));
   aegis->xsize = 32;
   aegis->ysize = 32;
   aegis->tmp   = (unsigned char *)malloc(aegis->xsize*aegis->ysize*4);
   gluScaleImage(GL_RGB,
          aegisimage->sizeX, aegisimage->sizeY, GL_UNSIGNED_BYTE, aegisimage->data,
          aegis->xsize, aegis->ysize, GL_UNSIGNED_BYTE, aegis->tmp);
   free(aegisimage->data);
   free(aegisimage);

   map_pnts = 0;

   XtPopup(map_shell, XtGrabNone);

   FlatMapVisible = TRUE;

   mapwindow->draw();
}

void FlatMapInit()
{
Widget          vf_control, done_button, view_button, azi_area_frame;
Widget          zoom_in, zoom_out;
Widget          vertsep, titlewidget, topsep, trailwidget, botsep, tmpsep;
XmString        *xstr, title, classtitle;
GR_Lines        *vf_map;
XGCValues       gcv;
GC              gc;
XColor          color, unused;
Pixel           bg_color, fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
Pixel           bg_yellow;
Colormap        cmap;
Display         *dpy;
XFontStruct     *font;
XmFontList      fontlist;


   map_shell = XtVaCreatePopupShell("FlatMap",
               topLevelShellWidgetClass, GR_toplevel,
               XmNwidth,            mapwindW,
               XmNheight,           mapwindH+120,
               NULL);
   map_form = XmCreateForm (map_shell, "MapForm", NULL, 0);

   dpy = XtDisplay(GR_toplevel);
   font = XLoadQueryFont(dpy,
          "-adobe-courier-bold-*-*-*-20-*-*-*-*-*-*-*");
   fontlist = XmFontListCreate(font, "charset1");

   XtVaGetValues(map_form, XmNcolormap, &cmap, NULL);
   XAllocNamedColor(XtDisplay(map_form), cmap, "red", &color, &unused);
   bg_color = color.pixel;
   XmGetColors(XtScreen(map_form), cmap, bg_color, &fg_ret, &top_shadow,
               &bottom_shadow, &select_color);
   XAllocNamedColor(XtDisplay(map_form), cmap, "blue", &color, &unused);
   fg_color = color.pixel;
   XAllocNamedColor(XtDisplay(map_form), cmap, "yellow", &color, &unused);
   bg_yellow = color.pixel;

   classtitle   = XmStringCreateLtoR("Unclassified", "charset1");
   titlewidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass, map_form,
                 XmNheight,           20,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNshadowThickness,  2,
                 XmNtopAttachment,    XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   topsep = XtVaCreateManagedWidget("TopSep", xmSeparatorWidgetClass, map_form,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        titlewidget,
                 NULL);
   trailwidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass, map_form,
                 XmNheight,           20,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 NULL);
   botsep = XtVaCreateManagedWidget("BotSep", xmSeparatorWidgetClass, map_form,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     trailwidget,
                 NULL);
   XmStringFree(classtitle);
//
// --------------------------   Menu Bar   ---------------------------
//   
   XmString logo       = XmStringCreateSimple("File");
   XmString views      = XmStringCreateSimple("View");
   XmString help       = XmStringCreateSimple("Help");

   Widget menubar = XmVaCreateSimpleMenuBar (map_form, "MapMenu",
		XmVaCASCADEBUTTON, logo,         'F',
                XmVaCASCADEBUTTON, views,        'V',
                XmVaCASCADEBUTTON, help,         'H',
		NULL);
   XtVaSetValues (menubar,
		 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
		 XmNleftAttachment,   XmATTACH_FORM,
		 XmNrightAttachment,  XmATTACH_FORM,
		 XmNtopOffset,        2,
		 XmNleftOffset,       5,
                 XmNspacing,          5,
		 NULL);
   XmStringFree (logo);
   XmStringFree (views);
   XmStringFree (help);  
//
// --------------------------   File menu   --------------------------
//
   XmString quit    = XmStringCreateSimple ("Quit");
   Widget menupane = XmVaCreateSimplePulldownMenu (menubar, "MapFile", 0, map_doneCB,
              XmVaPUSHBUTTON, quit,    NULL, NULL, NULL,
              NULL);
   XmStringFree (quit);
//
// --------------------------   View menu   --------------------------
//
   XmString redraw   = XmStringCreateSimple ("Redraw");
   XmString zoomin   = XmStringCreateSimple ("Normal View");
   XmString zoomout  = XmStringCreateSimple ("Tangential");

   menupane = XmVaCreateSimplePulldownMenu (menubar, "MapView", 1, map_viewCB,
              XmVaPUSHBUTTON, redraw, NULL, NULL, NULL,
              XmVaPUSHBUTTON, zoomin, NULL, NULL, NULL,
              XmVaPUSHBUTTON, zoomout, NULL, NULL, NULL,
              NULL);
   XmStringFree (redraw);
   XmStringFree (zoomin);
   XmStringFree (zoomout);

   XtManageChild(menubar);

/*
   done_button = XtVaCreateManagedWidget ("Quit", xmPushButtonWidgetClass, map_form,
               XmNwidth,            80,
               XmNheight,           40,
               XmNshadowThickness,  4,
               XmNleftAttachment,   XmATTACH_FORM,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);
   XtAddCallback (done_button, XmNactivateCallback, (XtCallbackProc)map_doneCB, NULL);
  
   view_button = XtVaCreateManagedWidget ("Set View", xmPushButtonWidgetClass, map_form,
               XmNwidth,            80,
               XmNheight,           40,
               XmNshadowThickness,  4,
               XmNleftAttachment,   XmATTACH_WIDGET,
               XmNleftWidget,       done_button,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);
   XtAddCallback (view_button, XmNactivateCallback, (XtCallbackProc)map_viewCB, NULL);

   zoom_in = XtVaCreateManagedWidget ("Zoom In", xmPushButtonWidgetClass, map_form,
               XmNwidth,            80,
               XmNheight,           40,
               XmNshadowThickness,  4,
               XmNleftAttachment,   XmATTACH_WIDGET,
               XmNleftWidget,       view_button,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);
   XtAddCallback (zoom_in, XmNactivateCallback, (XtCallbackProc)map_zoominCB, NULL);
 
   zoom_out = XtVaCreateManagedWidget ("Zoom Out", xmPushButtonWidgetClass, map_form,
               XmNwidth,            80,
               XmNheight,           40,
               XmNshadowThickness,  4,
               XmNleftAttachment,   XmATTACH_WIDGET,
               XmNleftWidget,       zoom_in,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);
   XtAddCallback (zoom_out, XmNactivateCallback, (XtCallbackProc)map_zoomoutCB, NULL);
  
   map_area = XtVaCreateManagedWidget ("AZI Area", xmDrawingAreaWidgetClass, azi_area_frame,
	       XmNwidth,            110, // that's what "View Box" takes;
	       XmNheight,           36,
	       XmNshadowThickness,  2,
               NULL);
   XtAddCallback (map_area, XmNinputCallback, (XtCallbackProc)map_areaCB, NULL);

   gcv.foreground = BlackPixelOfScreen (XtScreen(map_area));

   gc = XCreateGC (XtDisplay(map_area),
                   RootWindowOfScreen(XtScreen(map_area)),
                   GCForeground, &gcv);
   XtVaSetValues (map_area, XmNuserData, gc, NULL);
*/
   map_frame = XtVaCreateManagedWidget ("MapFrame", xmFrameWidgetClass, map_form,
               XmNshadowType,       XmSHADOW_IN,
               XmNtopAttachment,    XmATTACH_WIDGET,
               XmNtopWidget,        menubar,
               XmNleftAttachment,   XmATTACH_FORM,
               XmNrightAttachment,  XmATTACH_FORM,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);

   mapwindow = new GR_Window ();
   mapwindow->doublebuffer ();
   mapwindow->rgbmode ();
   mapwindow->settop(GR_toplevel, GR_appcontext, map_frame);
   mapwindow->GR_Widget::createWidget ("MapWindow", map_frame);
   mapwindow->set_drawfunction(FlatMapDraw);
//
//   Build all the object display lists
//
   map_displist = new GR_DispList;
   back_displist = new GR_DispList;
   mapwindow->addDispList (map_displist, "map_displist");
   mapwindow->addDispList (back_displist);

   XtManageChild (map_form);

   FlatMapInited = TRUE;
}

void FlatMapAdd(int icontype, int trkid, float lat, float lon, float alt,
                short r, short g, short b)
{
int             i, j, idindex;
double          Rmax;
double          sloc[3], tloc[3];
//double          lat, lon;
float           scale_factor, sfactor2, rx, ry, rz;
float           vmag2;
float           Px, Py, Pz;
float           d2r = M_PI/180.0;

   Py = lat*d2r;
   Px = lon*d2r;
   Pz = 0.0;

//
//   Find the object id, if it exists in the table
//
   idindex = 0;
   int id_found = FALSE;
   for (i=0; i<map_ids; i++) {
      if (trkid == mapids[i].id) {
         id_found = TRUE;
         idindex = i;
         break;
      }
   }
//
//   Track id not in table, put it in
//
   if (!id_found) {
     mapids[map_ids].id = trkid;
     mapids[map_ids].r = r;
     mapids[map_ids].g = g;
     mapids[map_ids].b = b;
     mapids[map_ids].trail = new GR_MapPoint(map_displist, trkid, icontype, r, g, b);
     idindex = map_ids;
     map_ids++;;
   }
   mapids[idindex].count++;
   //
   //   Save the point for plotting
   //
   mapids[idindex].trail->push_tpoint(Px, Py, Pz);
   mappoints[map_pnts].id = trkid;
   mappoints[map_pnts].x = Px;
   mappoints[map_pnts].y = Py;
   map_pnts++;
   //
   if (icontype == 70) {
     fprintf(stderr, "Found Mig\n");
     //aegisLat = Py;
     //aegisLon = Px;
     mapids[idindex].object = new GR_Image("../BitMaps/icon070.rgb", trkid, 32, 32, lat, lon);
     map_displist->add_object (*mapids[idindex].object);
   }
   if (icontype == 49) {
     fprintf(stderr, "Found TEL\n");
     //aegisLat = Py;
     //aegisLon = Px;
     mapids[idindex].object = new GR_Image("../BitMaps/icon049.rgb", trkid, 32, 32, lat, lon);
     map_displist->add_object (*mapids[idindex].object);
   }
   //
   //   Redraw the map
   //
   mapwindow->draw();
}

void FlatMapDraw ()
{
float           spoint[2], epoint[2];
int             xsize, ysize;
unsigned long   *lptr;
unsigned short  *base, *ibuf, *abuf;
int             i, j, y, x;
float           point[3];
float           londel, latdel;
float           d2r = M_PI/180.0;
int             gridnumber;
float           theta, phi;
float           rad = 1.000;
float           v[3];
float           degree = 10.0;

   if (!FlatMapInited) return;

   GR_pushattributes();
   GR_pushmatrix();

   if (aegisLat > 0.0) {
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   //gluOrtho2D(0.0, mapwindW, 0.0, mapwindH);
   gluOrtho2D(124.5*RADDEG, 129.66*RADDEG, 36.33*RADDEG, 39.66*RADDEG);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
  
   point[0] = aegisLon; //(mapwindW / 2) - (aegis->xsize / 2);
   point[1] = aegisLat; //(mapwindH / 2) - (aegis->ysize / 2);
   point[2] = 0;
   glRasterPos3fv(point); 
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
   glPixelZoom(1.0, 1.0);
   glDrawPixels(aegis->xsize, aegis->ysize, GL_RGB, GL_UNSIGNED_BYTE,
                aegis->tmp);
   }
   /*
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(124.5*RADDEG, 129.66*RADDEG, 36.33*RADDEG, 39.66*RADDEG);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   GR_linewidth (2);
   GR_color(255, 255, 255);
   for (j=0; j<map_ids; j++) {
     if (mapids[j].count > 1) {
       GR_color(mapids[j].r, mapids[j].g, mapids[j].b);
       glBegin(GL_LINE_STRIP);
       for (i=0; i<map_pnts; i++)
	 if (mappoints[i].id == mapids[j].id) glVertex2f(mappoints[i].x, mappoints[i].y);
       glEnd();
     }
   }

   glFlush(); 
   */
   GR_popmatrix();
   GR_popattributes ();
}

void map_doneCB (Widget, XtPointer, XtPointer)
{
   XtPopdown(map_shell);
   mapwindow->set_awake (FALSE);
   FlatMapVisible = FALSE;
}

void
map_zoominCB()
{
   MAP_XDEL = (MAP_XMAX-MAP_XMIN)/2.0;
   MAP_YDEL = (MAP_YMAX-MAP_YMIN)/2.0;
   MAP_XMIN = MAP_XMIN + (MAP_XDEL/2.0);
   MAP_XMAX = MAP_XMAX - (MAP_XDEL/2.0);
   MAP_YMIN = MAP_YMIN + (MAP_YDEL/2.0);
   MAP_YMAX = MAP_YMAX - (MAP_YDEL/2.0);

   mapwindow->draw ();
}

void
map_zoomoutCB()
{
   MAP_XDEL = (MAP_XMAX-MAP_XMIN)/2.0;
   MAP_YDEL = (MAP_YMAX-MAP_YMIN)/2.0;
   MAP_XMIN = MAP_XMIN - (MAP_XDEL/2.0);
   MAP_XMAX = MAP_XMAX + (MAP_XDEL/2.0);
   MAP_YMIN = MAP_YMIN - (MAP_YDEL/2.0);
   MAP_YMAX = MAP_YMAX + (MAP_YDEL/2.0);

   mapwindow->draw ();
}

#define max(a,b) (a)>(b)?(a):(b)
#define absdif(a,b) (a)>(b)?(a-b):(b-a) 

#define Nconst 105 // factor to get FOV if VMODE is N;
#define Tconst 80 // factor to get FOV if VMODE is T;

void
map_viewCB (Widget, XtPointer, XtPointer)
{
  short x1, y1, x2, y2;
  long xmax, ymax;
  float xratio, yratio;
  int FOVconst;
  
  if (mapwindow)
  {
     mapwindow->GR_Window::mouse_getRectangle (x1,y1,x2,y2,0);
printf("getRectangle: %d %d %d %d\n", x1, y1, x2, y2);
     GR_getsize (&xmax, &ymax);
     xmax = mapwindW;
     ymax = mapwindH;
     
     v_LON = (int)( ((float)(x1+x2)/xmax -1) * 180 ); 
     v_LAT = (int)( (1 - (float)(y1+y2)/ymax) * 90 );
     if (VMODE == 0)
       v_ALT = 6400; // assign some standard value;
     else
       v_ALT = 640;
     
     //xratio = (float)(absdif(x1,x2)) / xmax * 2;
     //yratio = (float)(absdif(y1,y2)) / ymax; 
     
     // to reflect that the full globe view width is ~ 180 deg near equator
     // and ~ 360 deg near poles: similar idea for LAT; 
     xratio = (float)(absdif(x1,x2)) / xmax * 
                (2 - (absdif(v_LAT,0))/90.0); 
     yratio = (float)(absdif(y1,y2)) / ymax * 
                (1.8 - 0.8*(absdif(v_LAT,0))/90.0); 

     if (xratio > 1)
        xratio = 1;
     if (yratio > 1)
        yratio = 1;

     if (VMODE == 0)
       FOVconst = Nconst;
     else
       FOVconst = Tconst;
     
     v_FOV = (int) ( (max(xratio,yratio)) * FOVconst );
     if (v_FOV < 2)
        v_FOV = 2;

     //printf ("   LON=%d, LAT=%d, xratio=%f, yratio=%f, FOV=%d\n",
     //   v_LON, v_LAT, xratio, yratio, v_FOV); 

     if (gwindow) {
	setvparams (gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
	XmScaleSetValue (scaleLAT, v_LAT);
	XmScaleSetValue (scaleLON, v_LON);
	XmScaleSetValue (scaleALT, v_ALT);
	XmScaleSetValue (scaleFOV, v_FOV);
	gwindow->draw ();                   /* IRIX may need gwindow->GR_Window::draw (); */
     }
     else
       printf ("Warning: gwindow non-existing?\n");
     
     mapwindow->draw ();                     /* IRIX may need vfwindow->GR_Window::draw (); */
     glBegin(GL_LINE_LOOP);
        glVertex2f(x1, y1);
        glVertex2f(x2, y1);
        glVertex2f(x2, y2);
        glVertex2f(x1, y2);
     glEnd();
  }
  else
     printf (" Warning: vfwindow non-existing?\n");
}

void
map_areaCB (Widget w, XtPointer, XmDrawingAreaCallbackStruct* cbs)
{
   int x, y;
   int centerx=55, centery=30;  // hard-coded for testing;
   XEvent *event = cbs->event;
   int theta;

   if (event->xany.type == ButtonPress)
   {
      //x = event->xbutton.x;
      //y = event->xbutton.y;
      //printf (" press on x=%d, y=%d;\n", x, y);
      XClearWindow (event->xany.display, XtWindow(w));
   }
   else if (event->xany.type == ButtonRelease)
   {
      x = event->xbutton.x;
      y = event->xbutton.y;
      //printf (" release on x=%d, y=%d;\n", x, y);

      theta = (int) - (atan((x-centerx)*1.0/(centery-y)) * 180 / M_PI);
      if (y < centery)
         theta += 180;
      printf (" azimuth angle is %d\n", theta);
      GC gc;
      XtVaGetValues (w, XmNuserData, &gc, NULL);
      XDrawLine (event->xany.display, cbs->window, gc, x, y, centerx, centery);
   }
}
