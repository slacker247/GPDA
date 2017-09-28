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
#include "GR_work.H"
#include "gltk.h"

#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/Label.h>
#include <Xm/DrawingA.h>
#include <Xm/Separator.h>

#define RE 6378.145

//#define MW_XMIN                 (-1.0)
//#define MW_XMAX                 (1.0)
//#define MW_YMIN                 (-1.0)
//#define MW_YMAX                 (1.0)

Widget		vf_shell;
GR_Window	*vfwindow;
Boolean		vffirst = TRUE;
extern int	VMODE, v_LAT, v_LON, v_FOV, v_ALT, v_AZI;
int             vfwindW=640, vfwindH=480;
TK_RGBImageRec  *vfimage;
IMAGE           *vfsized;
GR_DispList	*vf_displist;
char            mapfile[128];
int             glid;
float           MW_XMIN, MW_XMAX, MW_XDEL;
float           MW_YMIN, MW_YMAX, MW_YDEL;

extern GR_Window *gwindow;
extern Widget	scaleLAT, scaleLON, scaleFOV, scaleALT;
Widget		azi_area, azi_reset;
Widget		vf_form;
Widget		vf_frame;
 
void view_finderCB ();
void vfdraw ();
void vf_zoominCB ();
void vf_zoomoutCB ();
void vf_doneCB ();
void vf_viewCB ();
extern void setvparams (GR_Window* win, int vmode,
			int lat, int lon, int alt, int fov, int azi);
void azi_areaCB (Widget, XtPointer, XmDrawingAreaCallbackStruct*);

void
vf_init ()
{
Widget         vf_control, done_button, view_button, azi_area_frame;
Widget         zoom_in, zoom_out;
Widget         vertsep, titlewidget, topsep, trailwidget, botsep, tmpsep;
XmString       *xstr, title, classtitle;
GR_Lines       *vf_map;
char           *DATADIR;
char           *FWDIR;
XGCValues      gcv;
GC             gc;
XColor         color, unused;
Pixel          bg_color, fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
Pixel          bg_yellow;
Colormap       cmap;
Display        *dpy;
XFontStruct    *font;
XmFontList     fontlist;

   if ((DATADIR=getenv("DATADIR")) == NULL) {
        DATADIR = "../RSD_Data";
   }
   sprintf (mapfile, "%s%s", DATADIR, "/World.asc");

   vf_shell = XtVaCreatePopupShell("ViewFinder",
               topLevelShellWidgetClass, GR_toplevel,
               XmNwidth,            vfwindW,
               XmNheight,           vfwindH+120,
               NULL);
   vf_form = XmCreateForm (vf_shell, "VFForm", NULL, 0);

   dpy = XtDisplay(GR_toplevel);
   font = XLoadQueryFont(dpy,
          "-adobe-courier-bold-*-*-*-20-*-*-*-*-*-*-*");
   fontlist = XmFontListCreate(font, "charset1");

   XtVaGetValues(vf_form, XmNcolormap, &cmap, NULL);
   XAllocNamedColor(XtDisplay(vf_form), cmap, "red", &color, &unused);
   bg_color = color.pixel;
   XmGetColors(XtScreen(vf_form), cmap, bg_color, &fg_ret, &top_shadow,
               &bottom_shadow, &select_color);
   XAllocNamedColor(XtDisplay(vf_form), cmap, "blue", &color, &unused);
   fg_color = color.pixel;
   XAllocNamedColor(XtDisplay(vf_form), cmap, "yellow", &color, &unused);
   bg_yellow = color.pixel;

   classtitle   = XmStringCreateLtoR("Unclassified", "charset1");
   titlewidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,vf_form,
                 XmNwidth,            vfwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNshadowThickness,  2,
                 XmNtopAttachment,    XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 NULL);
   topsep = XtVaCreateManagedWidget("TopSep", xmSeparatorWidgetClass, vf_form,
                 XmNwidth,            vfwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        titlewidget,
                 NULL);
   trailwidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,vf_form,
                 XmNwidth,            vfwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 NULL);
   botsep = XtVaCreateManagedWidget("BotSep", xmSeparatorWidgetClass, vf_form,
                 XmNwidth,            vfwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     trailwidget,
                 NULL);
   XmStringFree(classtitle);

   done_button = XtVaCreateManagedWidget ("Quit", xmPushButtonWidgetClass, vf_form,
               XmNwidth,            80,
               XmNheight,           40,
               XmNshadowThickness,  4,
               XmNleftAttachment,   XmATTACH_FORM,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);
   XtAddCallback (done_button, XmNactivateCallback, (XtCallbackProc)vf_doneCB, NULL);
  
   view_button = XtVaCreateManagedWidget ("Set View", xmPushButtonWidgetClass, vf_form,
               XmNwidth,            80,
               XmNheight,           40,
               XmNshadowThickness,  4,
               XmNleftAttachment,   XmATTACH_WIDGET,
               XmNleftWidget,       done_button,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);
   XtAddCallback (view_button, XmNactivateCallback, (XtCallbackProc)vf_viewCB, NULL);

   zoom_in = XtVaCreateManagedWidget ("Zoom In", xmPushButtonWidgetClass, vf_form,
               XmNwidth,            80,
               XmNheight,           40,
               XmNshadowThickness,  4,
               XmNleftAttachment,   XmATTACH_WIDGET,
               XmNleftWidget,       view_button,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);
   XtAddCallback (zoom_in, XmNactivateCallback, (XtCallbackProc)vf_zoominCB, NULL);
 
   zoom_out = XtVaCreateManagedWidget ("Zoom Out", xmPushButtonWidgetClass, vf_form,
               XmNwidth,            80,
               XmNheight,           40,
               XmNshadowThickness,  4,
               XmNleftAttachment,   XmATTACH_WIDGET,
               XmNleftWidget,       zoom_in,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);
   XtAddCallback (zoom_out, XmNactivateCallback, (XtCallbackProc)vf_zoomoutCB, NULL);

   vf_frame = XtVaCreateManagedWidget ("VFFrame", xmFrameWidgetClass, vf_form,
               XmNshadowType,       XmSHADOW_IN,
               XmNtopAttachment,    XmATTACH_WIDGET,
               XmNtopWidget,        topsep,
               XmNleftAttachment,   XmATTACH_FORM,
               XmNrightAttachment,  XmATTACH_FORM,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     done_button,
               NULL);

   azi_area_frame = XtVaCreateManagedWidget ("AZIAreaFrame", xmFrameWidgetClass, vf_form,
               XmNshadowType,       XmSHADOW_IN,
               XmNshadowThickness,  4,
               XmNrightAttachment,  XmATTACH_FORM,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget,     botsep,
               NULL);
  
   azi_area = XtVaCreateManagedWidget ("AZI Area", xmDrawingAreaWidgetClass, azi_area_frame,
	       XmNwidth,            110, // that's what "View Box" takes;
	       XmNheight,           36,
	       XmNshadowThickness,  2,
               NULL);
   XtAddCallback (azi_area, XmNinputCallback, (XtCallbackProc)azi_areaCB, NULL);

   gcv.foreground = BlackPixelOfScreen (XtScreen(azi_area));

   gc = XCreateGC (XtDisplay(azi_area),
                   RootWindowOfScreen(XtScreen(azi_area)),
                   GCForeground, &gcv);
   XtVaSetValues (azi_area, XmNuserData, gc, NULL);

   vfwindow = new GR_Window ();
   vfwindow->doublebuffer ();
   vfwindow->rgbmode ();
   vfwindow->GR_Widget::createWidget ("VFWindow", vf_frame);
   vfwindow->set_drawfunction(vfdraw);

   vfwindow->set_viewmode (GR_ORTHO2);
   vfwindow->left (-1.0);
   vfwindow->right (+1.0);
   vfwindow->bottom (-1.0);
   vfwindow->top (+1.0);
   //vfwindow->world (MW_XMIN, MW_XMAX, MW_YMIN, MW_YMAX);

   //vfwindow->set_viewmode (GR_PERSPECTIVE);
   //vfwindow->aspect(1);
   //vfwindow->near(0.1);
   //vfwindow->far(100.0);
//
//   Build all the object display lists
//
   vf_displist = new GR_DispList;
   vfwindow->addDispList (vf_displist);
//
//   shell->realize ();
//
   /*
   VMODE = 0;
   v_LAT = 0;
   v_LON = 0;
   v_ALT = 6700;
   v_FOV = 62;
   v_AZI = 0;
   setvparams (vfwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
   */
   //vf_map = new GR_Lines (255, 255, 255, mapfile, 329);
   //vf_displist->add_object (vf_map);

   XtManageChild (vf_form);
   XtPopup(vf_shell, XtGrabNone);
}

void 
view_finderCB ()
{
char       *fileName = 0;
char       *DATADIR;
char       *FWDIR;
int        i, j;
int        gridnumber;
float      theta, phi;
float      rad = 1.000;
float      v[3];
float      degree = 10.0;    

   if (vffirst) {
      vf_init ();
      vffirst = FALSE;
   } else {
      XtPopup(vf_shell, XtGrabNone);
   }
   if ((DATADIR=getenv("DATADIR")) == NULL) {
        DATADIR = "../RSD_Data";
   }
   sprintf (mapfile, "%s%s", DATADIR, "/jpl_earth.rgb");
   printf("VF: Reading image file %s\n", mapfile);
   //work_progress (WP_CREATE, "Reading image file", (Widget)vfwindow);
   //fileName = "./RSD_Data/jpl_earth.rgb";
   vfimage = tkRGBImageLoad(mapfile);
   vfsized = (IMAGE *)malloc(sizeof(IMAGE));
   vfsized->xsize = vfwindW;
   vfsized->ysize = vfwindH;
   vfsized->tmp   = (unsigned char *)malloc(vfsized->xsize*vfsized->ysize*4);
   gluScaleImage(GL_RGB,
          vfimage->sizeX, vfimage->sizeY, GL_UNSIGNED_BYTE, vfimage->data,
          vfsized->xsize, vfsized->ysize, GL_UNSIGNED_BYTE, vfsized->tmp);
   free(vfimage->data);
   free(vfimage);
   //work_progress (WP_DONE, NULL, (Widget)vfwindow);
   //work_progress (WP_DESTROY, NULL, (Widget)vfwindow ); // remove dialog shell;

   gridnumber = 180/degree;
   theta = degree * M_PI / 180;

   glid = GR_genobj ();
   GR_makeobj (glid);
   glColor3f(0.5, 0.5, 0.0);

   GR_pushmatrix ();
   for (i=0; i<(gridnumber/2); i++) {
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18) {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();
     
     GR_bgnline();
     for (j=0, phi=0.0; j<=36; j++, phi+=M_PI/18) {
        v[0]=cos(theta*i)*cos(phi)*rad;
        v[1]=-sin(theta*i)*rad;
        v[2]=cos(theta*i)*sin(phi)*rad;
        GR_v3f(v);
     }
     GR_endline();      
   }
   GR_popmatrix ();

   GR_closeobj ();

   vfwindow->set_awake (TRUE);
   vfwindow->draw();
}


void
vf_doneCB ()
{
   XtPopdown(vf_shell);
   vfwindow->set_awake (FALSE);
   free(vfsized->tmp);
   free(vfsized);
}

void
vf_zoominCB()
{
   MW_XDEL = (MW_XMAX-MW_XMIN)/2.0;
   MW_YDEL = (MW_YMAX-MW_YMIN)/2.0;
   MW_XMIN = MW_XMIN + (MW_XDEL/2.0);
   MW_XMAX = MW_XMAX - (MW_XDEL/2.0);
   MW_YMIN = MW_YMIN + (MW_YDEL/2.0);
   MW_YMAX = MW_YMAX - (MW_YDEL/2.0);

   vfwindow->draw ();
}

void
vf_zoomoutCB()
{
   MW_XDEL = (MW_XMAX-MW_XMIN)/2.0;
   MW_YDEL = (MW_YMAX-MW_YMIN)/2.0;
   MW_XMIN = MW_XMIN - (MW_XDEL/2.0);
   MW_XMAX = MW_XMAX + (MW_XDEL/2.0);
   MW_YMIN = MW_YMIN - (MW_YDEL/2.0);
   MW_YMAX = MW_YMAX + (MW_YDEL/2.0);

   vfwindow->draw ();
}

#define max(a,b) (a)>(b)?(a):(b)
#define absdif(a,b) (a)>(b)?(a-b):(b-a) 

int Nconst=105; // factor to get FOV if VMODE is N;
int Tconst=80; // factor to get FOV if VMODE is T;

void
vf_viewCB ()
{
  short x1, y1, x2, y2;
  long xmax, ymax;
  float xratio, yratio;
  int FOVconst;
  
  if (vfwindow)
  {
     vfwindow->GR_Window::mouse_getRectangle (x1,y1,x2,y2,0);
printf("getRectangle: %d %d %d %d\n", x1, y1, x2, y2);
     GR_getsize (&xmax, &ymax);
     xmax = vfwindW;
     ymax = vfwindH;
     
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

     if (gwindow)
     {
	setvparams (gwindow, VMODE, v_LAT, v_LON, v_ALT, v_FOV, v_AZI);
	XmScaleSetValue (scaleLAT, v_LAT);
	XmScaleSetValue (scaleLON, v_LON);
	XmScaleSetValue (scaleALT, v_ALT);
	XmScaleSetValue (scaleFOV, v_FOV);
	gwindow->draw ();                   /* IRIX may need gwindow->GR_Window::draw (); */
     }
     else
       printf ("Warning: gwindow non-existing?\n");
     
     vfwindow->draw ();                     /* IRIX may need vfwindow->GR_Window::draw (); */
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
azi_areaCB (Widget w, XtPointer, XmDrawingAreaCallbackStruct* cbs)
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

void vfdraw ()
{
   float spoint[2], epoint[2];
   int xsize, ysize;
   unsigned long *lptr;
   unsigned short *base, *ibuf, *abuf;
   int y, x;
   float point[3];

   printf("Drawing flat texture for VF\n");
   GR_pushattributes ();
   GR_color (255, 0, 0);

   glViewport(0, 0, vfwindW, vfwindH);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(0.0, vfwindW, 0.0, vfwindH);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

//   glDrawBuffer(GL_FRONT_AND_BACK);
   glClear(GL_COLOR_BUFFER_BIT);

   point[0] = (vfwindW / 2) - (vfsized->xsize / 2);
   point[1] = (vfwindH / 2) - (vfsized->ysize / 2);
   point[2] = 0;
   glRasterPos3fv(point);

   glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
   glPixelZoom(1.0, 1.0);
   glDrawPixels(vfsized->xsize, vfsized->ysize, GL_RGB, GL_UNSIGNED_BYTE,
                vfsized->tmp);

   GR_callobj (glid);

   glFlush();
   GR_popattributes ();
}
