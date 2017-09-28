/************************************************************
  vf.C is for view finder used in rsd.C.
  
  -- 12/07/92: created by Tung; 
  -- 02/18/93: basic features including "view box" work;
               adding drawing area for azimuth, ...;
  -- 07/09/93: change map file path from NULL (using hard-coded default)
               to one that depends on the env vars DATADIR and
               FRAMEWORKHOME.

************************************************************/

#include "GR_Shell.H"
#include "GR_2Dlines.H"			// Was GR_2Dlines.C
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/Label.h>
#include <Xm/DrawingA.h>

GR_Shell *vf_shell;
GR_DispList *vf_displist;
extern GR_Window *gwindow;
   
GR_Window *vfwindow;
 
void view_finderCB ();
void fpointdraw ();
void vf_doneCB ();
void vf_viewCB ();

#define RE 6378.145
extern int VMODE, v_LAT, v_LON, v_FOV, v_ALT, v_AZI;
extern Widget scaleLAT, scaleLON, scaleFOV, scaleALT;
extern void setvparams (GR_Window* win, int vmode,
			int lat, int lon, int alt, int fov, int azi);
  
Boolean first_viewfinder = TRUE;

Widget azi_area, azi_reset;
void azi_areaCB (Widget, XtPointer, XmDrawingAreaCallbackStruct*);

void
vf_init ()
{
   Widget vf_form;
   Widget vf_frame;
   Widget vf_control, done_button, view_button, azi_area_frame;
   GR_2Dlines  *vf_map;
   char mapfile[80];
   char *DATADIR;
   char *FWDIR;

   if ((DATADIR=getenv("DATADIR")) == NULL)
   {
      if ((FWDIR=getenv("FRAMEWORKHOME")) == NULL)
        DATADIR = "../../data";
      else
      {
         DATADIR = (char*)malloc(80);
         sprintf (DATADIR, "%s/data", FWDIR);
      }
   }
   sprintf (mapfile, "%s%s", DATADIR, "/World.bin");
   
   vf_shell = new GR_Shell;
   vf_shell->createWidget ("ViewFinder");
   vf_form = XmCreateForm (vf_shell->widget(), "VFForm", NULL, 0);

   vf_control = XtVaCreateManagedWidget ("VFControl",
                 xmRowColumnWidgetClass, vf_form,
                 //XmNtopAttachment, XmATTACH_WIDGET,
                 //XmNtopWidget, vf_frame,
                 XmNleftAttachment, XmATTACH_FORM,
                 XmNrightAttachment, XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNheight, 50,
                 XmNtopOffset, 10,
                 XmNleftOffset, 20,
                 XmNspacing, 15,
                 XmNorientation, XmHORIZONTAL,
                 XmNpacking, XmPACK_COLUMN,
                 XmNnumColumns, 1,
                 NULL);

   vf_frame = XtVaCreateManagedWidget ("VFFrame",
               xmFrameWidgetClass, vf_form,
               XmNshadowType, XmSHADOW_IN,
               XmNtopAttachment, XmATTACH_FORM,
               XmNleftAttachment, XmATTACH_FORM,
               XmNrightAttachment, XmATTACH_FORM,
               XmNbottomAttachment, XmATTACH_WIDGET,
               XmNbottomWidget, vf_control,
               NULL);

   vfwindow = new GR_Window ();
   vfwindow->doublebuffer ();
   vfwindow->rgbmode ();
   vfwindow->GR_Widget::createWidget ("VFWindow", vf_frame);

   vfwindow->set_viewmode (GR_ORTHO2);
   vfwindow->left (-1.0);
   vfwindow->right (+1.0);
   vfwindow->bottom (-1.0);
   vfwindow->top (+1.0);

   vfwindow->set_drawfunction(fpointdraw);

   vf_displist = new GR_DispList;
   vfwindow->addDispList (vf_displist);

   vf_map = new GR_2Dlines (255, 255, 255, mapfile, 329);
   vf_displist->add_object (vf_map);

   done_button = XtVaCreateManagedWidget ("Quit",
                 xmPushButtonWidgetClass, vf_control,
                 XmNwidth, 110,
                 XmNheight, 60,
                 XmNshadowThickness, 4,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)vf_doneCB, NULL);
  
   view_button = XtVaCreateManagedWidget ("View Box",
                 xmPushButtonWidgetClass, vf_control,
                 XmNwidth, 110,
                 XmNheight, 60,
                 XmNshadowThickness, 4,
                 NULL);
   XtAddCallback (view_button, XmNactivateCallback,
                    (XtCallbackProc)vf_viewCB, NULL);

   azi_area_frame = XtVaCreateManagedWidget ("AZIAreaFrame",
                                xmFrameWidgetClass, vf_control,
                                XmNshadowType, XmSHADOW_OUT,
                                XmNshadowThickness, 4,
                                NULL);
  
   azi_area = XtVaCreateManagedWidget ("AZI Area",
				xmDrawingAreaWidgetClass, azi_area_frame,
				XmNwidth, 110, // that's what "View Box" takes;
				XmNheight, 60,
			        XmNshadowThickness, 4,
                         	NULL);
   XtAddCallback (azi_area, XmNinputCallback,
		  (XtCallbackProc)azi_areaCB, NULL);
   XGCValues gcv;
   gcv.foreground = BlackPixelOfScreen (XtScreen(azi_area));
   GC gc;
   gc = XCreateGC (XtDisplay(azi_area),
                   RootWindowOfScreen(XtScreen(azi_area)),
                   GCForeground, &gcv);
   XtVaSetValues (azi_area, XmNuserData, gc, NULL);


   XtManageChild (vf_form);
   vf_shell->realize ();
}

void 
view_finderCB ()
{
   if (first_viewfinder)
   {
      first_viewfinder = FALSE;
      vf_init ();
   }
   else
   {
      XRaiseWindow (XtDisplay(vf_shell->widget()),
                    XtWindow(vf_shell->widget()) );
   }

   vfwindow->draw();
}


void
vf_doneCB ()
{
  XtDestroyWidget (vf_shell->widget());
  vfwindow->set_awake (FALSE);
  first_viewfinder = TRUE;
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
     GR_getsize (&xmax, &ymax);
     
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
	gwindow->draw ();
     }
     else
       printf ("Warning: gwindow non-existing?\n");
     
     vfwindow->draw ();
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




/* ---------------------------------- */

void fpointdraw ()
{
  float spoint[2], epoint[2];

  GR_pushattributes ();
  GR_color (255, 0, 0);

  spoint[0] = v_LON/180.0;
  spoint[1] = -1.0;
  epoint[0] = v_LON/180.0;
  epoint[1] = 1.0;
  GR_bgnline ();
  GR_v2f (spoint);
  GR_v2f (epoint);
  GR_endline (); 

  spoint[0] = -1.0;
  spoint[1] = v_LAT/90.0;
  epoint[0] = 1.0;
  epoint[1] = v_LAT/90.0;
  GR_bgnline ();
  GR_v2f (spoint);
  GR_v2f (epoint);
  GR_endline (); 

  GR_color (255, 20, 0);
  GR_linewidthf(1.5);
  GR_circ (v_LON/180.0, v_LAT/90.0, 0.02);

  GR_popattributes ();

}
