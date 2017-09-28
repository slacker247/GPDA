/*	@(#)MapWindow.C	1.24		4/20/92		*/

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

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
#include <Xm/CascadeB.h>

//include "GR_MapWindow.H"
#include "GR_Window.H"
#include "GR_Shell.H"

#define	XMSTRING(t, s) XtVaTypedArg, t, XmRString, s, strlen(s) + 1
#define	MW_XMIN			(-1.0)
#define	MW_XMAX			(1.0)
#define	MW_YMIN			(-1.0)
#define	MW_YMAX			(1.0)

Widget          mp_shell;
GR_Window       *mpwindow;
Boolean         mpfirst = TRUE;
int             mpwindW=620, mpwindH=500;
int             GLmpwindW=620, GLmpwindH=400;
int             MPVisible;

GLuint          displayList;
GLuint          mp_glid;

int             JPROJ, JLTS, JGRID, IUSOUT;
float           POLONG, POLAT, ROT, PL1[2], PL2[2], PL3[2], PL4[2];

int projections[25] = {
 0,        // Linear
 1,        // Logrithmic
 2,        // Power
 10,       // Mercator
 11,       // Oblique Mercator
 12,       // Transverse Mercator
 13,       // Universal Transverse Mercator
 14,       // Cassini Cylindrical
 15,       // Cylindrical Equal-area
 16,       // Cylindrical Equidistant
 100,      // Stereographic
 101,      // Lambert Azimuthal Equal-Area
 102,      // Orthographic
 103,      // Azimuthal Equidistant
 110,      // Polar
 1000,     // Lambert Conformal Conic
 1001,     // Albers Equal-Area Conic
 10000,    // Mollweide Equal-Area
 10001,    // Hammer-Aitoff Equal-Area
 10002,    // Sinusoidal Equal-Area
 10003,    // Winkel Tripel
 10004,    // Robinson
 10005,    // Eckert IV
 -1,
 -2
};

unsigned short hourcursor [128] =
{
  0x3FFF,0xFFFC,0x3FFF,0xFFFC,
  0x3DAF,0xBF5C,0x3375,0xFFFC,
  0x38F6,0x99CC,0x180F,0xFF0C,
  0x1807,0x701C,0x0C07,0xE018,
  0x0603,0xC030,0x0301,0xC060,
  0x0180,0x80C0,0x00C0,0x8180,
  0x0070,0x8700,0x0018,0x8E00,
  0x000C,0x9800,0x0006,0x3000,
  0x0007,0xB000,0x0007,0xF000,
  0x000D,0xD800,0x000F,0xDC00,
  0x003F,0xFE00,0x00E4,0x5B80,
  0x01BB,0xBFC0,0x03B0,0x0360,
  0x0780,0x0030,0x0C00,0x0018,
  0x1800,0x000C,0x1800,0x000C,
  0x3000,0x0006,0x3000,0x0006,
  0x3000,0x0006,0x3FFF,0xFFFE,
};


void map_viewerCB (Widget);
void mp_fileCB (Widget, XtPointer, XtPointer);
void mp_editCB (Widget, XtPointer, XtPointer);
void mp_optsCB (Widget, XtPointer, XtPointer);
void mp_projCB (Widget, XtPointer, XtPointer);
void mpdraw ();
extern "C" void gropen_();
extern "C" void supmap_();

void
quitapp (Widget, caddr_t, caddr_t)
{
   exit (0);
}

void
zoomfunc (Widget, caddr_t client_data, caddr_t)
{
//GR_MapWindow		*window;

  //window = (GR_MapWindow *) client_data;
	//window->zoom ();
}

void
cancel_zoomfunc (Widget, caddr_t client_data, caddr_t)
{
//GR_MapWindow		*window;

  //window = (GR_MapWindow *) client_data;
	//window->cancel_zoom ();

}

void
unzoomfunc (Widget, caddr_t client_data, caddr_t)
{
//GR_MapWindow  *window;

   //window = (GR_MapWindow *) client_data;
   //window->unzoom ();
}

void
unzoom_onefunc (Widget, caddr_t client_data, caddr_t)
{
//GR_MapWindow	*window;

   //window = (GR_MapWindow *) client_data;
   //window->unzoom_one ();
}



struct MapWCBstruct
{
//GR_MapWindow	*mapwindow;
char		*data;
};

void 
set_proj (Widget, caddr_t client_data, caddr_t)
{
MapWCBstruct		*cbstruct;

   cbstruct = (MapWCBstruct *) client_data;
   //cbstruct->mapwindow->setMapProjection (cbstruct->data);
}
/*
void     gridfunc (Widget, caddr_t, caddr_t);
void     pboundfunc (Widget, caddr_t, caddr_t);
void     mapfunc (Widget, caddr_t, caddr_t);
void     quitfunc (Widget, caddr_t, caddr_t);

class	C_ViewSize
{
	public:
		float		p_xmin;
		float		p_xmax;
		float		p_ymin;
		float		p_ymax;

	C_ViewSize::C_ViewSize (float xmin, float xmax, float ymin, float ymax);
};

C_ViewSize::C_ViewSize (float xmin, float xmax, float ymin, float ymax)
{
	p_xmin = xmin;
	p_xmax = xmax;
	p_ymin = ymin;
	p_ymax = ymax;
}
*/

mp_init ()
{
Widget	     mp_form, menubar, cbutton, button, pulldownmenu;
Widget       p_zoom_w, p_cancel_w, p_unzoom_one_w,p_unzoom_w, p_proj_pulldown;
Widget       mp_frame;
Widget       vertsep, titlewidget, topsep, trailwidget, botsep, tmpsep;
Widget       filepane, editpane, projpane;
XmString     *xstr, title, classtitle;
XmString     file, focus, projects, options, help;
XmString     quit, redraw, normal, origin, rotate;
XmString     gridline, usoutlin, rivers, roads;
XmString     proj01, proj02, proj03, proj04, proj05, proj06, proj07, proj08, proj09, proj10;
XmString     proj11, proj12, proj13, proj14, proj15, proj16, proj17, proj18, proj19, proj20;
XmString     proj21, proj22, proj23, proj24;
XColor       color, unused;
Pixel        bg_color, fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
Pixel        bg_yellow;
Colormap     cmap;
Display      *dpy;
XFontStruct  *font;
XmFontList   fontlist;

   mp_shell = XtCreatePopupShell("MapViewer", topLevelShellWidgetClass,
                                GR_toplevel, 0, 0);

   mp_form = XmCreateForm (mp_shell, "MPForm", NULL, 0);
   XtVaSetValues(mp_form,
               XmNwidth,             mpwindW,
               XmNheight,            mpwindH,
               NULL);

   dpy = XtDisplay(GR_toplevel);
   font = XLoadQueryFont(dpy,
          "-adobe-courier-bold-*-*-*-20-*-*-*-*-*-*-*");
   fontlist = XmFontListCreate(font, "charset1");

   XtVaGetValues(mp_shell, XmNcolormap, &cmap, NULL);
   XAllocNamedColor(XtDisplay(mp_shell), cmap, "red", &color, &unused);
   bg_color = color.pixel;
   XmGetColors(XtScreen(mp_shell), cmap, bg_color, &fg_ret, &top_shadow,
               &bottom_shadow, &select_color);
   XAllocNamedColor(XtDisplay(mp_shell), cmap, "blue", &color, &unused);
   fg_color = color.pixel;
   XAllocNamedColor(XtDisplay(mp_shell), cmap, "yellow", &color, &unused);
   bg_yellow = color.pixel;

   classtitle   = XmStringCreateLtoR("Unclassified", "charset1");
   titlewidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,mp_form,
                 XmNwidth,            mpwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNshadowThickness,  2,
                 XmNtopAttachment,    XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 NULL);
   topsep = XtVaCreateManagedWidget("TopSep", xmSeparatorWidgetClass, mp_form,
                 XmNwidth,            mpwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        titlewidget,
                 NULL);
   trailwidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,mp_form,
                 XmNwidth,            mpwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 NULL);
   botsep = XtVaCreateManagedWidget("BotSep", xmSeparatorWidgetClass, mp_form,
                 XmNwidth,            mpwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     trailwidget,
                 NULL);
   XmStringFree(classtitle);

   file       = XmStringCreateSimple("File");
   focus      = XmStringCreateSimple("Edit");
   projects   = XmStringCreateSimple("Projections");
   options    = XmStringCreateSimple("Options");
   help       = XmStringCreateSimple("Help");

   menubar = XmVaCreateSimpleMenuBar(mp_form, "MapMenu",
                XmVaCASCADEBUTTON, file,         'F',
                XmVaCASCADEBUTTON, focus,        'E',
                XmVaCASCADEBUTTON, projects,     'P',
                XmVaCASCADEBUTTON, options,      'O',
                XmVaCASCADEBUTTON, help,         'H',
                NULL);
   XtVaSetValues (menubar,
		XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       topsep,
		XmNleftAttachment,  XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
                XmNspacing,         5,
		NULL);
   XtManageChild (menubar);
   XmStringFree(file);
   XmStringFree(focus);
   XmStringFree(projects);
   XmStringFree(options);
   XmStringFree(help);

   // ----- File menu -----
   quit     = XmStringCreateSimple ("Close");
   filepane = XmVaCreateSimplePulldownMenu (menubar, "File", 0, mp_fileCB,
              XmVaPUSHBUTTON, quit, NULL, NULL, NULL,
              NULL);
   XmStringFree (quit);

   // ----- Edit menu -----
   origin   = XmStringCreateSimple ("Origin");
   rotate   = XmStringCreateSimple ("Rotate");
   redraw   = XmStringCreateSimple ("Zoom In");
   normal   = XmStringCreateSimple ("Zoom Out");
   editpane = XmVaCreateSimplePulldownMenu (menubar, "Edit", 1, mp_editCB,
              XmVaPUSHBUTTON, origin, NULL, NULL, NULL,
              XmVaPUSHBUTTON, rotate, NULL, NULL, NULL,
	      XmVaSEPARATOR,
              XmVaPUSHBUTTON, redraw, NULL, NULL, NULL,
              XmVaPUSHBUTTON, normal, NULL, NULL, NULL,
              NULL);
   XmStringFree (origin);
   XmStringFree (rotate);
   XmStringFree (redraw);
   XmStringFree (normal);

   // ----- Projection menu -----
   proj01 = XmStringCreateSimple("Linear");
   proj02 = XmStringCreateSimple("Logarithmic");
   proj03 = XmStringCreateSimple("Power");
   proj04 = XmStringCreateSimple("Mercator *");
   proj05 = XmStringCreateSimple("Oblique Mercator");
   proj06 = XmStringCreateSimple("Transverse Mercator");
   proj07 = XmStringCreateSimple("Universal Transverse Mercator");
   proj08 = XmStringCreateSimple("Cassini Cylindrical");
   proj09 = XmStringCreateSimple("Cylindrical Equal-area");
   proj10 = XmStringCreateSimple("Cylindrical Equidistant *");
   proj11 = XmStringCreateSimple("Stereographic *");
   proj12 = XmStringCreateSimple("Lambert Azimuthal Equal-Area *");
   proj13 = XmStringCreateSimple("Orthographic *");
   proj14 = XmStringCreateSimple("Azimuthal Equidistant *");
   proj15 = XmStringCreateSimple("Polar");
   proj16 = XmStringCreateSimple("Lambert Conformal Conic *");
   proj17 = XmStringCreateSimple("Albers Equal-Area Conic");
   proj18 = XmStringCreateSimple("Mollweide Equal-Area *");
   proj19 = XmStringCreateSimple("Hammer-Aitoff Equal-Area");
   proj20 = XmStringCreateSimple("Sinusoidal Equal-Area *");
   proj21 = XmStringCreateSimple("Winkel Tripel");
   proj22 = XmStringCreateSimple("Robinson");
   proj23 = XmStringCreateSimple("Eckert IV");
   proj24 = XmStringCreateSimple("Contour");
   projpane = XmVaCreateSimplePulldownMenu (menubar, "Proj", 2, mp_projCB,
              XmVaPUSHBUTTON, proj01, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj02, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj03, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj04, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj05, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj06, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj07, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj08, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj09, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj10, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj11, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj12, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj13, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj14, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj15, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj16, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj17, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj18, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj19, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj20, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj21, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj22, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj23, NULL, NULL, NULL,
              XmVaPUSHBUTTON, proj24, NULL, NULL, NULL,
					    //XmVaSEPARATOR,
              NULL);
   XmStringFree (proj01);
   XmStringFree (proj02);
   XmStringFree (proj03);
   XmStringFree (proj04);
   XmStringFree (proj05);
   XmStringFree (proj06);
   XmStringFree (proj07);
   XmStringFree (proj08);
   XmStringFree (proj09);
   XmStringFree (proj10);
   XmStringFree (proj11);
   XmStringFree (proj12);
   XmStringFree (proj13);
   XmStringFree (proj14);
   XmStringFree (proj15);
   XmStringFree (proj16);
   XmStringFree (proj17);
   XmStringFree (proj18);
   XmStringFree (proj19);
   XmStringFree (proj20);
   XmStringFree (proj21);
   XmStringFree (proj22);
   XmStringFree (proj23);
   XmStringFree (proj24);

   // ----- Options menu -----
   gridline  = XmStringCreateSimple ("Grid Lines");
   usoutlin  = XmStringCreateSimple ("US States");
   rivers    = XmStringCreateSimple ("Rivers");
   roads     = XmStringCreateSimple ("Roads");
   editpane = XmVaCreateSimplePulldownMenu (menubar, "Options", 3, mp_optsCB,
              XmVaPUSHBUTTON, gridline, NULL, NULL, NULL,
              XmVaPUSHBUTTON, usoutlin, NULL, NULL, NULL,
	      XmVaSEPARATOR,
              XmVaPUSHBUTTON, rivers,   NULL, NULL, NULL,
              XmVaPUSHBUTTON, roads,    NULL, NULL, NULL,
              NULL);
   XmStringFree (gridline);
   XmStringFree (usoutlin);
   XmStringFree (rivers);
   XmStringFree (roads);
/*
 *      Do the OpenGL window piece
 *      --------------------------
 */
   mp_frame = XtVaCreateManagedWidget ("MPFrame", xmFrameWidgetClass, mp_form,
               XmNwidth,          GLmpwindW,
               XmNheight,         GLmpwindH,
               XmNshadowType,     XmSHADOW_IN,
               XmNtopAttachment,  XmATTACH_WIDGET,
	       XmNtopWidget,      menubar,
               XmNleftAttachment, XmATTACH_FORM,
               NULL);
   mpwindow = new GR_Window ();
   mpwindow->doublebuffer ();
   mpwindow->rgbmode ();
   mpwindow->GR_Widget::createWidget ("MPWindow", mp_frame);
   mpwindow->set_viewmode (GR_ORTHO2);
   mpwindow->left (-1.0);
   mpwindow->right (+1.0);
   mpwindow->bottom (-1.0);
   mpwindow->top (+1.0);
   mpwindow->world (MW_XMIN, MW_XMAX, MW_YMIN, MW_YMAX);
   mpwindow->set_drawfunction(mpdraw);

   XtManageChild (mp_form);
   XtPopup(mp_shell, XtGrabNone);

   JPROJ  = 9;
   POLAT  = 0.0;
   POLONG = 0.0;
   ROT    = 0.0;
   PL1[0] = 0.0;
   PL2[0] = 0.0;
   PL3[0] = 0.0;
   PL4[0] = 0.0;
   JGRID  = 0;
   IUSOUT = 0;
   JLTS   = 1;
}

void 
map_viewerCB (Widget toplevel)
{
   if (mpfirst) {
      GR_toplevel = toplevel;
      mpfirst = FALSE;
      mp_init ();
   } else {
      XtPopup(mp_shell, XtGrabNone);
   }
   MPVisible = TRUE;
}

void
mp_fileCB(Widget, XtPointer client_data, XtPointer)
{
   XtPopdown (mp_shell);
   MPVisible = FALSE;
   mpwindow->set_awake (FALSE);
}

void
mp_editCB(Widget, XtPointer client_data, XtPointer)
{
   int item_no = (int) client_data;

   switch (item_no)
   {
     case 0:                                    // Set origin (POLAT & POLON)
       printf("Setting origin...\n");
       POLAT  = 0.0;
       POLONG = 0.0;
       break;
     case 1:                                    // Set rotation (ROT)
       printf("Setting rotation...\n");
       ROT    = 0.0;
       break;
     case 2:                                    // Zoom In
       printf("Zooming In...\n");
       break;
     case 3:                                    // Zoom Out
       printf("Zooming Out...\n");
       break;
     default:
       break;
   }
}

void
mp_optsCB(Widget, XtPointer client_data, XtPointer)
{
#define    numLats  10.0
#define    numLons  10.0
int        lon, lat;
int        vtxCnt;
GLdouble   latDelta = 180.0 / numLats,
           lonDelta = 360.0 / numLons;

   int item_no = (int) client_data;

   switch (item_no) {
   case 0:                                      // Set Grid lines
       printf("Setting Grid lines...\n");
       JGRID = 1;
       mp_glid =  glGenLists(1);
       glNewList(mp_glid, GL_COMPILE);
          glColor3f(0.5, 0.5, 0.0);
          glLineStipple(1, 0xf0f0);
          glEnable(GL_LINE_STIPPLE);
          glBegin(GL_LINES);

          // Longitude Lines
          vtxCnt = 0;
          for (lon = 1; lon < numLons; lon++, vtxCnt += 2) {
            glVertex2f((lon * lonDelta) - 180.0, -90.0);
            glVertex2f((lon * lonDelta) - 180.0,  90.0);
          }

          // Latitude Lines
	  for (lat = 1; lat < numLats; lat++, vtxCnt += 2) {
	    glVertex2f(-180.0, (lat * latDelta) - 90.0);
	    glVertex2f( 180.0, (lat * latDelta) - 90.0);
	  }

	  glEnd();
          glDisable(GL_LINE_STIPPLE);
       glEndList();
       break;
     case 1:                                    // Set US States outline
       printf("Setting US Outlines...\n");
       IUSOUT = 1;
       break;
     case 2:                                    // Set drawing of rivers
       printf("Setting Rivers...\n");
       break;
     case 3:                                    // Set drawing of roads
       printf("Setting Roads...\n");
       break;
     default:
       break;
   }

   mpwindow->draw();
}

void
mp_projCB(Widget, XtPointer client_data, XtPointer)
{
char           chfile[64];
int            flag;
int            zero = 0, eleven = 11;
float          fzero = 0.0;

   int item_no = (int) client_data;
   printf("Projection type is %d\n", projections[item_no]);

   PL1[0] = 0.0;
   PL2[0] = 0.0;
   PL3[0] = 0.0;
   PL4[0] = 0.0;
   JGRID  = 0;
   IUSOUT = 0;
   JLTS   = 1;
   flag   = 0;

   switch (item_no) {
     case 0:        // Linear
       JPROJ = 9;
       break;
     case 1:        // Logrithmic
       JPROJ = 9;
       break;
     case 2:        // Power
       JPROJ = 9;
       break;
     case 3:        // Mercator
       JPROJ = 9;
       break;
     case 4:        // Oblique Mercator
       JPROJ = 9;
       break;
     case 5:        // Transverse Mercator
       JPROJ = 9;
       break;
     case 6:        // Universal Transverse Mercator
       JPROJ = 9;
       break;
     case 7:        // Cassini Cylindrical
       JPROJ = 9;
       break;
     case 8:        // Cylindrical Equal-area
       JPROJ = 9;
       break;
     case 9:        // Cylindrical Equidistant
       JPROJ = 8;
       break;
     case 10:       // Stereographic
       JPROJ = 1;
       break;
     case 11:       // Lambert Azimuthal Equal-Area
       JPROJ = 4;
       break;
     case 12:       // Orthographic
       JPROJ = 2;
       break;
     case 13:       // Azimuthal Equidistant
       JPROJ = 6;
       break;
     case 14:       // Polar
       JPROJ = 9;
       break;
     case 15:       // Lambert Conformal Conic
       JPROJ = 3;
       break;
     case 16:       // Albers Equal-Area Conic
       JPROJ = 9;
       break;
     case 17:       // Mollweide Equal-Area
       JPROJ = 10;
       break;
     case 18:       // Hammer-Aitoff Equal-Area
       JPROJ = 9;
       break;
     case 19:       // Sinusoidal Equal-Area
       JPROJ = 5;
       break;
     case 20:       // Winkel Tripel
       JPROJ = 9;
       break;
     case 21:       // Robinson
       JPROJ = 9;
       break;
     case 22:       // Eckert IV
       JPROJ = 9;
       break;
     case 23:       // Contour
       flag = 1;
       break;
   }

   gropen_();

   displayList = glGenLists(1);
   glNewList(displayList, GL_COMPILE);

   if (flag == 0) {
     supmap_(&JPROJ,&POLAT,&POLONG,&ROT,&PL1,&PL2,&PL3,&PL4,&JLTS,&JGRID,&IUSOUT);
     fwrit_(&fzero, &fzero, "Test String", &eleven, &zero, &zero, &zero);
   } else {
     sprintf(chfile, "%s", "platte.dem");
     flag = 1;
     demplot_(&flag);
   }

      glColor3f(0.5, 0.5, 0.0);
      glLineWidth(2.0);
      glBegin(GL_LINES);                          // gen2DPrimeMeridian
         glVertex2f(0.0, -90.0);
         glVertex2f(0.0,  90.0);
      glEnd();

      glBegin(GL_LINES);                          // gen2DEquator
         glVertex2f(-180.0, 0.0);
         glVertex2f( 180.0, 0.0);
      glEnd();

      glLineWidth(1.0);   
   glEndList();

   mpwindow->draw();
}

void mpdraw ()
{
int            mpwindW, mpwindH;
unsigned long  *lptr;
GLUquadricObj  *quadObj;
GLdouble       *sizeArray, *tmp;

/*
 *      Setup the OpenGL Window
 *      -----------------------
 */
   mpwindW = GLmpwindW;
   mpwindH = GLmpwindH;

   glViewport(0, 0, GLmpwindW, GLmpwindH);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   //gluPerspective(60.0, GLmpwindW/GLmpwindH, 10.0, 60.0);
   glOrtho(-180.0, 180.0, -90.0, 90.0, -10.0, 10.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   glCallList(displayList);

   if (JGRID) glCallList(mp_glid);

   glFlush();
}

