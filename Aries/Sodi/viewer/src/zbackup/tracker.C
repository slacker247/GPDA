#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
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
#define SIGCHILD SIGCLD 
/*
 *   Include the X-window stuff
 */
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
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/DrawingA.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
//#include "X11/Xc/Led.h"

#include "def.H"
#include "GR_Interface.H"
#include "GR_Shell.H"
#include "GISP_Obj.H"
#include "GR_Trail.H"
#include "GR_AirObj.H"
#include "GR_String.H"
#include "GR_Impact.H"
#include "GISP_Globals.H"

#define YES          1
#define NO           0
#define POLLRATE     10

struct tracksinfo {
char       trackid[16];
int        targets;
float      lethality;
float      latitude;
float      longitude;
float      ltime;
float      itime;
char       objtype[16];
char       msltype[16];
char       tgttype[16];
char       impactloc[16];
char       impactime[16];
};

struct engageinfo {
int        n_weapons;
int        n_remain;
int        taskid;
int        hold;
float      Pk;
char       trkstatus[16];
char       lsite[16];
char       engagestat[16];
char       TTI[16];
};

struct gispobjs {
  int              id;
  int              icon;
  int              type;
  int              dropped;
  int              labeled;
  float            old_x;
  float            old_y;
  float            old_z;
  int              red;
  int              green;
  int              blue;
  GR_AirObj        *airobj;
  GISP_Obj         *object;
  GR_String        *labels;
  GR_Impact        *impactarea;
};

/* --------------------------------------------------------------------- */

Widget          mt_shell, mt_form;
GR_Window       *mtwindow;
Boolean         mtfirst = TRUE;
int             mtwindW=600, mtwindH=460;

extern FILE     *INFOfp;
extern FILE     *TIMEfp;
extern FILE     *DBUGfp;
extern FILE     *STATfp;

int             trackselect = 0;
int             assetselect = 1;
char            chline[1280];
char            filename[120];
//
//   X-windows display stuff
//
Widget        toplevel, track_widget, track_shell, track_label;
//Pixmap        redledpix, grnledpix, yelledpix;
Boolean       trkvisible = FALSE;
Widget        piechart;
GC            PieGC[10];
Pixmap        piepix;
//Pixel         bg_color, bg_yellow, bg_grey, fg_color, fg_green;
Pixel         fgcolors[10];
float         piesizes[10];

/* --------------------------------------------------------------------- */

void track_viewCB (Widget);
void TrackDoneCB();

void TrackedPicked(int id);
void TrackInit(Widget);
void TrackAdd(int pos, TRACK tracks);
void TrackDelete(int pos);
void TrackReplace(int pos, TRACK tracks);
void TrackUpdate(double time, TRACK tracks, SENSOR sensors);
void TrackDraw();

void CallAssetCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void tracklistCB (Widget, XtPointer, XmListCallbackStruct* call_data);

extern void AssetPopup(int);
extern void AssetUpdate(int);

void PieDraw(Widget self, int narea, float *areas, Pixel *fillcolors);
void pie_infoCB ();

/* --------------------------------------------------------------------- */

void
TrackInit(Widget toplevel)
{
   Display       *dpy;
   Widget        done_button, asset_button, track_info, header, vertsep;
   XmString      *xstr;
   XmString      title;
   XmStringTable threatlist;
   XFontStruct   *font;
   XmFontList    fontlist;
   XColor        color, unused;
   Pixel         top_shadow, bottom_shadow, fg_ret, select_color;
   Colormap      cmap;
   char          line[240];
   char          *DATADIR;
   char          *FWDIR;
   int           i;

   mt_shell = XtCreatePopupShell("Threat Monitor", topLevelShellWidgetClass,
                                toplevel, 0, 0);
   mt_form = XmCreateForm (mt_shell, "MTForm", NULL, 0);

   XtVaSetValues(mt_form,
               XmNwidth,             mtwindW,
               XmNheight,            mtwindH,
               NULL);
   XtManageChild (mt_form);
   /*
   piepix = XCreatePixmap(dpy, XRootWindow(dpy,0), 40, 40, DefaultDepth(dpy,0));

   piechart = XtVaCreateManagedWidget("Led", xmPushButtonWidgetClass, form,
		 XmNwidth,            48,
                 XmNheight,           48,
                 XmNmarginHeight,     0,
                 XmNspacing,          0,
                 XmNentryAlignment,   XmALIGNMENT_CENTER,
                 XmNlabelType,        XmPIXMAP,
                 XmNlabelPixmap,      piepix,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     midsep,
                 XmNrightAttachment,  XmATTACH_WIDGET,
                 XmNrightWidget,      modestat,
                 NULL);
   XtAddCallback (piechart, XmNactivateCallback,
                    (XtCallbackProc)pie_infoCB, NULL);
   XtManageChild(piechart);

   piesizes[1] = ((float)gbitotal-(float)gbiwithheld-(float)gbiexpended)/(float)gbitotal;
   piesizes[2] = (float)gbiwithheld/(float)gbitotal;
   piesizes[3] = (float)gbiexpended /(float)gbitotal;
   fgcolors[1] = fg_green;  // green - available
   fgcolors[2] = bg_yellow; // yellow - withheld
   fgcolors[3] = bg_color;  // red - expended

   PieDraw(piechart, 3, piesizes, fgcolors);
   */
   done_button = XtVaCreateManagedWidget ("Quit", xmPushButtonWidgetClass, mt_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)TrackDoneCB, NULL);

   asset_button = XtVaCreateManagedWidget ("Asset", xmPushButtonWidgetClass, mt_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     done_button,
                 NULL);
   XtAddCallback (asset_button, XmNactivateCallback,
                    (XtCallbackProc)CallAssetCB, NULL);

   track_label = XtVaCreateManagedWidget("Track", xmLabelWidgetClass, mt_form,
                 XmNwidth,            320,
                 XmNheight,           400,
                 XmNmarginHeight,     2,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_FORM,
                 NULL);

   vertsep = XtVaCreateManagedWidget("VertSep", xmSeparatorWidgetClass, mt_form,
                 XmNwidth,            10,
                 XmNorientation,      XmVERTICAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       track_label,
                 XmNtopAttachment,    XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);

   char str1[80] = "   Trk         Msl        Track";
   char str2[80] = "   ID          Type       Status";
   sprintf(line, "%s\n%s", str1,str2);
   title    = XmStringCreateLtoR(line, "charset1");
   header   = XtVaCreateManagedWidget("Header", xmLabelWidgetClass, mt_form,
		 XmNwidth,            260,
                 XmNheight,           40,
                 XmNmarginHeight,     2,
                 XmNmarginWidth,      2,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
		 XmNlabelType,        XmSTRING,
		 XmNlabelString,      title,
		 //XmNforeground,       fg_color,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       vertsep,
                 //XmNrightAttachment,  XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_FORM,
                 NULL);
   XmStringFree(title);

   track_widget = XmCreateScrolledList (mt_form, "Tracks", NULL, 0);
   XtVaSetValues (track_widget,
                 XmNitems,             NULL,
                 XmNitemCount,         0,
                 XmNvisibleItemCount,  16,
                 XmNscrollBarDisplayPolicy, XmSTATIC,
                 NULL);
   XtVaSetValues (XtParent(track_widget),
                 XmNscrolledWindowMarginWidth, 2,
                 XmNtopAttachment,     XmATTACH_WIDGET,
                 XmNtopWidget,         header,
                 XmNbottomAttachment,  XmATTACH_FORM,
                 XmNleftAttachment,    XmATTACH_WIDGET,
                 XmNleftWidget,        vertsep,
                 XmNrightAttachment,   XmATTACH_FORM,
                 NULL);
   XtManageChild(track_widget);
   XtAddCallback(track_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)tracklistCB, NULL);

   XtManageChild (mt_form);
   mtfirst = FALSE;
}

void
track_viewCB (Widget toplevel)
{
   if (mtfirst) {
      TrackInit(toplevel);
      GR_toplevel = toplevel;
   }
   trkvisible = TRUE;
   XtPopup(mt_shell, XtGrabNone);
}

void
TrackDoneCB()
{
   trkvisible = FALSE;
   XtPopdown (mt_shell);
}

void
CallAssetCB(Widget, XtPointer, XmListCallbackStruct* call_data)
{
   AssetPopup(assetselect);
}

void
TrackPicked(int objid)
{
int     i;
extern int n_rtns;
extern struct TRACK trackobj[];

   for (i=0; i<n_rtns; i++){
      fprintf(stderr, "Tracker: checking %d against %d\n", objid, trackobj[i].id);
      if (objid == trackobj[i].id) {
         trackselect = i;
	 break;
      }
   }
   fprintf(stderr, "Tracker: call track_view\n");
   if (!trkvisible) track_viewCB(GR_toplevel);
}

void
TrackReplace(int pos, TRACK *Tsave)
{
XmString  *chitem;

   sprintf(chline, " %d  %s", (int)Tsave[pos].id, "SS-18");
   chitem[0] = XmStringCreateSimple(chline);
   XmListReplaceItemsPos(track_widget, chitem, 1, pos+1);
}

void
TrackDelete(int pos)
{
   XmListDeletePos(track_widget, pos+1);
}

void
TrackAdd(int pos, TRACK *Tsave)
{
XmString  chitem;

   sprintf(chline, " %d  %12s  %12s", (int)Tsave[pos].id, Tsave[pos].chmarking,
           Tsave[pos].chstatus);
   chitem = XmStringCreateSimple(chline);   
   XmListAddItemUnselected(track_widget, chitem, 0);
}

void
TrackUpdate(double time, TRACK *Tsave, SENSOR *Sensors)
{
Widget   dialog;
Arg      arg[10];
char     str [2280];
XmString xstr;
float    X[3], V[3];
int      i;

   if (trkvisible) {
      i = trackselect;
      assetselect = Tsave[i].stn;
      X[0] = Tsave[i].X[0],
      X[1] = Tsave[i].X[1],
      X[2] = Tsave[i].X[2],
      V[0] = Tsave[i].V[0],
      V[1] = Tsave[i].V[1],
      V[2] = Tsave[i].V[2],
      sprintf (str,
" %s%d\n %s%s\n %s%s\n %s%s\n %s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s\n %s%f\n %s%f\n %s%f\n %s%f\n %s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n",
                "Track Id:              ", (int)Tsave[i].id,
                "Track Marking:         ", Tsave[i].chmarking,
                "Track Status:          ", Tsave[i].chstatus,
                "R2 Asset:              ", Sensors[Tsave[i].stn].gbiname,
                "Track Position:        ",
                "    At Time:           ", (float)time,
                "    Longitude:         ", Tsave[i].longitude,
                "    Latitude:          ", Tsave[i].latitude,
	        "    Altitude:          ", Tsave[i].altitude, //sqrt(X[0]*X[0]+X[1]*X[1]+X[2]*X[2])-RE,
                "    Speed (Kps):       ", Tsave[i].speed/1000.0,
                "Predicted Impact:      ",
                "    At Time:           ", 0.0,
                "    Longitude:         ", Tsave[i].impact_lon,
                "    Latitude:          ", Tsave[i].impact_lat,
                "    Error (Km):        ", Tsave[i].impact_err,
                "Interceptor:           ",
                "    Intercept Time:    ", 0.0,
                "    Prob. of Kill:     ", Sensors[Tsave[i].stn].gbipkill,
                "    Flight Time:       ", 0.0,
                "    Max. Altitude:     ", 0.0,
                "    Min. Altitude:     ", 0.0,
                "    Miss Distance:     ", 0.0);
      xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
      XtVaSetValues(track_label, XmNlabelString, xstr, NULL);
   }
}

void
TrackDraw(void)
{
}

/* ----------------------- list selection CB's ----------------------- */

void
tracklistCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
int i;

   i = call_data->item_position - 1;
   trackselect = i;
}

void
PieDraw(Widget self, int narea, float *areas, Pixel *fillcolor)
{
int          i, x, y, len, Sangle, Eangle;
XFontStruct  *fn;
double       a;
unsigned int height = 40, width = 40;
static GC    PieGC=NULL;
XtGCMask     mask;
XGCValues    values;

   if (PieGC != NULL) XtReleaseGC(self, PieGC);
      /*if (DefaultDepthOfScreen(XtScreen(self)) > 4
        && choose_color(self, 0.5, ((XfwfPieMenuWidget)self)->core.background_pixel,
	                &values.foreground)) mask = GCForeground;*/
      mask = GCFillStyle | GCBackground | GCForeground | GCArcMode | GCLineWidth;
      values.fill_style = FillSolid;
      values.background = BlackPixelOfScreen(XtScreen(self));
      values.foreground = WhitePixelOfScreen(XtScreen(self));
      values.arc_mode   = ArcPieSlice;
      values.line_width = 1;
      PieGC = XCreateGC(XtDisplay(self), piepix, mask, &values);
   /*
    * Draw the segments each with size areas[i]
    */
   Sangle = 0;
   for (i = 1; i <= narea; i++) {
       Eangle = (int)(360.0*64.0*areas[i]);
       XSetForeground(XtDisplay(self), PieGC, fillcolor[i]); 
       XFillArc(XtDisplay(self), piepix, PieGC, 0, 0,
                width, height, Sangle, Eangle);
       Sangle = Sangle + Eangle;
   }
   XtVaSetValues(self, XmNlabelPixmap, piepix, NULL); 
}

void
pie_infoCB ()
{

}



