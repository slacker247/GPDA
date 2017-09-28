/************************************************************
  shoot.C is a GBI -> threat manual shooter.
************************************************************/

#include <math.h>
#include <sys/types.h>
#include <malloc.h>

#include "GR_Shell.H"
#include "GR_Model.H"
#include "parsman.H"

#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>

#include "host_user.H"
#include "ccc_plan_mess.H"

#define RE 6378.145

Widget		sh_shell;
GR_Window	*shshwindow;
Boolean		shfirst = TRUE;
int		shwindW=440, shwindH=380;
Widget		sh_str00, sh_str01, sh_str02;
char		*gbitext, *gbrtext, *threattext;
   
extern int	PLAYBACK;
C_HOST_USER	*shoot_user;

int	gbicount   = 15;	// No. GBIs listed
int	gbiselect  = 0;		// Currently selected GBI
int	gbitotal;		// Total Interceptor count
int	gbiicon;		// GBI display icon id
double	gbipkill;		// Probability of Kill
int	*gbingbis;		// No. of interceptors in farm
int	*gbiident;		// Farm identifier
double	*gbilatit;		// Latitude of Farm
double	*gbilongi;		// Longitude of Farm
char	**gbilatNS;		// Latitude is N or S
char	**gbilonEW;		// Longitude is E or W
char    **gbinames;		// Farm names
char	**gbitypes;		// Type of GBI at Farm

int     gbrcount   = 15;        // No. GBRs listed
int     gbrselect  = 0;         // Currently selected GBR
int     gbrtotal;               // Total Interceptor count
int     *gbricon;		// GBR display icon id
int     *gbrident;              // Site identifier
float	*gbrscanT;		// Scan Time
float	*gbrrmin;		//
float	*gbrrmax;		//
float	*gbrrdot;		//
float	*gbrsignal;		//
float	*gbrlumin;		// Luminosity
float	*gbrerror;		//
double  *gbrlatit;              // Latitude of Site
double  *gbrlongi;              // Longitude of Site
char    **gbrlatNS;             // Latitude is N or S
char    **gbrlonEW;             // Longitude is E or W
char    **gbrnames;             // Site names
char    **gbrtypes;             // Type of GBR at Site

int	threatcount  = 6;       // No. of threats listed
int	threatselect = 0;       // Currently selected threat
int	threattotal;            // Total Threat count
char	**threatnames;          // Threat names
char    **threattypes;          // Threat types (ie: SCUD, SS18)
char    **threatlsite;          // Launch site
char    **threattsite;          // Target site
float   *threatltime;           // Launch time
float   *threatrdist;           // Random distance
double  *threatllat;            // Launch latitude
double  *threatllon;            // Launch longitude
double  *threattlat;            // Target latitude
double  *threattlon;            // Target longitude
char    **threatlatNS;
char    **threatlonEW;
char    **threattatNS;
char    **threattonEW;

void          shoot_viewerCB (Widget);
void          gbi_shootCB ();
void          gbi_infoCB ();
void          gbr_infoCB ();
void          threat_infoCB();
void          gbilistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void          gbrlistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void          threatlistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void          sh_doneCB ();

void
sh_init ()
{
Widget        sh_form;
Widget        sh_list; 
Widget        sh_frame;
Widget        sh_control, done_button, shoot_button;
Widget        gbr_info, gbi_info, threat_info;
Widget        gbr_widget, gbi_widget, threat_widget;
Widget        septitle, titlewidget, topsep, trailwidget, botsep;
XmStringTable gbrlist, gbilist, threatlist;
XmString      *xstr, classtitle;
XmString      nodenoid;
XColor        color, unused;
Pixel         bg_color, fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
Colormap      cmap;
Display       *dpy;
XFontStruct   *font;
XmFontList    fontlist;
int           i;
int           count; 
char	      line[80];
char	      *PARMDIR;
char          gbifile[80];
char          gbrfile[80];
char          threatfile[80];

static C_PARSER *parameters_parser;
static C_PARSER *position_parser;
static C_PARSER *gbi_parser = NULL;
static C_PARSER *ground_sensor_parser = NULL;
static C_PARSER *threat_parser = NULL;
C_BASETYPE *gbi_sites, *site, *sensor_type, *basetype;
C_BASETYPE *sensor_names, *sensor_name, *radar_name;
C_BASETYPE *missile_names, *missile_name;
double latitude, longitude;
double lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec;
/*
 *	Parse the Ground-based Interceptor parameter file
 *	-------------------------------------------------
 */
   if ((PARMDIR=getenv("PARMDIR")) == NULL)
        PARMDIR = "./ParFiles";

   sprintf (gbifile, "%s%s", PARMDIR, "/gbi.par");
   gbi_parser = new C_PARSER(gbifile);
//   basetype = parameters_parser->get_basetype("parameters");
//   on_off = basetype->get_logical("on_off");
   gbi_sites = gbi_parser->get_basetype("gbi_sites");
   gbipkill = gbi_sites->get_float("pkill");
   gbiicon  = gbi_sites->get_int("icon");
//   if (on_off) on_off = gbi_sites->get_logical("on_off");
   gbicount = gbi_sites->get_ntypes();
   gbingbis = new int[gbicount];
   gbiident = new int[gbicount];
   gbinames = new char*[gbicount];
   gbitypes = new char*[gbicount];
   gbilatNS = new char*[gbicount];
   gbilonEW = new char*[gbicount];
   gbilatit = new double[gbicount];
   gbilongi = new double[gbicount];
   gbitotal = 0;

   site = gbi_sites->get_first_type();
   for (i=0; i<gbicount; i++) {
     gbinames[i] = site->get_name();
     gbitypes[i] = site->get_string("gbi_type");
     gbiident[i] = site->get_int("gbi_id");
     gbingbis[i] = site->get_int("n_gbi");
     gbitotal += gbingbis[i];

     lat_deg = site->get_float("lat_deg");
     lat_min = site->get_float("lat_min");
     lat_sec = site->get_float("lat_sec");
     lon_deg = site->get_float("lon_deg");
     lon_min = site->get_float("lon_min");
     lon_sec = site->get_float("lon_sec");
     latitude = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     gbilatNS[i] = "N";
     if (!site->get_logical("north")) gbilatNS[i] = "S";
     gbilonEW[i] = "E";
     if (!site->get_logical("east"))  gbilonEW[i] = "W";
     gbilatit[i] = latitude;
     gbilongi[i] = longitude;
     site = gbi_sites->get_next_type();
   }
/*
 *	Parse the Ground-based Sensors parameter file
 *	---------------------------------------------
 */
   sprintf (gbrfile, "%s%s", PARMDIR, "/ground_sensors.par");
   ground_sensor_parser = new C_PARSER(gbrfile);
   sensor_names = ground_sensor_parser->get_basetype("sensor_names");
   gbrcount = sensor_names->get_ntypes();
   gbrnames = new char*[gbrcount];
   gbricon = new int[gbrcount];
   gbrident = new int[gbrcount];
   gbrscanT = new float[gbrcount];
   gbrrmin = new float[gbrcount];
   gbrrmax = new float[gbrcount];
   gbrrdot = new float[gbrcount];
   gbrsignal = new float[gbrcount];
   gbrlumin = new float[gbrcount];
   gbrerror = new float[gbrcount];
   gbrlatNS = new char*[gbrcount];
   gbrlonEW = new char*[gbrcount];
   gbrlatit = new double[gbrcount];
   gbrlongi = new double[gbrcount];
   gbrtotal = 0;
   sensor_name = sensor_names->get_first_type();
   for (i=0; i<gbrcount; i++) {
     gbrnames[i] = sensor_name->get_name();
     gbrident[i] = i;
     gbrtotal = gbrtotal+1;
     lat_deg = sensor_name->get_float("lat_deg");
     lat_min = sensor_name->get_float("lat_min");
     lat_sec = sensor_name->get_float("lat_sec");
     lon_deg = sensor_name->get_float("lon_deg");
     lon_min = sensor_name->get_float("lon_min");
     lon_sec = sensor_name->get_float("lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     gbrlatNS[i] = "N";
     if (!sensor_name->get_logical("north")) gbrlatNS[i] = "S";
     gbrlonEW[i] = "E";
     if (!sensor_name->get_logical("east")) gbrlonEW[i] = "W";
     gbrlatit[i] = latitude;
     gbrlongi[i] = longitude;
     radar_name = sensor_name->get_first_type();
	gbrrmin[i]  = radar_name->get_float("rmin");
	gbrrmax[i]  = radar_name->get_float("rmax");
	gbrrdot[i]   = radar_name->get_float("rdotmin");
        gbrsignal[i] = radar_name->get_float("signal");
        gbrlumin[i]  = radar_name->get_float("luminosity");
        gbrerror[i]  = radar_name->get_float("error");
        gbrscanT[i] = radar_name->get_float("scan_time");
        gbricon[i]   = radar_name->get_int("icon");
     sensor_name = sensor_names->get_next_type();
   }
/*
 *      Parse the Missile Threat parameter file
 *      ---------------------------------------
 */
   sprintf (threatfile, "%s%s", PARMDIR, "/missile.par");
   threat_parser = new C_PARSER(threatfile);
   missile_names = threat_parser->get_basetype("missiles");
   threatcount = missile_names->get_ntypes();
   threatnames = new char*[threatcount];
   threattypes = new char*[threatcount];
   threatlsite = new char*[threatcount];
   threattsite = new char*[threatcount];
   threatltime = new float[threatcount];
   threatrdist = new float[threatcount];
   threatlatNS = new char*[threatcount];
   threatlonEW = new char*[threatcount];
   threattatNS = new char*[threatcount];
   threattonEW = new char*[threatcount];
   threatllat  = new double[threatcount];
   threatllon  = new double[threatcount];
   threattlat  = new double[threatcount];
   threattlon  = new double[threatcount]; 
   threattotal = 0;
   missile_name = missile_names->get_first_type();
   for (i=0; i<threatcount; i++) {
     threatnames[i] = missile_name->get_name();
     threattotal = threattotal+1;
     threattypes[i] = missile_name->get_string("missile_type");
     threatlsite[i] = missile_name->get_string("init_position");
     threattsite[i] = missile_name->get_string("final_position");
     threatltime[i] = missile_name->get_float("launch_time");
     threatrdist[i] = missile_name->get_float("random_distance");
     threatllat[i]  = 0.0;
     threatllon[i]  = 0.0;
     threattlat[i]  = 0.0;
     threattlon[i]  = 0.0;
     basetype = threat_parser->get_basetype(threatlsite[i]);
     lat_deg = basetype->get_float("lat_deg");
     lat_min = basetype->get_float("lat_min");
     lat_sec = basetype->get_float("lat_sec");
     lon_deg = basetype->get_float("lon_deg");
     lon_min = basetype->get_float("lon_min");
     lon_sec = basetype->get_float("lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     threatlatNS[i] = "N";
     if (!basetype->get_logical("north")) threatlatNS[i] = "S";
     threatlonEW[i] = "E";
     if (!basetype->get_logical("east")) threatlonEW[i] = "W";
     threatllat[i] = latitude;
     threatllon[i] = longitude;
     basetype = threat_parser->get_basetype(threattsite[i]);
     lat_deg = basetype->get_float("lat_deg");
     lat_min = basetype->get_float("lat_min");
     lat_sec = basetype->get_float("lat_sec");
     lon_deg = basetype->get_float("lon_deg");
     lon_min = basetype->get_float("lon_min");
     lon_sec = basetype->get_float("lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     threattatNS[i] = "N";
     if (!basetype->get_logical("north")) threattatNS[i] = "S";
     threattonEW[i] = "E";
     if (!basetype->get_logical("east")) threattonEW[i] = "W";
     threattlat[i] = latitude;
     threattlon[i] = longitude;
     missile_name = missile_names->get_next_type();
   }
/*
 *	Create the Motif window
 *	-----------------------
 */
   sh_shell = XtCreatePopupShell("Shooter", topLevelShellWidgetClass,
                                GR_toplevel, 0, 0);
   sh_form = XmCreateForm (sh_shell, "SHForm", NULL, 0);
   XtVaSetValues(sh_form,
               XmNwidth,             shwindW,
               XmNheight,            shwindH,
               NULL);
   dpy = XtDisplay(GR_toplevel);
   font = XLoadQueryFont(dpy,
          "-adobe-courier-bold-*-*-*-20-*-*-*-*-*-*-*");
   fontlist = XmFontListCreate(font, "charset1");
   classtitle   = XmStringCreateLtoR("Unclassified", "charset1");
   titlewidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,sh_form,
                 XmNwidth,            shwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNshadowThickness,  2,
                 XmNtopAttachment,    XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 NULL);
   topsep = XtVaCreateManagedWidget("TopSep", xmSeparatorWidgetClass, sh_form,
                 XmNwidth,            shwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        titlewidget,
                 NULL);
   trailwidget  = XtVaCreateManagedWidget("class",xmLabelWidgetClass,sh_form,
                 XmNwidth,            shwindW,
                 XmNheight,           30,
                 XmNalignment,        XmALIGNMENT_CENTER,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      classtitle,
                 XmNfontList,         fontlist,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNleftAttachment,   XmATTACH_FORM,
                 NULL);
   botsep = XtVaCreateManagedWidget("BotSep", xmSeparatorWidgetClass, sh_form,
                 XmNwidth,            shwindW,
                 XmNheight,           6,
                 XmNorientation,      XmHORIZONTAL,
                 XmNseparatorType,    XmSHADOW_ETCHED_IN,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     trailwidget,
                 NULL);
   XmStringFree(classtitle);
/*
 *	Setup the various buttons
 *	-------------------------
 */ 
   done_button = XtVaCreateManagedWidget ("Quit",
                 xmPushButtonWidgetClass, sh_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);
   XtAddCallback (done_button, XmNactivateCallback,
                    (XtCallbackProc)sh_doneCB, NULL);
   shoot_button = XtVaCreateManagedWidget ("Shoot",
                 xmPushButtonWidgetClass, sh_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     done_button,
                 NULL);
   XtAddCallback (shoot_button, XmNactivateCallback,
                    (XtCallbackProc)gbi_shootCB, NULL);
   gbi_info = XtVaCreateManagedWidget ("GBI Info",
                 xmPushButtonWidgetClass, sh_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     shoot_button,
                 NULL);
   XtAddCallback (gbi_info, XmNactivateCallback,
                    (XtCallbackProc)gbi_infoCB, NULL);
   gbr_info = XtVaCreateManagedWidget ("GBR Info",
                 xmPushButtonWidgetClass, sh_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     gbi_info,
                 NULL);
   XtAddCallback (gbr_info, XmNactivateCallback,
                    (XtCallbackProc)gbr_infoCB, NULL);
   threat_info = XtVaCreateManagedWidget ("Threat",
                 xmPushButtonWidgetClass, sh_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     gbr_info,
                 NULL);
   XtAddCallback (threat_info, XmNactivateCallback,
                    (XtCallbackProc)threat_infoCB, NULL);

   septitle = XtVaCreateManagedWidget("VertSep", xmSeparatorWidgetClass, sh_form,
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
 *	Setup the Lists of things
 *	-------------------------
 */
   gbilist = (XmStringTable)XtMalloc(gbicount*sizeof(XmString *));
   for (i=0; i<gbicount; i++)
       gbilist[i] = XmStringCreateSimple(gbinames[i]);
   gbi_widget = XmCreateScrolledList (sh_form, "GBIs", NULL, 0);
   XtVaSetValues (gbi_widget,
               XmNitems,             gbilist,
               XmNitemCount,         gbicount,
               XmNvisibleItemCount,  14,
               XmNscrollBarDisplayPolicy, XmSTATIC,
               NULL);
   XtVaSetValues (XtParent(gbi_widget),
               XmNtopAttachment,     XmATTACH_WIDGET,
               XmNtopWidget,         topsep,
               XmNleftAttachment,    XmATTACH_WIDGET,
               XmNleftWidget,        septitle,
               NULL);
   XtManageChild(gbi_widget);
   XtAddCallback(gbi_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)gbilistCB, NULL);

   threatlist = (XmStringTable)XtMalloc(threatcount*sizeof(XmString *));
   for (i=0; i<threatcount; i++)
       threatlist[i] = XmStringCreateSimple(threatnames[i]);
   threat_widget = XmCreateScrolledList (sh_form, "Threats", NULL, 0);
   XtVaSetValues (threat_widget,
               XmNitems,             threatlist,
               XmNitemCount,         threatcount,
               XmNvisibleItemCount,  14,
               XmNscrollBarDisplayPolicy, XmSTATIC,
               NULL);
   XtVaSetValues (XtParent(threat_widget),
               XmNtopAttachment,     XmATTACH_WIDGET,
               XmNtopWidget,         topsep,
               XmNleftAttachment,    XmATTACH_WIDGET,
               XmNleftWidget,        XtParent(gbi_widget),
               NULL);
   XtManageChild(threat_widget);
   XtAddCallback(threat_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)threatlistCB, NULL);

   gbrlist = (XmStringTable)XtMalloc(gbrcount*sizeof(XmString *));
   for (i=0; i<gbrcount; i++)
       gbrlist[i] = XmStringCreateSimple(gbrnames[i]);
   gbr_widget = XmCreateScrolledList (sh_form, "GBRs", NULL, 0);
   XtVaSetValues (gbr_widget,
               XmNitems,             gbrlist,
               XmNitemCount,         gbrcount,
               XmNvisibleItemCount,  14,
               XmNscrollBarDisplayPolicy, XmSTATIC,
               NULL);
   XtVaSetValues (XtParent(gbr_widget),
               XmNtopAttachment,     XmATTACH_WIDGET,
               XmNtopWidget,         topsep,
               XmNleftAttachment,    XmATTACH_WIDGET,
               XmNleftWidget,        XtParent(threat_widget),
               NULL);
   XtManageChild(gbr_widget);
   XtAddCallback(gbr_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)gbrlistCB, NULL);

   sprintf(line, "Selected Threat: %s", threatnames[threatselect]);
   nodenoid  = XmStringCreateSimple((char *)line);
   sh_str00  = XtVaCreateManagedWidget((String)line,xmLabelWidgetClass,sh_form,
                 XmNwidth,            230,
                 XmNheight,           20,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      nodenoid,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       septitle,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);
   XmStringFree(nodenoid);
   sprintf(line, "Selected GBI:    %s", gbinames[gbiselect]);
   nodenoid  = XmStringCreateSimple((char *)line);
   sh_str01  = XtVaCreateManagedWidget((String)line,xmLabelWidgetClass,sh_form,
                 XmNwidth,            230,
                 XmNheight,           20,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      nodenoid,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       septitle,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     sh_str00,
                 NULL);
   XmStringFree(nodenoid);
   sprintf(line, "Selected GBR:    %s", gbrnames[gbrselect]);
   nodenoid  = XmStringCreateSimple((char *)line);
   sh_str02  = XtVaCreateManagedWidget((String)line,xmLabelWidgetClass,sh_form,
                 XmNwidth,            230,
                 XmNheight,           20,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      nodenoid,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       septitle,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     sh_str01,
                 NULL);
   XmStringFree(nodenoid);
/*
   sh_frame = XtVaCreateManagedWidget ("SHFrame",
               xmFrameWidgetClass, sh_form,
               XmNwidth,          shwindW,
               XmNheight,         shwindH,
               XmNshadowType,     XmSHADOW_IN,
               XmNtopAttachment,  XmATTACH_FORM,
               XmNleftAttachment, XmATTACH_WIDGET,
               XmNleftWidget,     done_button,
               NULL);

   shshwindow = new GR_Window ();
   shshwindow->doublebuffer ();
   shshwindow->rgbmode ();
   shshwindow->GR_Widget::createWidget ("SHWindow", sh_frame);

   shshwindow->set_viewmode (GR_ORTHO2);
   shshwindow->left (-1.0);
   shshwindow->right (+1.0);
   shshwindow->bottom (-1.0);
   shshwindow->top (+1.0);

   shshwindow->set_drawfunction(tvdraw);
*/ 
   if (!PLAYBACK)
       shoot_user = new C_HOST_USER ();

   XtManageChild (sh_form);
   XtPopup(sh_shell, XtGrabNone);
   XmListSelectPos(gbi_widget, gbiselect, True);
   XmListSelectPos(threat_widget, threatselect, True);
   XmListSelectPos(gbr_widget, gbrselect, True);
}

void
shoot_viewerCB (Widget toplevel)
{
   if (shfirst)
   {
      GR_toplevel = toplevel;
      shfirst = FALSE;
      sh_init ();
   }
   else
   {
      XtPopup(sh_shell, XtGrabNone);
   }
}

void 
gbi_shootCB ()
{
/************************************************************************
* shoot_gbi - shoot a ground based interceptor                          *
************************************************************************/
  int id;
  int farm_id = 1745;
  int threat_id = 2101;
  CCC_PLAN_MESS *ccc_plan_mess;
  CCC_PLAN_DATA *ccc_plan_data;
  char *buff;
  char name[40];

  if (!PLAYBACK) {

  id = shoot_user->getid("NORAD_COMMAND_CENTER");

//...... create the command message

  buff = new char[sizeof(CCC_PLAN_MESS) + sizeof(CCC_PLAN_DATA)];

  ccc_plan_mess = (CCC_PLAN_MESS *)buff;
  ccc_plan_mess->init(1);
  ccc_plan_mess->objid = id;

  ccc_plan_data = (CCC_PLAN_DATA *)&buff[sizeof(CCC_PLAN_MESS)];
  ccc_plan_data->asset_type = -1;
  ccc_plan_data->intercept_time = 0.0;

//...... get the GBI and threat ids

  farm_id = shoot_user->getid(gbinames[gbiselect]);
  ccc_plan_data->asset_id = farm_id;

  threat_id = shoot_user->getid(threatnames[threatselect]);
  ccc_plan_data->threat_id = threat_id;

//...... send the message

  shoot_user->command(ccc_plan_mess);

//...... delete the buffer that was created

  delete buff;

  }
  printf("Shooting from %s (%d) at %s (%d)\n",
         gbinames[gbiselect], farm_id, threatnames[threatselect], threat_id);
}

void
gbi_infoCB ()
{
Widget dialog;
Arg arg[10];
char str [1280];
XmString xstr;

  sprintf (str, " %s%s\n %s%d\n %s%f\n %s%d\n %s%f%s\n %s%f%s\n %s%s\n %s%d\n", 
                "Name of GBI Farm:      ", gbitext,
                "Id of Farm:            ", gbiident[gbiselect],
                "Probability of Kill:   ", gbipkill,
                "No. of Interceptors:   ", gbingbis[gbiselect],
                "Latitude of Farm:      ", gbilatit[gbiselect], gbilatNS[gbiselect],
                "Longitude of Farm:     ", gbilongi[gbiselect], gbilonEW[gbiselect],
                "Operational Status:    ", "GREEN",
                "SPEEDES Object Id:     ", 9999);
  xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
  XtSetArg (arg[0], XmNmessageString, xstr);
  dialog = XmCreateMessageDialog (sh_shell, "GBI Info", arg, 1);
  XmStringFree (xstr);
  XtManageChild (dialog);
}

void
gbr_infoCB ()
{
Widget dialog;
Arg arg[10];
char str [1280];
XmString xstr;

  sprintf (str, " %s%s\n %s%d\n %s%f%s\n %s%f%s\n %s%s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%d\n", 
                "Name of GBR Site:      ", gbrtext,
                "Id of Site:            ", gbrident[gbrselect],
                "Latitude of Site:      ", gbrlatit[gbrselect], gbrlatNS[gbrselect],
                "Longitude of Site:     ", gbrlongi[gbrselect], gbrlonEW[gbrselect],
                "Operational Status:    ", "GREEN",
		"Scan Time:             ", gbrscanT[gbrselect],
		"Minimum Range:         ", gbrrmin[gbrselect],
		"Maximum Range:         ", gbrrmax[gbrselect],
		"Minimum Rdot:          ", gbrrdot[gbrselect],
		"Signal:                ", gbrsignal[gbrselect],
		"Luminosity:            ", gbrlumin[gbrselect],
		"Error:                 ", gbrerror[gbrselect],
                "SPEEDES Object Id:     ", 9999);
  xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
  XtSetArg (arg[0], XmNmessageString, xstr);
  dialog = XmCreateMessageDialog (sh_shell, "GBR Info", arg, 1);
  XmStringFree (xstr);
  XtManageChild (dialog);
}

void
threat_infoCB ()
{
Widget dialog;
Arg arg[10];
char str [1280];
XmString xstr;

  sprintf (str, 
        " %s%s\n %s%s\n %s%f\n %s%f\n %s%s\n %s%f%s\n %s%f%s\n %s%s\n %s%f%s\n %s%f%s\n %s%d\n ", 
                "Name of Threat:        ", threattext,
                "Type of Threat:        ", threattypes[threatselect],
	        "Threat Launch Time:    ", threatltime[threatselect],
	        "Threat Random Distance:", threatrdist[threatselect],
	        "Name of Launch Site    ", threatlsite[threatselect],
                "Latitude of Launch:    ", threatllat[threatselect], "N",
                "Longitude of Launch:   ", threatllon[threatselect], "E",
	        "Name of Target Site    ", threattsite[threatselect],
                "Latitude of Target:    ", threattlat[threatselect], "N",
		"Longitude of Target:   ", threattlon[threatselect], "E",
                "SPEEDES Object Id:     ", 9999);
  xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
  XtSetArg (arg[0], XmNmessageString, xstr);
  dialog = XmCreateMessageDialog (sh_shell, "Threat Info", arg, 1);
  XmStringFree (xstr);
  XtManageChild (dialog);
}

void
gbilistCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
   char line[80];
   int  j;
   XmString     nodenoid;
   Arg args[10];

   XmStringGetLtoR (call_data->item, XmSTRING_DEFAULT_CHARSET, &gbitext);
   gbiselect = call_data->item_position - 1;
   sprintf(line, "Selected GBI:    %s", gbinames[gbiselect]);
   nodenoid = XmStringCreateSimple((char *)line);
   j = 0;
   XtSetArg(args[j], XmNlabelString, nodenoid); j++;
   XtSetValues(sh_str01, args, j);
   //printf("Selected GBI - %s\n", gbitext);
}

void
gbrlistCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
   char line[80];
   int  j;
   XmString     nodenoid;
   Arg args[10];

   XmStringGetLtoR (call_data->item, XmSTRING_DEFAULT_CHARSET, &gbrtext);
   gbrselect = call_data->item_position - 1;
   sprintf(line, "Selected GBR:    %s", gbrnames[gbrselect]);
   nodenoid = XmStringCreateSimple((char *)line);
   j = 0;
   XtSetArg(args[j], XmNlabelString, nodenoid); j++;
   XtSetValues(sh_str02, args, j);
   //printf("Selected GBR - %s\n", gbrtext);
}

void
threatlistCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
   char line[80];
   int  j;
   XmString     nodenoid;
   Arg args[10];

   XmStringGetLtoR (call_data->item, XmSTRING_DEFAULT_CHARSET, &threattext);
   threatselect = call_data->item_position - 1;
   sprintf(line, "Selected Threat: %s", threatnames[threatselect]);
   nodenoid = XmStringCreateSimple((char *)line);
   j = 0;
   XtSetArg(args[j], XmNlabelString, nodenoid); j++;
   XtSetValues(sh_str00, args, j);
   //printf("Selected Threat - %s\n", threattext);
}

void
sh_doneCB ()
{
  XtPopdown (sh_shell);
//  shshwindow->set_awake (FALSE);
}
