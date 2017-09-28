/************************************************************
  shoot.C is a GBI -> threat manual shooter.
************************************************************/

#include <math.h>
#include <sys/types.h>
#include <malloc.h>

#include "def.H"
#include "GR_Shell.H"
#include "GR_Model.H"
#include "GR_DispList.H"
#include "GISP_Obj.H"
#include "GR_String.H"
#include "GR_Defend.H"
#include "GR_Impact.H"
#include "parsman.H"
#include "convert.H"
#include "GISP_Globals.H"

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


#define GBRTYPE 1                       // Ground-based radars
#define EYETYPE 2                       // SIBRS
#define DSPTYPE 3                       // DSP
#define GBITYPE 4                       // Interceptor Farms
#define AIRTYPE 5                       // Air breathing (aircraft, cruise, etc)
#define SEATYPE 6                       // Sea going (ships, AEGIS, etc)

extern GR_Window *gwindow;
Widget	      sh_shell;
GR_Window     *shshwindow;
Boolean	      shinit = FALSE;
int	      shwindW=480, shwindH=480;
Widget	      sh_str00, sh_str01, sh_str02, sh_str09;
Widget        gbr_widget, gbi_widget, threat_widget, asset_details;
char	      *gbitext, *gbrtext, *threattext;
int           asset_vis;

int	      gbicount   = 15;	        // No. GBIs listed
int           gbimaxid   = 20;
int	      gbiselect  = 0;		// Currently selected GBI
int	      gbitotal;		        // Total Interceptor count
int           gbiwithheld;              // GBIs withheld for future use
int           gbiexpended = 10;         // GBIs used
int           gbi_per_rv = 2;           // GBIs fired per threat
int           gbidefend;                // True if defended area to be displayed
int           gbiarealabel;             // True if defended area is to be labelled
float         *gbiradius;               // Radius (Km) of defended area
int	      gbiicon;		        // GBI display icon id from .par file
float         gbipk;                    // Default probability of kill from .par file
int           gbilabel;                 // True if GBI site is to be labelled (.par file)
int           gbicover;                 // True if coverage patterns are to be displayed
struct SENSOR *Assets;

int           gbrbase    = 1950;        // First GL Display List ID for GBRs
int           gbrcount   = 15;          // No. GBRs listed
int           gbrselect  = 0;           // Currently selected GBR
int           gbrtotal;                 // Total Interceptor count
int           *gbricon;		        // GBR display icon id
int           *gbrident;                // Site identifier
float	      *gbrscanT;		// Scan Time
float	      *gbrrmin;		        //
float	      *gbrrmax;		        //
float	      *gbrrdot;		        //
float	      *gbrsignal;		//
float	      *gbrlumin;		// Luminosity
float	      *gbrerror;		//
float	      *gbrrlow;	  	        // Low-power range
float         *gbrelev;                 // Sighting elevation angle
float         *gbrazi;                  // Sighting elevation angle (from N)
float         *gbrfovhi;                // High-power Field-of-View
float         *gbrfovlo;                // Low-power Field-of-View
double        *gbrlatit;                // Latitude of Site
double        *gbrlongi;                // Longitude of Site
char          **gbrlatNS;               // Latitude is N or S
char          **gbrlonEW;               // Longitude is E or W
char          **gbrnames;               // Site names
char          **gbrtypes;               // Type of GBR at Site
GR_Sensor     *gbrsensor[64];
GR_String     *gbrlabels[64];

int           eyecount  = 1;
int           eyeselect = 0;
int           eyetotal;
int           eyerings, eyesize;
float         eyealt, eyeincl, eyescanT, eyermin, eyermax, eyerdot;
float         eyesignal, eyelumin, eyeerror;
char          *eyenames;

int           dspcount   = 5;           // No. GBRs listed
int           dspselect  = 0;           // Currently selected GBR
int           dsptotal;                 // Total Interceptor count
int           dspicon;
int           dsprings, dspsize;
float         dspalt;
float         dspscanT;                 // Scan Time
float         dsprmin;                  //
float         dsprmax;                  //
float         dsprdot;                  //
float         dspsignal;                //
float         dsplumin;                 // Luminosity
float         dsperror;                 //
double        dsplongi;
char          *dspnames;                // Sensor names

int           aircount  = 0;
int           airselect = 0;
int           airtotal;

int           seacount  = 0;
int           seaselect = 0;
int           seatotal;

int           assetcount;
int           *astypes;                 // Asset type

int	      threatcount  = 6;         // No. of threats listed
int	      threatselect = 0;         // Currently selected threat
int	      threattotal;              // Total Threat count
char	      **threatnames;            // Threat names
char          **threattypes;            // Threat types (ie: SCUD, SS18)
char          **threatlsite;            // Launch site
char          **threattsite;            // Target site
float         *threatltime;             // Launch time
float         *threatrdist;             // Random distance
double        *threatllat;              // Launch latitude
double        *threatllon;              // Launch longitude
double        *threattlat;              // Target latitude
double        *threattlon;              // Target longitude
char          **threatlatNS;
char          **threatlonEW;
char          **threattatNS;
char          **threattonEW;

int           armycount = 0, armybase = 1995, armytype;
float         armysize;

void          AssetPopup(int);
void          AssetUpdate(int);
void          asset_init ();
void          asset_viewerCB (Widget);
void          AssetPicked(int type);
void          detaildoneCB(Widget, XtPointer, XtPointer call_data);
void          gbi_infoCB ();
void          threat_infoCB();
void          gbilistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void          threatlistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void          asset_doneCB ();

/* ================================================================================= */

void
asset_init ()
{
Widget        sh_form;
Widget        sh_list; 
Widget        sh_frame;
Widget        sh_control, done_button;
Widget        gbr_info, gbi_info, eye_info, dsp_info;
Widget        air_info, sea_info, gnd_info, threat_info;
Widget        septitle, titlewidget, topsep, trailwidget, botsep;
Widget        label01, label02, label03;
XmStringTable gbrlist, gbilist, threatlist;
XmString      *xstr, classtitle;
XmString      nodenoid;
XColor        color, unused;
Pixel         bg_color, fg_color, top_shadow, bottom_shadow, fg_ret, select_color;
Colormap      cmap;
Display       *dpy;
XFontStruct   *font;
XmFontList    fontlist;

int           i, j, k, id, index;
int           count;
int           npolys = 0;
char	      line[180];
char	      *PARMDIR;
char          gbifile[128];
char          gbrfile[128];
char          threatfile[128];
float         values[10];

GR_Impact     *impact[64];
extern GR_DispList *displist;
extern GR_DispList *sensor_displist;

static C_PARSER *parameters_parser;
static C_PARSER *position_parser;
static C_PARSER *gbi_parser = NULL;
static C_PARSER *ground_sensor_parser = NULL;
static C_PARSER *threat_parser = NULL;

C_BASETYPE      *gbi_sites, *site, *area, *sensor_type, *basetype;
C_BASETYPE      *sensor_names, *sensor_name, *radar_name;
C_BASETYPE      *missile_names, *missile_name;

C_CONVERT       conversion;

double          latitude, longitude, X[3];
double          lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec;
float           scale_factor;
int             area_shape;

   if (shinit) return;
/*
 *	Parse the Ground-based Interceptor parameter file
 *	-------------------------------------------------
 *
   fprintf(stderr, "PARMDIR is %s\n", getenv("PARMDIR"));
   if ((PARMDIR=getenv("PARMDIR")) == NULL)
        PARMDIR = "./ParFiles";
*/ 
   sprintf (gbifile, "%s", "./assets.par");
   gbi_parser = new C_PARSER("assets.par");
   gbi_sites  = gbi_parser->get_basetype("asset_names");
   gbilabel   = (int)gbi_sites->get_logical("label");
   gbimaxid   = gbi_sites->get_int("max_id");
   gbicount   = gbi_sites->get_ntypes();
   printf("Doing %d assets\n", gbicount);
   Assets = new SENSOR[gbimaxid+1];
   for (i=0; i<gbimaxid+1; i++) {
     Assets[i].gbingbis  = 0;
     Assets[i].gbiident  = 0;
     Assets[i].gbinhold  = 0;
     Assets[i].gbinarea  = 0;
     Assets[i].gbipkill  = 0.0;
     Assets[i].gbiname   = NULL;
     Assets[i].gbitype   = NULL;
     Assets[i].gbisensr  = NULL;
     Assets[i].gbilatNS  = NULL;
     Assets[i].gbilonEW  = NULL;
     Assets[i].gbielev   = 0.0;
     Assets[i].gbiazi    = 0.0;
     Assets[i].gbilatit  = 0.0;
     Assets[i].gbilongi  = 0.0;
     Assets[i].gbiscanT  = 0.0;
     Assets[i].gbirmin   = 0.0;
     Assets[i].gbirmax   = 0.0;
     Assets[i].gbirdot   = 0.0;
     Assets[i].gbisignal = 0.0;
     Assets[i].gbilumin  = 0.0;
     Assets[i].gbierror  = 0.0;
     Assets[i].gbirlow   = 0.0;
     Assets[i].gbifovhi  = 0.0;
     Assets[i].gbifovlo  = 0.0;
     Assets[i].gbisensor = NULL;
     Assets[i].gbistn    = -1;
   }
   gbitotal  = 0;

   site = gbi_sites->get_first_type();
   for (k=0; k<gbicount; k++) {
     //
     //   Get the site information
     //
     i = site->get_int("asset_id");
     if (i > gbimaxid) {
        fprintf(stderr, "Asset ID %d greater than specified Maximum ID of %d\n", i, gbimaxid);
        exit (99);
     }
     Assets[i].gbiname  = site->get_name();
     Assets[i].gbistn   = i;
     Assets[i].gbiicon  = site->get_int("icon");
     Assets[i].gbiscale = site->get_float("scale");
     lat_deg = site->get_float("lat_deg");
     lat_min = site->get_float("lat_min");
     lat_sec = site->get_float("lat_sec");
     latitude = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     Assets[i].gbilatNS = "N";
     if (!site->get_logical("north")) {
          latitude = -latitude;
	  Assets[i].gbilatNS = "S";
     }
     lon_deg = site->get_float("lon_deg");
     lon_min = site->get_float("lon_min");
     lon_sec = site->get_float("lon_sec");
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     Assets[i].gbilonEW = "E";
     if (!site->get_logical("east")) {
          longitude = -longitude;
	  Assets[i].gbilonEW = "W";
     }
     Assets[i].gbilatit = latitude;
     Assets[i].gbilongi = longitude;
     Assets[i].gbialt   = site->get_float("altitude");
     conversion.latlon_to_xyz((double)(latitude*M_PI/180.0),
                              (double)(longitude*M_PI/180.0), X);
     Assets[i].gbix = X[0];
     Assets[i].gbiy = X[1];
     Assets[i].gbiz = X[2];
     //
     //   Get the sensor information
     //
     if (site->get_logical("sensor")) {
        Assets[i].gbisensr = site->get_string("sensor_type");
        gbicover   = site->get_logical("coverage");
        Assets[i].gbielev  = site->get_float("elevation");
        Assets[i].gbiazi   = site->get_float("azimuth");
        Assets[i].gbir     = site->get_int("r");
        Assets[i].gbig     = site->get_int("g");
        Assets[i].gbib     = site->get_int("b");
        radar_name   = gbi_parser->get_basetype(Assets[i].gbisensr);
        Assets[i].gbistype  = radar_name->get_int("sensor_type");;
        Assets[i].gbirmin   = radar_name->get_float("rmin");
        Assets[i].gbirmax   = radar_name->get_float("rmax");
        Assets[i].gbirlow   = radar_name->get_float("rmax_low");
        Assets[i].gbifovhi  = radar_name->get_float("fov_high");
        Assets[i].gbifovlo  = radar_name->get_float("fov_low");
        Assets[i].gbirdot   = radar_name->get_float("rdotmin");
        Assets[i].gbisignal = radar_name->get_float("signal");
        Assets[i].gbilumin  = radar_name->get_float("luminosity");
        Assets[i].gbierror  = radar_name->get_float("error");
        Assets[i].gbiscanT  = radar_name->get_float("scan_time");

        Assets[i].sensorobj = new GISP_Obj(Assets[i].gbistn, Assets[i].gbiicon);
        Assets[i].sensorobj->init(Assets[i].gbistn, Assets[i].gbiicon);
        sensor_displist->add_object(Assets[i].sensorobj);
        X[0] = Assets[i].gbix;
        X[1] = Assets[i].gbiy;
        X[2] = Assets[i].gbiz;
        Assets[i].sensorobj->translate((float)X[1]/RE, (float)X[2]/RE, (float)X[0]/RE);
        Assets[i].sensorobj->rotate_z(180.0);
        scale_factor = get_scale(Assets[i].gbiicon)*Assets[i].gbiscale;
        Assets[i].sensorobj->scale(scale_factor, scale_factor, scale_factor);
        Assets[i].sensorobj->finish();

        if (gbicover != 0) {
           values[0]    = Assets[i].gbielev;
           values[1]    = Assets[i].gbiazi;
           values[2]    = Assets[i].gbirmax;
           values[3]    = Assets[i].gbirlow;
           values[4]    = Assets[i].gbifovhi;
           Assets[i].gbisensor = new GR_Sensor(1900+i, Assets[i].gbistn, Assets[i].gbistype, 
                                      (float)latitude, (float)longitude, values);
           sensor_displist->add_object(Assets[i].gbisensor);
           //fprintf(stderr, "Putting a sensor at %f %f\n", latitude, longitude);
	}

        if (gbilabel != 0) {
           Assets[i].gbilabel = new GR_String((float)latitude, (float)longitude,
                            Assets[i].gbiname);
           displist->add_object(Assets[i].gbilabel);
        }
     } else Assets[i].gbistype = -1;
     //
     //   Get the weapons information
     //
     if (site->get_logical("weapons")) {
        Assets[i].gbiident = site->get_int("gbi_id");
        Assets[i].gbingbis = site->get_int("n_gbi");
        Assets[i].gbinhold = site->get_int("n_hold");
        Assets[i].gbipkill = site->get_float("pkill");
        Assets[i].gbitype  = site->get_string("gbi_type");
        gbiwithheld += Assets[i].gbinhold;
        gbitotal += Assets[i].gbingbis;
     }
     //
     //   Get the Defended Area information
     //
     if(site->get_logical("area_defense")) {
        Assets[i].gbinarea  = site->get_ntypes();
        Assets[i].gbiareas  = new char*[Assets[i].gbinarea];
        Assets[i].gbishape  = new char*[Assets[i].gbinarea];
        area = site->get_first_type();
        for (j=0; j<Assets[i].gbinarea; j++) {
           Assets[i].gbiareas[j] = area->get_name();
           gbiarealabel = area->get_logical("area_label");
           lat_deg = area->get_float("area_lat_deg");
           lat_min = area->get_float("area_lat_min");
           lat_sec = area->get_float("area_lat_sec");
           latitude = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
           if (!area->get_logical("area_north")) {
               latitude = -latitude;
           }
           lon_deg = area->get_float("area_lon_deg");
           lon_min = area->get_float("area_lon_min");
           lon_sec = area->get_float("area_lon_sec");
           longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
           if (!area->get_logical("area_east")) {
               longitude = -longitude;
           }
           Assets[i].gbishape[j]  = area->get_string("area_type");
           area_shape = AREA_CIRCLE;
           if (strstr(Assets[i].gbishape[j], "BOX")) area_shape = AREA_BOX;
           if (strstr(Assets[i].gbishape[j], "ELLIPSE")) area_shape = AREA_ELLIPSE;
           if (strstr(Assets[i].gbishape[j], "POLY")) area_shape = AREA_POLY;
	   // Open polygon file and get # of polygons
           gbiradius = new float[4+3*npolys];
           gbiradius[0] = area->get_float("area_major");
           gbiradius[1] = area->get_float("area_minor");
	   gbiradius[2] = area->get_float("area_orient");
           gbiradius[3] = (float)npolys;
	   // Load polygons here into [4+3*i], [5+3*i], [6+3*i]
           Assets[i].defarea = new GR_Defend(2100+i, area_shape, latitude, longitude,
                        gbiradius);
           //Assets[i].defarea->set_llah((float)latitude, (float)longitude, (float)gbiradius, 0.0);
           displist->add_object(Assets[i].defarea);
           area = site->get_next_type();
        }
     }
     //
     //   Get the Army/Navy unit information
     //
     if(site->get_logical("unit")) {
       printf("Processing Army %s\n", site->get_name() );
       armytype = site->get_int("type");
       armysize = site->get_float("size");
       Assets[i].unit = new GR_Army(armytype, latitude, longitude, armysize);
       displist->add_object (Assets[i].unit);
     }

     site = gbi_sites->get_next_type();
   }
/*
 *      Parse the Missile Threat parameter file
 *      ---------------------------------------
 */
   sprintf (threatfile, "%s%s", PARMDIR, "/missile.par");
   threat_parser = new C_PARSER("missile.par");
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
     if (!basetype->get_logical("north")) {
        threatlatNS[i] = "S";
        latitude = -latitude;
     }
     threatlonEW[i] = "E";
     if (!basetype->get_logical("east")) {
        threatlonEW[i] = "W";
        longitude = -longitude;
     }
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
     if (!basetype->get_logical("north")) {
        threattatNS[i] = "S";
        latitude = -latitude;
     }
     threattonEW[i] = "E";
     if (!basetype->get_logical("east")) {
        threattonEW[i] = "W";
        longitude = -longitude;
     }
     threattlat[i] = latitude;
     threattlon[i] = longitude;
     impact[i] = new GR_Impact (0, (float)latitude, (float)longitude, 
                      threatrdist[i], threatrdist[i]/3.0, 90.0);
     impact[i]->set_llah(latitude, longitude, 0.0, 0.0);
     displist->add_object(*impact[i]);
     missile_name = missile_names->get_next_type();
   }
/*
 *	Create the Motif window
 *	-----------------------
 */
   sh_shell = XtCreatePopupShell("Assets", topLevelShellWidgetClass, GR_toplevel, 0, 0);
   sh_form = XmCreateForm (sh_shell, "SHForm", NULL, 0);
   XtVaSetValues(sh_form,
               XmNwidth,              shwindW,
               XmNheight,             shwindH,
               NULL);
   dpy = XtDisplay(GR_toplevel);
   font = XLoadQueryFont(dpy,
          "-adobe-courier-bold-*-*-*-20-*-*-*-*-*-*-*");
   fontlist = XmFontListCreate(font, "charset1");
   classtitle   = XmStringCreateLtoR("Unclassified", "charset1");
   titlewidget  = XtVaCreateManagedWidget("class", xmLabelWidgetClass, sh_form,
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
                    (XtCallbackProc)asset_doneCB, NULL);

   gbi_info = XtVaCreateManagedWidget ("Details",
                 xmPushButtonWidgetClass, sh_form,
                 XmNwidth,            80,
                 XmNheight,           40,
                 XmNshadowThickness,  4,
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     done_button,
                 NULL);
   XtAddCallback (gbi_info, XmNactivateCallback,
                    (XtCallbackProc)gbi_infoCB, NULL);

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
   sprintf(line, "%s", "Assets");
   nodenoid  = XmStringCreateSimple((char *)line);
   label01  = XtVaCreateManagedWidget((String)line, xmLabelWidgetClass, sh_form,
                 XmNwidth,            100,
                 XmNheight,           20,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      nodenoid,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       septitle,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   XmStringFree(nodenoid);

   gbilist = (XmStringTable)XtMalloc(gbimaxid*sizeof(XmString *));
   for (i=0; i<gbimaxid; i++) {
       k = Assets[i].gbistn;
       printf("  [%d] Asset %d is %s\n", i, k, Assets[i].gbiname);
       gbilist[i] = XmStringCreateSimple(Assets[i].gbiname);
   }
   gbi_widget = XmCreateScrolledList (sh_form, "Assets", NULL, 0);
   XtVaSetValues (gbi_widget,
               XmNitems,              gbilist,
               XmNitemCount,          gbimaxid,
               XmNvisibleItemCount,   12,
               XmNscrollBarDisplayPolicy, XmSTATIC,
               NULL);
   XtVaSetValues (XtParent(gbi_widget),
               XmNtopAttachment,      XmATTACH_WIDGET,
               XmNtopWidget,          label01,
               XmNleftAttachment,     XmATTACH_WIDGET,
               XmNleftWidget,         septitle,
               NULL);
   XtManageChild(gbi_widget);
   XtAddCallback(gbi_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)gbilistCB, NULL);
   /*
   sprintf(line, "%s", "Threats");
   nodenoid  = XmStringCreateSimple((char *)line);
   label03  = XtVaCreateManagedWidget((String)line, xmLabelWidgetClass, sh_form,
                 XmNwidth,            100,
                 XmNheight,           20,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      nodenoid,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       label02,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   XmStringFree(nodenoid);
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
               XmNtopWidget,         label03,
               XmNleftAttachment,    XmATTACH_WIDGET,
               XmNleftWidget,        XtParent(gbr_widget),
               NULL);
   XtManageChild(threat_widget);
   XtAddCallback(threat_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)threatlistCB, NULL);
   */
   sprintf(line, "Selected Asset   %s", Assets[gbiselect].gbiname);
   nodenoid  = XmStringCreateSimple((char *)line);
   sh_str01  = XtVaCreateManagedWidget((String)line, xmLabelWidgetClass, sh_form,
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

   sprintf(line, "\n %s %d\n %s %d\n %s %d\n %s %d\n %s %d\n %s %d\n %s %d\n",
           "    GBI Assets .....", gbicount,
           "    GBR Assets .....", gbrcount,
           "    SIBRS Assets ...", eyecount,
           "    DSP Assets .....", dspcount,
           "    Air Assets .....", aircount,
           "    Sea Assets .....", seacount,
           "    Ground Assets ..", 0);
   nodenoid = XmStringCreateLtoR((char *)line, XmSTRING_DEFAULT_CHARSET);
   sh_str09  = XtVaCreateManagedWidget((String)line, xmLabelWidgetClass, sh_form,
                 XmNwidth,            180,
                 XmNheight,           120,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      nodenoid,
                 XmNrightAttachment,  XmATTACH_FORM,
                 XmNbottomAttachment, XmATTACH_WIDGET,
                 XmNbottomWidget,     botsep,
                 NULL);
   XmStringFree(nodenoid);

   XtManageChild (sh_form);

   shinit = TRUE;
}

void
asset_viewerCB (Widget toplevel)
{
   if (!shinit) asset_init();

   XtPopup(sh_shell, XtGrabNone);
   XmListSelectPos(gbi_widget, gbiselect+1, True);
}

void
asset_doneCB ()
{
   if (shinit) XtPopdown (sh_shell);
}

void
AssetPicked(int objid)
{
int   i;

   if (shinit) {
     for (i=0; i<gbimaxid; i++)
       if (objid == Assets[i].gbistn ) {
          gbiselect = i;
          AssetPopup(gbiselect);
	  return;
       }
   }
}

void
AssetPopup(int select)
{
   if (Assets[select].gbistn < 0) return;
   if (!asset_vis) {
      asset_details = XmCreateMessageDialog (sh_shell, "GBI Info", NULL, 0);
      XtAddCallback(asset_details, XmNokCallback,     detaildoneCB, NULL);
      XtAddCallback(asset_details, XmNcancelCallback, detaildoneCB,    NULL);
      XtManageChild (asset_details);
      asset_vis = True;
   }
   AssetUpdate(select);
}

void
AssetUpdate(int gbiselect)
{
Arg      arg[10];
char     str2[640];
char     str3[320];
char     str[1280];
XmString xstr;
int      i;

   if (!asset_vis) return;
   if (Assets[gbiselect].gbistn < 0) return;

   sprintf(str, " %s%s\n %s%d\n %s%f%s\n %s%f%s\n %s%s\n\n",
                "Name of Asset:         ", Assets[gbiselect].gbiname,
                "Id of Asset:           ", Assets[gbiselect].gbistn,
	        "Latitude of Asset:     ", (float)Assets[gbiselect].gbilatit,
                                                  Assets[gbiselect].gbilatNS,
	        "Longitude of Asset:    ", (float)Assets[gbiselect].gbilongi,
                                                  Assets[gbiselect].gbilonEW,
	        "Operational Status:    ", "GREEN");

   if (Assets[gbiselect].gbingbis > 0) {
      sprintf(str2, " %s%d\n %s%d\n %s%f\n\n",
                "No. of Interceptors:   ", Assets[gbiselect].gbingbis,
                "Interceptors Withheld: ", Assets[gbiselect].gbinhold,
                "Probability of Kill:   ", Assets[gbiselect].gbipkill);
      strcat(str, str2);
   }

   if (strcmp(Assets[gbiselect].gbisensr, "NONE") != 0) {
      sprintf(str2, " %s%s\n %s%f\n %s%f\n %s%f/%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f/%f\n %s%f\n\n",
                "Sensor Type:           ", Assets[gbiselect].gbisensr, 
		"Scan  Time:            ", Assets[gbiselect].gbiscanT,
		"Minimum  Range:        ", Assets[gbiselect].gbirmin,
		"Maximum  Range:        ", Assets[gbiselect].gbirmax,
                                           Assets[gbiselect].gbirlow,
		"Minimum  Rdot:         ", Assets[gbiselect].gbirdot,
		"Signal:                ", Assets[gbiselect].gbisignal,
		"Luminosity:            ", Assets[gbiselect].gbilumin,
		"Elevation Angle:       ", Assets[gbiselect].gbielev,
		"Azimuth Angle (N):     ", Assets[gbiselect].gbiazi,
		"Field-of-View:         ", Assets[gbiselect].gbifovhi,
                                           Assets[gbiselect].gbifovlo,
		"Error:                 ", Assets[gbiselect].gbierror);
      strcat(str, str2);
   }

   if (Assets[gbiselect].gbinarea > 0) {
      sprintf(str3, " %s%d\n",
		"No. of Defended Areas: ", Assets[gbiselect].gbinarea);
      strcat(str, str3);
      for (i=0; i<Assets[gbiselect].gbinarea; i++) {
         sprintf(str3, " %s%2d%s%s\n",
                "   Area ", i+1, ":            ", Assets[gbiselect].gbiareas[i]);
         strcat(str, str3);
      }
   }

   xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(asset_details, XmNmessageString, xstr, NULL);
   //XtSetArg (arg[0], XmNmessageString, xstr);
   XmStringFree (xstr);
}

void
detaildoneCB(Widget, XtPointer, XtPointer call_data)
{
   asset_vis = False;
}

void
gbi_infoCB ()
{
   AssetPopup(gbiselect);
}

void
threat_infoCB ()
{
Widget      dialog;
Arg         arg[10];
char        str[1280];
XmString    xstr;

   sprintf (str, 
        " %s%s\n %s%s\n %s%f\n %s%f\n %s%s\n %s%f%s\n %s%f%s\n %s%s\n %s%f%s\n %s%f%s\n", 
                "Name of Threat:        ", threatnames[threatselect],
                "Type of Threat:        ", threattypes[threatselect],
	        "Threat Launch Time:    ", threatltime[threatselect],
	        "Threat Random Distance:", threatrdist[threatselect],
	        "Name of Launch Site    ", threatlsite[threatselect],
                "Latitude of Launch:    ", threatllat[threatselect], "N",
                "Longitude of Launch:   ", threatllon[threatselect], "E",
	        "Name of Target Site    ", threattsite[threatselect],
                "Latitude of Target:    ", threattlat[threatselect], "N",
		"Longitude of Target:   ", threattlon[threatselect], "E");

   xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
   XtSetArg (arg[0], XmNmessageString, xstr);
   dialog = XmCreateMessageDialog (sh_shell, "Threat Info", arg, 1);
   XmStringFree (xstr);
   XtManageChild (dialog);
}

void
gbilistCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
char         line[80];
int          j;
XmString     nodenoid;
Arg          args[10];

   XmStringGetLtoR (call_data->item, XmSTRING_DEFAULT_CHARSET, &gbitext);
   gbiselect = call_data->item_position - 1;
   sprintf(line, "Selected GBI:    %s", Assets[gbiselect].gbiname);
   nodenoid = XmStringCreateSimple((char *)line);
   j = 0;
   XtSetArg(args[j], XmNlabelString, nodenoid); j++;
   XtSetValues(sh_str01, args, j);
}

void
threatlistCB (Widget, XtPointer, XmListCallbackStruct* call_data)
{
char         line[80];
int          j;
XmString     nodenoid;
Arg          args[10];

   XmStringGetLtoR (call_data->item, XmSTRING_DEFAULT_CHARSET, &threattext);
   threatselect = call_data->item_position - 1;
   sprintf(line, "Selected Threat: %s", threatnames[threatselect]);
   nodenoid = XmStringCreateSimple((char *)line);
   j = 0;
   XtSetArg(args[j], XmNlabelString, nodenoid); j++;
   XtSetValues(sh_str00, args, j);
   //printf("Selected Threat - %s\n", threattext);
}
