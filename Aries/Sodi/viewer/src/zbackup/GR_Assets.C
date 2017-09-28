/************************************************************
  shoot.C is a GBI -> threat manual shooter.
************************************************************/

#include <math.h>
#include <sys/types.h>
#include <malloc.h>

#include "GR_Shell.H"
#include "GR_Model.H"
#include "GR_DispList.H"
#include "GISP_Obj.H"
#include "GR_String.H"
#include "GR_Defend.H"
#include "GR_Impact.H"
#include "parsman.H"
#include "convert.H"

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

#define RE 6378.145

extern GR_Window *gwindow;
Widget	      sh_shell;
GR_Window     *shshwindow;
Boolean	      shinit = FALSE;
int	      shwindW=480, shwindH=480;
Widget	      sh_str00, sh_str01, sh_str02, sh_str09;
Widget        gbr_widget, gbi_widget, threat_widget;
char	      *gbitext, *gbrtext, *threattext;
GR_Sensor     *ground_radars[64];
GR_String     *radars_labels[64];

int	      gbicount   = 15;	        // No. GBIs listed
int	      gbiselect  = 0;		// Currently selected GBI
int	      gbitotal;		        // Total Interceptor count
int	      gbiicon;		        // GBI display icon id
int           gbiwithheld;              // GBIs withheld for future use
int           gbiexpended = 10;         // GBIs used
int           gbi_per_rv = 2;           // GBIs fired per threat
int           gbidefend;                // True if defended area to be displayed
float         gbiradius;                // Radius (Km) of defended area
float         gbipk; 
int	      *gbingbis;		// No. of interceptors in farm
int           *gbinhold;                // No. of interceptors withheld
int	      *gbiident;		// Farm identifier
float	      *gbipkill;		// Probability of Kill
int           *gbiholds;                // GBIs withheld
double	      *gbilatit;		// Latitude of Farm
double	      *gbilongi;		// Longitude of Farm
char	      **gbilatNS;		// Latitude is N or S
char	      **gbilonEW;		// Longitude is E or W
char          **gbinames;		// Farm names
char	      **gbitypes;		// Type of GBI at Farm

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
double        *gbrlatit;                // Latitude of Site
double        *gbrlongi;                // Longitude of Site
char          **gbrlatNS;               // Latitude is N or S
char          **gbrlonEW;               // Longitude is E or W
char          **gbrnames;               // Site names
char          **gbrtypes;               // Type of GBR at Site
GISP_Obj      *gbrsensor=NULL;

int           sentype;                  // 0=CONE, 1=DOME, 2=DISK, 3=TORUS, 4=OTHB
float         senscanT;                 // Scan Time
float         senrmin;                  // Minimum range
float         senrmaxhi;                // Maximum range in high power
float         senrmaxlo;                // Maximum range in low power
float         senelev;                  // Aiming elevation
float         senazim;                  // Aiming azimuth
float         senfovhi;                 // Field of view in high power
float         senfovlo;                 // Field of view in low power
float         senrdot;                  //
float         sensignal;                //
float         senlumin;                 // Luminosity
float         senerror;                 //

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
float         *dspscanT;                // Scan Time
float         *dsprmin;                 //
float         *dsprmax;                 //
float         *dsprdot;                 //
float         *dspsignal;               //
float         *dsplumin;                // Luminosity
float         *dsperror;                //
double        *dsplongi;
char          **dspnames;               // Sensor names

int           aircount  = 0;
int           airselect = 0;
int           airtotal;

int           seacount  = 0;
int           seaselect = 0;
int           seatotal;

#define GBRTYPE 1                       // Ground-based radars
#define EYETYPE 2                       // SIBRS
#define DSPTYPE 3                       // DSP
#define GBITYPE 4                       // Interceptor Farms
#define AIRTYPE 5                       // Air breathing (aircraft, cruise, etc)
#define SEATYPE 6                       // Sea going (ships, AEGIS, etc)
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

void          asset_init ();
void          asset_viewerCB (Widget);
void          gbi_infoCB ();
void          gbr_infoCB ();
void          threat_infoCB();
void          gbilistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void          gbrlistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void          threatlistCB (Widget, XtPointer, XmListCallbackStruct* call_data);
void          asset_doneCB ();


void
asset_init ()
{
Widget        sh_form;
Widget        sh_list; 
Widget        sh_frame;
Widget        sh_control, done_button;
Widget        gbr_info, gbi_info, threat_info;
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

int           i, id, index;
int           count; 
char	      line[80];
char	      *PARMDIR;
char          gbifile[80];
char          gbrfile[80];
char          threatfile[80];
float         values[10];
GR_Defend     *defarea;
GR_Impact     *impact[64];
extern GR_DispList *displist;
extern GR_DispList *sensor_displist;

static C_PARSER *parameters_parser;
static C_PARSER *position_parser;
static C_PARSER *gbi_parser = NULL;
static C_PARSER *ground_sensor_parser = NULL;
static C_PARSER *threat_parser = NULL;
static C_PARSER *eye_parser = NULL;
static C_PARSER *dsp_parser;

C_BASETYPE      *gbi_sites, *site, *sensor_type, *basetype;
C_BASETYPE      *sensor_names, *sensor_name, *radar_name;
C_BASETYPE      *missile_names, *missile_name;
C_BASETYPE      *eye_names, *eye_name;
C_BASETYPE      *dsp_names, *dsp_name;

C_CONVERT       conversion;

double          latitude, longitude, X[3];
double          lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec;
float           scale_factor;

   if (shinit) return;
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
   gbipk    = gbi_sites->get_float("pkill");
   gbiicon  = gbi_sites->get_int("icon");
//   if (on_off) on_off = gbi_sites->get_logical("on_off");
   gbicount = gbi_sites->get_ntypes();
   gbingbis = new int[gbicount];
   gbiident = new int[gbicount];
   gbiholds = new int[gbicount];
   gbipkill = new float[gbicount];
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
     gbiholds[i] = site->get_int("n_hold");
     gbipkill[i] = site->get_float("pkill");
     gbiwithheld += gbiholds[i];
     gbitotal += gbingbis[i];

     lat_deg = site->get_float("lat_deg");
     lat_min = site->get_float("lat_min");
     lat_sec = site->get_float("lat_sec");
     latitude = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     gbilatNS[i] = "N";
     if (!site->get_logical("north")) {
          latitude = -latitude;
	  gbilatNS[i] = "S";
     }
     lon_deg = site->get_float("lon_deg");
     lon_min = site->get_float("lon_min");
     lon_sec = site->get_float("lon_sec");
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     gbilonEW[i] = "E";
     if (!site->get_logical("east")) {
          longitude = -longitude;
	  gbilonEW[i] = "W";
     }
     gbilatit[i] = latitude;
     gbilongi[i] = longitude;

     gbidefend = site->get_logical("area_defense");
     if (gbidefend) {
        lat_deg = site->get_float("area_lat_deg");
        lat_min = site->get_float("area_lat_min");
        lat_sec = site->get_float("area_lat_sec");
        latitude = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
        if (!site->get_logical("area_north")) {
            latitude = -latitude;
        }
        lon_deg = site->get_float("area_lon_deg");
        lon_min = site->get_float("area_lon_min");
        lon_sec = site->get_float("area_lon_sec");
        longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
        if (!site->get_logical("area_east")) {
            longitude = -longitude;
        }
        gbiradius = site->get_float("area_size");
        //fprintf(stderr, "Defending area %f %f %f\n", latitude, longitude, radius);
        defarea = new GR_Defend(0, latitude, longitude, gbiradius);
        displist->add_object(*defarea);
     }
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
   gbrsensor = new GISP_Obj[gbrcount];
   gbrtotal = 0;
   sensor_name = sensor_names->get_first_type();
   for (i=0; i<gbrcount; i++) {
     gbrnames[i] = sensor_name->get_name();
     gbrident[i] = i;
     //gbricon[i]  = sensor_name->get_int("icon");
     lat_deg = sensor_name->get_float("lat_deg");
     lat_min = sensor_name->get_float("lat_min");
     lat_sec = sensor_name->get_float("lat_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     gbrlatNS[i] = "N";
     if (!sensor_name->get_logical("north")) {
          latitude = -latitude;
	  gbrlatNS[i] = "S";
     }
     lon_deg = sensor_name->get_float("lon_deg");
     lon_min = sensor_name->get_float("lon_min");
     lon_sec = sensor_name->get_float("lon_sec");
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     gbrlonEW[i] = "E";
     if (!sensor_name->get_logical("east")) {
          longitude = -longitude;
	  gbrlonEW[i] = "W";
     }
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
        //gbricon[i]   = radar_name->get_int("icon");
	      sentype   = radar_name->get_int("type");
              senrmin   = radar_name->get_float("rmin");
              senrmaxhi = radar_name->get_float("rmax");
              senrmaxlo = radar_name->get_float("rmax_low");
              senelev   = radar_name->get_float("elevation");
              senazim   = radar_name->get_float("azimuth");
	      senfovhi  = radar_name->get_float("fov_high");
	      senfovlo  = radar_name->get_float("fov_low");
              senrdot   = radar_name->get_float("rdotmin");
              sensignal = radar_name->get_float("signal");
              senlumin  = radar_name->get_float("luminosity");
              senerror  = radar_name->get_float("error");
              senscanT  = radar_name->get_float("scan_time");
              switch (sentype) {
	      case 0:
                values[0] = senrmin;
		values[1] = senrmaxhi;
		values[2] = senelev;    // elevation angle
		break;
	      case 1:
		values[0] = senrmaxhi;
		break;
	      case 2:
                values[0] = 240.0;
		values[1] = 70.0;
                values[2] = senrmaxhi;
		values[3] = senrmin;
		break;
	      case 3:
                values[0] = 120.0;
		values[1] = senrmaxhi;
		break;
	      case 4:
                values[0] = 240.0;
		values[1] = 70.0;
                values[2] = senrmaxhi;
		values[3] = senrmin;
		break;
	      default:
		break;
	      }
              ground_radars[i] = new GR_Sensor(gbrbase+i,gbrbase+i, sentype,
                                                longitude,latitude, values);
	      sensor_displist->add_object(ground_radars[i]);
	      /*
              radars_labels[i] =
	            new GR_String((float)latitude, (float)longitude, "GBR");
              displist->add_object(radars_labels[i]);

              conversion.latlon_to_xyz(latitude, longitude, X);
              id = 5000+i;
              gbrsensor[i].init(id, 20);
              gbrsensor[i].translate ((float)X[0]/RE, (float)X[1]/RE, (float)X[2]/RE);
              gbrsensor[i].rotate_z (180);
              scale_factor = 1.0; //get_scale (icontype)*Sensors[i].scale;
              gbrsensor[i].scale (scale_factor, scale_factor, scale_factor);
              gbrsensor[i].finish();
              displist->add_object(&gbrsensor[i]);
	      */
     gbrtotal = gbrtotal+1;
     sensor_name = sensor_names->get_next_type();
   }
/*
 *      Parse the SBIRS Sensors parameter file
 *      --------------------------------------
 */
   eye_parser = new C_PARSER("eye.par");
   eye_names = eye_parser->get_basetype("constellation");
   eyecount  = 1;
   eyenames  = "SIBRS";
   eyerings  = eye_names->get_int("n_rings");
   eyesize   = eye_names->get_int("n_per_ring");
   eyealt    = eye_names->get_float("altitude");
   eyeincl   = eye_names->get_float("inclination");
   eye_name  = eye_parser->get_basetype("BRILLIANT_EYE_SENSOR");
   eyescanT  = eye_name->get_float("scan_time");
   eyermin   = eye_name->get_float("rmin");
   eyermax   = eye_name->get_float("rmax");
   eyerdot   = eye_name->get_float("rdotmin");
   eyesignal = eye_name->get_float("signal");
   eyelumin  = eye_name->get_float("luminosity");
   eyeerror  = eye_name->get_float("error");
/*
 *      Parse the DSP Sensors parameter file
 *      ------------------------------------
 */
   dsp_parser = new C_PARSER("dsp.par");
   dsp_names = dsp_parser->get_basetype("dsp_names");
   dspcount = dsp_names->get_ntypes();
   dspnames = new char*[dspcount];
   dspscanT = new float[dspcount];
   dsprmin = new float[dspcount];
   dsprmax = new float[dspcount];
   dsprdot = new float[dspcount];
   dspsignal = new float[dspcount];
   dsplumin = new float[dspcount];
   dsperror = new float[dspcount];
   dsplongi = new double[dspcount];
   dsptotal = 0;
   dsp_name = dsp_names->get_first_type();
   for (i=0; i<dspcount; i++) {
     dspnames[i] = dsp_name->get_name();
     dsptotal = dsptotal+1;
     dsplongi[i] = dsp_name->get_float("longitude");
     radar_name = dsp_name->get_first_type();
        dsprmin[i]   = radar_name->get_float("rmin");
        dsprmax[i]   = radar_name->get_float("rmax");
        dsprdot[i]   = radar_name->get_float("rdotmin");
        dspsignal[i] = radar_name->get_float("signal");
        dsplumin[i]  = radar_name->get_float("luminosity");
        dsperror[i]  = radar_name->get_float("error");
        dspscanT[i]  = radar_name->get_float("scan_time");
     dsp_name = dsp_names->get_next_type();
   }

   assetcount = gbrcount+eyecount+dspcount+gbicount+aircount+seacount;
   astypes = new int[assetcount];
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
                      threatrdist[i], threatrdist[i], 0.0);
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
                    (XtCallbackProc)asset_doneCB, NULL);

   gbi_info = XtVaCreateManagedWidget ("GBI Info",
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
   sprintf(line, "%s", "GBIs");
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
               XmNtopWidget,         label01,
               XmNleftAttachment,    XmATTACH_WIDGET,
               XmNleftWidget,        septitle,
               NULL);
   XtManageChild(gbi_widget);
   XtAddCallback(gbi_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)gbilistCB, NULL);

   sprintf(line, "%s", "Sensors");
   nodenoid  = XmStringCreateSimple((char *)line);
   label02  = XtVaCreateManagedWidget((String)line, xmLabelWidgetClass, sh_form,
                 XmNwidth,            100,
                 XmNheight,           20,
                 XmNalignment,        XmALIGNMENT_BEGINNING,
                 XmNlabelType,        XmSTRING,
                 XmNlabelString,      nodenoid,
                 XmNleftAttachment,   XmATTACH_WIDGET,
                 XmNleftWidget,       label01,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        topsep,
                 NULL);
   XmStringFree(nodenoid);
   printf("Asset count id %d\n", gbrcount+eyecount+dspcount);
   gbrlist = (XmStringTable)XtMalloc((gbrcount+eyecount+dspcount)*sizeof(XmString *));
   index = -1;   
   for (i=0; i<gbrcount; i++) {
       index = index+1;
       astypes[index] = GBRTYPE;
       gbrlist[index] = XmStringCreateSimple(gbrnames[i]);
   }
   for (i=0; i<eyecount; i++) {
       index = index+1;
       astypes[index] = EYETYPE;
       gbrlist[index] = XmStringCreateSimple(eyenames);
   }
   for (i=0; i<dspcount; i++) {
       index = index+1;
       astypes[index] = DSPTYPE;
       gbrlist[index] = XmStringCreateSimple(dspnames[i]);
   }
   printf("  Index value is %d\n", index);
   gbr_widget = XmCreateScrolledList (sh_form, "GBRs", NULL, 0);
   XtVaSetValues (gbr_widget,
               XmNitems,             gbrlist,
               XmNitemCount,         index,
               XmNvisibleItemCount,  14,
               XmNscrollBarDisplayPolicy, XmSTATIC,
               NULL);
   XtVaSetValues (XtParent(gbr_widget),
               XmNtopAttachment,     XmATTACH_WIDGET,
               XmNtopWidget,         label02,
               XmNleftAttachment,    XmATTACH_WIDGET,
               XmNleftWidget,        XtParent(gbi_widget),
               NULL);
   XtManageChild(gbr_widget);
   XtAddCallback(gbr_widget, XmNbrowseSelectionCallback,
                 (XtCallbackProc)gbrlistCB, NULL);

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

   sprintf(line, "\n %s %d\n %s %d\n %s %d\n %s %d\n %s %d\n %s %d\n",
           "    GBR Assets .....", gbrcount,
           "    SIBRS Assets ...", eyecount,
           "    DSP Assets .....", dspcount,
           "    GBI Assets .....", gbicount,
           "    Air Assets .....", aircount,
           "    Sea Assets .....", seacount);
   nodenoid = XmStringCreateLtoR((char *)line, XmSTRING_DEFAULT_CHARSET);
   sh_str09  = XtVaCreateManagedWidget((String)line,xmLabelWidgetClass,sh_form,
                 XmNwidth,            180,
                 XmNheight,           80,
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
   XmListSelectPos(gbi_widget, gbiselect, True);
   XmListSelectPos(threat_widget, threatselect, True);
   XmListSelectPos(gbr_widget, gbrselect, True);
}

void
gbi_infoCB ()
{
Widget   dialog;
Arg      arg[10];
char     str[1280];
XmString xstr;

   fprintf(stderr, "  Select GBI %d\n", gbiselect);
   sprintf(str, " %s%s\n %s%d\n %s%f\n %s%d\n %s%f%s\n %s%f%s\n %s%s\n",
                "Name of GBI Farm:      ", gbinames[gbiselect],
                "Id of Farm:            ", gbiident[gbiselect],
                "Probability of Kill:   ", gbipkill[gbiselect],
                "No. of Interceptors:   ", gbingbis[gbiselect],
	        "Latitude of Farm:      ", (float)gbilatit[gbiselect], gbilatNS[gbiselect],
	        "Longitude of Farm:     ", (float)gbilongi[gbiselect], gbilonEW[gbiselect],
	        "Operational Status:    ", "GREEN");

   xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
   XtSetArg (arg[0], XmNmessageString, xstr);
   dialog = XmCreateMessageDialog (sh_shell, "GBI Info", arg, 1);
   XmStringFree (xstr);
   XtManageChild (dialog);
}

void
gbr_infoCB ()
{
Widget   dialog;
Arg      arg[10];
char     str[1280];
XmString xstr;
int      i, isave;

   switch (astypes[gbrselect]) {
   case GBRTYPE:
     i = gbrselect;
    sprintf (str, " %s%s\n %s%d\n %s%f%s\n %s%f%s\n %s%s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n", 
                 "Name of GBR Site:      ", gbrnames[gbrselect],
                 "Id of Site:            ", gbrident[gbrselect],
                 "Latitude of Site:      ", gbrlatit[gbrselect], gbrlatNS[gbrselect],
                 "Longitude of Site:     ", gbrlongi[gbrselect], gbrlonEW[gbrselect],
                 "Operational Status:    ", "GREEN",
		 "Scan  Time:            ", gbrscanT[gbrselect],
		 "Minimum  Range:        ", gbrrmin[gbrselect],
		 "Maximum  Range:        ", gbrrmax[gbrselect],
		 "Minimum  Rdot:         ", gbrrdot[gbrselect],
		 "Signal:                ", gbrsignal[gbrselect],
		 "Luminosity:            ", gbrlumin[gbrselect],
		 "Error:                 ", gbrerror[gbrselect]);
     break;
   case EYETYPE:
     sprintf (str,
" %s%s\n %s%d\n %s%d\n %s%f\n %s%f\n %s%s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n",
                 "Name of Sensor:        ", eyenames,
                 "No. of Rings:          ", eyerings,
                 "No. of Sensors/Ring:   ", eyesize,
                 "Altitude:              ", eyealt,
                 "Inclination:           ", eyeincl,
                 "Operational Status:    ", "GREEN",
                 "Scan Time:             ", eyescanT,
                 "Minimum Range:         ", eyermin,
                 "Maximum Range:         ", eyermax,
                 "Minimum Rdot:          ", eyerdot,
                 "Signal:                ", eyesignal,
                 "Luminosity:            ", eyelumin,
                 "Error:                 ", eyeerror);
     break;
   case DSPTYPE:
     i = gbrselect-gbrcount-eyecount;
     sprintf (str,
" %s%s\n %s%f\n %s%s\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n %s%f\n",
                 "Name of Sensor:        ", dspnames[i],
                 "Longitude:             ", dsplongi[i],
                 "Operational Status:    ", "GREEN",
                 "Scan Time:             ", dspscanT[i],
                 "Minimum Range:         ", dsprmin[i],
                 "Maximum Range:         ", dsprmax[i],
                 "Minimum Rdot:          ", dsprdot[i],
                 "Signal:                ", dspsignal[i],
                 "Luminosity:            ", dsplumin[i],
                 "Error:                 ", dsperror[i]);
     break;
   case GBITYPE:
     i = gbrselect-gbrcount-eyecount-dspcount;
     sprintf (str, " %s%s\n %s%d\n %s%f\n %s%d\n %s%d\n %s%f%s\n %s%f%s\n %s%s\n",
                 "Name of GBI Farm:      ", gbinames[i],
                 "Id of Farm:            ", gbiident[i],
                 "Probability of Kill:   ", gbipkill[i],
                 "No. of Interceptors:   ", gbingbis[i],
                 "No. Withheld:          ", gbinhold[i],
                 "Latitude of Farm:      ", gbilatit[i], gbilatNS[i],
                 "Longitude of Farm:     ", gbilongi[i], gbilonEW[i],
                 "Operational Status:    ", "GREEN"/*gbistatus[i]*/);
     break;
   case AIRTYPE:
     i = gbrselect-gbrcount-eyecount-dspcount-gbicount;
     sprintf(str, "Air information not yet available.\n");
     break;
   case SEATYPE:
     i = gbrselect-gbrcount-eyecount-dspcount-gbicount-aircount;
     sprintf(str, "Sea information not yet available.\n");
     break;
   default:
     sprintf(str, "Error - Illegal asset type chosen.\n");
     break;
   }

   xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
   XtSetArg (arg[0], XmNmessageString, xstr);
   dialog = XmCreateMessageDialog (sh_shell, "GBR Info", arg, 1);
   XmStringFree (xstr);
   XtManageChild (dialog);
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
char         line[80];
int          j;
XmString     nodenoid;
Arg          args[10];

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

void
asset_doneCB ()
{
   if (shinit) XtPopdown (sh_shell);
}
