/************************************************************
  ---   assets.C loads the assets.par file   ---
************************************************************/

#include <math.h>
#include <sys/types.h>
#include <malloc.h>

#include "def.H"
#include "parser.H"
#include "convert.H"
#include "assets.H"

#define GBRTYPE   1                       // Ground-based radars
#define EYETYPE   2                       // SIBRS
#define DSPTYPE   3                       // DSP
#define GBITYPE   4                       // Interceptor Farms
#define AIRTYPE   5                       // Air breathing (aircraft, cruise, etc)
#define SEATYPE   6                       // Sea going (ships, AEGIS, etc)

int	        shinit = FALSE;
int	        shwindW=480, shwindH=480;
char	        *gbitext, *gbrtext, *threattext;
int             asset_vis;

int	        gbicount   = 15;	  // No. GBIs listed
int             gbimaxid   = 20;
int	        gbiselect  = 0;		  // Currently selected GBI
int	        gbitotal;		  // Total Interceptor count
int             gbiwithheld;              // GBIs withheld for future use
int             gbiexpended = 10;         // GBIs used
int             gbi_per_rv = 2;           // GBIs fired per threat
int             gbidefend;                // True if defended area to be displayed
int             gbiarealabel;             // True if defended area is to be labelled
float           *gbiradius;               // Radius (Km) of defended area
int	        gbiicon;		  // GBI display icon id from .par file
float           gbipk;                    // Default probability of kill from .par file
int             gbilabel;                 // True if GBI site is to be labelled (.par file)
int             gbicover;                 // True if coverage patterns are to be displayed
struct SENSOR   *Assets;

int             gbrbase    = 1950;        // First GL Display List ID for GBRs
int             gbrcount   = 15;          // No. GBRs listed
int             gbrselect  = 0;           // Currently selected GBR
int             gbrtotal;                 // Total Interceptor count
int             *gbricon;		  // GBR display icon id
int             *gbrident;                // Site identifier
float	        *gbrscanT;		  // Scan Time
float	        *gbrrmin;		  //
float	        *gbrrmax;		  //
float	        *gbrrdot;		  //
float	        *gbrsignal;	          //
float	        *gbrlumin;		  // Luminosity
float	        *gbrerror;		  //
float	        *gbrrlow;	  	  // Low-power range
float           *gbrelev;                 // Sighting elevation angle
float           *gbrazi;                  // Sighting elevation angle (from N)
float           *gbrfovhi;                // High-power Field-of-View
float           *gbrfovlo;                // Low-power Field-of-View
double          *gbrlatit;                // Latitude of Site
double          *gbrlongi;                // Longitude of Site
char            **gbrlatNS;               // Latitude is N or S
char            **gbrlonEW;               // Longitude is E or W
char            **gbrnames;               // Site names
char            **gbrtypes;               // Type of GBR at Site
//GR_Sensor       *gbrsensor[64];
//GR_String       *gbrlabels[64];

int             eyecount  = 1;
int             eyeselect = 0;
int             eyetotal;
int             eyerings, eyesize;
float           eyealt, eyeincl, eyescanT, eyermin, eyermax, eyerdot;
float           eyesignal, eyelumin, eyeerror;
char            *eyenames;

int             dspcount   = 5;           // No. GBRs listed
int             dspselect  = 0;           // Currently selected GBR
int             dsptotal;                 // Total Interceptor count
int             dspicon;
int             dsprings, dspsize;
float           dspalt;
float           dspscanT;                 // Scan Time
float           dsprmin;                  //
float           dsprmax;                  //
float           dsprdot;                  //
float           dspsignal;                //
float           dsplumin;                 // Luminosity
float           dsperror;                 //
double          dsplongi;
char            *dspnames;                // Sensor names

int             aircount  = 0;
int             airselect = 0;
int             airtotal;
int             seacount  = 0;
int             seaselect = 0;
int             seatotal;

int             assetcount;
int             *astypes;                 // Asset type

int	        threatcount  = 6;         // No. of threats listed
int	        threatselect = 0;         // Currently selected threat
int	        threattotal;              // Total Threat count
char	        **threatnames;            // Threat names
char            **threattypes;            // Threat types (ie: SCUD, SS18)
char            **threatlsite;            // Launch site
char            **threattsite;            // Target site
float           *threatltime;             // Launch time
float           *threatrdist;             // Random distance
double          *threatllat;              // Launch latitude
double          *threatllon;              // Launch longitude
double          *threattlat;              // Target latitude
double          *threattlon;              // Target longitude
char            **threatlatNS;
char            **threatlonEW;
char            **threattatNS;
char            **threattonEW;

int             armycount = 0, armybase = 1995, armytype;
float           armysize;

int   AssetLoad(char *loadfile);
int   AssetGetLLAH(int index, float *lat, float *lon, float *alt, float *head);
int   AssetGetIcon(int index);
float AssetGetScale(int index);
char  *AssetGetName(int index);
void  AssetPicked(int type);
void  AssetPopup(int);
void  AssetUpdate(int);
void  gbi_infoCB ();
void  threat_infoCB();

/* ================================================================================= */

int
AssetLoad(char *loadfile)
{
int             i, j, k, id, index;
int             count;
int             npolys = 0;
char	        line[180];
char	        *PARMDIR;
char            gbifile[128];
char            gbrfile[128];
char            threatfile[128];
float           values[10];

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

   if (shinit) return(-1);
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
     //Assets[i].gbisensor = NULL;
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
        fprintf(stderr, "Asset ID %d greater than specified Max ID of %d\n", i, gbimaxid);
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
        Assets[i].gbisensr  = site->get_string("sensor_type");
        gbicover            = site->get_logical("coverage");
        Assets[i].gbielev   = site->get_float("elevation");
        Assets[i].gbiazi    = site->get_float("azimuth");
        Assets[i].gbir      = site->get_int("r");
        Assets[i].gbig      = site->get_int("g");
        Assets[i].gbib      = site->get_int("b");
        radar_name          = gbi_parser->get_basetype(Assets[i].gbisensr);
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
	/*
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
        //
        //   Define sensor with coverage
        //
        if (gbicover != 0) {
           values[0]    = Assets[i].gbielev;
           values[1]    = Assets[i].gbiazi;
           values[2]    = Assets[i].gbirmax;
           values[3]    = Assets[i].gbirlow;
           values[4]    = Assets[i].gbifovhi;
           Assets[i].gbisensor = new GR_Sensor(1900+i, Assets[i].gbistn, Assets[i].gbistype, 
                                      (float)latitude, (float)longitude, values);
           sensor_displist->add_object(Assets[i].gbisensor);
           fprintf(INFOfp, "ASSETS:  Putting a sensor at %f %f\n", latitude, longitude);
	}
        //
        //   Label the sensor if wanted
        //
        if (gbilabel != 0) {
           Assets[i].gbilabel = new GR_String((float)latitude, (float)longitude,
                            Assets[i].gbiname);
           displist->add_object(Assets[i].gbilabel);
        }
	*/
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
	   /*
           Assets[i].defarea = new GR_Defend(2100+i, area_shape, latitude, longitude,
                        gbiradius);
           //Assets[i].defarea->set_llah((float)latitude, (float)longitude, (float)gbiradius, 0.0);
           displist->add_object(Assets[i].defarea);
	   */
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
       /*
       Assets[i].unit = new GR_Army(armytype, latitude, longitude, armysize);
       displist->add_object (Assets[i].unit);
       */
     }

     site = gbi_sites->get_next_type();
   }
/*
 *      Parse the Missile Threat parameter file
 *      ---------------------------------------
 */
   //sprintf (threatfile, "%s%s", PARMDIR, "/missile.par");
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
     /*
     impact[i] = new GR_Impact (0, (float)latitude, (float)longitude, 
                      threatrdist[i], threatrdist[i]/3.0, 90.0);
     impact[i]->set_llah(latitude, longitude, 0.0, 0.0);
     displist->add_object(*impact[i]);
     */

     missile_name = missile_names->get_next_type();
   }

   shinit = TRUE;

   return (gbicount);
}

int
AssetGetLLAH(int index, float *lat, float *lon, float *alt, float *head)
{
   if (!shinit || index < 0 || index > gbicount) return(-1);
   *lat = Assets[index].gbilatit;
   *lon = Assets[index].gbilongi;
   *alt = Assets[index].gbialt;
   *head = 0.0;
   return(0);
}

int
AssetGetIcon(int index)
{
   if (!shinit || index < 0 || index > gbicount) return(-1);
   return(Assets[index].gbiicon);
}

float
AssetGetScale(int index)
{
   if (!shinit || index < 0 || index > gbicount) return(-1);
   return(Assets[index].gbiscale);
}

char *
AssetGetName(int index)
{
   if (!shinit || index < 0 || index > gbicount) return(NULL);
   return(Assets[index].gbiname);
}

void
AssetPicked(int objid)
{
int             i;

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
     //asset_details = XmCreateMessageDialog (sh_shell, "GBI Info", NULL, 0);
     //XtAddCallback(asset_details, XmNokCallback,     detaildoneCB, NULL);
     //XtAddCallback(asset_details, XmNcancelCallback, detaildoneCB,    NULL);
     //XtManageChild (asset_details);
      asset_vis = TRUE;
   }
   AssetUpdate(select);
}

void
AssetUpdate(int gbiselect)
{
char            str2[640];
char            str3[320];
char            str[1280];
int             i;

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
      sprintf(str2, " %s%d\n %s%d\n %s%f\n %s%f\n %s%f\n\n",
                "No. of Interceptors:   ", Assets[gbiselect].gbingbis,
                "Interceptors Withheld: ", Assets[gbiselect].gbinhold,
                "Pk (assuming hit):     ", Assets[gbiselect].gbipkill,
                "Minimum Altitude:      ", Assets[gbiselect].gbipkill,
                "Maximum Altitude:      ", Assets[gbiselect].gbipkill);
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

   //xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
   //XtVaSetValues(asset_details, XmNmessageString, xstr, NULL);
   //XtSetArg (arg[0], XmNmessageString, xstr);
   //XmStringFree (xstr);
}

void
gbi_infoCB ()
{
   AssetPopup(gbiselect);
}

void
threat_infoCB ()
{
char             str[1280];

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

   //xstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
   //XtSetArg (arg[0], XmNmessageString, xstr);
   //dialog = XmCreateMessageDialog (sh_shell, "Threat Info", arg, 1);
   //XmStringFree (xstr);
   //XtManageChild (dialog);
}

