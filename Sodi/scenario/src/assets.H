/**********************************************************************
    
*********************************************************************/

#ifndef ASSETS_H
#define ASSETS_H                            "GISP global variables"

#define   T_SENSOR       2000
#define   T_DEFEND       2100
#define   T_IMPACT       2200
#define   T_TERRAIN      2300
#define   T_SURFACE      2400
#define   T_TRAIL        2500
#define   T_AIROBJ       2600
#define   T_ARMY         2700
#define   T_STARS        2800
#define   T_CARIB        2900
#define   T_STRING       3000

#define   AREA_CIRCLE    0
#define   AREA_BOX       1
#define   AREA_ELLIPSE   2
#define   AREA_POLY      3

struct TRACK {
  int              id;                  // Track ID
  int              stn;                 // Reporting asset
  int              istn;                // Intercepting asset
  int              icon;                // Model # to display
  int              type;                //
  int              dropped;             // Number of 'drop track' msg this track has received
  int              labeled;             // This track has been labelled
  int              hostile;             // Friendly or Hostile
  float            X[3];                // Track location in meters from center of earth
  float            V[3];                // Track velocity in Km/sec
  float            latitude;            // Track location in degrees latitude
  float            longitude;           // Track location in degrees longitude
  float            altitude;            // Track location in meters altitude
  float            speed;               // Track speed in Km/sec
  float            heading;             // Track heading in degrees from North
  float            impact_tim;          // Estimated time of impact
  float            impact_lat;          // Estimated impact latitude
  float            impact_lon;          // Estimated impact longitude
  float            impact_spd;          // Estimated speed on impact
  float            impact_err;          // Estimated radius of error in Km
  float            iltime;              // Intercept launch time
  float            itime;               // Time at intercept
  float            old_x;
  float            old_y;
  float            old_z;
  int              red;                 // Red component of trail RGB color
  int              green;               // Green component of trail RGB color
  int              blue;                // Blue component of trail RGB color
  char             chmsg[8];            // Message type when RTN 1st encountered
  char             weapon[8];           // Targeting Weapon System
  char             engage[8];           // Targeting Weapon Engagement Status
  char             chmarking[16];       // Marking when RTN 1st encountered
  char             chstatus[16];        // Current RTN status (In-Flight or Dropped)
  char             chcolor[16];         // X color name of trail color
  /*
  GR_AirObj        *airobj;
  GISP_Obj         *object;
  GR_String        *labels;
  GR_Impact        *impactarea;
  */
};

struct SENSOR {
   int             gbistn;              // Asset ID
   char            *gbiname;		// Asset name
   char	           *gbitype;		// Type of GBI at asset
   char            *gbisensr;           // Name of sensor at asset
   char            *gbisystem;
   float           gbix;                // X position in Km
   float           gbiy;                // Y position in Km
   float           gbiz;                // Z position in Km
   float           gbialt;              // Altitude asset
   double	   gbilatit;		// Latitude of asset
   double	   gbilongi;		// Longitude of asset
   char	           *gbilatNS;		// Latitude is N or S
   char	           *gbilonEW;		// Longitude is E or W
   float           gbiinclin;           // Inclination of orbit
   short           gbir;                // Red component of asset color
   short           gbig;                // Green component of asset color
   short           gbib;                // Blue component of asset color
   int             gbiicon;
   int             gbirvicon;
   float           gbiscale;            // Amount to scale icon model by
//
//   GBI related information for those assets which have interceptors
//
   int             gbi_per_rv;          // GBIs fired per threat
   int             gbistype;            // GBI Sensor type (CONE, DOME, etc)
   int             gbinarea;            // No. of areas this GBI is defending
   int	           gbingbis;		// No. of interceptors in GBI farm
   int             gbinhold;            // No. of interceptors withheld
   int	           gbiident;		// Farm identifier
   float	   gbipkill;		// Probability of Kill
//
//   Sensor related information
//
   int             gbifixed;            // Fixed or moving sensor
   int             gbilos;              // Does it handle loss-of-signal?
   float	   gbiscanT;		// Scan Time
   float	   gbirmin;		// Minimum range
   float	   gbirmax;		// High-power maximum range
   float	   gbirlow;	  	// Low-power maximum range
   float	   gbirdot;		//
   float	   gbisignal;		//
   float	   gbilumin;		// Luminosity
   float	   gbierror;		//
   float           gbielev;             // Sighting elevation angle
   float           gbiazi;              // Sighting azimuth angle (from N)
   float           gbifovhi;            // High-power Field-of-View
   float           gbifovlo;            // Low-power Field-of-View
//
//   Defended area related information
//
   float           gbimajor;            // Major axis length (Km)
   float           gbiminor;            // Minor axis length (Km)
   float           gbiorient;           // Orientation (degrees)
   char            *gbifile;            // Name of file containing polygon info
   char            **gbiareas;          // Names of defended areas
   char            **gbishape;          // Shapes of defended areas
   char            chname[16];
  /*
   GR_Sensor       *gbisensor;
   GR_String       *gbilabel;
   GR_Defend       *defarea;
   GR_Army         *unit;
   GISP_Obj        *sensorobj;
  */
};

struct TRAIL {
   short           r;
   short           g;
   short           b;
   int             icon;
};

#endif

