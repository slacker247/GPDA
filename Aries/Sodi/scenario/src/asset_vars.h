
/*** struct and variable file for assets and there sensor and
defense area parameters
***/

typedef struct {

    char asset_name[128];
 
     int asset_id;        
     int north;                 //logical
     int east;                  //logical
   float lat_deg;
   float lat_min;
   float lat_sec;
   float lon_deg;
   float lon_min;
   float lon_sec;
   float altitude;
    char status[128];
     int icon;
   float scale;
     int r;
     int g;
     int b;

     int sensor;               //logical
     char sensor_type[128];
   float elevation;
   float azimuth;
     int coverage;             //logical
 
     int weapons;              //logical
     int gbi_id;
     int n_gbi;
     int n_hold;
   float pkill;
    char gbi_type[128];

     int area_defense;         //logical
    char type[128];

     int unit;                 //logical
    char unit_type[128];
    char unit_name[128];
     int unit_int_type;
   float size;

} Asset_Vars;

typedef struct {

  char name[128]; 
 
    int sensor_type;
  float scan_time;
  float rmin;
  float rmax;
  float rmax_low;
  float rdotmin;
  float signal;
  float luminosity;
  float fov_high;
  float fov_low;
    int fixed;                //logical
    int los;                  //logical
  float error;
    int icon;
  float scale;

} Sensor_Vars;

typedef struct {

  char name[128];

    int area_label;          //logical
    int area_north;          //logical
    int area_east;           //logical
  float area_lat_deg;
  float area_lat_min;
  float area_lat_sec;
  float area_lon_deg;
  float area_lon_min;
  float area_lon_sec;
   char area_type[128];
  float area_major;
  float area_minor;
  float area_orient;
   char area_file[128];
    int r;
    int g;
    int b;

} Defense_Vars;


//Current variables hold original asset values
//in case user decides not to save new ones

Asset_Vars Current;
Sensor_Vars CurrentSensors;
Defense_Vars CurrentDefense;

//Changing variables hold changes user has made
//to any parameter, assigned to Current upon save

Asset_Vars Changing;
Sensor_Vars ChangingSensors;
Defense_Vars ChangingDefense;

//Arrays that hold all defined assets, sensors, and defense areas
//to allow ease in writing to file.  -Change to linked list to be more dynamic


Asset_Vars Save_Data[100];
Sensor_Vars Save_Sensors[100];   
Defense_Vars Save_Defense[100];

//selected object on map screen user is creating

FL_OBJECT *object;

//name of selected asset user is creating
char asset[25];

//highest asset id defined (0 - 100)
int Highest_id = 0;

//flag specifying an asset has been selected to add to screen
int Clicked = 0;

//current location in array to add next asset
int array_place = 0;

//current location in array to add next sensor
int sensor_array = 0;

//current location in array to add next area defense 
int defense_array = 0;

//defines part of the asset a color is being defined for,
//asset or defense
int color_call = 0;
