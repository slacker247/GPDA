/**********************************************************************
    
*********************************************************************/

#ifndef GISP_GLOBALS
#define GISP_GLOBALS                            "GISP global variables"

#include "GR_AirObj.H"
#include "GISP_Obj.H"
#include "GR_String.H"
#include "GR_Impact.H"

#define       IDENTITY  0               // Color code by IDENTITY field
#define       MARKING   1               // Color code by MARKING field
#define       TRACKID   2               // Color code by Track ID

struct TRACK {
  int              id;
  int              icon;
  int              type;
  int              dropped;
  int              labeled;
  float            X[3];
  float            V[3];
  float            latitude;
  float            longitude;
  float            old_x;
  float            old_y;
  float            old_z;
  int              red;
  int              green;
  int              blue;
  char             chmarking[16];
  char             chstatus[16];
  GR_AirObj        *airobj;
  GISP_Obj         *object;
  GR_String        *labels;
  GR_Impact        *impactarea;
};

struct SENSOR {
   int             stn;
   float           x;
   float           y;
   float           z;
   float           scale;
   short           r;
   short           g;
   short           b;
   int             icon;
   int             rvicon;
   char            chname[24];
   GR_String       *sensorlbl;
};

#endif

