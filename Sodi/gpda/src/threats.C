/************************************************************
  ---   threats.C loads the missile.par file   ---
************************************************************/

#include <math.h>
#include <sys/types.h>
#include <malloc.h>

#include "def.H"
#include "parser.H"
#include "convert.H"
#include "threats.H"

#include "forms.c"

typedef struct {
  FL_FORM   *ThreatDialog;
  void      *vdata;
  char      *cdata;
  long      ldata;
  FL_OBJECT *info_title;
  FL_OBJECT *info_text;
} FD_ThreatDialog;

int	        Threatinit = FALSE;
int             Threat_vis;
FD_ThreatDialog *fd_ThreatDialog;

FD_ThreatDialog *ThreatDialog(void);
void ThreatNone(FL_OBJECT *, long);
void ThreatDone(FL_OBJECT *, long);

/* ================================================================================= */

FD_ThreatDialog *
ThreatDialog(void)
{
  FL_OBJECT *obj;
  FD_ThreatDialog *fdui = (FD_ThreatDialog *) fl_calloc(1, sizeof(*fdui));

  fdui->ThreatDialog = fl_bgn_form(FL_NO_BOX, 340, 430);
  obj = fl_add_box(FL_UP_BOX,0,0,340,430,"");
  fdui->info_title = obj = fl_add_text(FL_NORMAL_TEXT,10,20,320,20,"title");
    fl_set_object_lcolor(obj,FL_YELLOW);
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER|FL_ALIGN_INSIDE);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE+FL_SHADOW_STYLE);
    fl_set_object_callback(obj,ThreatNone,0);
  fdui->info_text = obj = fl_add_text(FL_NORMAL_TEXT,30,50,280,330,"");
    fl_set_object_lalign(obj,FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,ThreatNone,0);
  obj = fl_add_button(FL_NORMAL_BUTTON,120,390,90,30,"Dismiss");
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,ThreatDone,0);
  fl_end_form();

  fdui->ThreatDialog->fdui = fdui;

  return fdui;
}

int
ThreatLoad(char *loadfile)
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

C_CONVERT       conversion;

double          latitude, longitude, X[3];
double          lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec;

   if (Threatinit) return(-1);
/*
*      Parse the Missile Threat parameter file
*      ---------------------------------------
*/
   //sprintf (threatfile, "%s%s", PARMDIR, "/missile.par");
   strcpy(threatfile, loadfile);
   threat_parser = new C_PARSER(threatfile);

   missile_names = threat_parser->get_basetype("missiles");
   threatcount = missile_names->get_ntypes();
   Threats = new THREAT[threatcount];
   threattotal = 0;
   missile_name = missile_names->get_first_type();
   for (i=0; i<threatcount; i++) {
     Threats[i].names = missile_name->get_name();
     threattotal = threattotal+1;
     Threats[i].types = missile_name->get_string("missile_type");
     Threats[i].lsite = missile_name->get_string("init_position");
     Threats[i].tsite = missile_name->get_string("final_position");
     Threats[i].ltime = missile_name->get_float("launch_time");
     Threats[i].rdist = missile_name->get_float("random_distance");
     Threats[i].llat  = 0.0;
     Threats[i].llon  = 0.0;
     Threats[i].tlat  = 0.0;
     Threats[i].tlon  = 0.0;
     basetype = threat_parser->get_basetype(Threats[i].lsite);
     lat_deg = basetype->get_float("lat_deg");
     lat_min = basetype->get_float("lat_min");
     lat_sec = basetype->get_float("lat_sec");
     lon_deg = basetype->get_float("lon_deg");
     lon_min = basetype->get_float("lon_min");
     lon_sec = basetype->get_float("lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     Threats[i].latNS = "N";
     if (!basetype->get_logical("north")) {
        Threats[i].latNS = "S";
        latitude = -latitude;
     }
     Threats[i].lonEW = "E";
     if (!basetype->get_logical("east")) {
        Threats[i].lonEW = "W";
        longitude = -longitude;
     }
     Threats[i].llat = latitude;
     Threats[i].llon = longitude;
     basetype = threat_parser->get_basetype(Threats[i].tsite);
     lat_deg = basetype->get_float("lat_deg");
     lat_min = basetype->get_float("lat_min");
     lat_sec = basetype->get_float("lat_sec");
     lon_deg = basetype->get_float("lon_deg");
     lon_min = basetype->get_float("lon_min");
     lon_sec = basetype->get_float("lon_sec");
     latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0);
     longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0);
     Threats[i].tatNS = "N";
     if (!basetype->get_logical("north")) {
        Threats[i].tatNS = "S";
        latitude = -latitude;
     }
     Threats[i].tonEW = "E";
     if (!basetype->get_logical("east")) {
        Threats[i].tonEW = "W";
        longitude = -longitude;
     }
     Threats[i].tlat = latitude;
     Threats[i].tlon = longitude;
/*
     impact[i] = new GR_Impact (0, (float)latitude, (float)longitude, 
                      threatrdist[i], threatrdist[i]/3.0, 90.0);
     impact[i]->set_llah(latitude, longitude, 0.0, 0.0);
     displist->add_object(*impact[i]);
*/
     missile_name = missile_names->get_next_type();
   }

   Threatinit = TRUE;

   return (threatcount);
}

int
ThreatGetLLAH(int index, float *lat, float *lon, float *alt, float *head)
{
   if (!Threatinit || index < 0 || index > threatcount) return(-1);
   *lat = threatllat[index];
   *lon = threatllon[index];
   *alt = 0.0;
   *head = 0.0;
   return(0);
}

int
ThreatGetIcon(int index)
{
   if (!Threatinit || index < 0 || index > threatcount) return(-1);
   return(0);
}

float
ThreatGetScale(int index)
{
   if (!Threatinit || index < 0 || index > threatcount) return(-1);
   return(1.0);
}

char *
ThreatGetName(int index)
{
   if (!Threatinit || index < 0 || index > threatcount) return(NULL);
   return(threatnames[index]);
}

void
ThreatPopup(int select)
{
char            chtitle[40];

   if (!Threat_vis) {
      fd_ThreatDialog = T[threatselect].hreatDialog();
      strcpy(chtitle, "Battle Plan");
      fl_set_object_label(fd_ThreatDialog->info_title, chtitle);
      fl_show_form(fd_ThreatDialog->ThreatDialog, FL_PLACE_CENTER,FL_FULLBORDER,
                   "Status Info");
      Threat_vis = TRUE;
   }
   ThreatUpdate(select);
}

void
ThreatUpdate(int threatselect)
{
char             str[1280];

   if (!Threat_vis) return;

   sprintf (str, 
        " %s%s\n %s%s\n %s%f\n %s%f\n %s%s\n %s%f%s\n %s%f%s\n %s%s\n %s%f%s\n %s%f%s\n", 
                "Name of Threat:        ", threats[threatselect].names,
                "Type of Threat:        ", threats[threatselect].types,
	        "Threat Launch Time:    ", threats[threatselect].ltime,
	        "Threat Random Distance:", threats[threatselect].rdist,
	        "Name of Launch Site    ", threats[threatselect].lsite,
                "Latitude of Launch:    ", threats[threatselect].llat, "N",
                "Longitude of Launch:   ", threats[threatselect].llon, "E",
	        "Name of Target Site    ", threats[threatselect].tsite,
                "Latitude of Target:    ", threats[threatselect].tlat, "N", 
		"Longitude of Target:   ", threats[threatselect].tlon, "E");

   fl_set_object_label(fd_ThreatDialog->info_text, str);
}

void
ThreatDone(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_ThreatDialog->ThreatDialog);
   fl_free_form(fd_ThreatDialog->ThreatDialog);
   Threat_vis = FALSE;
}

void
ThreatNone((FL_OBJECT *ob, long data)
{
}

