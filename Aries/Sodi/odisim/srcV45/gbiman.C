// gbiman.C method file

#include <stdio.h>
#include <memory.h>
#include <math.h>
#include "gbiman.H"
#include "def.h"
#include "defunc.H"

#include "eye_model.H"
#include "ground_radar.H"

/************************************************************************
* C_GBIMAN : construct a gbiman object					*
************************************************************************/
C_GBIMAN::C_GBIMAN() {
  C_BASETYPE *gbi_sites;
  C_BASETYPE *site;
  C_BASETYPE *sensor_type;
  C_BASETYPE *basetype;
  int i,j;
  double latitude, longitude;
  double lat_deg, lat_min, lat_sec;
  double lon_deg, lon_min, lon_sec;
  int offset;
  int index;
  int icon;
  double R[3];
  double d;
  C_EYE_MODEL *eye_model;
  C_GROUND_RADAR *ground_radar;
  int on_off;
  int start_uid;
  char s[80];
  char *gbi_name;

  GBI = object_type("GBI");

  basetype = parameters_parser->get_basetype("parameters");
  on_off = basetype->get_logical("on_off");

  gbi_sites = gbi_parser->get_basetype("gbi_sites");
  pkill = gbi_sites->get_float("pkill");
  icon = gbi_sites->get_int("icon");
  if (on_off) on_off = gbi_sites->get_logical("on_off");
  n_sites = gbi_sites->get_ntypes();
  n_each_site = new int[n_sites];
  gbi_id = new int[n_sites];
  gbi_sum = new int[n_sites];
  site_name = new char*[n_sites];
  gbi_type = new char*[n_sites];
  lat_site = new double[n_sites];
  lon_site = new double[n_sites];
  total_gbis = 0;

  site = gbi_sites->get_first_type();
  for (i=0; i<n_sites; i++) {

//...... get the site name

    site_name[i] = site->get_name();
    gbi_type[i] = site->get_string("gbi_type");
    gbi_id[i] = site->get_int("gbi_id");
    n_each_site[i] = site->get_int("n_gbi");
    total_gbis += n_each_site[i];

//...... get the position of the site

    lat_deg = site->get_float("lat_deg");
    lat_min = site->get_float("lat_min");
    lat_sec = site->get_float("lat_sec");
    lon_deg = site->get_float("lon_deg");
    lon_min = site->get_float("lon_min");
    lon_sec = site->get_float("lon_sec");

    latitude = (lat_deg + lat_min/60.0 + lat_sec/3600.0) * 0.017453292;
    longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0) * 0.017453292;

    if (!site->get_logical("north")) latitude *= -1.0;
    if (!site->get_logical("east")) longitude *= -1.0;

    lat_site[i] = latitude;
    lon_site[i] = longitude;


    site = gbi_sites->get_next_type();

  }

//...... get the cumulative sum of gbis at each site

  if (n_sites > 0) gbi_sum[0] = 0;
  for (i=1; i<n_sites; i++) gbi_sum[i] = gbi_sum[i-1] + n_each_site[i-1];

//...... create the gbi objects

  N_TOT = total_gbis + n_sites;

  N_LOC = deal_me(N_TOT, offset);
  first_id = TOTAL_OBJECTS;
  first_node = (N_NODES+NODE-offset) % N_NODES;

  gbiobj = new C_GBIOBJ[N_LOC];

//...... get the starting unique id

  if (offset == 0) {
    start_uid = gbiobj[0].get_GLOBAL_ID();
  }else{
    start_uid = -1;
  }
  start_uid = SpComm_GlobalMax(start_uid);
  //scombine(&start_uid,SP_MAXINT,sizeof(int),1);

//...... initialize the sites

  index = 0;
  for (i=0; i<n_sites; i++) {

    set_communications(site_name[i], i, offset, start_uid);

    if ((i-offset+N_NODES)%N_NODES == 0) {

      check_communications(gbiobj[index].get_GLOBAL_ID(), index);

      gbiobj[index].init_site(
		gbi_type[i],
		site_name[i],
		lat_site[i],
		lon_site[i],
		icon);

      gbiobj[index].latlon_to_xyz(lat_site[i],lon_site[i],R);

      d = sqrt(R[0]*R[0]+R[1]*R[1]+R[2]*R[2])/RE;
      if ((d < 0.99) || (d > 1.01)) {
        fprintf(stderr,"Error, bad initial position for sensor\n");
      }

      gbiobj[index].set_start_position(R);

      index++;
    }
  }

//...... initialize the sites as radars

  ground_radar = new C_GROUND_RADAR[index];
  sensor_type = gbi_parser->get_basetype("GBR_SENSOR");

  for (i=0; i<index; i++) {
    ground_radar[i].init(sensor_type);
    ground_radar[i].set_sensor_id(gbiobj[i].get_GLOBAL_ID());
    gbiobj[i].set_sensor_model(&ground_radar[i]);
    gbiobj[i].set_on_off(on_off);
  }

//...... initialize the GBIs as ir sensors

  eye_model = new C_EYE_MODEL[N_LOC-index];
  sensor_type = gbi_parser->get_basetype("GBI_SENSOR");

  for (i=index; i<N_LOC; i++) {
    j = i-index;
    eye_model[j].init(sensor_type);
    eye_model[j].set_sensor_id(gbiobj[i].get_GLOBAL_ID());
    eye_model[j].set_NOISE_BE_SCAN();
    gbiobj[i].set_sensor_model(&eye_model[j]);
    gbiobj[i].set_on_off(on_off);

    sprintf(s,"GBI_%3.3d_%3.3d",NODE,i);
    gbi_name = strdup(s);
//    fprintf(stderr,"%s\n",gbi_name);
    gbiobj[i].set_NAME(gbi_name);

  }

//...... tell SPEEDES about the gbi objects

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&gbiobj[i]);
  set_interact();

  printf("GBIMAN %d sites, %4d local objects created (%d tot)\n",
	n_sites, N_LOC, N_TOT);

}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_GBIMAN::init_events() {
  int i;
  int EXT_GRAPHICS_DEFINE;
  int FIXED_COVERAGE;
  int scan_type_site;
  int scan_type_gbi;

  printf("GBIMAN initializing ...\n");

  EXT_GRAPHICS_DEFINE = event_type("EXT_GRAPHICS_DEFINE");
  FIXED_COVERAGE = event_type("FIXED_COVERAGE");

  scan_type_site = event_type("SCAN");
  scan_type_gbi = event_type("SCAN_GBI");

//...... start up the graphics "define" event

  for (i=0; i<N_LOC; i++) {
    if (gbiobj[i].get_site_flag()) {
      gbiobj[i].set_scan_type(scan_type_site);
      schedule (0.0, EXT_GRAPHICS_DEFINE, GBI, i, NODE);
      schedule (0.0, FIXED_COVERAGE, GBI, i, NODE);
    }else{
      gbiobj[i].set_scan_type(scan_type_gbi);
    }
  }

}

/************************************************************************
* get_nodid_site : get the node and local id of a site			*
************************************************************************/
void C_GBIMAN::get_nodid_site(int uid, int &nd, int &lid) {

  nd = (uid - first_id + first_node) % N_NODES;
  lid = (uid - first_id) / N_NODES;

}

/************************************************************************
* get_nodid_gbi : get the node and local id of a gbi			*
************************************************************************/
void C_GBIMAN::get_nodid_gbi(int gid, int &nd, int &lid) {

  nd = (gid + n_sites + first_node) % N_NODES;
  lid = (gid + n_sites) / N_NODES;

}
