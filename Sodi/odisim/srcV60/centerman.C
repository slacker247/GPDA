// centerman.C method file

#include <stdio.h>
#include <math.h>
#include "centerman.H"
#include "def.h"
#include "defunc.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int EXT_GRAPHICS_DEFINE;
extern int CENTER;

/************************************************************************
* C_CENTERMAN : construct a centerman object				*
************************************************************************/
C_CENTERMAN::C_CENTERMAN() {
  int i;
  int offset;
  int start_uid;
  double R[3];
  double latitude, longitude;
  double lat_deg, lat_min, lat_sec;
  double lon_deg, lon_min, lon_sec;
  C_BASETYPE *basetype;
  C_BASETYPE *center_type;
  C_ITEM *item;
  C_SENSOR_MODEL *sensor_model;

//...... get the command center information from the parser

  basetype = center_parser->get_basetype("command_centers");
  icon = basetype->get_int("icon");
  center_type = basetype->get_first_type();

//...... create the command center object

  N_TOT = 1;
  N_LOC = deal_me(N_TOT, offset);
  if (N_LOC) {
    centobj = new C_CENTOBJ[N_LOC];
    centobj->set_blocking(); 
    item = new C_ITEM();
    item->set_time_tag(center_type->get_float("lanl_bp_start_time"));
    item->set_id(-centobj->get_GLOBAL_ID());
    add_block(item);
  }

//...... initialize

  if (N_LOC) {

    lat_deg = center_type->get_float("lat_deg");
    lat_min = center_type->get_float("lat_min");
    lat_sec = center_type->get_float("lat_sec");

    lon_deg = center_type->get_float("lon_deg");
    lon_min = center_type->get_float("lon_min");
    lon_sec = center_type->get_float("lon_sec");

    latitude  = (lat_deg + lat_min/60.0 + lat_sec/3600.0) * 0.017453292;
    longitude = (lon_deg + lon_min/60.0 + lon_sec/3600.0) * 0.017453292;

    if (!center_type->get_logical("north")) latitude *= -1.0;
    if (!center_type->get_logical("east")) longitude *= -1.0;

    centobj->latlon_to_xyz(latitude,longitude,R);

    centobj->set_start_position(R);
    centobj->set_NAME(center_type->get_name());
    centobj->set_stale_track_time(center_type->get_float("stale_track_time"));
    centobj->set_icon(icon);

    sensor_model = new C_SENSOR_MODEL();
    sensor_model->set_sensor_id(centobj->get_GLOBAL_ID());
    sensor_model->reset_track_flag();
    sensor_model->set_fuse_flag();
    sensor_model->reset_print_flag();
    centobj->set_sensor_model(sensor_model);

  }

//...... set up the communications

  if (offset == 0) {
    start_uid = centobj[0].get_GLOBAL_ID();
  }else{
    start_uid = 0;
  }
  start_uid = SpComm_GlobalSum(start_uid);
  //scombine(&start_uid,SUMINT,sizeof(int),1);
  set_communications(center_type, 0, offset, start_uid);

//...... plug the object into speedes

  TOTAL_OBJECTS += N_TOT;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&centobj[i]);
  set_interact();

  printf("CENTERMAN object created with %4d objects (%4d loc)\n", N_TOT, N_LOC);

}

/************************************************************************
* init_events : initialize events for the com element objects		*
************************************************************************/
void C_CENTERMAN::init_events() {
  int i;
  int EXT_GRAPHICS_DEFINE;

  printf("GBIMAN initializing ...\n");

  EXT_GRAPHICS_DEFINE = event_type("EXT_GRAPHICS_DEFINE");

//...... start up the graphics "define" event

  for (i=0; i<N_LOC; i++) {
    schedule (0.0, EXT_GRAPHICS_DEFINE, CENTER, i, NODE);
  }


}

