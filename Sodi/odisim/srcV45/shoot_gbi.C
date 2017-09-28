// shoot_gbi.C method file

#include <stdio.h>
#include "gbiman.H"
#include "gridman.H"
#include "centobj.H"
#include "shoot_gbi.H"
#include "shoot_gbi_mess.H"
#include "add_s2m_mess.H"
#include "kill_mess.H"
#include "bus.H"

#include "freetypes.H"
#include "oid.H"
#include "def.h"

int C_SHOOT_GBI::done = 0;
int C_SHOOT_GBI::SHOOT_GBI = 0;
int C_SHOOT_GBI::ADD_S2M = 0;
int C_SHOOT_GBI::GRID = 0;
int C_SHOOT_GBI::GBI = 0;
int C_SHOOT_GBI::KILL = 0;
int C_SHOOT_GBI::EXT_GRAPHICS_SCRIPT = 0;

/************************************************************************
* C_SHOOT_GBI : construct a shoot_gbi object				*
************************************************************************/
C_SHOOT_GBI::C_SHOOT_GBI() {

  if (!done) {
    done = 1;
    GBI = object_type("GBI");
    GRID = object_type("GRID");
    KILL = event_type("KILL");
    SHOOT_GBI = event_type("SHOOT_GBI");
    ADD_S2M = event_type("ADD_S2M");
    EXT_GRAPHICS_SCRIPT = event_type("EXT_GRAPHICS_SCRIPT");
  }

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_SHOOT_GBI::init(C_HEADER *header) {
  SHOOT_GBI_MESS *shoot_gbi_mess;
  C_SHOOT_INFO *si;

  shoot_gbi_mess = (SHOOT_GBI_MESS *)header;
  si = shoot_gbi_mess->get_shoot_info();
  memcpy((char *)&shoot_info, (char *)si, sizeof(C_SHOOT_INFO));

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_SHOOT_GBI::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_SHOOT_GBI::temp_process() {
  C_GBIOBJ *gbiobj;
  C_GRIDMAN *gridman;
  double kill_time;
  double X[3];
  double V[3];
  double timp;
  double lat;
  double lon;
  C_XQUEUE *script;
  C_EOM *eom;
  C_BUS bus;
  C_BASE_SPACE *gbibus;
  int i;
  KILL_MESS *kill_mess;
  //ADD_S2M_MESS *add_s2m_mess;
  C_ID_ARRAY *id_array;
  char *bf;
  double tsched;

  cerr << "\nSHOOT GBI event at time " << TIME_TAG << "\n";

//...... get the gbi missile from the gbiobj (or create one if necessary)

  gbiobj = (C_GBIOBJ *)SIMOBJ;
  gbi = (C_GBI *)gbiobj->get_missile();
  if (gbi == NULL) {
    gbi = new C_GBI();
    gbiobj->set_missile(gbi);
  }

//  bus = (C_BUS *)RB_FREE_NEW(BUS);
//  bus = (C_BUS *)RB_NEW(new C_BUS);

  bus.set_start_time(shoot_info.truth_threat_time);
  bus.init(shoot_info.X_threat_truth, shoot_info.V_threat_truth);
  bus.update_impact();

  timp = bus.get_timp();
  bus.get_pos_vel(timp-0.000001,X,V);
  bus.xyz_to_latlon(timp,X,lat,lon);
  lat /= RADDEG;
  lon /= RADDEG;

  fprintf(stderr,"threat: impact time=%f, lat=%f, lon=%f\n",timp,lat,lon);
  fprintf(stderr,"threat: type=%d, uid=%d, lid=%d, node=%d\n",
	shoot_info.threat_type,
	shoot_info.threat_uid,
	shoot_info.threat_id,
	shoot_info.threat_node);

  fprintf(stderr,"site: lat=%f, lon=%f\n",
	shoot_info.lat_site/RADDEG, shoot_info.lon_site/RADDEG);

  bus.get_pos_vel(shoot_info.truth_threat_time+0.000001,X,V);


  cerr << "SHOOT GBI> threat time = " << shoot_info.truth_threat_time
       << ", current_time = " << TIME_TAG << "\n";

  fprintf(stderr,"threat X = %f %f %f, V = %f %f %f\n",
	shoot_info.X_threat_truth[0],
	shoot_info.X_threat_truth[1],
	shoot_info.X_threat_truth[2],
	shoot_info.V_threat_truth[0],
	shoot_info.V_threat_truth[1],
	shoot_info.V_threat_truth[2]);

  fprintf(stderr,"bus X = %f %f %f, V = %f %f %f\n",
	X[0],
	X[1],
	X[2],
	V[0],
	V[1],
	V[2]);

  kill_time = gbi->aim_gbi(TIME_TAG, shoot_info.lat_site,
					shoot_info.lon_site, &bus);

  if (kill_time != -1.0) {

    gbi->set_ECR();
    gbiobj->make_script(kill_time);
    script = gbiobj->get_script();

    eom = (C_EOM *)script->get_top();
    for (i=0; i<3; i++) {
      fprintf(stderr,"SHOOT GBI> script %d start time %f, end time %f\n",
	i,eom->get_start_time(), eom->get_endtime());
      eom = (C_EOM *)eom->get_link();
    }


//...... send the script for the graphics to fly the missile

    schedule (GONE*TIME_TAG, EXT_GRAPHICS_SCRIPT, OBJECT_TYPE, LOCAL_ID, NODE);

//...... send the kill message to the target

    kill_mess = (KILL_MESS *)schedule (
	kill_time,
	KILL,
	shoot_info.threat_type,
	shoot_info.threat_id,
	shoot_info.threat_node);

    gbi->get_pos_vel(kill_time,X,V);

    kill_mess->kill_position[0] = X[0];
    kill_mess->kill_position[1] = X[1];
    kill_mess->kill_position[2] = X[2];

    kill_mess->bomb_size = 50.0;

//...... get the equation of motion from the threat using prox

    gridman = (C_GRIDMAN *)get_manager(GRID);
    tsched = TIME_TAG + gridman->get_delay_g2m();
    /*add_s2m_mess = (ADD_S2M_MESS *)*/schedule(
	tsched,
	ADD_S2M,
	shoot_info.threat_type,
	shoot_info.threat_id,
	shoot_info.threat_node,
	sizeof(C_ID_ARRAY),
	bf);

    id_array = (C_ID_ARRAY *)bf;

    id_array->object_id = LOCAL_ID;
    id_array->object_type = OBJECT_TYPE;
    id_array->object_node = NODE;
    id_array->unique_id = SIMOBJ->get_GLOBAL_ID();

//...... set up the time that the sensor turns on

    gbibus = gbi->get_bus();
    gbiobj->set_sensor_time(gbibus->get_start_time());

  }else{

    fprintf(stderr,"SHOOT GBI> Warning, could not aim gbi\n");

  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_SHOOT_GBI::perm_process() {

}

/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_SHOOT_GBI::cleanup() {

}


