// moving_coverage.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>
#include "moving_coverage.H"
#include "moving_coverage_mess.H"
#include "add_s2g_mess.H"
#include "del_sfg_mess.H"
#include "gridman.H"
#include "gridid.H"
#include "def.h"
#include "mover.H"
#include "sensobj.H"

int C_MOVING_COVERAGE::done = 0;
int C_MOVING_COVERAGE::MOVING_COVERAGE = 0;
int C_MOVING_COVERAGE::ADD_S2G = 0;
int C_MOVING_COVERAGE::DEL_SFG = 0;
int C_MOVING_COVERAGE::GRID = 0;

/************************************************************************
* C_MOVING_COVERAGE : construct a moving_coverage object		*
************************************************************************/
C_MOVING_COVERAGE::C_MOVING_COVERAGE() {

  if (!done) {
    done = 1;
    MOVING_COVERAGE = event_type("MOVING_COVERAGE");
    ADD_S2G = event_type("ADD_S2G");
    DEL_SFG = event_type("DEL_SFG");
    GRID = object_type("GRID");
  }

  new_coverage = new C_XQUEUE();

}

/************************************************************************
* init : initialize the event with the message                          *
************************************************************************/
void C_MOVING_COVERAGE::init(C_HEADER *) {

}

/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_MOVING_COVERAGE::exchange() {
  C_SENSOBJ *sensobj;

  sensobj = (C_SENSOBJ *)SIMOBJ;
  sensobj->exchange_coverage(new_coverage);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_MOVING_COVERAGE::temp_process() {
  C_SENSOBJ *sensobj;
  C_MOVER *mover;
  C_GRIDMAN *gridman;
  C_GRIDID *old_gridid;
  C_GRIDID *new_gridid;
  C_XQUEUE *script;
  double la,lo;
  double rmax;
  int i,len;
  double grid_resolution;
  double sensor_resolution;
  double time_resolution;
  double tnext;
  double slop;
  double delay;
  double X[3],V[3];
  ADD_S2G_MESS *add_s2g_mess;
  DEL_SFG_MESS *del_sfg_mess;

//...... get sensobj stuff

  sensobj = (C_SENSOBJ *)SIMOBJ;
  rmax = sensobj->get_rmaxcoverage();
  sensobj->get_pos_vel(TIME_TAG,X,V);
  sensobj->xyz_to_latlon(X,la,lo);
  old_coverage = sensobj->get_coverage();

//...... get gridman stuff

  gridman = (C_GRIDMAN *)get_manager(GRID);
  grid_resolution = gridman->get_grid_resolution();
  sensor_resolution = gridman->get_sensor_resolution();
  time_resolution = gridman->get_time_resolution();

//...... the sensor is also a mover (important for stoping)

  mover = (C_MOVER *)SIMOBJ;
  script = mover->get_script();

//...... get the time for the next scheduled event

  tnext = sensobj->get_tdist(sensor_resolution);
//  if (tnext < time_resolution*1.001) tnext = time_resolution*1.001;
  grid_resolution = tnext * sensobj->get_max_vel();

//...... set up rmax and the delay

  slop = sensobj->get_VMAX() * time_resolution;
  rmax += grid_resolution + sensor_resolution + slop;
  delay = gridman->get_delay_s2g();

//...... get the new grid coverage list

  if (script->get_length()) gridman->get_gridids(la, lo, rmax, new_coverage);

//...... find which are new and which are old

  len = new_coverage->get_length();
  new_gridid = (C_GRIDID *)new_coverage->get_top();

  for (i=0; i<len; i++) {

    new_gridid->set_time_tag(TIME_TAG);
    old_gridid = (C_GRIDID *)old_coverage->find(new_gridid->get_id());

    if (old_gridid == NULL) {

      add_s2g_mess = (ADD_S2G_MESS *) schedule (
	  TIME_TAG + delay,
	  ADD_S2G,
	  GRID,
	  new_gridid->get_gid(),
	  new_gridid->get_node());

      add_s2g_mess->object_id = LOCAL_ID;
      add_s2g_mess->object_node = NODE;
      add_s2g_mess->object_type = OBJECT_TYPE;
      add_s2g_mess->unique_id = GLOBAL_ID;

//      fprintf(stderr,"New gridid added to coverage\n");

    }else{

      *old_coverage -= old_gridid;
      RB_FREE_DELETE(old_gridid);

    }

    new_gridid = (C_GRIDID *)new_gridid->get_link();

  }

//...... remove the grids that are no longer in the coverage

  len = old_coverage->get_length();
  for (i=0; i<len; i++) {

    old_gridid = (C_GRIDID *)old_coverage->get_top();
    *old_coverage -= old_gridid;

    del_sfg_mess = (DEL_SFG_MESS *) schedule (
	  TIME_TAG + delay,
	  DEL_SFG,
	  GRID,
	  old_gridid->get_gid(),
	  old_gridid->get_node());

    del_sfg_mess->unique_id = GLOBAL_ID;

    RB_FREE_DELETE(old_gridid);
//    fprintf(stderr,"Old gridid removed from coverage\n");

  }

//...... reschedule this event again

  if (script->get_length()) {
    schedule(	TIME_TAG + tnext,
		MOVING_COVERAGE,
		OBJECT_TYPE,
		LOCAL_ID,
		NODE	);
  }

//  fprintf(stderr,"scheduling MOVING_COVERAGE event at dt %f\n",tnext);

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_MOVING_COVERAGE::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_MOVING_COVERAGE::cleanup() {

}

