// fixed_coverage.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>
#include "fixed_coverage.H"
#include "fixed_coverage_mess.H"
#include "add_s2g_mess.H"
#include "gridman.H"
#include "gridid.H"
#include "def.h"
#include "sensobj.H"

extern int FIXED_COVERAGE;
extern int ADD_S2G;
extern int GRID;

/************************************************************************
* C_FIXED_COVERAGE : construct a fixed_coverage object			*
************************************************************************/
C_FIXED_COVERAGE::C_FIXED_COVERAGE() {}

/************************************************************************
* init : initialize the event with the message                          *
************************************************************************/
void C_FIXED_COVERAGE::init(C_HEADER *) {

}

/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_FIXED_COVERAGE::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_FIXED_COVERAGE::temp_process() {
  C_SENSOBJ *sensobj;
  C_GRIDMAN *gridman;
  C_GRIDID *gridid;
  double la,lo;
  double rmax;
  int i,len;
  double grid_resolution;
  double delay;
  double X[3],V[3];
  C_XQUEUE *coverage_temp;
  ADD_S2G_MESS *add_s2g_mess;

//...... get sensobj stuff

  sensobj = (C_SENSOBJ *)SIMOBJ;
  rmax = sensobj->get_rmaxcoverage();
  sensobj->get_pos_vel(time_tag,X,V);
  sensobj->xyz_to_latlon(X,la,lo);
  coverage_temp = sensobj->get_coverage();

//...... get gridman stuff

  gridman = (C_GRIDMAN *)get_manager(GRID);
  grid_resolution = gridman->get_grid_resolution();
  rmax += grid_resolution;
  gridman->get_gridids(la, lo, rmax, coverage_temp);

  delay = 0.001;

//...... schedule the events to add sensor to grid

  len = coverage_temp->get_length();
  gridid = (C_GRIDID *)coverage_temp->get_top();

  for (i=0; i<len; i++) {

    gridid->set_time_tag(time_tag);

    add_s2g_mess = (ADD_S2G_MESS *) schedule (
	time_tag + delay,
	ADD_S2G,
	GRID,
	gridid->get_gid(),
	gridid->get_node());

      add_s2g_mess->object_id = LOCAL_ID;
      add_s2g_mess->object_node = NODE;
      add_s2g_mess->object_type = OBJECT_TYPE;
      add_s2g_mess->unique_id = sensobj->get_GLOBAL_ID();

      gridid = (C_GRIDID *)gridid->get_link();

  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_FIXED_COVERAGE::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_FIXED_COVERAGE::cleanup() {

}

