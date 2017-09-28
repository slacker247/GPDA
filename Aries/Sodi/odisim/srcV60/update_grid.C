// update_grid.C method file

#include <stdio.h>
#include <time.h>
#include <math.h>
#include "def.h"

#include "update_grid.H"
#include "gridman.H"
#include "mover.H"
#include "eom.H"
#include "gridid.H"

#include "add_m2g_mess.H"
#include "del_mfg_mess.H"
#include "update_grid_mess.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int UPDATE_GRID;
extern int ADD_M2G;
extern int DEL_MFG;
extern int GRID;

/************************************************************************
* C_UPDATE_GRID : construct a update_grid object			*
************************************************************************/
C_UPDATE_GRID::C_UPDATE_GRID() {}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_UPDATE_GRID::init(C_HEADER * /*header*/) {
  //UPDATE_GRID_MESS *update_grid_mess;

  //update_grid_mess = (UPDATE_GRID_MESS *)header;

}

/************************************************************************
* exchange : exchange state variables					*
************************************************************************/
void C_UPDATE_GRID::exchange() {
  C_MOVER *mover;
  C_GRIDID *gridid;

  mover = (C_MOVER *)SIMOBJ;
  gridid = mover->get_gridid();
  gridid->exchange(gid, nd, delay_g2m);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_UPDATE_GRID::temp_process() {
  C_MOVER *mover;
  C_GRIDMAN *gridman;
  C_GRIDID *gridid;
  C_XQUEUE *script;
  C_EOM *eom;
  ADD_M2G_MESS *add_m2g_mess;
  DEL_MFG_MESS *del_mfg_mess;
  double grid_resolution;
  double time_step;
  double P[3];
  double V[3];
  double la,lo;
  int new_grid_flag;
  int old_grid_flag;

//...... trap object id

/*
  int trap_id;
  trap_id = 2348;
  if (SIMOBJ->get_GLOBAL_ID() == trap_id) {
    fprintf(stderr,"UPDATE_GRID mover %d trapped at time %f\n",
	trap_id, TIME_TAG);
  }
*/

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... get the mover and grid-manager objects

  mover = (C_MOVER *)SIMOBJ;
  gridman = (C_GRIDMAN *)get_manager(GRID);

//...... get some grid parameters

  delay_g2m = gridman->get_delay_g2m();
  grid_resolution = gridman->get_grid_resolution();
  gridid = mover->get_gridid();

//...... check if mover is still moving

  script = mover->get_script();
  if (script->get_length() != 0) {

//...... determine the current grid

    mover->get_pos_vel(TIME_TAG,P,V);
    mover->xyz_to_latlon(P,la,lo);
    gridman->get_idnd(la, lo, nd, gid);

//...... compare with the old grid

    new_grid_flag = 0;
    old_grid_flag = 0;
    if ((nd != gridid->get_node()) || (gid != gridid->get_gid())) {
      new_grid_flag = 1;
      old_grid_flag = 1;
      if (gridid->get_node() == -1) old_grid_flag = 0;
    }

  }else{

//...... end of the script so exit the grid

    new_grid_flag = 0;
    old_grid_flag = 1;

  }

//...... add mover to new grid if the grid has changed

  if (new_grid_flag) {

    add_m2g_mess = (ADD_M2G_MESS *)schedule(
	TIME_TAG + delay_g2m,
	ADD_M2G,
	GRID,
	gid,
	nd);

    add_m2g_mess->object_id = id;
    add_m2g_mess->object_type = OBJECT_TYPE;
    add_m2g_mess->object_node = NODE;
    add_m2g_mess->unique_id = mover->get_GLOBAL_ID();

  }

//...... delete mover from old grid if not first time and the grid has changed

  if (old_grid_flag) {

    del_mfg_mess = (DEL_MFG_MESS *)schedule(
	TIME_TAG + delay_g2m,
	DEL_MFG,
	GRID,
	gridid->get_gid(),
	gridid->get_node());

    del_mfg_mess->object_id = id;
    del_mfg_mess->object_type = OBJECT_TYPE;
    del_mfg_mess->object_node = NODE;
    del_mfg_mess->unique_id = mover->get_GLOBAL_ID();

  }

//...... schedule this event again

  if (script->get_length()) {

    time_step = mover->get_tdist(grid_resolution);

//    fprintf(stderr,"UPDATE_GRID time %f, next time %f\n",TIME_TAG, time_step);


/*
    if (time_step < gridman->get_time_resolution()) {
	time_step = gridman->get_time_resolution();
    }
    schedule(TIME_TAG+time_step, UPDATE_GRID, OBJECT_TYPE, LOCAL_ID, NODE);
*/

    eom = (C_EOM *)script->get_bot();

    if (TIME_TAG + time_step < eom->get_endtime()) {

      schedule(TIME_TAG+time_step,
		UPDATE_GRID,
		OBJECT_TYPE,
		LOCAL_ID,
		NODE);
    }else{

      schedule(GONE*(eom->get_endtime()),
		UPDATE_GRID,
		OBJECT_TYPE,
		LOCAL_ID,
		NODE);

    }

  }

}


/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_UPDATE_GRID::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_UPDATE_GRID::cleanup() {

}

