// kill.C method file

#include <strstream.h>
#include <stdio.h>

#include "kill.H"
#include "kill_mess.H"
#include "ext_graphics_kill_output.H"
#include "gbiman.H"
#include "graphman.H"
#include "locobj.H"
#include "ext_mess.H"
#include "host_user.H"

#include "def.h"

int C_KILL::done = 0;
int C_KILL::KILL = 0;
int C_KILL::GBI = 0;
int C_KILL::GRAPHICS = 0;

/************************************************************************
* C_KILL : construct a kill object					*
************************************************************************/
C_KILL::C_KILL() {

  if (!done) {
    done = 1;
    GBI = object_type("GBI");
    GRAPHICS = object_type("GRAPHICS");
    KILL = event_type("KILL");
  }

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_KILL::init(C_HEADER *header) {
  KILL_MESS *kill_mess_in;

  kill_mess_in = (KILL_MESS *)header;

  kill_position[0] = kill_mess_in->kill_position[0];
  kill_position[1] = kill_mess_in->kill_position[1];
  kill_position[2] = kill_mess_in->kill_position[2];

  bomb_size = kill_mess_in->bomb_size;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_KILL::exchange() {
  C_LOCOBJ *locobj;

  locobj = (C_LOCOBJ *)SIMOBJ;

  locobj->exchange_alive(alive);
  locobj->EXCHANGE_SEED(sd);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_KILL::temp_process() {
  C_LOCOBJ *locobj;
  C_GBIMAN *gbiman;
  double X[3];
  double V[3];
  double d0,d1,d2,d;
  double random_kill;
  double pkill;

  gbiman = (C_GBIMAN *)get_manager(GBI);
  pkill = gbiman->get_pkill();

  sd = SIMOBJ->get_SEED();
  RANDOM->set_seed(sd);

  locobj = (C_LOCOBJ *)SIMOBJ;
  locobj->get_pos_vel(TIME_TAG,X,V);

  d0 = X[0] - kill_position[0];
  d1 = X[1] - kill_position[1];
  d2 = X[2] - kill_position[2];
  d = sqrt(d0*d0+d1*d1+d2*d2);


//...... kill the mover if the hit distance is less than the bomb size

  if (d < bomb_size) {
    RANDOM->set_float_limits(0.0,1.0);
    random_kill = RANDOM->get_random_float();
    if (random_kill < pkill) {
      alive = 0;
    }else{
      alive = 1;
    }
  }else{
    alive = 1;
  }

//...... print out whether the object was killed or not

  ostrstream so;
  if (alive) {
    so << "KILL (" << pkill << " " << random_kill
       << ") Failed for object " << SIMOBJ->get_GLOBAL_ID()
       << " at time " << TIME_TAG << " with error " << d << "\n" << ends;
  }else{
    so << "KILL Successful for object " << SIMOBJ->get_GLOBAL_ID()
       << " at time " << TIME_TAG << "\n";
  }
  RB_PRINT(stderr,so.str());

  sd = RANDOM->get_seed();
  alive_flag = alive;

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_KILL::perm_process() {
  C_GRAPHMAN *graphman;
  C_GRAPHOBJ *graphobj;
  EXT_GRAPHICS_KILL_OUTPUT ext_graphics_kill_output;

  if (alive_flag) return;

  graphman = (C_GRAPHMAN *)get_manager(GRAPHICS);
  graphobj = (C_GRAPHOBJ *)graphman->get_obj(0);

//...... fill the header of the output message to the graphics

  ext_graphics_kill_output.time_tag = TIME_TAG;
  ext_graphics_kill_output.evtype = -1;
  ext_graphics_kill_output.objid = -1;
  ext_graphics_kill_output.obtype = OBJECT_TYPE;
  ext_graphics_kill_output.ext = 0;

  ext_graphics_kill_output.data_bytes =
	sizeof(EXT_GRAPHICS_KILL_OUTPUT) - sizeof(C_EM_HEADER);

  ext_graphics_kill_output.EM_interaction = EM_MODULE;
  ext_graphics_kill_output.EM_socket = graphobj->get_socket();
  ext_graphics_kill_output.EM_done_time = TIME_TAG;
  ext_graphics_kill_output.EM_flag = GRAPHICS_KILL;

//...... fill the data of the output message

  ext_graphics_kill_output.kill_object_id = SIMOBJ->get_GLOBAL_ID();

//...... send the message out

  HOST_USER->send_message(&ext_graphics_kill_output);

  fprintf(stderr,"KILL> killing %d, (GRAPHICS_KILL = %d)\n",
	SIMOBJ->get_GLOBAL_ID(), ext_graphics_kill_output.EM_flag);

}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_KILL::cleanup() {


}


