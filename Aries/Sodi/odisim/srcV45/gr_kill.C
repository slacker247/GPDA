// gr_kill.C method file

#include <stdio.h>
#include "gr_kill.H"


/************************************************************************
* process : process a graphics event					*
************************************************************************/
void C_GR_KILL::process() {

  if (speedes_object == NULL) {
    speedes_object = speedes_state->get_object(kill_object_id);
  }

  if (speedes_object != NULL) {
    previous_status = speedes_object->get_alive();
    speedes_object->set_alive(0);
  }else{
    fprintf(stderr,"Error, (gr_kill) could not kill object %d\n",
	kill_object_id);
  }

}

/************************************************************************
* unprocess : unprocess a graphics event				*
************************************************************************/
void C_GR_KILL::unprocess() {

  if (speedes_state != NULL) {
    speedes_object->set_alive(previous_status);
  }

}
