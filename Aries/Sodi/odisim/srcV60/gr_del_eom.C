// gr_del_eom.C method file

#include <stdio.h>
#include "gr_del_eom.H"


/************************************************************************
* process : process a graphics event					*
************************************************************************/
void C_GR_DEL_EOM::process() {

//...... dont do anything if good_sequence flag is not set

  if (!good_sequence) return;

//...... remove the old object from the graphics state

  old_speedes_object = speedes_state->remove_object(unique_id);

//...... if old object exists, check send time in case of more recent updates

  if (old_speedes_object != NULL) {

    if (speedes_state->remove_object(unique_id)) {
      fprintf(stderr,"Error (gr_del_eom) two objects with same id\n");
    }

    if (send_time < old_speedes_object->get_send_time()) {
      good_sequence = 0;
      speedes_state->add_object(old_speedes_object);
    }

  }else{

    good_sequence = 0;

  }

}

/************************************************************************
* unprocess : unprocess a graphics event				*
************************************************************************/
void C_GR_DEL_EOM::unprocess() {

//...... dont do anything if good_sequence flag is not set

  if (!good_sequence) return;

//...... put back the old object

  speedes_state->add_object(old_speedes_object);

}

