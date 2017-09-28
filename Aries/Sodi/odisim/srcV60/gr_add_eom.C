// gr_add_eom.C method file

#include <stdio.h>
#include "gr_add_eom.H"

#include "freetypes.H"

//#define NOISY

/************************************************************************
* process : process a graphics event					*
************************************************************************/
void C_GR_ADD_EOM::process() {
  //C_EOM *eom;

//...... dont do anything if good_sequence flag is not set

  if (!good_sequence) return;

//...... print out stuff if impact

#ifdef NOISY
  eom = new_speedes_object.get_eom();
  if (eom != NULL) {
    if (eom->get_freeid() == STOP) {
      printf("Impact for %d at time %f\n",
	new_speedes_object.get_unique_id(), time_tag);
    }
  }
#endif

//...... remove the old object from the graphics state

  old_speedes_object =
	speedes_state->remove_object(new_speedes_object.get_id());

//...... if old object exists, check send time in case of more recent updates

  if (old_speedes_object != NULL) {
    if (new_speedes_object.get_send_time() <
	old_speedes_object->get_send_time() ) {
      good_sequence = 0;
    }else{
      new_speedes_object.set_GR_object(old_speedes_object->get_GR_object());
      new_speedes_object.set_alive(old_speedes_object->get_alive());
    }
  }

  if (speedes_state->remove_object(new_speedes_object.get_id())) {
    fprintf(stderr,"Error (gr_add_eom) two objects with same id\n");
  }

  if (good_sequence) {
    speedes_state->add_object(&new_speedes_object);
  }else{
    speedes_state->add_object(old_speedes_object);
  }

}

/************************************************************************
* unprocess : unprocess a graphics event				*
************************************************************************/
void C_GR_ADD_EOM::unprocess() {

//...... dont do anything if good_sequence flag is not set

  if (!good_sequence) return;

//...... take out the new object and put back the old if it existed

  speedes_state->remove_object(&new_speedes_object);

//...... put back the old object if it existed

  if (old_speedes_object != NULL) {
    speedes_state->add_object(old_speedes_object);
  }

}

/************************************************************************
* init : initialize this event						*
************************************************************************/
void C_GR_ADD_EOM::init(C_EOM *eom) {

//  eom->set_icon(2);	// temp test

  new_speedes_object.set_unique_id(eom->get_id());
  new_speedes_object.set_icon(eom->get_icon());
  new_speedes_object.set_eom(eom);
  good_sequence = 1;

}

/************************************************************************
* init : initialize this event						*
************************************************************************/
void C_GR_ADD_EOM::init(EXT_GRAPHICS_DEFINE_OUTPUT *mess) {
  double V[3];

  V[0] = 0.0;
  V[1] = 0.0;
  V[2] = 0.0;

  new_speedes_object.set_unique_id(mess->unique_id);

  new_speedes_object.set_icon(mess->icon);
//  new_speedes_object.set_icon(199);

  new_speedes_object.set_eom(NULL);
  new_speedes_object.set_pos_vel(mess->time_tag, mess->X, V);
  new_speedes_object.set_Rmin(mess->Rmin);
  new_speedes_object.set_Rmax(mess->Rmax);

  good_sequence = 1;

}
