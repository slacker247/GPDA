// gr_link.C method file

#include <stdio.h>
#include "gr_link.H"

//#define NOISY

/************************************************************************
* process : process a graphics event					*
************************************************************************/
void C_GR_LINK::process() {

  if (link_flag) {
    speedes_state->add_link(&link);
#ifdef NOISY
    printf("add link %d to %d at time %f\n",
	sensor_id, track_id, time_tag);
#endif
  }else{
    unlink = speedes_state->remove_link(sensor_id, track_id);
    if (unlink == NULL) {
      fprintf(stderr,"Error (gr_link), could not unlink\n");
    }
#ifdef NOISY
    printf("removing link %d to %d at time %f\n",
	sensor_id, track_id, time_tag);
#endif
  }

}

/************************************************************************
* unprocess : unprocess a graphics event				*
************************************************************************/
void C_GR_LINK::unprocess() {
  C_LINK *ul;

  if (link_flag) {

    ul = speedes_state->remove_link(sensor_id, track_id);
    if (ul != &link) {
      printf("Error, bad link remove when unprocessing\n");
    }

#ifdef NOISY
    printf("removing link %d to %d at time %f\n",
	sensor_id, track_id, time_tag);
#endif

  }else{

    speedes_state->add_link(unlink);

#ifdef NOISY
    printf("add link %d to %d at time %f\n", sensor_id, track_id, time_tag);
#endif

  }

}

/************************************************************************
* init : initialize this event						*
************************************************************************/
void C_GR_LINK::init(int sid, int tid, int color, double in, int flag) {

  sensor_id = sid;
  track_id = tid;
  link_flag = flag;

  unlink = NULL;

  if (link_flag) {
    link.set_sensor_id(sensor_id);
    link.set_track_id(track_id);
    link.set_color(color);
    link.set_intensity(in);
  }

}

