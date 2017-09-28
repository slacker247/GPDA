// gr_event.C method file

#include <stdio.h>
#include "gr_event.H"

#include "rqueue.H"
C_RQUEUE *rqueue;

C_SPEEDES_STATE *C_GR_EVENT::speedes_state = NULL;

/************************************************************************
* set_rqueue : set the speedes queue				*
************************************************************************/
void C_GR_EVENT::set_rqueue(void *rq) {

  rqueue = (C_RQUEUE *)rq;

}

/************************************************************************
* schedule : schedule a graphics event					*
************************************************************************/
void C_GR_EVENT::schedule(C_GR_EVENT *gr_event) {

  rqueue->insert(gr_event);

}

/************************************************************************
* reschedule : reschedule this graphics event				*
************************************************************************/
void C_GR_EVENT::reschedule(double time) {

  time_tag = time;
  rqueue->insert(this);

}

/************************************************************************
* process : process a graphics event					*
************************************************************************/
void C_GR_EVENT::process() {

//  printf("Processing dummy graphics event %d at time %f\n", id, time_tag);

}

/************************************************************************
* unprocess : unprocess a graphics event				*
************************************************************************/
void C_GR_EVENT::unprocess() {

//  printf("Unprocessing dummy graphics event %d at time %f\n", id, time_tag);

}

/************************************************************************
* check_process : check if event processed correctly			*
************************************************************************/
int C_GR_EVENT::check_processed(int pf) {

  if (freeid == 0) return 1;

  if (pf != get_processed()) {
    fprintf(stderr,"Error (gr_event) processed = %d, pf = %d\n",
	get_processed(), pf);
    return 0;
  }else{
    return 1;
  }

}
