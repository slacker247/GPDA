// ext_graphics_define.C method file

#include <stdio.h>

#include "ext_graphics_define.H"
#include "mover.H"
#include "sensobj.H"
#include "graphman.H"
#include "eom.H"

#include "ext_mess.H"
#include "def.h"
#include "host_user.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int EXT_GRAPHICS_DEFINE;
extern int GRAPHICS;
extern int DSP;

/************************************************************************
* C_EXT_GRAPHICS_DEFINE : construct a ext_graphics_define object	*
************************************************************************/
C_EXT_GRAPHICS_DEFINE::C_EXT_GRAPHICS_DEFINE() {}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_EXT_GRAPHICS_DEFINE::init(C_HEADER *) {

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_EXT_GRAPHICS_DEFINE::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_EXT_GRAPHICS_DEFINE::temp_process() {
  C_LOCOBJ *locobj;
  C_SENSOBJ *sensobj;
  double X[3];
  double V[3];
  char *buff;
  int temp_size;
  GLINK glink;

//...... get the essential information out of the locobj

  locobj = (C_LOCOBJ *)SIMOBJ;
  locobj->get_pos_vel(TIME_TAG,X,V);

  ext_graphics_define_output.X[0] = X[0];
  ext_graphics_define_output.X[1] = X[1];
  ext_graphics_define_output.X[2] = X[2];
  ext_graphics_define_output.icon = locobj->get_icon();
  ext_graphics_define_output.unique_id = locobj->get_GLOBAL_ID();
  ext_graphics_define_output.Rmin = locobj->get_rmin();
  ext_graphics_define_output.Rmax = locobj->get_rmax();

//...... fill the header information

  ext_graphics_define_output.time_tag = TIME_TAG;
  ext_graphics_define_output.evtype = EXT_GRAPHICS_DEFINE;
  ext_graphics_define_output.objid = -1;
  ext_graphics_define_output.obtype = OBJECT_TYPE;
  ext_graphics_define_output.ext = 0;
  ext_graphics_define_output.EM_interaction = EM_MODULE;
  ext_graphics_define_output.EM_done_time = TIME_TAG;
  ext_graphics_define_output.EM_flag = GRAPHICS_DEFINE;
  ext_graphics_define_output.data_bytes =
	sizeof(EXT_GRAPHICS_DEFINE_OUTPUT) - sizeof(C_EM_HEADER);

//...... set the default that there is no message

  ext_graphics_link_output = NULL;

//...... if sensor is a DSP, link with partner in ring

  if (OBJECT_TYPE != DSP) return;

  sensobj = (C_SENSOBJ *)SIMOBJ;
  if ((sensobj->get_n_per_ring() != -1) && (1)) {

    temp_size = sizeof(GLINK) + sizeof(EXT_GRAPHICS_LINK_OUTPUT);
    buff = RB_NEW_ARRAY_char(temp_size);
    ext_graphics_link_output = (EXT_GRAPHICS_LINK_OUTPUT *)buff;

//...... initialize glink (graphics link information)

    glink.sensor_unique_id = SIMOBJ->get_GLOBAL_ID();

    if (sensobj->get_id_in_ring() == (sensobj->get_n_per_ring()-1)) {
      glink.track_unique_id = SIMOBJ->get_GLOBAL_ID() + 1
				-sensobj->get_n_per_ring();
    }else{
      glink.track_unique_id = SIMOBJ->get_GLOBAL_ID() + 1;
    }

    glink.link_flag = 1;
    glink.color = SP_WHITE;
    glink.intensity = 0.15;

//...... set the data in the link stuff

    ext_graphics_link_output->init((char *)&glink, sizeof(GLINK));

//...... fill in the header stuff

    ext_graphics_link_output->time_tag = TIME_TAG + 0.000001;
    ext_graphics_link_output->evtype = -1;
    ext_graphics_link_output->objid = -1;
    ext_graphics_link_output->obtype = OBJECT_TYPE;
    ext_graphics_link_output->ext = 0;
/*
    ext_graphics_link_output->data_bytes =
	sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT) - sizeof(C_EM_HEADER);
*/
    ext_graphics_link_output->EM_interaction = EM_MODULE;
//  ext_graphics_link_output->EM_socket = graphobj->get_socket();
    ext_graphics_link_output->EM_done_time = TIME_TAG;
    ext_graphics_link_output->EM_flag = GRAPHICS_LINK;

  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_EXT_GRAPHICS_DEFINE::perm_process() {
  C_GRAPHMAN *graphman;
  C_GRAPHOBJ *graphobj;

/*
  fprintf(stderr,"External Graphics define %d bytes at time %f\n",
	ext_graphics_define_output.data_bytes, TIME_TAG);
*/

  graphman = (C_GRAPHMAN *)get_manager(GRAPHICS);
  graphobj = (C_GRAPHOBJ *)graphman->get_obj(0);

//...... send out the define message

  ext_graphics_define_output.EM_socket = graphobj->get_socket();
  HOST_USER->send_message(&ext_graphics_define_output);

//...... send out the link if necessary

  if (ext_graphics_link_output) {
    ext_graphics_link_output->EM_socket = graphobj->get_socket();
    HOST_USER->send_message(ext_graphics_link_output);
    delete ext_graphics_link_output;
  }


}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_EXT_GRAPHICS_DEFINE::cleanup() {

}


