// ext_graphics_script.C method file

#include <stdio.h>
#include <stdlib.h>

#include "ext_graphics_script.H"
#include "sensobj.H"
#include "mover.H"
#include "graphman.H"
#include "eom.H"

#include "ext_mess.H"
#include "def.h"
#include "host_user.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int EXT_GRAPHICS_SCRIPT;
extern int GRAPHICS;

/************************************************************************
* C_EXT_GRAPHICS_SCRIPT : construct a ext_graphics_script object		*
************************************************************************/
C_EXT_GRAPHICS_SCRIPT::C_EXT_GRAPHICS_SCRIPT() {}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_EXT_GRAPHICS_SCRIPT::init(C_HEADER *) {

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_EXT_GRAPHICS_SCRIPT::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_EXT_GRAPHICS_SCRIPT::temp_process() {
  C_MOVER *mover;
  C_SENSOBJ *sensobj;
  C_GRAPHMAN *graphman;
  C_GRAPHOBJ *graphobj;
  C_XQUEUE *script;
  C_EOM *eom;
  int i,len;
  int size;
  int size1;
  char *buff;
  char *pbuff;
  GLINK glink;
  int temp_size;

  mover = (C_MOVER *)SIMOBJ;
  graphman = (C_GRAPHMAN *)get_manager(GRAPHICS);
  graphobj = (C_GRAPHOBJ *)graphman->get_obj(0);

//...... go through the script and find out how many bytes

  script = mover->get_script();
  len = script->get_length();
  size = 0;
  eom = (C_EOM *)script->get_top();
  for (i=0; i<len; i++) {
    size += freeobjs.get_size(eom);
    eom = (C_EOM *)eom->get_link();
  }

  buff = new char[size];
  if (buff == NULL ) {
    fprintf(stderr,"Error, (EXT_GRAPHICS_SCRIPT) out of memory\n");
    exit(1);
  }

  pbuff = buff;
  eom = (C_EOM *)script->get_top();
  for (i=0; i<len; i++) {
    size1 = freeobjs.get_size(eom);
    memcpy(pbuff, (char *)eom, size1);
    pbuff += size1;
    eom = (C_EOM *)eom->get_link();
  }

  ext_graphics_script_output = (EXT_GRAPHICS_SCRIPT_OUTPUT *) RB_NEW_ARRAY_char(size+sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT));
  if (ext_graphics_script_output == NULL ) {
    fprintf(stderr,"Error, (EXT_GRAPHICS_SCRIPT) out of memory\n");
    exit(1);
  }

  ext_graphics_script_output->init(buff,size);
  delete [] buff; //RVI 2/18/98

//...... fill the header information

  ext_graphics_script_output->time_tag = TIME_TAG;
  ext_graphics_script_output->evtype = EXT_GRAPHICS_SCRIPT;
  ext_graphics_script_output->objid = -1;
  ext_graphics_script_output->obtype = OBJECT_TYPE;
  ext_graphics_script_output->ext = 0;
/*  Uncomment by DRE - 02/09/01 */
  ext_graphics_script_output->data_bytes =
	sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT) - sizeof(C_EM_HEADER);
/* */
  ext_graphics_script_output->EM_interaction = EM_MODULE;
  ext_graphics_script_output->EM_socket = graphobj->get_socket();
  ext_graphics_script_output->EM_done_time = TIME_TAG;
  ext_graphics_script_output->EM_flag = GRAPHICS_SCRIPT;

//...... if sensor is a space based sensor, link with partner in ring

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
/* Uncomment by DRE - 02/09/01 */
    ext_graphics_link_output->data_bytes =
	sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT) - sizeof(C_EM_HEADER);
/* */
    ext_graphics_link_output->EM_interaction = EM_MODULE;
  ext_graphics_link_output->EM_socket = graphobj->get_socket();
    ext_graphics_link_output->EM_done_time = TIME_TAG;
    ext_graphics_link_output->EM_flag = GRAPHICS_LINK;


  }else{

    ext_graphics_link_output = NULL;

  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_EXT_GRAPHICS_SCRIPT::perm_process() {
  C_GRAPHMAN *graphman;
  C_GRAPHOBJ *graphobj;


  fprintf(stderr,"External Graphics script %d bytes at time %f\n",
	ext_graphics_script_output->data_bytes, TIME_TAG);


  graphman = (C_GRAPHMAN *)get_manager(GRAPHICS);
  graphobj = (C_GRAPHOBJ *)graphman->get_obj(0);

//...... send out the script

  ext_graphics_script_output->EM_socket = graphobj->get_socket();
  HOST_USER->send_message(ext_graphics_script_output);
  delete ext_graphics_script_output;

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
void C_EXT_GRAPHICS_SCRIPT::cleanup() {

}


