// ext_graphics_gvt.C method file

#include <stdio.h>

#include "ext_graphics_gvt.H"
#include "ext_graphics_gvt_mess.H"
#include "rsd_socket_mess.H"
#include "graphman.H"
#include "graphobj.H"
#include "StatsMessage.H"
#include "ext_mess.H"
#include "SpeedesComm.H"
#include "def.h"
#include "host_user.H"

extern int GRAPHICS;
extern int EXT_GRAPHICS_GVT;
extern int RSD_SOCKET;
/* Changed by RVI on 9/23 along with the change of
   ext_graphics_gvt_mess to a pointer rather than non-pointer.  This
   was done so that a bunch of statistics info could be tacked on to
   the end of this message */

/************************************************************************
* C_EXT_GRAPHICS_GVT : construct a ext_graphics_gvt object		*
************************************************************************/
C_EXT_GRAPHICS_GVT::C_EXT_GRAPHICS_GVT() {
  static int n_nodes = SpComm_GetNumNodes();
  ext_graphics_gvt_mess = (EXT_GRAPHICS_GVT_MESS *)
    new char[sizeof(EXT_GRAPHICS_GVT_MESS)+
	    n_nodes*sizeof(StatsMessage) + 8];
}

C_EXT_GRAPHICS_GVT::~C_EXT_GRAPHICS_GVT() {
  delete [] (char *)ext_graphics_gvt_mess;
}
/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_EXT_GRAPHICS_GVT::init(C_HEADER *header) {
  EXT_GRAPHICS_GVT_MESS *ext_graphics_gvt_mess_in;

  ext_graphics_gvt_mess_in = (EXT_GRAPHICS_GVT_MESS *)header;
  EM_done_time = ext_graphics_gvt_mess_in->EM_done_time;
  EM_socket = ext_graphics_gvt_mess_in->EM_socket;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_EXT_GRAPHICS_GVT::exchange() {
  C_GRAPHOBJ *graphobj;

  if (toggle) {
    SIMOBJ->add_block(item);
    toggle = 0;
  }else{
    SIMOBJ->remove_block(item);
    toggle = 1;
  }

  graphobj = (C_GRAPHOBJ *)SIMOBJ;
  graphobj->exchange_blocking(new_blocking);
  graphobj->exchange_socket(socket);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_EXT_GRAPHICS_GVT::temp_process() {
  C_GRAPHMAN *graphman;
  C_GRAPHOBJ *graphobj;
  RSD_SOCKET_MESS *rsd_socket_mess;
  double btime;

  cerr << "External Graphics at time " << TIME_TAG << endl;

  graphobj = (C_GRAPHOBJ *)SIMOBJ;
  graphman = (C_GRAPHMAN *)get_manager(GRAPHICS);

  blocking = graphobj->get_blocking();

#ifdef KSR1
  if (blocking) {
    SIMOBJ->unblock(-SIMOBJ->get_GLOBAL_ID());
  }
#endif

  new_blocking = 0;

  toggle = 1;

  item = RB_NEW_C_ITEM(new C_ITEM());

  btime = TIME_TAG + graphman->get_cycle_time();
  if (btime < graphman->get_start_time()) btime = graphman->get_start_time();
  item->set_time_tag(btime);

//  item->set_time_tag(TIME_TAG + EM_done_time);

  item->set_id(EM_socket);

//...... fill the header information

  ext_graphics_gvt_mess->time_tag = TIME_TAG;
  ext_graphics_gvt_mess->evtype = EXT_GRAPHICS_GVT;
  ext_graphics_gvt_mess->objid = -1;
  ext_graphics_gvt_mess->obtype = OBJECT_TYPE;
  ext_graphics_gvt_mess->ext = 0;
  ext_graphics_gvt_mess->data_bytes =
	sizeof(EXT_GRAPHICS_GVT_MESS) - sizeof(C_EM_HEADER);
  ext_graphics_gvt_mess->EM_interaction = EM_MODULE;
  ext_graphics_gvt_mess->EM_socket = item->get_id();
  ext_graphics_gvt_mess->EM_done_time = item->get_time_tag();
  ext_graphics_gvt_mess->EM_flag = GRAPHICS_GVT;

  FILE * InFile = fopen("./StatOutput", "r+b");
  if (InFile){
    static int n_nodes = SpComm_GetNumNodes();
    buff = new char[sizeof(StatsMessage)*n_nodes+8];
    fread(buff+8, sizeof(StatsMessage), n_nodes, InFile);
    int * ibuff = (int *)buff;
    *ibuff = n_nodes;
    fclose(InFile);
    ext_graphics_gvt_mess->init(buff,sizeof(StatsMessage)*n_nodes+8); 
  }

//...... undo the first time blocking

  socket = EM_socket;

  if (blocking) {
    for (int i=1; i<N_NODES; i++) {

      rsd_socket_mess = (RSD_SOCKET_MESS *)schedule(
        TIME_TAG,
        RSD_SOCKET,
	OBJECT_TYPE,
	0,
	i);

      rsd_socket_mess->socket = EM_socket;

    }
  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_EXT_GRAPHICS_GVT::perm_process() {

fprintf(stderr, "C_EXT_GRAPHICS_GVT::perm_process called\n");

#ifndef KSR1
  if (blocking) {
    SIMOBJ->unblock(-SIMOBJ->get_GLOBAL_ID());
  }
#endif

  HOST_USER->send_message(ext_graphics_gvt_mess);

}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_EXT_GRAPHICS_GVT::cleanup() {

}


