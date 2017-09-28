// rsd_socket.C method file

#include <stdio.h>

#include "rsd_socket.H"
#include "rsd_socket_mess.H"
#include "graphobj.H"

#include "def.h"

int C_RSD_SOCKET::done = 0;
int C_RSD_SOCKET::RSD_SOCKET = 0;

/************************************************************************
* C_RSD_SOCKET : construct a rsd_socket object				*
************************************************************************/
C_RSD_SOCKET::C_RSD_SOCKET() {

  if (!done) {
    done = 1;
    RSD_SOCKET = event_type("RSD_SOCKET");
  }

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_RSD_SOCKET::init(C_HEADER *header) {
  RSD_SOCKET_MESS *rsd_socket_mess_in;

  rsd_socket_mess_in = (RSD_SOCKET_MESS *)header;
  socket = rsd_socket_mess_in->socket;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_RSD_SOCKET::exchange() {
  C_GRAPHOBJ *graphobj;

  graphobj = (C_GRAPHOBJ *)SIMOBJ;
  graphobj->exchange_socket(socket);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_RSD_SOCKET::temp_process() {

  fprintf(stderr,"RSD socket = %d\n",socket);

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_RSD_SOCKET::perm_process() {


}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_RSD_SOCKET::cleanup() {

}


