// send_message.C method file

#include <stdio.h>
#include <stdlib.h>

#include "send_message.H"
#include "send_message_mess.H"
#include "receive_message_mess.H"

#include "comman.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_SEND_MESSAGE::done = 0;
int C_SEND_MESSAGE::SEND_MESSAGE = 0;
int C_SEND_MESSAGE::RECEIVE_MESSAGE = 0;
int C_SEND_MESSAGE::COM = 0;

/************************************************************************
* C_SEND_MESSAGE : construct a send_message object			*
************************************************************************/
C_SEND_MESSAGE::C_SEND_MESSAGE() {

  if (!done) {
    done = 1;
    SEND_MESSAGE = event_type("SEND_MESSAGE");
    RECEIVE_MESSAGE = event_type("RECEIVE_MESSAGE");
    COM = object_type("COM");
  }

//  set_lazy();

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_SEND_MESSAGE::init(C_HEADER *header) {
  int size;

  size = header->bytes + sizeof(SEND_MESSAGE_MESS);
  send_message_mess = (SEND_MESSAGE_MESS *)new char[size];
  if (send_message_mess == NULL) {
    fprintf(stderr,"Error (SEND_MESSAGE) no more memory\n");
    exit(1);
  }
  memcpy((char *)send_message_mess, (char *)header, size);

  if (send_message_mess->nbytes != send_message_mess->bytes) {
    fprintf(stderr,"Error (SEND_MESSAGE) bad init\n");
  }

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_SEND_MESSAGE::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_SEND_MESSAGE::temp_process() {
  C_COMMAN *comman;
  C_COMID *comid;
  char *buff;
  char *bf1;
  char *bf2;
  char *bf;
  RECEIVE_MESSAGE_MESS *receive_message_mess;

  comman = (C_COMMAN *)get_manager(COM);
  comid = comman->get_comid(send_message_mess->destination);

//...... for now, just send the message to the destination

  receive_message_mess = (RECEIVE_MESSAGE_MESS *)schedule(
	TIME_TAG + comman->get_com_delay(),
	RECEIVE_MESSAGE,
	COM,
	comid->get_local_id(),
	comid->get_object_node(),
	send_message_mess->bytes,
	buff);

  bf1 = (char *)receive_message_mess;
  bf1 += sizeof(C_HEADER);
  bf2 = (char *)send_message_mess;
  bf2 += sizeof(C_HEADER);
  memcpy(bf1, bf2, sizeof(RECEIVE_MESSAGE_MESS) - sizeof(C_HEADER));

  if (send_message_mess->bytes) {
    bf = (char *)send_message_mess;
    bf += sizeof(SEND_MESSAGE_MESS);
    memcpy(buff,bf,send_message_mess->bytes);
  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_SEND_MESSAGE::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_SEND_MESSAGE::cleanup() {

  delete send_message_mess;

}
