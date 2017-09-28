// receive_message.C method file

#include <stdio.h>
#include <stdlib.h>

#include "receive_message.H"
#include "receive_message_mess.H"

#include "comman.H"
#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_RECEIVE_MESSAGE::done = 0;
int C_RECEIVE_MESSAGE::RECEIVE_MESSAGE = 0;
int C_RECEIVE_MESSAGE::COM = 0;

/************************************************************************
* C_RECEIVE_MESSAGE : construct a receive_message object		*
************************************************************************/
C_RECEIVE_MESSAGE::C_RECEIVE_MESSAGE() {

  if (!done) {
    done = 1;
    RECEIVE_MESSAGE = event_type("RECEIVE_MESSAGE");
    COM = object_type("COM");
  }

//  set_lazy();

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_RECEIVE_MESSAGE::init(C_HEADER *header) {
  int size;

  size = header->bytes + sizeof(RECEIVE_MESSAGE_MESS);
  receive_message_mess = (RECEIVE_MESSAGE_MESS *)new char[size];
  if (receive_message_mess == NULL) {
    fprintf(stderr,"Error (RECEIVE_MESSAGE) no more memory\n");
    exit(1);
  }
  memcpy((char *)receive_message_mess, (char *)header, size);

  if (receive_message_mess->nbytes != receive_message_mess->bytes) {
    fprintf(stderr,"Error (RECEIVE_MESSAGE) bad init\n");
  }

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_RECEIVE_MESSAGE::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_RECEIVE_MESSAGE::temp_process() {
  char *event_name;
  C_BASETYPE *message_types;
  C_COMMAN *comman;
  C_PARSER *message_parser;
  int et;
  C_COMID *comid;
  C_COMOBJ *comobj;
  RECEIVE_MESSAGE_MESS *dummy_message_mess;
  char *buff;
  char *bf1;
  char *bf2;
  char *bf;

  comobj = (C_COMOBJ *)SIMOBJ;
  comid = comobj->get_comid();
  comman = (C_COMMAN *)get_manager(COM);
  message_parser = comman->get_message_parser();

  message_types = message_parser->get_basetype("message_types");
  event_name = message_types->get_string(receive_message_mess->message_type);
  et = event_type(event_name);

/*
  fprintf(stderr,"Receiving message from %s to %s\n",
	receive_message_mess->source,
	receive_message_mess->destination);

  fprintf(stderr,"Activating event %d %s\n", et, event_name);
*/

  dummy_message_mess = (RECEIVE_MESSAGE_MESS *)schedule(
	GONE*TIME_TAG,
	et,
	comid->get_object_type(),
	comid->get_object_id(),
	comid->get_object_node(),
	receive_message_mess->bytes,
	buff);

  bf1 = (char *)dummy_message_mess;
  bf1 += sizeof(C_HEADER);
  bf2 = (char *)receive_message_mess;
  bf2 += sizeof(C_HEADER);
  memcpy(bf1, bf2, sizeof(RECEIVE_MESSAGE_MESS) - sizeof(C_HEADER));

  if (dummy_message_mess->bytes) {
    bf = (char *)receive_message_mess;
    bf += sizeof(RECEIVE_MESSAGE_MESS);
    memcpy(buff,bf,receive_message_mess->bytes);
  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_RECEIVE_MESSAGE::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_RECEIVE_MESSAGE::cleanup() {

  delete receive_message_mess;

}
