// base_scan.C method file

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "base_scan.H"
#include "sensobj.H"
#include "send_message_mess.H"
#include "oid.H"
#include "sq_item.H"
#include "def.h"

#include "ext_mess.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int COM;
extern int GRAPHICS;
extern int SEND_MESSAGE;

/************************************************************************
* C_BASE_SCAN : construct a base_scan object				*
************************************************************************/
C_BASE_SCAN::C_BASE_SCAN() {
  new_tracks = new C_XQUEUE();
}


/************************************************************************
* graphics : send out links and unlinks to the graphics                 *
************************************************************************/
void C_BASE_SCAN::graphics(C_XQUEUE *old_trks, C_XQUEUE *new_trks) {
  C_QUEUE new_links;
  C_QUEUE old_links;
  C_OID *oid;
  C_SQ_ITEM *track;
  int new_len;
  int old_len;
  int i;
  int nlinks;
  GLINK *glink;
  int index;
  char *buff;
  int temp_size;
  C_SENSOBJ *sensobj;

  sensobj = (C_SENSOBJ *)SIMOBJ;

//...... get the size of the new and old track list

  new_len = new_trks->get_length();
  old_len = old_trks->get_length();

//...... check for new tracks

  track = (C_SQ_ITEM *)new_trks->get_top();
  for (i=0; i<new_len; i++) {
    if (old_trks->find(track->get_id()) == NULL) {
      oid = (C_OID *)freeobjs.new_object(OID);
      oid->set_id(track->get_id());
      new_links.push_bot(oid);
    }
    track = (C_SQ_ITEM *)track->get_link();
  }

//...... check for old tracks that are no longer in the list of tracks

  track = (C_SQ_ITEM *)old_trks->get_top();
  for (i=0; i<old_len; i++) {
    if (new_trks->find(track->get_id()) == NULL) {
      oid = (C_OID *)freeobjs.new_object(OID);
      oid->set_id(track->get_id());
      old_links.push_bot(oid);
    }
    track = (C_SQ_ITEM *)track->get_link();
  }

  size_link_output = 0;
  nlinks = new_links.get_length() + old_links.get_length();

//...... return if there are no new links for the graphics

  if (nlinks == 0) return;

//...... create the graphics output buffer of links

  size_link_output = nlinks*sizeof(GLINK);
  glink = new GLINK[nlinks];
  index = 0;

//...... fill the buffer

  oid = (C_OID *)new_links.get_top();
  for (i=0; i<new_links.get_length(); i++) {

    glink[index].sensor_unique_id = SIMOBJ->get_GLOBAL_ID();
    glink[index].track_unique_id = oid->get_id();
    glink[index].link_flag = 1;

    glink[index].color = SP_YELLOW;
    if (sensobj->get_icon() == 45) {
      glink[index].color = SP_PURPLE;
    }
    if (sensobj->get_icon() == 44) {
      glink[index].color = SP_CYAN;
    }
    if (sensobj->get_icon() == 151) {
      glink[index].color = SP_RED;
    }

    glink[index].intensity = 1.0;
    index++;

    oid = (C_OID *)oid->get_link();
  }
  
  oid = (C_OID *)old_links.get_top();
  for (i=0; i<old_links.get_length(); i++) {

    glink[index].sensor_unique_id = SIMOBJ->get_GLOBAL_ID();
    glink[index].track_unique_id = oid->get_id();
    glink[index].link_flag = 0;
    index++;

    oid = (C_OID *)oid->get_link();
  }
  

//...... initialize the output message

  temp_size = size_link_output + sizeof(EXT_GRAPHICS_LINK_OUTPUT);
  buff = RB_NEW_ARRAY_char(temp_size);
  ext_graphics_link_output = (EXT_GRAPHICS_LINK_OUTPUT *)buff;

  ext_graphics_link_output->init((char *)glink,size_link_output);
  delete [] glink; //RVI 2/18/98

//...... fill in the header stuff

  ext_graphics_link_output->time_tag = TIME_TAG;
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


//...... clean up all of the oid objects

  freeobjs.delete_list(&new_links, OID);
  freeobjs.delete_list(&old_links, OID);

}
