// comman.C method file

#include <stdio.h>
#include <math.h>
#include "comman.H"
#include "locobj.H"
#include "def.h"

#include "comid.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

/************************************************************************
* C_COMMAN : construct a comman object					*
************************************************************************/
C_COMMAN::C_COMMAN() {
  int i;
  int index;
  C_COMID *comid;
  int *lids;
  int cnode;
  C_LOCOBJ *locobj;
  C_OBJMAN *manager;
  C_BASETYPE *basetype;

  basetype = message_parser->get_basetype("parameters");
  com_delay = basetype->get_float("com_delay");

  lids = new int[N_NODES];
  for (i=0; i<N_NODES; i++) lids[i] = 0;

  COM = object_type("COM");

  N_LOC = 0;
  N_TOT = comqueue->get_length();
  comid = (C_COMID *)comqueue->get_top();
  for (i=0; i<N_TOT; i++) {
    if (comid->get_object_node() == NODE) N_LOC++;
    comid = (C_COMID *)comid->get_link();
  }

  if (N_LOC) comobj = new C_COMOBJ[N_LOC];
//  fprintf(stderr,"Comman creating %d loc out of %d\n",N_LOC,N_TOT);

  index = 0;
  comid = (C_COMID *)comqueue->get_top();
  for (i=0; i<N_TOT; i++) {

    cnode = comid->get_object_node();
    comid->set_local_id(lids[cnode]);
    lids[cnode]++;

//    fprintf(stderr,"node = %d, local id = %d\n",cnode,comid->get_local_id());

    if (cnode == NODE) {

      manager = (C_OBJMAN *)get_manager(comid->get_object_type());
      locobj = (C_LOCOBJ *)manager->get_obj(comid->get_object_id());
      locobj->set_com_local_id(comid->get_local_id());

      comobj[index].init(comid);
      comobj[index].set_SEED(comid->get_unique_id() + 12345);
      comobj[index].set_GLOBAL_ID(1000+TOTAL_OBJECTS+i);
      index++;
    }

    comid = (C_COMID *)comid->get_link();
  }

  delete [] lids; //RVI 2/18/98

  TOTAL_OBJECTS += N_TOT + 2000;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&comobj[i]);
  set_interact();

  printf("COMMAN object created with %4d objects (%4d loc)\n", N_TOT, N_LOC);


}

/************************************************************************
* init_events : initialize events for the com element objects		*
************************************************************************/
void C_COMMAN::init_events() {

}

/************************************************************************
* get_comid : get the comid based on the named destination		*
************************************************************************/
C_COMID *C_COMMAN::get_comid(char *destination) {
  int i,len;
  C_COMID *comid;

  len = comqueue->get_length();
  comid = (C_COMID *)comqueue->get_top();
  for (i=0; i<len; i++) {
    if (!strcmp(destination,comid->get_object_name())) return comid;
    comid = (C_COMID *)comid->get_link();
  }

  fprintf(stderr,"Error (COMMAN), could not find comid %s\n",destination);
  return NULL;

}

