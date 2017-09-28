// del_mfs.C method file

#include <strstream.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include "del_mfs.H"
#include "del_mfs_mess.H"
#include "sensobj.H"
#include "oid.H"

#include "def.h"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

extern int DEL_MFS;

/************************************************************************
* C_DEL_MFS : construct a del_mfs object				*
************************************************************************/
C_DEL_MFS::C_DEL_MFS() {}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_DEL_MFS::init(C_HEADER *header) {
  DEL_MFS_MESS *del_mfs_mess;

  del_mfs_mess = (DEL_MFS_MESS *)header;
  unique_id = del_mfs_mess->unique_id;
  peom = del_mfs_mess->peom;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_DEL_MFS::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_DEL_MFS::temp_process() {
  C_SENSOBJ *sensobj;
  C_XQUEUE *movers;
  C_OID *oid;

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);

//...... check that the eom has the right id

/*
  if ( ((C_EOM *)peom)->get_id() != unique_id) {
    sprintf(s,"Error (ADD_M2S) unique id %d not right for peom\n",unique_id);
    RB_PRINT(stderr,s);
  }
*/

//...... get the list of moving objects from the sensor

  sensobj = (C_SENSOBJ *)SIMOBJ;
  movers = sensobj->get_movers();

//...... delete the mover from the list

  oid = (C_OID *)movers->find(unique_id);
  if (oid) {

    if (oid->get_peom() != peom) {
      ostrstream so;
      so << "Error (DEL_MFS) mover (m " << unique_id
	 << ", s " << sensobj->get_GLOBAL_ID()
	 << ") has wrong peom at " << TIME_TAG << "\n" << ends;
      RB_PRINT(stderr,so.str());
    }

    *movers -= oid;
    RB_FREE_DELETE(oid);

  }else{
    ostrstream so;
    so << "Error (DEL_MFS) mover (m " << unique_id
       << ", s " << sensobj->get_GLOBAL_ID() << ") not there at "
       << TIME_TAG << "\n" << ends;
    RB_PRINT(stderr,so.str());
  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_DEL_MFS::perm_process() {

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_DEL_MFS::cleanup() {


}
