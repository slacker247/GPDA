// freeobjs.C method file

#include <stdio.h>

#include "freeobjs.H"
#include "oid.H"
#include "gridid.H"
#include "eom.H"
#include "bus.H"
#include "stage.H"
#include "int_stage.H"
#include "int_bus.H"
#include "greatcirc.H"
#include "loiter.H"
#include "stop.H"
#include "eom_holder.H"
#include "track.H"
#include "track1.H"
#include "track2.H"
#include "track3.H"
#include "detection.H"
#include "stereo_track.H"

void *new_OID(int n)			{return new C_OID[n];}
void *new_GRIDID(int n)			{return new C_GRIDID[n];}
void *new_EOM(int n)			{return new C_EOM[n];}
void *new_BUS(int n)			{return new C_BUS[n];}
void *new_STAGE(int n)			{return new C_STAGE[n];}
void *new_INT_STAGE(int n)		{return new C_INT_STAGE[n];}
void *new_INT_BUS(int n)		{return new C_INT_BUS[n];}
void *new_GREATCIRC(int n)		{return new C_GREATCIRC[n];}
void *new_LOITER(int n)			{return new C_LOITER[n];}
void *new_STOP(int n)			{return new C_STOP[n];}
void *new_EOM_HOLDER(int n)		{return new C_EOM_HOLDER[n];}
void *new_TRACK(int n)			{return new C_TRACK[n];}
void *new_TRACK1(int n)			{return new C_TRACK1[n];}
void *new_TRACK2(int n)			{return new C_TRACK2[n];}
void *new_TRACK3(int n)			{return new C_TRACK3[n];}
void *new_DETECTION(int n)		{return new C_DETECTION[n];}
void *new_STEREO_TRACK(int n)		{return new C_STEREO_TRACK[n];}

/************************************************************************
* FREEOBJS : FREEOBJS constructor					*
************************************************************************/
C_FREEOBJS::C_FREEOBJS() {

  set_ntypes(N_FREEOBJS);

  set_type("OID", OID, new_OID, sizeof(C_OID), 1000);
  set_type("GRIDID", GRIDID, new_GRIDID, sizeof(C_GRIDID), 1000);
  set_type("EOM", EOM, new_EOM, sizeof(C_EOM), 100);
  set_type("GREATCIRC", GREATCIRC, new_GREATCIRC, sizeof(C_GREATCIRC), 1000);
  set_type("BUS", BUS, new_BUS, sizeof(C_BUS), 100);
  set_type("STAGE", STAGE, new_STAGE, sizeof(C_STAGE), 100);
  set_type("INT_STAGE", INT_STAGE, new_INT_STAGE, sizeof(C_INT_STAGE), 100);
  set_type("INT_BUS", INT_BUS, new_INT_BUS, sizeof(C_INT_BUS), 100);
  set_type("LOITER", LOITER, new_LOITER, sizeof(C_LOITER), 100);
  set_type("STOP", STOP, new_STOP, sizeof(C_STOP), 100);
  set_type("EOM_HOLDER", EOM_HOLDER, new_EOM_HOLDER, sizeof(C_EOM_HOLDER), 100);
  set_type("TRACK", TRACK, new_TRACK, sizeof(C_TRACK), 100);
  set_type("TRACK1", TRACK1, new_TRACK1, sizeof(C_TRACK1), 100);
  set_type("TRACK2", TRACK2, new_TRACK2, sizeof(C_TRACK2), 100);
  set_type("TRACK3", TRACK3, new_TRACK3, sizeof(C_TRACK3), 100);
  set_type("DETECTION", DETECTION, new_DETECTION, sizeof(C_DETECTION), 100);
  set_type("STEREO_TRACK", STEREO_TRACK, new_STEREO_TRACK,
		sizeof(C_STEREO_TRACK), 100);

}


