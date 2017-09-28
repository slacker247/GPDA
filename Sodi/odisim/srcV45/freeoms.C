// freeobjs.C method file

#include <stdio.h>

#include "freeoms.H"
#include "eom.H"
#include "bus.H"
#include "stage.H"
#include "int_stage.H"
#include "int_bus.H"
#include "greatcirc.H"
#include "stop.H"

void *new_EOMg(int n)			{return new C_EOM[n];}
void *new_BUSg(int n)			{return new C_BUS[n];}
void *new_STAGEg(int n)			{return new C_STAGE[n];}
void *new_INT_STAGEg(int n)		{return new C_INT_STAGE[n];}
void *new_INT_BUSg(int n)		{return new C_INT_BUS[n];}
void *new_GREATCIRCg(int n)		{return new C_GREATCIRC[n];}
void *new_STOPg(int n)			{return new C_STOP[n];}

/************************************************************************
* FREEOBJS : FREEOBJS constructor					*
************************************************************************/
C_FREEOMS::C_FREEOMS() {

  set_ntypes(N_FREEOBJS);

  set_type("EOM", EOM, new_EOMg, sizeof(C_EOM), 100);
  set_type("GREATCIRC", GREATCIRC, new_GREATCIRCg, sizeof(C_GREATCIRC), 1000);
  set_type("BUS", BUS, new_BUSg, sizeof(C_BUS), 100);
  set_type("STAGE", STAGE, new_STAGEg, sizeof(C_STAGE), 100);
  set_type("INT_STAGE", INT_STAGE, new_INT_STAGEg, sizeof(C_INT_STAGE), 100);
  set_type("INT_BUS", INT_BUS, new_INT_BUSg, sizeof(C_INT_BUS), 100);
  set_type("STOP", STOP, new_STOPg, sizeof(C_STOP), 100);

}


