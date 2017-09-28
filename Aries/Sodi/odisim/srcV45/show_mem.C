// show_mem.C method file

#include <stdio.h>

#include "show_mem.H"
#include "show_mem_mess.H"

#include "gridman.H"

#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

int C_SHOW_MEM::done = 0;
int C_SHOW_MEM::SHOW_MEM = 0;
int C_SHOW_MEM::GRID = 0;

/************************************************************************
* C_SHOW_MEM : construct a show_mem object				*
************************************************************************/
C_SHOW_MEM::C_SHOW_MEM() {

  if (!done) {
    done = 1;
    GRID = object_type("GRID");
    SHOW_MEM = event_type("SHOW_MEM");
  }

  set_lazy();

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_SHOW_MEM::init(C_HEADER * /*header*/) {
  //SHOW_MEM_MESS *show_mem_mess;

  //show_mem_mess = (SHOW_MEM_MESS *)header;

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_SHOW_MEM::exchange() {

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_SHOW_MEM::temp_process() {
  C_GRIDMAN *gridman;
  double next_time;

  gridman = (C_GRIDMAN *)get_manager(GRID);
  next_time = TIME_TAG + gridman->get_mem_time();
  schedule(next_time, SHOW_MEM, OBJECT_TYPE, LOCAL_ID, NODE);

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_SHOW_MEM::perm_process() {

  memory_print();
  freeobjs.print();

}

/************************************************************************
* cleanup : clean up data structures used                               *
************************************************************************/
void C_SHOW_MEM::cleanup() {


}
