// free_gr_events.C method file

#include <stdio.h>

#include "free_gr_events.H"
#include "gr_event.H"
#include "gr_add_eom.H"
#include "gr_del_eom.H"
#include "gr_link.H"
#include "gr_kill.H"

void *new_GR_EVENT(int n)		{return new C_GR_EVENT[n];}
void *new_GR_ADD_EOM(int n)		{return new C_GR_ADD_EOM[n];}
void *new_GR_DEL_EOM(int n)		{return new C_GR_DEL_EOM[n];}
void *new_GR_LINK(int n)		{return new C_GR_LINK[n];}
void *new_GR_KILL(int n)		{return new C_GR_KILL[n];}

/************************************************************************
* FREE_GR_EVENTS : FREE_GR_EVENTS constructor				*
************************************************************************/
C_FREE_GR_EVENTS::C_FREE_GR_EVENTS() {

  set_ntypes(N_GR_EVENTS);

  set_type("GR_EVENT", GR_EVENT, new_GR_EVENT, sizeof(C_GR_EVENT), 1);
  set_type("GR_ADD_EOM", GR_ADD_EOM, new_GR_ADD_EOM, sizeof(C_GR_ADD_EOM), 100);
  set_type("GR_DEL_EOM", GR_DEL_EOM, new_GR_DEL_EOM, sizeof(C_GR_DEL_EOM), 100);
  set_type("GR_LINK", GR_LINK, new_GR_LINK, sizeof(C_GR_LINK), 100);
  set_type("GR_KILL", GR_KILL, new_GR_KILL, sizeof(C_GR_KILL), 100);

}


