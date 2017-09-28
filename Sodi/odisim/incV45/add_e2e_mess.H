// add_e2e_mess.H message file

#ifndef adde2e_mess
#define adde2e_mess

#include "headers.H"

/************************************************************************
*	add eom to eoman message 					*
************************************************************************/
class ADD_E2E_MESS : public C_HEADER {

  public :

    int unique_id;	// unique id for mover
    double script_time;	// time that script was generated

};

#endif
