// add_m2s_mess.H message file

#ifndef addm2s_mess
#define addm2s_mess

#include "headers.H"

/************************************************************************
*	add mover to sensor message 					*
************************************************************************/
class ADD_M2S_MESS : public C_HEADER {

  public:

    int unique_id;	// unique id of moving object
    void *peom;		// pointer to the eom of the object

};

#endif
