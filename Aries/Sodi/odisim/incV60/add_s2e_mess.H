// add_s2e_mess.H message file

#ifndef adds2e_mess
#define adds2e_mess

#include "headers.H"

/************************************************************************
*	add mover to eoman message 					*
************************************************************************/
class ADD_S2E_MESS : public C_HEADER {

  public:

    int object_id;		// id of object
    int object_type;		// type of object
    int object_node;		// node of object
    int unique_id;		// unique id of object
    int mover_unique_id;	// unique id of mover object

};

#endif
