// add_m2g_mess.H message file

#ifndef addm2g_mess
#define addm2g_mess

#include "headers.H"

/************************************************************************
*	add mover to grid message 					*
************************************************************************/
class ADD_M2G_MESS : public C_HEADER {

  public:

    int object_id;	// id of object
    int object_type;	// type of object
    int object_node;	// node of object
    int unique_id;	// unique id of object

};

#endif
