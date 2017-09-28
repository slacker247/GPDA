// add_s2g_mess.H message file

#ifndef adds2g_mess
#define adds2g_mess

#include "headers.H"

/************************************************************************
*	add mover to grid message 					*
************************************************************************/
class ADD_S2G_MESS : public C_HEADER {

  public:

    int object_id;	// id of object
    int object_type;	// type of object
    int object_node;	// node of object
    int unique_id;	// unique id of object

};

#endif
