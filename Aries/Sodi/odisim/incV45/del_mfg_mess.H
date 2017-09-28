// del_mfg_mess.H message file

#ifndef delmfg_mess
#define delmfg_mess

#include "headers.H"

/************************************************************************
*	delete mover from grid message 					*
************************************************************************/
class DEL_MFG_MESS : public C_HEADER {

  public:

    int object_id;	// id of object
    int object_type;	// type of object
    int object_node;	// node of object
    int unique_id;	// unique id of object

};

#endif
