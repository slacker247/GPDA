// del_sfe_mess.H message file

#ifndef delsfe_mess
#define delsfe_mess

#include "headers.H"

/************************************************************************
*	add mover to eoman message 					*
************************************************************************/
class DEL_SFE_MESS : public C_HEADER {

  public:

    int object_id;		// id of object
    int object_type;		// type of object
    int object_node;		// node of object
    int unique_id;		// unique id of object
    int mover_unique_id;	// unique id of mover object

};

#endif
