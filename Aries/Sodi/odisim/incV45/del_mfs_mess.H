// del_mfs_mess.H message file

#ifndef delmfs_mess
#define delmfs_mess

#include "headers.H"

/************************************************************************
*	del mover to sensor message 					*
************************************************************************/
class DEL_MFS_MESS : public C_HEADER {

  public:

    int unique_id;	// unique id of moving object
    void *peom;		// pointer to the eom of the object

};

#endif
