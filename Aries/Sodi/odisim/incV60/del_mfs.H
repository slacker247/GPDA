// del_mfs.H header file

#ifndef del_mfs_object
#define del_mfs_object

#include "event.H"

/************************************************************************
*	del_mfs object 							*
************************************************************************/
class C_DEL_MFS : public C_EVENT {

  private:

    void *peom;			// pointer to the eom of the object
    int unique_id;		// unique_id of object

  public:

    C_DEL_MFS();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
