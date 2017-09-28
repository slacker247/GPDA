// ext_tracker.H header file

#ifndef ext_tracker_object
#define ext_tracker_object

#include "event.H"
#include "ext_tracker_mess.H"

/************************************************************************
*	ext_tracker object 						*
************************************************************************/
class C_EXT_TRACKER : public C_EVENT {

  private:
    EXT_TRACKER_MESS *ext_tracker_mess;
    int blocking;
    int new_blocking;

    C_ITEM *item;
    int toggle;

    char *buff;
    int size;

  protected:

  public:

    C_EXT_TRACKER();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy();

};


#endif
