// track_in.H header file

#ifndef track_in_object
#define track_in_object

#include "event.H"
#include "track_in_mess.H"

/************************************************************************
*	track_proc object 						*
************************************************************************/
class C_TRACK_IN : public C_EVENT {

  private:
    TRACK_IN_MESS *track_in_mess;

  protected:

  public:

    C_TRACK_IN();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy();

};


#endif
