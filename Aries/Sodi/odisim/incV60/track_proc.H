// track_proc.H header file

#ifndef track_proc_object
#define track_proc_object

#include "event.H"
#include "track_proc_mess.H"

/************************************************************************
*	track_proc object 						*
************************************************************************/
class C_TRACK_PROC : public C_EVENT {

  private:
    TRACK_PROC_MESS *track_proc_mess;

  protected:

  public:

    C_TRACK_PROC();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy();

};


#endif
