// add_det.H header file

#ifndef add_det_object
#define add_det_object

#include "event.H"
#include "add_det_mess.H"

/************************************************************************
*	add_det object 							*
************************************************************************/
class C_ADD_DET : public C_EVENT {

  private:

    static int done;
    static int ADD_DET;
    static int TRACK_OUT;
    static int CENTER;
    ADD_DET_MESS *add_det_mess;

    double trackup_time;

  protected:

  public:

    C_ADD_DET();
    virtual void init(C_HEADER *header);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
