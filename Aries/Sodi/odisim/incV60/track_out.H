// track_out.H header file

#ifndef track_out_object
#define track_out_object

#include "event.H"

/************************************************************************
*	track_out object 						*
************************************************************************/
class C_TRACK_OUT : public C_EVENT {

  private:
    int trackup_node;

  protected:

  public:

    C_TRACK_OUT();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
