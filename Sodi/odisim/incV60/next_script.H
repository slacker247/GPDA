// next_script.H header file

#ifndef next_script_object
#define next_script_object

#include "event.H"
#include "mover.H"

/************************************************************************
*	next_script object 						*
************************************************************************/
class C_NEXT_SCRIPT : public C_EVENT {

  private:
    int alive;

  protected:

  public:

    C_NEXT_SCRIPT();
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
//    virtual int lazy();

};


#endif
