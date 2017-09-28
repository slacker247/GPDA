// eoman_script.H header file

#ifndef eoman_script_object
#define eoman_script_object

#include "event.H"
#include "mover.H"

/************************************************************************
*	eoman_script object 						*
************************************************************************/
class C_EOMAN_SCRIPT : public C_EVENT {
 private:
    int uid;
  public:

    C_EOMAN_SCRIPT();
    virtual void init(C_HEADER *header);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
//    virtual int lazy();

};


#endif
