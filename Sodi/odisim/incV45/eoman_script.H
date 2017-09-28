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

    static int done;
    static int EOMAN;
    static int EOMAN_SCRIPT;
    static int ADD_M2S;
    static int DEL_MFS;

    int uid;

  protected:

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
