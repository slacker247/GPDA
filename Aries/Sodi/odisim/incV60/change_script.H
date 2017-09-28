// change_script.H header file

#ifndef change_script_object
#define change_script_object

#include "change_prox.H"

/************************************************************************
*	change_script object 						*
************************************************************************/
class C_CHANGE_SCRIPT : public C_CHANGE_PROX {

  private:

    int repeat_flag;
    int sd;
    double script_time;

    C_XQUEUE *script;

    static int done;

  protected:

  public:

    C_CHANGE_SCRIPT();
    virtual void init(C_HEADER *header);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
