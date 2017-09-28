// add_e2e.H header file

#ifndef add_e2e_object
#define add_e2e_object

#include "event.H"

/************************************************************************
*	add_e2e object 							*
************************************************************************/
class C_ADD_E2E : public C_EVENT {

  private:

    char *buff;
    int size;
    int unique_id;
    double script_time;

    static int done;
    static int ADD_E2E;
    static int ADD_M2S;
    static int EOMAN_SCRIPT;

  protected:

  public:

    C_ADD_E2E();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
