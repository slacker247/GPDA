// ext_lanl_bp.H header file

#ifndef ext_lanl_bp_object
#define ext_lanl_bp_object

#include "event.H"
#include "ext_lanl_bp_mess.H"

/************************************************************************
*	ext_lanl_bp object 						*
************************************************************************/
class C_EXT_LANL_BP : public C_EVENT {

  private:
    EXT_LANL_BP_MESS *ext_lanl_bp_mess;
    int blocking;
    int new_blocking;

    int toggle;
    char *buff;
    C_ITEM *item;
    int size;

  protected:

  public:

    C_EXT_LANL_BP();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();

    void carry_out_plan();

};


#endif
