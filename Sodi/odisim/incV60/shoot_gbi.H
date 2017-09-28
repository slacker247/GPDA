// shoot_gbi.H header file

#ifndef shoot_gbi_object
#define shoot_gbi_object

#include "event.H"
#include "shoot_info.H"
#include "gbi.H"

/************************************************************************
*	shoot_gbi object 						*
************************************************************************/
class C_SHOOT_GBI : public C_EVENT {

  private:
    C_SHOOT_INFO shoot_info;

    C_GBI *gbi;

  protected:

  public:

    C_SHOOT_GBI();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
