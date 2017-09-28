// ccc_plan.H header file

#ifndef ccc_plan_object
#define ccc_plan_object

#include "event.H"
#include "ccc_plan_mess.H"

/************************************************************************
*	ccc_plan object 						*
************************************************************************/
class C_CCC_PLAN : public C_EVENT {

  private:

    static int done;
    static int CCC_PLAN;
    static int SHOOT_GBI;
    static int GBI;
    static int MISSILE;

    CCC_PLAN_DATA *plan;
    int n_plans;
    int gbi_index;
    int *new_gbis_used;

  protected:

  public:

    C_CCC_PLAN();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
