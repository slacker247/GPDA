// comobj.H header file

#ifndef comobj_object
#define comobj_object

#include "simobj.H"
#include "eom.H"
#include "comid.H"

/************************************************************************
*	comobj object 							*
************************************************************************/
class C_COMOBJ : public C_SIMOBJ {

  private:

    C_EOM *eom;			// my current equation of motion
    C_COMID *comid;		// communications descriptor
    double time;		// time of comobj

  protected:

  public:

    C_COMOBJ();
    void init(C_COMID *c) {comid = c;}
    C_EOM *get_eom() {return eom;}
    C_COMID *get_comid() {return comid;}

};


#endif
