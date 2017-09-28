// comman.H header file

#ifndef comman_object
#define comman_object

#include "comobj.H"
#include "parsman.H"

/************************************************************************
*	comman object - manager for space objects covering the earth	*
************************************************************************/
class C_COMMAN : public C_PARSMAN {

  private:

    int COM;			// communication object type
    C_COMOBJ *comobj;		// local com objects
    double com_delay;		// end-to-end com delay

  protected:

  public:

    C_COMMAN();
    virtual void init_events();
    C_COMID *get_comid(char *destination);

    void set_com_delay(double cd) {com_delay = cd;}
    double get_com_delay() {return com_delay;}

};


#endif
