// eom_holder.H header file

#ifndef eom_holder_object
#define eom_holder_object

#include "sq_item.H"
#include "xqueue.H"
#include "eom.H"

/************************************************************************
*	eom_holder object 						*
************************************************************************/
class C_EOM_HOLDER : public C_SQ_ITEM {

  private:

    C_XQUEUE sensors;
    C_XQUEUE script;

  protected:

  public:

    C_EOM_HOLDER() {;}
    void reset() {sensors.reset(); script.reset();}
    C_XQUEUE *get_sensors() {return &sensors;}
    C_XQUEUE *get_script() {return &script;}
    C_EOM *get_eom() {return (C_EOM *)script.get_top();}

};


#endif
