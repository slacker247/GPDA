// dspobj.H header file

#ifndef dspobj_object
#define dspobj_object

#include "sensobj.H"

/************************************************************************
*	dspobj object 							*
************************************************************************/
class C_DSPOBJ : public C_SENSOBJ {

  private:

  protected:

  public:

    C_DSPOBJ();

    virtual void get_next_name(char *s) {
      sprintf(s,"DSP%d",(id_in_ring+1) % n_per_ring);
    }

    virtual void get_previous_name(char *s) {
      sprintf(s,"DSP%d",(id_in_ring-1+n_per_ring) % n_per_ring);
    }

};


#endif
