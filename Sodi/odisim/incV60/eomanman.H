// eomanman.H header file

#ifndef eomanman_object
#define eomanman_object

#include "eoman.H"
#include "parsman.H"

/************************************************************************
*	eomanman object - manager for equations of motion		*
************************************************************************/
class C_EOMANMAN : public C_PARSMAN {

  private:

    C_EOMAN *eoman;		// equation of motion manager
    int n_eomans;		// number of local eomans per node

  protected:

  public:

    C_EOMANMAN();
    virtual void init_events();
    int get_hash_lid(int uid) {return (uid % n_eomans);}

};


#endif
