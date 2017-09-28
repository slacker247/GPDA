// eoman.H header file

#ifndef eoman_object
#define eoman_object

#include "simobj.H"
#include "xhash.H"

/************************************************************************
*	eoman object - manager for space objects covering the earth	*
************************************************************************/
class C_EOMAN : public C_SIMOBJ {

  private:

    C_XHASH *eoms;	// list of equations of motion on this node

  protected:

  public:

    C_EOMAN();
    C_XHASH *get_eoms() {return eoms;}
    int check_eomtimes(double time);

};


#endif
