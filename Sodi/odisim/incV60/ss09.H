// ss09.H header file

#ifndef ss09_object
#define ss09_object

#include "missile.H"

/************************************************************************
*	ss09 object 							*
************************************************************************/
class C_SS09 : public C_MISSILE {

  private:

    static int done;
    static double Range_Angle[180];
    static double Time_Angle[180];

  protected:

  public:

    C_SS09();

};


#endif
