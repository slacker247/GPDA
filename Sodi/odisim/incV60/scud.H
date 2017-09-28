// scud.H header file

#ifndef scud_object
#define scud_object

#include "missile.H"

/************************************************************************
*	scud object 							*
************************************************************************/
class C_SCUD : public C_MISSILE {

  private:

    static int done;
    static double Range_Angle[180];
    static double Time_Angle[180];

  protected:

  public:

    C_SCUD();

};


#endif
