// gbi.H header file

#ifndef gbi_object
#define gbi_object

#include "missile.H"

#define Nr 72
#define Na 20
#define Malt 2000.0
#define RESOLUTION 50.0

/************************************************************************
*	gbi object 							*
************************************************************************/
class C_GBI : public C_MISSILE {

  private:

    static int done;
    static double Range_Angle[180];
    static double Time_Angle[180];
    static C_QUEUE **Down_Range;

  protected:

  public:

    C_GBI();

};


#endif
