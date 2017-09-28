// missobj.H header file

#ifndef missobj_object
#define missobj_object

#include "mover.H"
#include "missile.H"

/************************************************************************
*	missobj object 							*
************************************************************************/
class C_MISSOBJ : public C_MOVER {

  private:

    C_MISSILE *missile;

    double lati;
    double loni;
    double latf;
    double lonf;
    double launch_time;

  protected:

  public:

    C_MISSOBJ();
    void init_missobj(char *nm, char *ty);
    void aim(double t, double lai, double loi, double laf, double lof,
             double apogee, double tof);
    void make_script();
    void print_trajectory(int nsteps);
    int test_trajectory();

};


#endif
