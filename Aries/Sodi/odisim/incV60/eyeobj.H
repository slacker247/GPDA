// eyeobj.H header file

#ifndef eyeobj_object
#define eyeobj_object

#include "mover.H"
#include "eom.H"

/************************************************************************
*	eyeobj object 							*
************************************************************************/
class C_EYEOBJ : public C_MOVER {

  private:

    C_EOM *eom;			// B.E. general equation of motion
    double period;		// period of circular orbit

  protected:

  public:

    C_EYEOBJ();

    void init_eom(double time, int n_rings, int ring, int n_per_ring,
			int n, double alt, double inc, int stag);

    void init_eom(double time, double X[3], double V[3]);

    void init_eom(	double time,
			double aa,
			double ee,
			double ii,
			double oo,
			double ww,
			double mm	);

    void init_icon(int ic) {icon = ic; eom->set_icon(ic);}
    virtual void get_pos_vel(double time, double X[3], double V[3]);

    virtual void get_next_name(char *s) {
      sprintf(s,"BRILLIANT_EYE_%3.3d_%3.3d",
	ring, (id_in_ring+1) % n_per_ring);
    }

    virtual void get_previous_name(char *s) {
      sprintf(s,"BRILLIANT_EYE_%3.3d_%3.3d",
	ring, (id_in_ring-1+n_per_ring) % n_per_ring);
    }


};


#endif
