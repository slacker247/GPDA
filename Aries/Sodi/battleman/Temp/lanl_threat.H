// lanl_threat.H header file

#ifndef lanl_threat_object
#define lanl_threat_object

#include <stdio.h>
#include <math.h>

#include "def.h"
#include "qitem.H"
#include "kepler.H"

/************************************************************************
*	lanl_threat object 						*
************************************************************************/
class C_LANL_THREAT : public C_ITEM {

  private:

    double X[3];
    double V[3];
    double XX[3];
    double VV[3];
    double *Ximp;
    double *Vimp;
    double lat;
    double lon;
    double timp;
    C_KEPLER kepler;

  protected:

  public:

    C_LANL_THREAT(){;}
    C_LANL_THREAT(float fX[3], float fV[3], float ftime, int i) {
	X[0] = fX[0]/1000.0;
	X[1] = fX[1]/1000.0;
	X[2] = fX[2]/1000.0;
	V[0] = fV[0]/1000.0;
	V[1] = fV[1]/1000.0;
	V[2] = fV[2]/1000.0;
	time_tag = ftime;
	id = i;
	kepler.init(X,V);
	kepler.update_impact();
	timp = kepler.get_timp() + time_tag;
	Ximp = kepler.get_imp_pos();
	Vimp = kepler.get_imp_vel();
	XX[0] = Ximp[0];
	XX[1] = Ximp[1];
	XX[2] = Ximp[2];
	VV[0] = Vimp[0];
	VV[1] = Vimp[1];
	VV[2] = Vimp[2];
	kepler.eci_to_ecr(timp,XX,VV);
	kepler.xyz_to_latlon(XX,lat,lon);
    }

    virtual void print() {
	printf("id = %d, time = %f\n", id, (float)time_tag);
	printf("X = %f %f %f, V = %f %f %f\n",
		X[0],X[1],X[2], V[0],V[1],V[2]);
	printf("alt = %f, speed = %f\n",
		sqrt(X[0]*X[0]+X[1]*X[1]+X[2]*X[2])-RE,
		sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2])	);
	printf("Impact time = %f, X = %f %f %f, V = %f %f %f\n",
		timp, Ximp[0],Ximp[1],Ximp[2], Vimp[0],Vimp[1],Vimp[2]);
	printf("Impact latitude = %f, longitude = %f\n",
		lat*DEGRAD,lon*DEGRAD);
        printf("\n");
    }

    double *get_X() {return X;}
    double get_X(int i) {return X[i];}

    double *get_V() {return V;}
    double get_V(int i) {return V[i];}

    double *get_Vi() {return VV;}
    double get_Vi(int i) {return VV[i];}

    double get_lat() { return lat*DEGRAD; }
    double get_lon() { return lon*DEGRAD; }

    double get_Ti() { return timp; }    

};


#endif
