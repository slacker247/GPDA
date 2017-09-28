// gbiobj.H header file

#ifndef gbiobj_object
#define gbiobj_object

#include "mover.H"
#include "gbi.H"
#include "intgbi.H"

/************************************************************************
*	gbiobj object 							*
************************************************************************/
class C_GBIOBJ : public C_MOVER {

  private:

    char *gbi_type;
    C_MISSILE *missile;		// gbi missile
    C_EOM *eom;			// threat eom

    double latitude;
    double longitude;
    double kill_time;
    double sensor_time;
    int site_flag;

  protected:

  public:

    C_GBIOBJ() {missile = NULL; cruise_vel = 8.0;
			kill_time = -1.0; site_flag = 0;}

    void init_site(char *ty, char *nm, double lat, double lon, int ic) {
		NAME = nm;
		gbi_type = ty;
		latitude = lat;
		longitude = lon;
		icon = ic;
		site_flag = 1;
	}

    void init_gbi() {
		if (!strcmp(gbi_type,"GBI")) missile = new C_GBI();
		if (!strcmp(gbi_type,"INTGBI")) missile = new C_INTGBI();
		if (missile == NULL) missile = new C_GBI();
	}


    int get_site_flag() {return site_flag;}
    void set_sensor_time(double st) {sensor_time = st;}
    double get_sensor_time() {return sensor_time;}

    void aim();
    void make_script(double ktime);

    void set_missile(C_MISSILE *m) {missile = m;}
    C_MISSILE *get_missile() {return missile;}

    void set_eom(C_EOM *e) {eom = e;}
    C_EOM *get_eom() {return eom;}

};


#endif
