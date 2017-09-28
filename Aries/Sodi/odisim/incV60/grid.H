// grid.H header file

#ifndef grid_object
#define grid_object

#include "simobj.H"
#include "xqueue.H"

/************************************************************************
*	grid object 							*
************************************************************************/
class C_GRID : public C_SIMOBJ {

  private:

    int glob_gid;		// global grid id

    C_XQUEUE movers;		// queue of movers;
    C_XQUEUE sensors;		// queue of sensors;

    double tmovers;		// last time for mover list update
    double tsensors;		// last time for sensor list update
    int mover_counter;		// mover event counter for lazy cancellation
    int sensor_counter;		// sensor event counter for lazy cancellation

  protected:

  public:

    C_GRID();

    void set_glob_gid(int g) {glob_gid = g;}
    int get_glob_gid() {return glob_gid;}

    C_XQUEUE *get_movers() {return &movers;}
    C_XQUEUE *get_sensors() {return &sensors;}

    void set_tmovers(double tu) {tmovers = tu;}
    double get_tmovers() {return tmovers;}
    void exchange_tmovers(double &tu) {EXCHANGE(tmovers,tu);}
    int get_mover_counter() {return mover_counter;}
    void update_mover_counter() {mover_counter++;}

    void set_tsensors(double ts) {tsensors = ts;}
    double get_tsensors() {return tsensors;}
    void exchange_tsensors(double &ts) {EXCHANGE(tsensors,ts);}
    int get_sensor_counter() {return sensor_counter;}
    void update_sensor_counter() {sensor_counter++;}

    void print();

};


#endif
