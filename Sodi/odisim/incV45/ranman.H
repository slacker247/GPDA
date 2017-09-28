// ranman.H header file

#ifndef ranman_object
#define ranman_object

#include "parsman.H"
#include "ranobj.H"

/************************************************************************
*	ranman object - manager for space objects covering the earth	*
************************************************************************/
class C_RANMAN : public C_PARSMAN {

  private:

    int RANDOM_AIR;		// id for random aircraft mover types
    int n_rantypes;		// number of random mover types
    char **type_name;		// names for each random mover type
    int *n_each_type;		// number of each type
    int *n_loc_type;		// local number of each type
    float *n_each_start;	// start time of each type
    C_RANOBJ *ranobj;		// random mover simulation objects
    int interact;		// flag for turning random interactions on
    int first_id;		// id of the first object created
    int first_node;		// node of the first object created
    int test_prox;		// test prox flag (makes sense on 1 node)
    int change_script;		// flag for randomly changing script
    double change_script_rate;	// rate of randomly changing script

  protected:

  public:

    C_RANMAN();

    int get_local_id(int u_id);
    virtual void init_events();
    virtual void get_nodid(int oi, int &n, int &i);

    double get_change_script_rate() {return change_script_rate;}

};


#endif
