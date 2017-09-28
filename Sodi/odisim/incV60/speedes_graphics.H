// speedes_graphics.H header file

#ifndef sp_graphics_object
#define sp_graphics_object

#include "host_user.H"
#include "parser.H"
#include "xqueue.H"
#include "gr_event.H"
#include "rqueue.H"
#include "speedes_state.H"
#include "free_list.H"
#include "ext_graphics_gvt_mess.H"

/************************************************************************
*	speedes_graphics 						*
************************************************************************/
class C_SPEEDES_GRAPHICS {

  private:

    C_SPEEDES_STATE *speedes_state;	// state of the SPEEDES simulation
    C_RQUEUE *rqueue;			// recursive SPEEDES queue
    C_GR_EVENT *current_event;		// current event
    C_XQUEUE *qproc;			// list of processed events
    C_FREE_LIST *free_gr_events;	// free list for graphics events
    C_FREE_LIST *freeoms;		// free list for equations of motion
    C_PARSER *graphics_parser;		// parser for input file
    C_BASETYPE *parameters;		// parameters from the parser
    C_HOST_USER *host_user;		// host user connection to SPEEDES
    EXT_GRAPHICS_GVT_MESS *out_mess;	// output GVT message to SPEEDES
    EXT_GRAPHICS_GVT_MESS *in_mess;	// input GVT message from SPEEDES
    double gvt;				// time from SPEEDES
    int id;
    double cycle_time;
    char *name;
    double first_time;
    double start_time;
    double tend;
    double end_time;
    long cpu_start;
    long cpu_time;
    int cycle;
    int test_rollback;

    int readall;
    int input_file;
    int output_file;
    char *input_file_name;
    char *output_file_name;

  public:

    C_SPEEDES_GRAPHICS();
    int messages();
    void process();
    void process(double t);
    int simulating();
    C_SPEEDES_STATE *get_speedes_state() {return speedes_state;}
    double get_current_time() {return current_event->get_time_tag();}
    double get_gvt() {return gvt;}

    double get_start_time() {return start_time;}
    double get_end_time() {return tend;}

    int check_queue();

};

#endif

