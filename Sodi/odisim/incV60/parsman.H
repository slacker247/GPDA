// parsman.H header file

#ifndef prsman_object
#define prsman_object

#include "objman.H"
#include "parser.H"
#include "queue.H"

/************************************************************************
*	parsman object - manager for space objects covering the earth	*
************************************************************************/
class C_PARSMAN : public C_OBJMAN {

  private:

  protected:

    static int done;				// done flag for first time

    static C_PARSER *parameters_parser;		// general parameters parser
    static C_PARSER *random_parser;		// random mover parser
    static C_PARSER *oag_parser;		// oag parser
    static C_PARSER *dsp_parser;		// dsp parser
    static C_PARSER *eye_parser;		// eye parser
    static C_PARSER *ground_sensor_parser;	// sensor parser
    static C_PARSER *missile_parser;		// missile parser
    static C_PARSER *gbi_parser;		// gbi missile parser
    static C_PARSER *grid_parser;		// grid parser
    static C_PARSER *center_parser;		// command center parser
    static C_PARSER *message_parser;		// message parser
    static C_PARSER *graphics_parser;		// graphics parser

    static C_QUEUE *comqueue;			// queue of all com objects

  public:

    C_PARSMAN();
    void set_communications(C_BASETYPE *basename, int index,
				int offset, int start_uid);
    void set_communications(char *name, int index,
				int offset, int start_uid);
    void check_communications(int uid, int lid);

    C_PARSER *get_parameters_parser() {return parameters_parser;}
    C_PARSER *get_random_parser() {return random_parser;}
    C_PARSER *get_oag_parser() {return oag_parser;}
    C_PARSER *get_dsp_parser_parser() {return dsp_parser;}
    C_PARSER *get_eye_parser_parser() {return eye_parser;}
    C_PARSER *get_ground_sensor_parser() {return ground_sensor_parser;}
    C_PARSER *get_missile_parser() {return missile_parser;}
    C_PARSER *get_gbi_parser() {return gbi_parser;}
    C_PARSER *get_grid_parser() {return grid_parser;}
    C_PARSER *get_center_parser() {return center_parser;}
    C_PARSER *get_message_parser() {return message_parser;}
    C_PARSER *get_graphics_parser() {return graphics_parser;}


};


#endif
