// oagobj.C method file

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "def.h"
#include "oagobj.H"

#include "greatcirc.H"
#include "stop.H"
#include "freeobjs.H"
extern C_FREEOBJS freeobjs;

/************************************************************************
* C_OAGOBJ : construct a oagobj object					*
************************************************************************/
C_OAGOBJ::C_OAGOBJ() {

  luminosity = 0.0;

  icon = 69;
  max_vel = 0.6;
  cruise_vel = 0.6;

}

/************************************************************************
* init_oagobj : initialize the oagobj object				*
************************************************************************/
void C_OAGOBJ::init_oagobj( int sequence,
	double start_lat, double start_lon, double start_altitude,
	double end_lat, double end_lon, double end_altitude,
	double start_t, double end_t ) {

  C_EOM *e;
  C_STOP *stop;
  double x[3];
  double v[3];

  e = (C_EOM *)freeobjs.new_object(GREATCIRC);

  ((C_GREATCIRC *)e)->set_course_TT( start_lat, start_lon, start_altitude,
				       end_lat, end_lon, end_altitude,
				       start_t, end_t );

//...... fill stuff for generic eom

  e->set_object_type(OBJECT_TYPE);
  e->set_object_id(LOCAL_ID);
  e->set_object_node(NODE);
  e->set_id(GLOBAL_ID);
  e->set_icon(icon);

  e->set_cross_section(cross_section);
  e->set_luminosity(luminosity);

  e->set_sequence(sequence);
  script->push_bot(e);
  sequence++;

  e->get_pos_vel(end_t, x, v);
  stop = (C_STOP *)freeobjs.new_object(STOP);
  stop->init(end_t, x);
  stop->set_object_type(OBJECT_TYPE);
  stop->set_object_id(LOCAL_ID);
  stop->set_object_node(NODE);
  stop->set_id(GLOBAL_ID);
  stop->set_sequence(sequence);
  stop->set_luminosity(0.0);
  stop->set_cross_section(0.0);

  script->push_bot(stop);

}
