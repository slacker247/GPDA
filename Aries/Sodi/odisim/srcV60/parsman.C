// parsman.C method file

#include <stdio.h>
#include "parsman.H"
#include "comid.H"

int C_PARSMAN::done = 0;
C_QUEUE *C_PARSMAN::comqueue = 0;
C_PARSER *C_PARSMAN::parameters_parser = NULL;
C_PARSER *C_PARSMAN::missile_parser = NULL;
C_PARSER *C_PARSMAN::gbi_parser = NULL;
C_PARSER *C_PARSMAN::random_parser = NULL;
C_PARSER *C_PARSMAN::oag_parser = NULL;
C_PARSER *C_PARSMAN::dsp_parser = NULL;
C_PARSER *C_PARSMAN::eye_parser = NULL;
C_PARSER *C_PARSMAN::ground_sensor_parser = NULL;
C_PARSER *C_PARSMAN::grid_parser = NULL;
C_PARSER *C_PARSMAN::center_parser = NULL;
C_PARSER *C_PARSMAN::message_parser = NULL;
C_PARSER *C_PARSMAN::graphics_parser = NULL;

/************************************************************************
* C_PARSMAN : construct a parsman object				*
************************************************************************/
C_PARSMAN::C_PARSMAN() {

  if (!done) {
    done = 1;

    parameters_parser = new C_PARSER("parameters.par");
    random_parser = new C_PARSER("random.par");
    oag_parser = new C_PARSER("oag.par");
    dsp_parser = new C_PARSER("dsp.par");
    eye_parser = new C_PARSER("eye.par");
    ground_sensor_parser = new C_PARSER("ground_sensors.par");
    missile_parser = new C_PARSER("missile.par");
    gbi_parser = new C_PARSER("gbi.par");
    grid_parser = new C_PARSER("grid.par");
    center_parser = new C_PARSER("center.par");
    message_parser = new C_PARSER("message.par");
    graphics_parser = new C_PARSER("graphics.par");

    comqueue = new C_QUEUE();

  }

}

/************************************************************************
* set_communications : set up a communications id object		*
************************************************************************/
void C_PARSMAN::set_communications
	(C_BASETYPE *basename, int index, int offset, int start_uid) {

/*
  char *com_name;

  com_name = strdup(basename->get_name());
  set_communications(com_name, index, offset, start_uid);
  fprintf(stderr,"comm name %s\n",com_name);
*/

  set_communications(basename->get_name(), index, offset, start_uid);

}

/************************************************************************
* set_communications : set up a communications id object		*
************************************************************************/
void C_PARSMAN::set_communications
	(char *name, int index, int offset, int start_uid) {
  C_COMID *comid;
  int nd;
  int i;

  nd = (NODE + N_NODES + index - offset) % N_NODES;
  i = index / N_NODES;

  comid = new C_COMID();

  comid->set_object_type(OBJECT_TYPE);
  comid->set_object_node(nd);
  comid->set_object_id(i);
  comid->set_unique_id(start_uid+index);
  comid->set_object_name(name);

  comqueue->push_bot(comid);

}


/************************************************************************
* check_communications : check that the communications is correct	*
************************************************************************/
void C_PARSMAN::check_communications(int uid, int lid) {
  C_COMID *comid;

  comid = (C_COMID *)comqueue->get_bot();

  if (comid->get_object_node() != NODE) {
    fprintf(stderr,"Error (PARSMAN) bad comid: node\n");
  }

  if (comid->get_object_id() != lid) {
    fprintf(stderr,"Error (PARSMAN) bad comid: id\n");
  }

  if (comid->get_unique_id() != uid) {
    fprintf(stderr,"Error (PARSMAN) bad comid: uid\n");
  }

}
