// speedes_graphics.C method file

#include <stdio.h>

#include "speedes_graphics.H"

//...... equations of motion

#include "freeoms.H"
#include "eom.H"

//...... graphics input messages

#include "ext_mess.H"
#include "ext_graphics_script_output.H"
#include "ext_graphics_link_output.H"
#include "ext_graphics_kill_output.H"

//...... graphics events

#include "free_gr_events.H"
#include "gr_event.H"
#include "gr_add_eom.H"
#include "gr_link.H"
#include "gr_kill.H"

//#define NOISY

/************************************************************************
* Constructor for the SPEEDES graphics object				*
************************************************************************/
C_SPEEDES_GRAPHICS::C_SPEEDES_GRAPHICS() {
  C_GR_EVENT gr_event;

  graphics_parser = new C_PARSER("graphics.par");
  parameters = graphics_parser->get_basetype("parameters");

  name = parameters->get_string("name");
  cycle_time = (double)parameters->get_float("cycle_time");
  start_time = (double)parameters->get_float("start_time");
  first_time = (double)parameters->get_float("first_time");
  tend = (double)parameters->get_float("tend");
  cpu_time = parameters->get_int("cpu_time");
  test_rollback = parameters->get_logical("test_rollback");

  readall = parameters->get_logical("readall");
  input_file = parameters->get_logical("input_file");
  output_file = parameters->get_logical("output_file");
  input_file_name = parameters->get_string("input_file_name");
  output_file_name = parameters->get_string("output_file_name");

  speedes_state = new C_SPEEDES_STATE();
  gr_event.set_speedes_state(speedes_state);
  free_gr_events = new C_FREE_GR_EVENTS();
  freeoms = new C_FREEOMS();

  cycle = 0;
  rqueue = new C_RQUEUE();
  gr_event.set_rqueue(rqueue);
  qproc = new C_XQUEUE();

  current_event = (C_GR_EVENT *)free_gr_events->new_object(GR_EVENT);
  current_event->set_time_tag(-1.0e20);
  current_event->set_processed();
  qproc->push_bot(current_event);

  printf("name = %s (%d)\n",name,strlen(name));

//...... create the host user object

  if (input_file) {
    host_user = new C_HOST_USER(input_file_name);
  }else{
    host_user = new C_HOST_USER();
  }
  if (output_file && !input_file) {
    host_user->set_output_file(output_file_name);
  }

//...... initialize

  id = host_user->getid(name);
  out_mess = new EXT_GRAPHICS_GVT_MESS();
  out_mess->time_tag = first_time;
  out_mess->EM_done_time = cycle_time;
  out_mess->objid = id;

//...... send the first message

  in_mess = (EXT_GRAPHICS_GVT_MESS *)host_user->blocking_module(out_mess);
  if (in_mess == NULL) {
    gvt = 0.9e20;
    return;
  }

  first_time = in_mess->time_tag;
  end_time = in_mess->EM_done_time;
  out_mess->EM_done_time = cycle_time;
  out_mess->time_tag = end_time;
  gvt = first_time;

  fprintf(stderr,"SPEEDES GRAPHICS %d starting at time %f\n",
	cycle, first_time);

//...... send out the next message to get things going

  out_mess->objid = id;
  host_user->send_module(out_mess);

//...... if reading from a file, get all of the messages at the beginning

  if (input_file && readall) {
    while (messages());
  }

}

/************************************************************************
* messages : read messages from SPEEDES					*
************************************************************************/
int C_SPEEDES_GRAPHICS::messages() {
  int i;
  C_EM_HEADER *input_message;
  char *buff;
  C_EOM *eom;
  int data_size;
  C_GR_ADD_EOM *gr_add_eom;
  C_GR_LINK *gr_link;
  C_GR_KILL *gr_kill;
  int message_type;
  int nlinks;
  GLINK *glink;
  int rflag;
  EXT_GRAPHICS_KILL_OUTPUT *ext_graphics_kill_output;

  input_message = host_user->get_module();

  if (input_message != NULL) {

    if (input_message->time_tag < gvt) {
      cerr << "Error (SPEEDES GRAPHICS) late message "
	   << input_message->time_tag << " < gvt " << gvt << "\n";
    }

    message_type = input_message->EM_flag;
    rflag = 1;

//...... start switch statement on message type

    switch(message_type) {

//
//...... GRAPHICS "GVT" message
//

      case GRAPHICS_GVT:

        in_mess = (EXT_GRAPHICS_GVT_MESS *)input_message;
        cycle++;
        first_time = in_mess->time_tag;
        gvt = first_time;
        end_time = in_mess->EM_done_time;
        out_mess->EM_done_time = cycle_time;
        out_mess->time_tag = end_time;

#ifdef NOISY
        fprintf(stderr,"SPEEDES GRAPHICS %d starting at time %f\n",
		cycle, first_time);
#endif

        out_mess->objid = id;

        host_user->send_module(out_mess);
//        if (!readall) rflag = 0;

        break;

//
//...... GRAPHICS "SCRIPT" message
//

      case GRAPHICS_SCRIPT:

#ifdef NOISY
        fprintf (stderr,"Got a GRAPHICS_SCRIPT message...\n");
#endif

        buff = (char *)input_message;
        buff += sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT);
        data_size = input_message->data_bytes -
		(sizeof(EXT_GRAPHICS_SCRIPT_OUTPUT) - sizeof(C_EM_HEADER));

        while (data_size > 0) {
          eom = (C_EOM *)freeoms->generate(buff);
	  data_size -= freeoms->get_size(eom);
          gr_add_eom = (C_GR_ADD_EOM *)free_gr_events->new_object(GR_ADD_EOM);
	  eom->set_ECI(0);
	  gr_add_eom->init(eom);
	  gr_add_eom->set_time_tag(eom->get_start_time());
	  gr_add_eom->set_send_time(input_message->time_tag);
          rqueue->insert(gr_add_eom);
	  buff += freeoms->get_size(eom);
        }

	if (data_size != 0) {
          fprintf(stderr,"Error (speedes_graphics) buffer not lining up %d\n",
		data_size);
        }

        break;

//
//...... GRAPHICS "DEFINE" message
//

      case GRAPHICS_DEFINE:

#ifdef NOISY
        fprintf (stderr,"Got a GRAPHICS_DEFINE message...\n");
#endif

        gr_add_eom = (C_GR_ADD_EOM *)free_gr_events->new_object(GR_ADD_EOM);
        gr_add_eom->init((EXT_GRAPHICS_DEFINE_OUTPUT *)input_message);
        gr_add_eom->set_time_tag(input_message->time_tag);

        rqueue->insert(gr_add_eom);

        break;

//
//...... GRAPHICS "LINK" message
//

      case GRAPHICS_LINK:

#ifdef NOISY
        fprintf (stderr,"Got a GRAPHICS_LINK message...\n");
#endif

        if (input_message->time_tag < gvt) {
          cerr << "Error (SPEEDES GRAPHICS) late message "
	       << input_message->time_tag << " < gvt " << gvt << "\n";
        }

        buff = (char *)input_message;
        buff += sizeof(EXT_GRAPHICS_LINK_OUTPUT);
        data_size = input_message->data_bytes -
		(sizeof(EXT_GRAPHICS_LINK_OUTPUT) - sizeof(C_EM_HEADER));

	glink = (GLINK *)buff;
        nlinks = data_size / sizeof(GLINK);

	for (i=0; i<nlinks; i++) {

          gr_link = (C_GR_LINK *)free_gr_events->new_object(GR_LINK);
	  gr_link->init(	glink[i].sensor_unique_id,
				glink[i].track_unique_id,
				glink[i].color,
				glink[i].intensity,
				glink[i].link_flag		);

	  gr_link->set_time_tag(input_message->time_tag);
          rqueue->insert(gr_link);

	}

        break;

//
//...... GRAPHICS "KILL" message
//

      case GRAPHICS_KILL:

#ifdef NOISY
        fprintf (stderr,"Got a GRAPHICS_KILL message...\n");
#endif

        if (input_message->time_tag < gvt) {
          cerr << "Error (SPEEDES GRAPHICS) late message "
	       << input_message->time_tag << " < gvt " << gvt << "\n";
        }

	ext_graphics_kill_output = (EXT_GRAPHICS_KILL_OUTPUT *)input_message;

        gr_kill = (C_GR_KILL *)free_gr_events->new_object(GR_KILL);
	gr_kill->set_time_tag(input_message->time_tag);
	gr_kill->init(ext_graphics_kill_output->kill_object_id);

        rqueue->insert(gr_kill);

        break;

    }
//...... end of switch statement

    delete input_message;
    return rflag;

  }else{

    return 0;

  }

}

/************************************************************************
* process : process graphics events up to gvt				*
************************************************************************/
void C_SPEEDES_GRAPHICS::process() {

  process(gvt);

}

/************************************************************************
* process : process a graphics events to a given time			*
************************************************************************/
void C_SPEEDES_GRAPHICS::process(double t) {
  double time;
  C_GR_EVENT *next_event;
  double dummy_time;
  int counter;

//...... check that the time is reasonable

  if (t > gvt) {
/*
    fprintf(stderr,
	"Warning (speedes_graphics) time %f is greater than gvt %f\n",
	t, gvt);
*/
    time = gvt;
  }else{
    time = t;
  }

//...... check the impossible

  if ((time > gvt) || (t > gvt)) {
    printf("Error (sp_graphics) impossible error!\n");
  }

//...... put all of the events up to gvt into the qproc queue

  counter = 0;
  while (rqueue->get_time() <= gvt) {

    if ((time > gvt) || (t > gvt)) {
      printf("Error (sp_graphics) impossible error!\n");
    }

    dummy_time = rqueue->get_time();
    next_event = (C_GR_EVENT *)rqueue->pop();
    if (dummy_time != next_event->get_time_tag()) {
      printf("Error (sp_graphics) speedes queue time missmatch\n");
    }
    next_event->reset_processed();
    qproc->push_bot(next_event);
    counter++;
  }

  if (current_event->get_time_tag() <= time) {

//...... go forward in time

    if (current_event == qproc->get_bot()) return;

    next_event = (C_GR_EVENT *)current_event->get_link();
    while (next_event->get_time_tag() <= time) {
      current_event = next_event;
      current_event->process();
      if (test_rollback) {
        current_event->unprocess();
        current_event->process();
      }
      current_event->set_processed();
      if (current_event == qproc->get_bot()) break;
      next_event = (C_GR_EVENT *)current_event->get_link();
    }

  }else{

//...... go backwards in time

    if (current_event == qproc->get_top()) return;

    while (current_event->get_time_tag() > time) {
      current_event->check_processed(1);
      current_event->unprocess();
      if (test_rollback) {
        current_event->process();
        current_event->unprocess();
      }
      current_event->reset_processed();
      current_event = (C_GR_EVENT *)current_event->get_backlink();
      if (current_event == qproc->get_top()) break;
    }

  }

//  check_queue();

}

/************************************************************************
* check_queue : check if the event queue is in order			*
************************************************************************/
int C_SPEEDES_GRAPHICS::check_queue() {
  int i,len;
  C_GR_EVENT *next_event;
  double time;

  len = qproc->get_length();
  if (len == 0) return 1;

  next_event = (C_GR_EVENT *)qproc->get_top();
  time = next_event->get_time_tag();

  for (i=1; i<len; i++) {
    next_event = (C_GR_EVENT *)next_event->get_link();
    if (time > next_event->get_time_tag()) {
//      printf("Error, (check_queue) bad qproc time order\n");
      return 0;
    }
    time = next_event->get_time_tag();
  }

  return 1;

}

/************************************************************************
* simulating : check if still simulating				*
************************************************************************/
int C_SPEEDES_GRAPHICS::simulating() {

  if (gvt > tend) return 0;
  return 1;

}
