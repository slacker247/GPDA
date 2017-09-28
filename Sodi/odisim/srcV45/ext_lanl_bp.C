// ext_lanl_bp.C method file

#include <stdio.h>

#include "ext_lanl_bp.H"
#include "ext_lanl_bp_mess.H"
#include "ccc_plan_mess.H"
#include "gbiman.H"
#include "centobj.H"
#include "sensobj.H"
#include "stereo_track.H"

#include "lanl_bp_input.H"
#include "lanl_bp_output.H"
#include "host_user.H"

#include "def.h"

int C_EXT_LANL_BP::done = 0;
int C_EXT_LANL_BP::EXT_LANL_BP = 0;
int C_EXT_LANL_BP::CCC_PLAN = 0;
int C_EXT_LANL_BP::CENTER = 0;
int C_EXT_LANL_BP::GBI = 0;

#define POLYORDER 3
#define STALE_TIME 300.0

/************************************************************************
* C_EXT_LANL_BP : construct a ext_lanl_bp object			*
************************************************************************/
C_EXT_LANL_BP::C_EXT_LANL_BP() {

  if (!done) {
    done = 1;
    EXT_LANL_BP = event_type("EXT_LANL_BP");
    CCC_PLAN = event_type("CCC_PLAN");
    GBI = object_type("GBI");
    CENTER = object_type("CENTER");
  }

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_EXT_LANL_BP::init(C_HEADER *header) {
  C_EM_MODULE *em_module;

  em_module = (C_EM_MODULE *)header;
  size = sizeof(C_EM_MODULE) + em_module->data_bytes;
  ext_lanl_bp_mess = (EXT_LANL_BP_MESS *) new char[size];
  memcpy((char *)ext_lanl_bp_mess, (char *)header, size);

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_EXT_LANL_BP::exchange() {
  C_CENTOBJ *centobj;

  if (toggle) {
    SIMOBJ->add_block(item);
    toggle = 0;
  }else{
    SIMOBJ->remove_block(item);
    toggle = 1;
  }

  centobj = (C_CENTOBJ *)SIMOBJ;
  centobj->exchange_blocking(new_blocking);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_EXT_LANL_BP::temp_process() {
  EXT_LANL_BP_MESS *out_mess;
  C_CENTOBJ *centobj;
  C_SENSOBJ *sensobj;
  int outsize;
  int len;
  int i;
  int index;
  double P[3];
  double V[3];
  C_XQUEUE *tracks;
  C_STEREO_TRACK *track;
  C_LANL_BP_INPUT *lanl_bp_input;

  centobj = (C_CENTOBJ *)SIMOBJ;
  sensobj = (C_SENSOBJ *)SIMOBJ;

  blocking = centobj->get_blocking();
  new_blocking = 0;

  toggle = 1;

  item = RB_NEW_C_ITEM();
  item->set_time_tag(TIME_TAG + ext_lanl_bp_mess->EM_done_time);
  item->set_id(ext_lanl_bp_mess->EM_socket);

//...... create the output buffer

  tracks = sensobj->get_tracks();
  track = (C_STEREO_TRACK *)tracks->get_top();
  len = tracks->get_length();
  outsize = 0;
  for (i=0; i<len; i++) {
    if (track->get_balistic()
	&& (track->get_poly_order() >= POLYORDER)
	&& (TIME_TAG - track->get_tupdate() < STALE_TIME)) {

      outsize += sizeof(C_LANL_BP_INPUT);

    }

    track = (C_STEREO_TRACK *)track->get_link();
  }

  out_mess = (EXT_LANL_BP_MESS *)
	RB_NEW_ARRAY_char(outsize+sizeof(EXT_LANL_BP_MESS));
  out_mess->init(outsize);

//...... fill the buffer for LANL

  buff = (char *)out_mess;
  buff += sizeof(EXT_LANL_BP_MESS);
  lanl_bp_input = (C_LANL_BP_INPUT *)buff;
  index = 0;

  track = (C_STEREO_TRACK *)tracks->get_top();
  for (i=0; i<len; i++) {

    if (track->get_balistic()
	&& (track->get_poly_order() >= POLYORDER)
	&& (TIME_TAG - track->get_tupdate() < STALE_TIME)) {

//      track->get_true_state(P,V);
      track->get_est_state(P,V);
//      centobj->ecr_to_eci(track->get_time_tag(),P,V);
      lanl_bp_input[index].set_id(track->get_id());
      lanl_bp_input[index].set_time(track->get_tupdate());
      lanl_bp_input[index].set_Xtrack(P);
      lanl_bp_input[index].set_Vtrack(V);
      lanl_bp_input[index].set_Xerror(track->get_Xerror());
      lanl_bp_input[index].set_Verror(track->get_Verror());
      index++;
    }

    track = (C_STEREO_TRACK *)track->get_link();
  }

//  out_mess = (EXT_LANL_BP_MESS *)RB_NEW(new EXT_LANL_BP_MESS());

//...... fill the header information

  out_mess->time_tag = TIME_TAG;
  out_mess->evtype = EXT_LANL_BP;
  out_mess->objid = -1;
  out_mess->obtype = OBJECT_TYPE;
  out_mess->ext = 0;
//  out_mess->data_bytes = sizeof(EXT_LANL_BP_MESS) - sizeof(C_EM_HEADER);
  out_mess->EM_interaction = EM_MODULE;
  out_mess->EM_socket = item->get_id();

  out_mess->EM_done_time = item->get_time_tag();
  buff = (char *)out_mess;

//...... now carry out the plan

  carry_out_plan();

}

/************************************************************************
* carry_out_plan : carry out the lanl battle plan			*
************************************************************************/
void C_EXT_LANL_BP::carry_out_plan() {
  C_LANL_BP_OUTPUT *lanl_bp_output;
  int n_assignments;
  char *bf;
  char *buff1;
  int bytes;
  int i,j;
  C_GBIMAN *gbiman;
  int n_sites;
  int site_index;
  int asset_id;
  int good_plan_index;
  CCC_PLAN_MESS *ccc_plan_mess;
  CCC_PLAN_DATA *ccc_plan_data;

  gbiman = (C_GBIMAN *)get_manager(GBI);
  n_sites = gbiman->get_n_sites();

//...... check the SPEEDES input message (or LANL output)

  bf = (char *)ext_lanl_bp_mess;
  bf += sizeof(EXT_LANL_BP_MESS);
  lanl_bp_output = (C_LANL_BP_OUTPUT *)bf;
  size = ext_lanl_bp_mess->data_bytes
	- (sizeof(EXT_LANL_BP_MESS) - sizeof(C_EM_HEADER));
  n_assignments = size / sizeof(C_LANL_BP_OUTPUT);

//...... print out the SPEEDES input message

  cerr << "\nEXT_LANL_BP: object " << id
       << ", time " << TIME_TAG << ", n assignments " << n_assignments
       << "\n";

  if (n_assignments == 0) return;

//...... schedule the event for carrying out the plan

  bytes = sizeof(CCC_PLAN_MESS) - sizeof(C_HEADER)
		+ n_assignments*sizeof(CCC_PLAN_DATA);

  ccc_plan_mess = (CCC_PLAN_MESS *)schedule(
	GONE*TIME_TAG,
	CCC_PLAN,
	CENTER,
	LOCAL_ID,
	NODE,
	bytes,
	buff1);

  good_plan_index = 0;

  buff1 = (char *)ccc_plan_mess;
  buff1 += sizeof(CCC_PLAN_MESS);

  ccc_plan_data =  (CCC_PLAN_DATA *)buff1;

//...... fill the plan message for the speedes event

  for (i=0; i<n_assignments; i++) {
    fprintf(stderr,"Assignment %d: Launch at %f from %d to %d\n",
	i,
	lanl_bp_output[i].get_launch_time(),
	lanl_bp_output[i].get_asset_id(),
	lanl_bp_output[i].get_threat_id());

    site_index = -1;
    for (j=0; j<n_sites; j++) {
      if (gbiman->get_gbi_id(j) == lanl_bp_output[i].get_asset_id()) {
        site_index = j;
	asset_id = gbiman->get_uid(site_index);
	fprintf(stderr,"gbi id %d, site_index %d, asset_id %d\n",
		lanl_bp_output[i].get_asset_id(), site_index, asset_id);
	break;
      }
    }
    if (site_index == -1) {
      fprintf(stderr,"Warning (EXT_LANL_BP), bad site id %d\n",
	lanl_bp_output[i].get_asset_id());
    }else{
      ccc_plan_data[good_plan_index].asset_type = -1;
      ccc_plan_data[good_plan_index].intercept_time = -1.0;
      ccc_plan_data[good_plan_index].asset_id = asset_id;
      ccc_plan_data[good_plan_index].threat_id =
				lanl_bp_output[i].get_threat_id();
      good_plan_index++;
    }

  }

//...... set the number of good plans in the message

  ccc_plan_mess->init(good_plan_index);
  fprintf(stderr,"\n");

}


/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_EXT_LANL_BP::perm_process() {
  EXT_LANL_BP_MESS *out_mess;

//...... send the output back to LANL

  out_mess = (EXT_LANL_BP_MESS *)buff;
  HOST_USER->send_message(out_mess);

  if (blocking) {
    SIMOBJ->unblock(-SIMOBJ->get_GLOBAL_ID());
  }

}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_EXT_LANL_BP::cleanup() {

  delete [] buff; //RVI 2/18/98
  delete ext_lanl_bp_mess;

}


