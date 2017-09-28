// ccc_plan.C method file

#include <stdio.h>
#include "gbiman.H"
#include "centobj.H"
#include "sensobj.H"
#include "ccc_plan.H"
#include "shoot_gbi_mess.H"

#include "freetypes.H"
#include "oid.H"
#include "stereo_track.H"
#include "def.h"

int C_CCC_PLAN::done = 0;
int C_CCC_PLAN::CCC_PLAN = 0;
int C_CCC_PLAN::SHOOT_GBI = 0;
int C_CCC_PLAN::GBI = 0;
int C_CCC_PLAN::MISSILE = 0;

/************************************************************************
* C_CCC_PLAN : construct a ccc_plan object				*
************************************************************************/
C_CCC_PLAN::C_CCC_PLAN() {

  if (!done) {
    done = 1;
    GBI = object_type("GBI");
    MISSILE = object_type("MISSILE");
    CCC_PLAN = event_type("CCC_PLAN");
    SHOOT_GBI = event_type("SHOOT_GBI");
  }

}

/************************************************************************
* init : initialize the event with the message				*
************************************************************************/
void C_CCC_PLAN::init(C_HEADER *header) {
  CCC_PLAN_MESS *ccc_plan_mess_in;
    char *p;

  ccc_plan_mess_in = (CCC_PLAN_MESS *)header;
  n_plans = ccc_plan_mess_in->n_plans;
  plan = new CCC_PLAN_DATA[n_plans];

  p = (char *)header;
  p += sizeof(CCC_PLAN_MESS);
  memcpy((char *)plan, p, n_plans*sizeof(CCC_PLAN_DATA));  

}

/************************************************************************
* exchange : exchange state variables                                   *
************************************************************************/
void C_CCC_PLAN::exchange() {
  C_CENTOBJ *centobj;

  centobj = (C_CENTOBJ *)SIMOBJ;
  centobj->exchange_gbi_index(gbi_index);
  centobj->exchange_gbis_used(new_gbis_used);

}

/************************************************************************
* temp_process : temporarily process event				*
************************************************************************/
void C_CCC_PLAN::temp_process() {
  int i;
  int global_site_id;
  int site_lid;
  int site_node;
  int gbi_lid;
  int gbi_node;
  C_GBIMAN *gbiman;
  C_CENTOBJ *centobj;
  C_SENSOBJ *sensobj;
  int n_sites;
  int *gbis_used;
  C_XQUEUE *shots;
  C_XQUEUE *tracks;
  C_STEREO_TRACK *track;
  C_OID *oid;
  SHOOT_GBI_MESS *shoot_gbi_mess;
  C_SHOOT_INFO *shoot_info;

  centobj = (C_CENTOBJ *)SIMOBJ;
  sensobj = (C_SENSOBJ *)SIMOBJ;

  gbiman = (C_GBIMAN *)get_manager(GBI);

  shots = centobj->get_shots();
  tracks = sensobj->get_tracks();

//...... check if gbis_used has been created in the centobj

  n_sites = gbiman->get_n_sites();
  gbis_used = centobj->get_gbis_used();
  if (gbis_used == NULL) {
    gbis_used = new int[n_sites];
    for (i=0; i<n_sites; i++) gbis_used[i] = 0;
    centobj->set_gbis_used(gbis_used);
  }

//...... create the new list of used gbis

  new_gbis_used = RB_NEW_ARRAY_int(n_sites);
  for (i=0; i<n_sites; i++) new_gbis_used[i] = gbis_used[i];
  RB_DELETE_ARRAY_int(gbis_used);

//...... print out the plan

  gbi_index = centobj->get_gbi_index();

  cerr << "CCC_PLAN event being executed at time " << TIME_TAG << "\n";
  for (i=0; i<n_plans; i++) {

    global_site_id = gbiman->get_gid_site(plan[i].asset_id);

    fprintf(stderr,"CCC_PLAN: shoot GBI %d from site[%d] %s at threat %d\n",
	gbi_index,
	global_site_id,
	gbiman->get_site_name(global_site_id),
	plan[i].threat_id);

    gbi_index++;
  }

//...... go through the plans and send the necessary information to the gbi

  gbi_index = centobj->get_gbi_index();

  for (i=0; i<n_plans; i++) {

    global_site_id = gbiman->get_gid_site(plan[i].asset_id);
    if (!shots->find(plan[i].threat_id)) {

      if (new_gbis_used[global_site_id] >=
	gbiman->get_n_each_site(global_site_id)) {

        fprintf(stderr,"Bad plan, could not shoot %d from site %d %s\n",
		plan[i].threat_id,
		global_site_id,
		gbiman->get_site_name(global_site_id));

      }else{

	track = (C_STEREO_TRACK *)tracks->find(plan[i].threat_id);
        if (track != NULL) {
	  if (track->get_ballistic()) {

	    oid = (C_OID *)RB_FREE_NEW(OID);
	    oid->set_id(plan[i].threat_id);
	    oid->set_object_id(plan[i].asset_id);
	    oid->set_object_type(plan[i].asset_type);
	    *shots += oid;

            gbiman->get_nodid_site(plan[i].asset_id, site_node, site_lid);
            gbiman->get_nodid_gbi(gbi_index, gbi_node, gbi_lid);

	    shoot_gbi_mess = (SHOOT_GBI_MESS *)schedule(
                TIME_TAG + 30.0,
                SHOOT_GBI,
                GBI,
                gbi_lid,
                gbi_node);

	    shoot_info = shoot_gbi_mess->get_shoot_info();

	    shoot_info->lat_site = gbiman->get_lat_site(global_site_id);
	    shoot_info->lon_site = gbiman->get_lon_site(global_site_id);

	    shoot_info->threat_type = track->get_object_type();
	    shoot_info->threat_id = track->get_object_id();
	    shoot_info->threat_node = track->get_object_node();
	    shoot_info->threat_uid = plan[i].threat_id;

	    track->get_true_state(shoot_info->X_threat_truth,
				shoot_info->V_threat_truth);
	    track->get_est_state(shoot_info->X_threat_track,
				shoot_info->V_threat_track);
	    shoot_info->truth_threat_time = track->get_tdetection();
	    shoot_info->est_threat_time = track->get_tupdate();
	    shoot_info->kill_time = TIME_TAG;

            gbi_index++;

          }else{

	    fprintf(stderr,"Warning, track %d is not ballistic\n",
		plan[i].threat_id);

	  }

        }else{

	  fprintf(stderr,"Warning, track %d does not exist\n",
		plan[i].threat_id);

	}

      }

    }else{

      fprintf(stderr,"Warning, threat %d already assigned\n",
	plan[i].threat_id);

    }

  }

}

/************************************************************************
* perm_process : permanently process event				*
************************************************************************/
void C_CCC_PLAN::perm_process() {


}


/************************************************************************
* cleanup : cleanup this event event					*
************************************************************************/
void C_CCC_PLAN::cleanup() {

  delete [] plan; //RVI 2/18/98

}


