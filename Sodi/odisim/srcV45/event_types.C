//...... event_types object

#include <stdio.h>
#include "event_types.H"

//...... event types enumerated

#ifdef IRIS4D
#define NPER 500
#else
#define NPER 500
#endif

enum {
  INITIAL_EVENT,
  QUERY,
  MONITOR,
  CANCEL,
  CREATE,
  DESTROY,
  NEXT_SCRIPT,
  EOMAN_SCRIPT,
  CHANGE_SCRIPT,
  UPDATE_GRID,
  ADD_M2G,
  DEL_MFG,
  ADD_S2G,
  DEL_SFG,
  ADD_S2M,
  DEL_SFM,
  ADD_E2E,
  ADD_S2E,
  DEL_SFE,
  ADD_M2S,
  DEL_MFS,
  FIXED_COVERAGE,
  MOVING_COVERAGE,
  TEST_PROX,
  SCAN,
  SCAN_EYE,
  SCAN_GBI,
  TRACK_OUT,
  TRACK_PROC,
  TRACK_IN,
  SHOW_MEM,
  SEND_MESSAGE,
  RECEIVE_MESSAGE,
  FUSE_TRACKS,
  ADD_DET,
  REMOVE_TRACKS,
  EXT_LANL_BP,
  EXT_TRACKER,
  CCC_PLAN,
  SHOOT_GBI,
  KILL,
  EXT_GRAPHICS_GVT,
  EXT_GRAPHICS_SCRIPT,
  EXT_GRAPHICS_DEFINE,
  RSD_SOCKET,
  N_EVENT_TYPES
};

int INITIAL_EVENT_TYPE = INITIAL_EVENT;

//...... message header files

#include "query_mess.H"
#include "monitor_mess.H"
#include "cancel_mess.H"
#include "create_mess.H"
#include "destroy_mess.H"
#include "next_script_mess.H"
#include "eoman_script_mess.H"
#include "change_script_mess.H"
#include "update_grid_mess.H"
#include "add_m2g_mess.H"
#include "del_mfg_mess.H"
#include "add_s2g_mess.H"
#include "del_sfg_mess.H"
#include "add_s2m_mess.H"
#include "del_sfm_mess.H"
#include "add_e2e_mess.H"
#include "add_s2e_mess.H"
#include "del_sfe_mess.H"
#include "add_m2s_mess.H"
#include "del_mfs_mess.H"
#include "fixed_coverage_mess.H"
#include "moving_coverage_mess.H"
#include "test_prox_mess.H"
#include "scan_mess.H"
#include "scan_eye_mess.H"
#include "scan_gbi_mess.H"
#include "track_out_mess.H"
#include "track_proc_mess.H"
#include "track_in_mess.H"
#include "show_mem_mess.H"
#include "send_message_mess.H"
#include "receive_message_mess.H"
#include "fuse_tracks_mess.H"
#include "add_det_mess.H"
#include "remove_tracks_mess.H"
#include "ext_lanl_bp_mess.H"
#include "ext_tracker_mess.H"
#include "ccc_plan_mess.H"
#include "shoot_gbi_mess.H"
#include "kill_mess.H"
#include "ext_graphics_gvt_mess.H"
#include "ext_graphics_script_mess.H"
#include "ext_graphics_define_mess.H"
#include "rsd_socket_mess.H"

//...... event header files

#include "InitialEvent.H"
#include "query.H"
#include "monitor.H"
#include "cancel.H"
#include "create.H"
#include "destroy.H"
#include "next_script.H"
#include "eoman_script.H"
#include "change_script.H"
#include "update_grid.H"
#include "add_m2g.H"
#include "del_mfg.H"
#include "add_s2g.H"
#include "del_sfg.H"
#include "add_s2m.H"
#include "del_sfm.H"
#include "add_e2e.H"
#include "add_s2e.H"
#include "del_sfe.H"
#include "add_m2s.H"
#include "del_mfs.H"
#include "fixed_coverage.H"
#include "moving_coverage.H"
#include "test_prox.H"
#include "scan.H"
#include "scan_eye.H"
#include "scan_gbi.H"
#include "track_out.H"
#include "track_proc.H"
#include "track_in.H"
#include "show_mem.H"
#include "send_message.H"
#include "receive_message.H"
#include "fuse_tracks.H"
#include "add_det.H"
#include "remove_tracks.H"
#include "ext_lanl_bp.H"
#include "ext_tracker.H"
#include "ccc_plan.H"
#include "shoot_gbi.H"
#include "kill.H"
#include "ext_graphics_gvt.H"
#include "ext_graphics_script.H"
#include "ext_graphics_define.H"
#include "rsd_socket.H"

//...... define creation functions for all of the event objects

void *new_C_INITIAL_EVENT(int n)        {return new C_INITIAL_EVENT[n];}
void *new_evt_QUERY(int n) 		{return new C_QUERY[n];}
void *new_evt_MONITOR(int n) 		{return new C_MONITOR[n];}
void *new_evt_CANCEL(int n) 		{return new C_CANCEL[n];}
void *new_evt_CREATE(int n) 		{return new C_CREATE[n];}
void *new_evt_DESTROY(int n) 		{return new C_DESTROY[n];}
void *new_evt_NEXT_SCRIPT(int n) 	{return new C_NEXT_SCRIPT[n];}
void *new_evt_EOMAN_SCRIPT(int n) 	{return new C_EOMAN_SCRIPT[n];}
void *new_evt_CHANGE_SCRIPT(int n) 	{return new C_CHANGE_SCRIPT[n];}
void *new_evt_UPDATE_GRID(int n) 	{return new C_UPDATE_GRID[n];}
void *new_evt_ADD_M2G(int n) 		{return new C_ADD_M2G[n];}
void *new_evt_DEL_MFG(int n) 		{return new C_DEL_MFG[n];}
void *new_evt_ADD_S2G(int n) 		{return new C_ADD_S2G[n];}
void *new_evt_DEL_SFG(int n) 		{return new C_DEL_SFG[n];}
void *new_evt_ADD_S2M(int n) 		{return new C_ADD_S2M[n];}
void *new_evt_DEL_SFM(int n) 		{return new C_DEL_SFM[n];}
void *new_evt_ADD_E2E(int n) 		{return new C_ADD_E2E[n];}
void *new_evt_ADD_S2E(int n) 		{return new C_ADD_S2E[n];}
void *new_evt_DEL_SFE(int n) 		{return new C_DEL_SFE[n];}
void *new_evt_ADD_M2S(int n) 		{return new C_ADD_M2S[n];}
void *new_evt_DEL_MFS(int n) 		{return new C_DEL_MFS[n];}
void *new_evt_FIXED_COVERAGE(int n) 	{return new C_FIXED_COVERAGE[n];}
void *new_evt_MOVING_COVERAGE(int n) 	{return new C_MOVING_COVERAGE[n];}
void *new_evt_TEST_PROX(int n) 		{return new C_TEST_PROX[n];}
void *new_evt_SCAN(int n) 		{return new C_SCAN[n];}
void *new_evt_SCAN_EYE(int n) 		{return new C_SCAN_EYE[n];}
void *new_evt_SCAN_GBI(int n) 		{return new C_SCAN_GBI[n];}
void *new_evt_TRACK_OUT(int n) 		{return new C_TRACK_OUT[n];}
void *new_evt_TRACK_PROC(int n) 	{return new C_TRACK_PROC[n];}
void *new_evt_TRACK_IN(int n) 		{return new C_TRACK_IN[n];}
void *new_evt_SHOW_MEM(int n) 		{return new C_SHOW_MEM[n];}
void *new_evt_SEND_MESSAGE(int n)	{return new C_SEND_MESSAGE[n];}
void *new_evt_RECEIVE_MESSAGE(int n)	{return new C_RECEIVE_MESSAGE[n];}
void *new_evt_FUSE_TRACKS(int n)	{return new C_FUSE_TRACKS[n];}
void *new_evt_ADD_DET(int n)		{return new C_ADD_DET[n];}
void *new_evt_REMOVE_TRACKS(int n)	{return new C_REMOVE_TRACKS[n];}
void *new_evt_EXT_LANL_BP(int n)	{return new C_EXT_LANL_BP[n];}
void *new_evt_EXT_TRACKER(int n)	{return new C_EXT_TRACKER[n];}
void *new_evt_CCC_PLAN(int n)		{return new C_CCC_PLAN[n];}
void *new_evt_SHOOT_GBI(int n)		{return new C_SHOOT_GBI[n];}
void *new_evt_KILL(int n)		{return new C_KILL[n];}
void *new_evt_EXT_GRAPHICS_GVT(int n)	{return new C_EXT_GRAPHICS_GVT[n];}
void *new_evt_EXT_GRAPHICS_SCRIPT(int n) {return new C_EXT_GRAPHICS_SCRIPT[n];}
void *new_evt_EXT_GRAPHICS_DEFINE(int n) {return new C_EXT_GRAPHICS_DEFINE[n];}
void *new_evt_RSD_SOCKET(int n)		{return new C_RSD_SOCKET[n];}

/************************************************************************
* C_EVENT_TYPES : define events and messages                            *
************************************************************************/
C_EVENT_TYPES::C_EVENT_TYPES() {

//...... define the number of event types

  set_ntypes(N_EVENT_TYPES);

//...... tell the event memory manager how to create events and messages

  define_event( "INITIAL_EVENT", INITIAL_EVENT,
                &new_C_INITIAL_EVENT,
                sizeof(C_INITIAL_EVENT),
                sizeof(C_HEADER),
                1 );

  define_event(	"QUERY", QUERY,
		&new_evt_QUERY,
		sizeof(C_QUERY),
		sizeof(QUERY_MESS),
		2	);

  define_event(	"MONITOR", MONITOR,
		&new_evt_MONITOR,
		sizeof(C_MONITOR),
		sizeof(MONITOR_MESS),
		2	);

  define_event(	"CANCEL", CANCEL,
		&new_evt_CANCEL,
		sizeof(C_CANCEL),
		sizeof(CANCEL_MESS),
		2	);

  define_event(	"CREATE", CREATE,
		&new_evt_CREATE,
		sizeof(C_CREATE),
		sizeof(CREATE_MESS),
		2	);

  define_event(	"DESTROY", DESTROY,
		&new_evt_DESTROY,
		sizeof(C_DESTROY),
		sizeof(DESTROY_MESS),
		2	);

  define_event(	"NEXT_SCRIPT", NEXT_SCRIPT,
		&new_evt_NEXT_SCRIPT,
		sizeof(C_NEXT_SCRIPT),
		sizeof(NEXT_SCRIPT_MESS),
		NPER	);

  define_event(	"EOMAN_SCRIPT", EOMAN_SCRIPT,
		&new_evt_EOMAN_SCRIPT,
		sizeof(C_EOMAN_SCRIPT),
		sizeof(EOMAN_SCRIPT_MESS),
		NPER	);

  define_event(	"CHANGE_SCRIPT", CHANGE_SCRIPT,
		&new_evt_CHANGE_SCRIPT,
		sizeof(C_CHANGE_SCRIPT),
		sizeof(CHANGE_SCRIPT_MESS),
		NPER	);

  define_event(	"UPDATE_GRID", UPDATE_GRID,
		&new_evt_UPDATE_GRID,
		sizeof(C_UPDATE_GRID),
		sizeof(UPDATE_GRID_MESS),
		NPER	);

  define_event(	"ADD_M2G", ADD_M2G,
		&new_evt_ADD_M2G,
		sizeof(C_ADD_M2G),
		sizeof(ADD_M2G_MESS),
		NPER	);

  define_event(	"DEL_MFG", DEL_MFG,
		&new_evt_DEL_MFG,
		sizeof(C_DEL_MFG),
		sizeof(DEL_MFG_MESS),
		NPER	);

  define_event(	"ADD_S2G", ADD_S2G,
		&new_evt_ADD_S2G,
		sizeof(C_ADD_S2G),
		sizeof(ADD_S2G_MESS),
		NPER	);

  define_event(	"DEL_SFG", DEL_SFG,
		&new_evt_DEL_SFG,
		sizeof(C_DEL_SFG),
		sizeof(DEL_SFG_MESS),
		NPER	);

  define_event(	"ADD_S2M", ADD_S2M,
		&new_evt_ADD_S2M,
		sizeof(C_ADD_S2M),
		sizeof(ADD_S2M_MESS),
		NPER	);

  define_event(	"DEL_SFM", DEL_SFM,
		&new_evt_DEL_SFM,
		sizeof(C_DEL_SFM),
		sizeof(DEL_SFM_MESS),
		NPER	);

  define_event(	"ADD_E2E", ADD_E2E,
		&new_evt_ADD_E2E,
		sizeof(C_ADD_E2E),
		sizeof(ADD_E2E_MESS),
		NPER	);

  define_event(	"ADD_S2E", ADD_S2E,
		&new_evt_ADD_S2E,
		sizeof(C_ADD_S2E),
		sizeof(ADD_S2E_MESS),
		NPER	);

  define_event(	"DEL_SFE", DEL_SFE,
		&new_evt_DEL_SFE,
		sizeof(C_DEL_SFE),
		sizeof(DEL_SFE_MESS),
		NPER	);

  define_event(	"ADD_M2S", ADD_M2S,
		&new_evt_ADD_M2S,
		sizeof(C_ADD_M2S),
		sizeof(ADD_M2S_MESS),
		NPER	);

  define_event(	"DEL_MFS", DEL_MFS,
		&new_evt_DEL_MFS,
		sizeof(C_DEL_MFS),
		sizeof(DEL_MFS_MESS),
		NPER	);

  define_event(	"FIXED_COVERAGE", FIXED_COVERAGE,
		&new_evt_FIXED_COVERAGE,
		sizeof(C_FIXED_COVERAGE),
		sizeof(FIXED_COVERAGE_MESS),
		NPER	);

  define_event(	"MOVING_COVERAGE", MOVING_COVERAGE,
		&new_evt_MOVING_COVERAGE,
		sizeof(C_MOVING_COVERAGE),
		sizeof(MOVING_COVERAGE_MESS),
		NPER	);

  define_event(	"TEST_PROX", TEST_PROX,
		&new_evt_TEST_PROX,
		sizeof(C_TEST_PROX),
		sizeof(TEST_PROX_MESS),
		NPER	);

  define_event(	"SCAN", SCAN,
		&new_evt_SCAN,
		sizeof(C_SCAN),
		sizeof(SCAN_MESS),
		NPER	);

  define_event(	"SCAN_EYE", SCAN_EYE,
		&new_evt_SCAN_EYE,
		sizeof(C_SCAN_EYE),
		sizeof(SCAN_EYE_MESS),
		NPER	);

  define_event(	"SCAN_GBI", SCAN_GBI,
		&new_evt_SCAN_GBI,
		sizeof(C_SCAN_GBI),
		sizeof(SCAN_GBI_MESS),
		NPER	);

  define_event(	"TRACK_OUT", TRACK_OUT,
		&new_evt_TRACK_OUT,
		sizeof(C_TRACK_OUT),
		sizeof(TRACK_OUT_MESS),
		NPER	);

  define_event(	"TRACK_PROC", TRACK_PROC,
		&new_evt_TRACK_PROC,
		sizeof(C_TRACK_PROC),
		sizeof(TRACK_PROC_MESS),
		NPER	);

  define_event(	"TRACK_IN", TRACK_IN,
		&new_evt_TRACK_IN,
		sizeof(C_TRACK_IN),
		sizeof(TRACK_IN_MESS),
		NPER	);

  define_event(	"SHOW_MEM", SHOW_MEM,
		&new_evt_SHOW_MEM,
		sizeof(C_SHOW_MEM),
		sizeof(SHOW_MEM_MESS),
		2	);

  define_event(	"SEND_MESSAGE", SEND_MESSAGE,
		&new_evt_SEND_MESSAGE,
		sizeof(C_SEND_MESSAGE),
		sizeof(SEND_MESSAGE_MESS),
		NPER	);

  define_event(	"RECEIVE_MESSAGE", RECEIVE_MESSAGE,
		&new_evt_RECEIVE_MESSAGE,
		sizeof(C_RECEIVE_MESSAGE),
		sizeof(RECEIVE_MESSAGE_MESS),
		NPER	);

  define_event(	"FUSE_TRACKS", FUSE_TRACKS,
		&new_evt_FUSE_TRACKS,
		sizeof(C_FUSE_TRACKS),
		sizeof(FUSE_TRACKS_MESS),
		NPER	);

  define_event(	"ADD_DET", ADD_DET,
		&new_evt_ADD_DET,
		sizeof(C_ADD_DET),
		sizeof(ADD_DET_MESS),
		NPER	);

  define_event(	"REMOVE_TRACKS", REMOVE_TRACKS,
		&new_evt_REMOVE_TRACKS,
		sizeof(C_REMOVE_TRACKS),
		sizeof(REMOVE_TRACKS_MESS),
		NPER	);

  define_event(	"EXT_LANL_BP", EXT_LANL_BP,
		&new_evt_EXT_LANL_BP,
		sizeof(C_EXT_LANL_BP),
		sizeof(EXT_LANL_BP_MESS),
		2	);

  define_event(	"EXT_TRACKER", EXT_TRACKER,
		&new_evt_EXT_TRACKER,
		sizeof(C_EXT_TRACKER),
		sizeof(EXT_TRACKER_MESS),
		2	);

  define_event(	"CCC_PLAN", CCC_PLAN,
		&new_evt_CCC_PLAN,
		sizeof(C_CCC_PLAN),
		sizeof(CCC_PLAN_MESS),
		2	);

  define_event(	"SHOOT_GBI", SHOOT_GBI,
		&new_evt_SHOOT_GBI,
		sizeof(C_SHOOT_GBI),
		sizeof(SHOOT_GBI_MESS),
		NPER	);

  define_event(	"KILL", KILL,
		&new_evt_KILL,
		sizeof(C_KILL),
		sizeof(KILL_MESS),
		NPER	);

  define_event(	"EXT_GRAPHICS_GVT", EXT_GRAPHICS_GVT,
		&new_evt_EXT_GRAPHICS_GVT,
		sizeof(C_EXT_GRAPHICS_GVT),
		sizeof(EXT_GRAPHICS_GVT_MESS),
		2	);

  define_event(	"EXT_GRAPHICS_SCRIPT", EXT_GRAPHICS_SCRIPT,
		&new_evt_EXT_GRAPHICS_SCRIPT,
		sizeof(C_EXT_GRAPHICS_SCRIPT),
		sizeof(EXT_GRAPHICS_SCRIPT_MESS),
		NPER	);

  define_event(	"EXT_GRAPHICS_DEFINE", EXT_GRAPHICS_DEFINE,
		&new_evt_EXT_GRAPHICS_DEFINE,
		sizeof(C_EXT_GRAPHICS_DEFINE),
		sizeof(EXT_GRAPHICS_DEFINE_MESS),
		NPER	);

  define_event(	"RSD_SOCKET", RSD_SOCKET,
		&new_evt_RSD_SOCKET,
		sizeof(C_RSD_SOCKET),
		sizeof(RSD_SOCKET_MESS),
		2	);

}

