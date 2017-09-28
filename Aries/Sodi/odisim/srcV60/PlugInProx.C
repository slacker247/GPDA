// PlugInProx.C

#include "Speedes.H"

#include "gridman.H"
#include "sensman.H"
#include "dspman.H"
#include "eyeman.H"
#include "missman.H"
#include "gbiman.H"
#include "ranman.H"
#include "oagman.H"
#include "eomanman.H"
#include "comman.H"
#include "centerman.H"
#include "graphman.H"
#include "nostateman.H"

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

int GRID;
int SENSOR;
int DSP;
int EYE;
int MISSILE;
int GBI;
int RANDOM_AIR;
int OAG;
int EOMAN;
int CENTER;
int COM;
int GRAPHICS;
int NOSTATE;
int NEXT_SCRIPT;
int EOMAN_SCRIPT;
int CHANGE_SCRIPT;
int UPDATE_GRID;
int ADD_M2G;
int DEL_MFG;
int ADD_S2G;
int DEL_SFG;
int ADD_S2M;
int DEL_SFM;
int ADD_E2E;
int ADD_S2E;
int DEL_SFE;
int ADD_M2S;
int DEL_MFS;
int FIXED_COVERAGE;
int MOVING_COVERAGE;
int TEST_PROX;
int SCAN;
int SCAN_EYE;
int SCAN_GBI;
int TRACK_OUT;
int TRACK_PROC;
int TRACK_IN;
int SHOW_MEM;
int SEND_MESSAGE;
int RECEIVE_MESSAGE;
int FUSE_TRACKS;
int ADD_DET;
int REMOVE_TRACKS;
int EXT_LANL_BP;
int EXT_TRACKER;
int CCC_PLAN;
int SHOOT_GBI;
int KILL;
int EXT_GRAPHICS_GVT;
int RSD_SOCKET;
int EXT_GRAPHICS_SCRIPT;
int EXT_GRAPHICS_DEFINE;

DEFINE_OBJECT_MANAGER(GRID,       C_GRIDMAN);
DEFINE_OBJECT_MANAGER(SENSOR,     C_SENSMAN);
DEFINE_OBJECT_MANAGER(DSP,        C_DSPMAN);
DEFINE_OBJECT_MANAGER(EYE,        C_EYEMAN);
DEFINE_OBJECT_MANAGER(MISSILE,    C_MISSMAN);
DEFINE_OBJECT_MANAGER(GBI,        C_GBIMAN);
DEFINE_OBJECT_MANAGER(RANDOM_AIR, C_RANMAN);
DEFINE_OBJECT_MANAGER(OAG,        C_OAGMAN);
DEFINE_OBJECT_MANAGER(EOMAN,      C_EOMANMAN);
DEFINE_OBJECT_MANAGER(CENTER,     C_CENTERMAN);
DEFINE_OBJECT_MANAGER(COM,        C_COMMAN);
DEFINE_OBJECT_MANAGER(GRAPHICS,   C_GRAPHMAN);
DEFINE_OBJECT_MANAGER(NOSTATE,    C_NOSTATEMAN);

DEFINE_EVENT(NEXT_SCRIPT,      C_NEXT_SCRIPT,      NEXT_SCRIPT_MESS,      10);
DEFINE_EVENT(EOMAN_SCRIPT,     C_EOMAN_SCRIPT,     EOMAN_SCRIPT_MESS,     10);
DEFINE_EVENT(CHANGE_SCRIPT,    C_CHANGE_SCRIPT,    CHANGE_SCRIPT_MESS,    10);
DEFINE_EVENT(UPDATE_GRID,      C_UPDATE_GRID,      UPDATE_GRID_MESS,      10);
DEFINE_EVENT(ADD_M2G,          C_ADD_M2G,          ADD_M2G_MESS,          10);
DEFINE_EVENT(DEL_MFG,          C_DEL_MFG,          DEL_MFG_MESS,          10);
DEFINE_EVENT(ADD_S2G,          C_ADD_S2G,          ADD_S2G_MESS,          10);
DEFINE_EVENT(DEL_SFG,          C_DEL_SFG,          DEL_SFG_MESS,          10);
DEFINE_EVENT(ADD_S2M,          C_ADD_S2M,          ADD_S2M_MESS,          10);
DEFINE_EVENT(DEL_SFM,          C_DEL_SFM,          DEL_SFM_MESS,          10);
DEFINE_EVENT(ADD_E2E,          C_ADD_E2E,          ADD_E2E_MESS,          10);
DEFINE_EVENT(ADD_S2E,          C_ADD_S2E,          ADD_S2E_MESS,          10);
DEFINE_EVENT(DEL_SFE,          C_DEL_SFE,          DEL_SFE_MESS,          10);
DEFINE_EVENT(ADD_M2S,          C_ADD_M2S,          ADD_M2S_MESS,          10);
DEFINE_EVENT(DEL_MFS,          C_DEL_MFS,          DEL_MFS_MESS,          10);
DEFINE_EVENT(FIXED_COVERAGE,   C_FIXED_COVERAGE,   FIXED_COVERAGE_MESS,   10);
DEFINE_EVENT(MOVING_COVERAGE,  C_MOVING_COVERAGE,  MOVING_COVERAGE_MESS,  10);
DEFINE_EVENT(TEST_PROX,        C_TEST_PROX,        TEST_PROX_MESS,        10);
DEFINE_EVENT(SCAN,             C_SCAN,             SCAN_MESS,             10);
DEFINE_EVENT(SCAN_EYE,         C_SCAN_EYE,         SCAN_EYE_MESS,         10);
DEFINE_EVENT(SCAN_GBI,         C_SCAN_GBI,         SCAN_GBI_MESS,         10);
DEFINE_EVENT(TRACK_OUT,        C_TRACK_OUT,        TRACK_OUT_MESS,        10);
DEFINE_EVENT(TRACK_PROC,       C_TRACK_PROC,       TRACK_PROC_MESS,       10);
DEFINE_EVENT(TRACK_IN,         C_TRACK_IN,         TRACK_IN_MESS,         10);
DEFINE_EVENT(SHOW_MEM,         C_SHOW_MEM,         SHOW_MEM_MESS,         10);
DEFINE_EVENT(SEND_MESSAGE,     C_SEND_MESSAGE,     SEND_MESSAGE_MESS,     10);
DEFINE_EVENT(RECEIVE_MESSAGE,  C_RECEIVE_MESSAGE,  RECEIVE_MESSAGE_MESS,  10);
DEFINE_EVENT(FUSE_TRACKS,      C_FUSE_TRACKS,      FUSE_TRACKS_MESS,      10);
DEFINE_EVENT(ADD_DET,          C_ADD_DET,          ADD_DET_MESS,          10);
DEFINE_EVENT(REMOVE_TRACKS,    C_REMOVE_TRACKS,    REMOVE_TRACKS_MESS,    10);
DEFINE_EVENT(EXT_LANL_BP,      C_EXT_LANL_BP,      EXT_LANL_BP_MESS,      10);
DEFINE_EVENT(EXT_TRACKER,      C_EXT_TRACKER,      EXT_TRACKER_MESS,      10);
DEFINE_EVENT(CCC_PLAN,         C_CCC_PLAN,         CCC_PLAN_MESS,         10);
DEFINE_EVENT(SHOOT_GBI,        C_SHOOT_GBI,        SHOOT_GBI_MESS,        10);
DEFINE_EVENT(KILL,             C_KILL,             KILL_MESS,             10);
DEFINE_EVENT(EXT_GRAPHICS_GVT, C_EXT_GRAPHICS_GVT, EXT_GRAPHICS_GVT_MESS, 10);
DEFINE_EVENT(RSD_SOCKET,       C_RSD_SOCKET,       RSD_SOCKET_MESS,       10);
DEFINE_EVENT(EXT_GRAPHICS_SCRIPT,
	     C_EXT_GRAPHICS_SCRIPT,
	     EXT_GRAPHICS_SCRIPT_MESS,
	     10);
DEFINE_EVENT(EXT_GRAPHICS_DEFINE,
	     C_EXT_GRAPHICS_DEFINE,
	     EXT_GRAPHICS_DEFINE_MESS,
	     10);

void PlugInProx()
{
   PLUG_IN_OBJECT_MANAGER(GRID);
   PLUG_IN_OBJECT_MANAGER(SENSOR);
   PLUG_IN_OBJECT_MANAGER(DSP);
   PLUG_IN_OBJECT_MANAGER(EYE);
   PLUG_IN_OBJECT_MANAGER(MISSILE);
   PLUG_IN_OBJECT_MANAGER(GBI);
   PLUG_IN_OBJECT_MANAGER(RANDOM_AIR);
   PLUG_IN_OBJECT_MANAGER(OAG);
   PLUG_IN_OBJECT_MANAGER(EOMAN);
   PLUG_IN_OBJECT_MANAGER(CENTER);
   PLUG_IN_OBJECT_MANAGER(COM);
   PLUG_IN_OBJECT_MANAGER(GRAPHICS);
   PLUG_IN_OBJECT_MANAGER(NOSTATE);

   PLUG_IN_EVENT(NEXT_SCRIPT);
   PLUG_IN_EVENT(EOMAN_SCRIPT);
   PLUG_IN_EVENT(CHANGE_SCRIPT);
   PLUG_IN_EVENT(UPDATE_GRID);
   PLUG_IN_EVENT(ADD_M2G);
   PLUG_IN_EVENT(DEL_MFG);
   PLUG_IN_EVENT(ADD_S2G);
   PLUG_IN_EVENT(DEL_SFG);
   PLUG_IN_EVENT(ADD_S2M);
   PLUG_IN_EVENT(DEL_SFM);
   PLUG_IN_EVENT(ADD_E2E);
   PLUG_IN_EVENT(ADD_S2E);
   PLUG_IN_EVENT(DEL_SFE);
   PLUG_IN_EVENT(ADD_M2S);
   PLUG_IN_EVENT(DEL_MFS);
   PLUG_IN_EVENT(FIXED_COVERAGE);
   PLUG_IN_EVENT(MOVING_COVERAGE);
   PLUG_IN_EVENT(TEST_PROX);
   PLUG_IN_EVENT(SCAN);
   PLUG_IN_EVENT(SCAN_EYE);
   PLUG_IN_EVENT(SCAN_GBI);
   PLUG_IN_EVENT(TRACK_OUT);
   PLUG_IN_EVENT(TRACK_PROC);
   PLUG_IN_EVENT(TRACK_IN);
   PLUG_IN_EVENT(SHOW_MEM);
   PLUG_IN_EVENT(SEND_MESSAGE);
   PLUG_IN_EVENT(RECEIVE_MESSAGE);
   PLUG_IN_EVENT(FUSE_TRACKS);
   PLUG_IN_EVENT(ADD_DET);
   PLUG_IN_EVENT(REMOVE_TRACKS);
   PLUG_IN_EVENT(EXT_LANL_BP);
   PLUG_IN_EVENT(EXT_TRACKER);
   PLUG_IN_EVENT(CCC_PLAN);
   PLUG_IN_EVENT(SHOOT_GBI);
   PLUG_IN_EVENT(KILL);
   PLUG_IN_EVENT(EXT_GRAPHICS_GVT);
   PLUG_IN_EVENT(RSD_SOCKET);
   PLUG_IN_EVENT(EXT_GRAPHICS_SCRIPT);
   PLUG_IN_EVENT(EXT_GRAPHICS_DEFINE);
}
