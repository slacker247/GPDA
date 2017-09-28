/*  Definitions of opcodes */

#define OP_INIT_SC     0       /*  Initialize the Simulated Commander  */
#define OP_ACK         1       /*  ACK from Battle Manager  */
#define OP_NACK        2       /*  NACK from Battle Manager  */

#define OP_DB_STATUS   100     /*  Pass current BM status of 6 factors  */
#define OP_DEFCON      101     /*  BM current Defense Condition status  */
#define OP_RP          102     /*  BM current Readyness Posture status  */
#define OP_DEA         103     /*  BM current Defense Engagement Authority status  */
#define OP_ROE         104     /*  BM current Rules Of Engagement status  */
#define OP_MSN_OBJ     105     /*  BM current Mission Objectives status  */
#define OP_BP          106     /*  BM current Battle Plan status  */
#define OP_WA          107     /*  BM current Weapons Available status  */
#define OP_TRK_ENGMT   108     /*  Track Engagement Summary  */

#define OP_MSL_TRK     130     /*  Hostile Missile Track data  */
#define OP_POT_EVENT   140     /*  Potential Event Alert/Update  */
#define OP_INTEL_INFO  150     /*  Intelligence Report data  */

#define OP_INIT_MO     301     /*  Initialize Mission Objective + 4  */
#define OP_INIT_BP     302     /*  Initialize Battle Plan + 4  */
#define OP_INIT_WA     303     /*  Initialize Weapons Available  */

#define OP_RTRN_DCN    501     /*  Decisions on the 6 factors  */
#define OP_RTRN_ACK    502     /*  Sim Cmdr ACK, no new decisions  */
#define OP_RTRN_ERR    503     /*  Some error found, treat as nack  */

#define OP_EXIT_SC     10      /*  Close the Simulated Commander  */



