#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/xdr.h>

#include "demo_opcodes.h"
#include "demo_players.h"
#include "demo_header.h"
#include "demo_msg_body.h"
#include "demo_strings.h"

#define II_Max  12

void SC_intel_time (int day, float hours);
void SC_intel_text (char *str);
void SC_intel_src (char *str);
void SC_update_hist (char *str);
void SC_update_def (int defcon);
void SC_update_rp (int rp);
void SC_update_dea (char *dea);
void SC_update_gvt (float gvt);
void SC_update_hist (char *str);


void translator (char *messages, char *decisions, unsigned int *xdrsize);

void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
void decode_header (XDR *xdrs_de, struct header *de, int *nargs);

int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);

int      toprttr;            /* 1 to print, anything else no print. */

main ()
{

   extern int           toprttr;

   struct SEND_ALL      BPen_body;
   struct RTRN_ALL      BPde_body;
   struct header        BPen, BPde;
   int                  nargs_in, nargs_out, i;

   unsigned int		xdrsize;
   char   		*xdrbuf_en, *xdrbuf_de, buf[2000];
   XDR			xdrs;

   char                 *SIMTEST;  

      /* The following arrays contain the intelligence information (II)    */
      /* which is generated when a 150 message comes in.                   */
 
   int   II_Day[II_Max] = {-12, -11, -10, -8, -6, -3, 0, 0, 0, 0, 0, 0};

   float II_Hrs[II_Max] = {12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 5.6, 10.5,
                                                   12.5, 13.5, 14.95, 15.13};

   int   II_Nam_Num[II_Max] = {2, 1, 1, 1, 1, 1, 3, 2, 1, 1, 1, 1};

   char  II_Nam_Src[II_Max][Max_Src][3] = {{"OS", " S", "  "},
                                           {" S", "  ", "  "},
                                           {"OS", "  ", "  "},
                                           {"OS", "  ", "  "},
                                           {"OS", "  ", "  "},
                                           {" S", "  ", "  "},
                                           {"OS", " H", " S"},
                                           {"OS", " S", "  "},
                                           {" S", "  ", "  "},
                                           {" S", "  ", "  "},
                                           {" S", "  ", "  "},
                                           {" S", "  ", "  "} };

   int   II_Nam_Loc[II_Max][Max_Src] = {{1, 1, 0}, {2, 0, 0}, {2, 0, 0},
                                        {3, 0, 0}, {4, 0, 0}, {1, 0, 0},
                                        {5, 1, 2}, {5, 3, 0}, {4, 0, 0},
                                        {5, 0, 0}, {6, 0, 0}, {7, 0, 0} };

   float II_Val[II_Max][Max_Src] = {{ 15.0, 10.0,  0.0}, { 7.5,  0.0,  0.0},
                                    { 5.0,  0.0,  0.0}, {25.0,  0.0,  0.0},
                                    {30.0,  0.0,  0.0}, {20.0,  0.0,  0.0},
                                    {35.0, 40.0, 25.0}, {60.0, 60.0,  0.0},
                                    {15.0,  0.0,  0.0}, {30.0,  0.0,  0.0},
                                    {60.0,  0.0,  0.0}, {25.0,  0.0,  0.0} };

   float II_Per[II_Max][Max_Src] = {{2000.0,  500.0,    0.0},
                                    {6000.0,    0.0,    0.0},
                                    {  72.0,    0.0,    0.0},
                                    { 200.0,    0.0,    0.0},
                                    { 200.0,    0.0,    0.0},
                                    {  72.0,    0.0,    0.0}, 
                                    {  45.5,   30.0,    1.5},
                                    {  37.0,   50.5,   10.5},
                                    {1000.0,    0.0,    0.0},
                                    { 250.0,    0.0,    0.0},
                                    {   2.0,    0.0,    0.0},
                                    { 100.0,    0.0,    0.0} };

   char  II_Txt[II_Max][5][51] = {{"China test fires short-range ballistic missile    ",
                                   "over Taiwan.  US, Taiwan, Japan, and South Korea  ",
                                   "issue strongly worded protests.                   ",
                                   "                                                  ",
                                   "                                                  " },
                                  {"SIGINT indicate increased activity at North Korean",
                                   "underground ICBM launch facilities.               ",
                                   "                                                  ",
                                   "                                                  ",
                                   "                                                  " },
                                  {"US/Japan/South Korea launch large joint military  ",
                                   "exercises off coast of Japan that includes ships, ",
                                   "subs, aircraft, missile launches. This causes     ",
                                   "escalation of inflammatory public rhetoric in the ",
                                   "neighboring countries.                            " },
                                  {"North Korea recalls political representatives and ",
                                   "ambassadors from US, Japan, and South Korea.      ",
                                   "                                                  ",
                                   "                                                  ",
                                   "                                                  " },
                                  {"Russia announces 'solidarity' with North Korea and",
                                   "warns the US and allies to scale back military    ",
                                   "exercises. Russia recalls its ambassadorors from  ",
                                   "Us, Japan, and South Korea.                       ",
                                   "                                                  " },
                                  {"North Korea launches a multistage Taepo Dong-1    ",
                                   "'scientific' missile test flight that flies over  ",
                                   "Japan and lands in the Pacific.                   ",
                                   "                                                  ",
                                   "                                                  " },
                                  {"Terrorist bomb destroys US military facility in   ",
                                   "South Korea. HUMINT/SIGINT sources suggests North ",
                                   "Korea is responsible.                             ",
                                   "                                                  ",
                                   "                                                  " },
                                  {"South Korea launches a preemptive strike against  ",
                                   "North Korean TBM missile launch sites.            ",
                                   "                                                  ",
                                   "                                                  ",
                                   "                                                  " },
                                  {"SIGINT reports increased Russian BM/C3 message    ",
                                   "message traffic involving ICBM sites.             ",
                                   "                                                  ",
                                   "                                                  ",
                                   "                                                  " },
                                  {"Russian nuclear subs on normal patrol in artic    ",
                                   "move to preferred launch locations.               ",
                                   "                                                  ",
                                   "                                                  ",
                                   "                                                  " },
                                  {"An unannounced missile carrying a weather         ",
                                   "satellite is launched from a Scandinavian country ",
                                   "at limits of Russian sensor capability (SIGINT    ",
                                   "indicates that this is mis-interpreted as a US    ",
                                   "ICBM attack).                                     " },
                                  {"SIGINT reports of heavy Russian BM/C3 message     ",
                                   "traffic involving sub forces.                     ",
                                   "                                                  ",
                                   "                                                  ",
                                   "                                                  " } };

  
 

   SIMTEST = getenv ("SIMTEST");
   if (strcmp (SIMTEST, "1") == 0)
     toprttr = 1;
   else
     toprttr = 0;

   /*  Show opcode   0.  */
   /*  Build a message to the translator, set up the buffers. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.SrcID     = BtlP;
   BPen.DstID     = BMDC1_DR1;
   BPen.SCID      = Sim_Cmdr_ID;
   BPen.opcode    = OP_INIT_SC;
   BPen.SCactive  = Sim_Cmdr_Active;
   BPen.gvt       = 1234.5;
   BPen.reserved7 = 0;
   BPen.reserved8 = 0;
   BPen.reserved9 = 0;


  
   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);


   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Messages done, remove all traces of them.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
 
  /*  Show opcode 502.  */

   /*  Show opcode 301.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_MO;
   BPen.gvt       = 1234.6;

   BPen_body.init_mo_body_struct.DEFCON_Level                 = 5;
   BPen_body.init_mo_body_struct.RP                           = 2;
   BPen_body.init_mo_body_struct.Msn_Obj_Pms                  = 0.78;
   BPen_body.init_mo_body_struct.Msn_Obj_Mode                 = 2;
   BPen_body.init_mo_body_struct.Msn_Obj_Withhold             = 4;
   BPen_body.init_mo_body_struct.Msn_Obj_Add_Bstr             = 2;

   strcpy (BPen_body.init_mo_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.init_mo_body_struct.ROE,                     ROE_NA);
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Name,            MSN_OBJ_CUR);
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Strategy,        "ASSURED");
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Tactic,          "ASSURED");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

    
   translator (xdrbuf_en, xdrbuf_de, &xdrsize);


   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 301.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_MO;
   BPen.gvt       = 1234.7;

   BPen_body.init_mo_body_struct.DEFCON_Level                 = 5;
   BPen_body.init_mo_body_struct.RP                           = 2;
   BPen_body.init_mo_body_struct.Msn_Obj_Pms                  = 0.85;
   BPen_body.init_mo_body_struct.Msn_Obj_Mode                 = 3;
   BPen_body.init_mo_body_struct.Msn_Obj_Withhold             = 2;
   BPen_body.init_mo_body_struct.Msn_Obj_Add_Bstr             = 2;

   strcpy (BPen_body.init_mo_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.init_mo_body_struct.ROE,                     ROE_CONUS);
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Name,            MSN_OBJ_ALT);
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Strategy,        "ASSURED");
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Tactic,          "ASSURED");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);
   
   translator (xdrbuf_en, xdrbuf_de, &xdrsize);
 
   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 302.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_BP;
   BPen.gvt       = 1234.8;

   BPen_body.init_bp_body_struct.DEFCON_Level                 = 5;
   BPen_body.init_bp_body_struct.RP                           = 2;
   BPen_body.init_bp_body_struct.BP_Override_Salvo            = 1;
   BPen_body.init_bp_body_struct.BP_Launch_Mode               = 1;
   BPen_body.init_bp_body_struct.BP_RV_Likelihood_Threshold   = 0.75;
   BPen_body.init_bp_body_struct.BP_Pk_Cutoff                 = 0.2200;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Population   = 1;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Military     = 0;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Self_Defense = 2;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_NCA          = 0;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Industrial   = 2;

   strcpy (BPen_body.init_bp_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.init_bp_body_struct.ROE,                     ROE_NA);
   strcpy (BPen_body.init_bp_body_struct.BP_Name,                 BP_GEN_UMB);
   strcpy (BPen_body.init_bp_body_struct.BP_Mode,                 "ASSURED");
   strcpy (BPen_body.init_bp_body_struct.BP_Tgt_Val_Cut_Off,      "Medium");
   strcpy (BPen_body.init_bp_body_struct.BP_Accept_Kill_Criteria, "Kill");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 302.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_BP;
   BPen.gvt       = 1234.9;

   BPen_body.init_bp_body_struct.DEFCON_Level                 = 4;
   BPen_body.init_bp_body_struct.RP                           = 2;
   BPen_body.init_bp_body_struct.BP_Override_Salvo            = 0;
   BPen_body.init_bp_body_struct.BP_Launch_Mode               = 2;
   BPen_body.init_bp_body_struct.BP_RV_Likelihood_Threshold   = 0.60;
   BPen_body.init_bp_body_struct.BP_Pk_Cutoff                 = 0.0001;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Population   = 1;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Military     = 2;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Self_Defense = 2;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_NCA          = 1;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Industrial   = 3;

   strcpy (BPen_body.init_bp_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.init_bp_body_struct.ROE,                     ROE_CONUS);
   strcpy (BPen_body.init_bp_body_struct.BP_Name,                 BP_AR_DEF);
   strcpy (BPen_body.init_bp_body_struct.BP_Mode,                 "ASSURED");
   strcpy (BPen_body.init_bp_body_struct.BP_Tgt_Val_Cut_Off,      "Low");
   strcpy (BPen_body.init_bp_body_struct.BP_Accept_Kill_Criteria, "Kill");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 303.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_WA;
   BPen.gvt       = 1235.0;

   BPen_body.wa_body_struct.Num_GBI_Farms = 5;

   BPen_body.wa_body_struct.Farm_Val[ 0][0] = 1101;
   BPen_body.wa_body_struct.Farm_Val[ 0][1] = 6;
   BPen_body.wa_body_struct.Farm_Val[ 0][2] = 4;

   BPen_body.wa_body_struct.Farm_Val[ 1][0] = 3101;
   BPen_body.wa_body_struct.Farm_Val[ 1][1] = 2;
   BPen_body.wa_body_struct.Farm_Val[ 1][2] = 1;

   BPen_body.wa_body_struct.Farm_Val[ 2][0] = 3111;
   BPen_body.wa_body_struct.Farm_Val[ 2][1] = 12;
   BPen_body.wa_body_struct.Farm_Val[ 2][2] = 6;

   BPen_body.wa_body_struct.Farm_Val[ 3][0] = 3121;
   BPen_body.wa_body_struct.Farm_Val[ 3][1] = 50;
   BPen_body.wa_body_struct.Farm_Val[ 3][2] = 25;

   BPen_body.wa_body_struct.Farm_Val[ 4][0] = 3131;
   BPen_body.wa_body_struct.Farm_Val[ 4][1] = 80;
   BPen_body.wa_body_struct.Farm_Val[ 4][2] = 20;

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 100.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);
   
   BPen.opcode    = OP_DB_STATUS;
   BPen.gvt       = 1235.1;

   BPen_body.db_status_body_struct.DEFCON_Level                 = 5;
   BPen_body.db_status_body_struct.RP                           = 2;
   BPen_body.db_status_body_struct.Msn_Obj_Pms                  = 0.78;
   BPen_body.db_status_body_struct.Msn_Obj_Mode                 = 2;
   BPen_body.db_status_body_struct.Msn_Obj_Withhold             = 4;
   BPen_body.db_status_body_struct.Msn_Obj_Add_Bstr             = 2;
   BPen_body.db_status_body_struct.BP_Override_Salvo            = 1;
   BPen_body.db_status_body_struct.BP_Launch_Mode               = 1;
   BPen_body.db_status_body_struct.BP_RV_Likelihood_Threshold   = 0.75;
   BPen_body.db_status_body_struct.BP_Pk_Cutoff                 = 0.2200;
   BPen_body.db_status_body_struct.BP_Asset_Weight_Population   = 1;
   BPen_body.db_status_body_struct.BP_Asset_Weight_Military     = 0;
   BPen_body.db_status_body_struct.BP_Asset_Weight_Self_Defense = 2;
   BPen_body.db_status_body_struct.BP_Asset_Weight_NCA          = 0;
   BPen_body.db_status_body_struct.BP_Asset_Weight_Industrial   = 2;

   strcpy (BPen_body.db_status_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.db_status_body_struct.ROE,                     ROE_NA);
   strcpy (BPen_body.db_status_body_struct.Msn_Obj_Name,            MSN_OBJ_CUR);
   strcpy (BPen_body.db_status_body_struct.Msn_Obj_Strategy,        "ASSURED");
   strcpy (BPen_body.db_status_body_struct.Msn_Obj_Tactic,          "ASSURED");
   strcpy (BPen_body.db_status_body_struct.BP_Name,                 BP_GEN_UMB);
   strcpy (BPen_body.db_status_body_struct.BP_Mode,                 "ASSURED");
   strcpy (BPen_body.db_status_body_struct.BP_Tgt_Val_Cut_Off,      "Medium");
   strcpy (BPen_body.db_status_body_struct.BP_Accept_Kill_Criteria, "Kill");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 150.  */
   /*  Build a message to the translator, set up the buffer. */
  
   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INTEL_INFO;
   BPen.gvt       = 1235.2;

   BPen_body.intel_info_body_struct.II_Day = II_Day[0];
   BPen_body.intel_info_body_struct.II_Hrs = II_Hrs[0];
   BPen_body.intel_info_body_struct.II_Nam_Num = II_Nam_Num[0];
   
   for (i = 0; i < 5; i++)
    {
      strcpy (BPen_body.intel_info_body_struct.II_Txt[i], II_Txt[0][i]);
      printf("the %dth line of the message is:%s\n", i, BPen_body.intel_info_body_struct.II_Txt[i]);
    }
   printf("the number of Intel sources reporting is %d\n",  BPen_body.intel_info_body_struct.II_Nam_Num);
   for (i = 0; i < II_Nam_Num[0]; i++)
     {
       strcpy (BPen_body.intel_info_body_struct.II_Nam_Src[i], II_Nam_Src[0][i]);
       BPen_body.intel_info_body_struct.II_Nam_Loc[i] = II_Nam_Loc[0][i];
       BPen_body.intel_info_body_struct.II_Val[i] = II_Val[0][i];
       BPen_body.intel_info_body_struct.II_Per[i] = II_Per[0][i];

       printf("Intel source is %s\n", &II_Nam_Src[0][i]);
       printf("Intel source number is %d\n", II_Nam_Loc[0][i]);
       printf("Intel threat value is %f\n", II_Val[0][i]);
       printf("Intel threat persistence is %f\n", II_Per[0][i]);
     }

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 150.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INTEL_INFO;
   BPen.gvt       = 1235.3;

   BPen_body.intel_info_body_struct.II_Day = II_Day[1];
   BPen_body.intel_info_body_struct.II_Hrs = II_Hrs[1];
   BPen_body.intel_info_body_struct.II_Nam_Num = II_Nam_Num[1];
   
   for (i = 0; i < 5; i++)
     strcpy (BPen_body.intel_info_body_struct.II_Txt[i], II_Txt[1][i]);
   
   for (i = 0; i < II_Nam_Num[1]; i++)
     {
       strcpy (BPen_body.intel_info_body_struct.II_Nam_Src[i], II_Nam_Src[1][i]);
       BPen_body.intel_info_body_struct.II_Nam_Loc[i] = II_Nam_Loc[1][i];
       BPen_body.intel_info_body_struct.II_Val[i] = II_Val[1][i];
       BPen_body.intel_info_body_struct.II_Per[i] = II_Per[1][i];
     }

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 150.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INTEL_INFO;
   BPen.gvt       = 1235.4;

   BPen_body.intel_info_body_struct.II_Day = II_Day[2];
   BPen_body.intel_info_body_struct.II_Hrs = II_Hrs[2];
   BPen_body.intel_info_body_struct.II_Nam_Num = II_Nam_Num[2];
   
   for (i =0; i < 5; i++)
     strcpy (BPen_body.intel_info_body_struct.II_Txt[i], II_Txt[2][i]);
   
   for (i = 0; i < II_Nam_Num[2]; i++)
     {
       strcpy (BPen_body.intel_info_body_struct.II_Nam_Src[i], II_Nam_Src[2][i]);
       BPen_body.intel_info_body_struct.II_Nam_Loc[i] = II_Nam_Loc[2][i];
       BPen_body.intel_info_body_struct.II_Val[i] = II_Val[2][i];
       BPen_body.intel_info_body_struct.II_Per[i] = II_Per[2][i];
     }


   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

    /*  Show opcode 150.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INTEL_INFO;
   BPen.gvt       = 1235.45;

   BPen_body.intel_info_body_struct.II_Day = II_Day[3];
   BPen_body.intel_info_body_struct.II_Hrs = II_Hrs[3];
   BPen_body.intel_info_body_struct.II_Nam_Num = II_Nam_Num[3];
   
   for (i = 0; i < 5; i++)
     strcpy (BPen_body.intel_info_body_struct.II_Txt[i], II_Txt[3][i]);
   
   for (i = 0; i < II_Nam_Num[3]; i++)
     {
       strcpy (BPen_body.intel_info_body_struct.II_Nam_Src[i], II_Nam_Src[3][i]);
       BPen_body.intel_info_body_struct.II_Nam_Loc[i] = II_Nam_Loc[3][i];
       BPen_body.intel_info_body_struct.II_Val[i] = II_Val[3][i];
       BPen_body.intel_info_body_struct.II_Per[i] = II_Per[3][i];
     }


   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 140.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_POT_EVENT;
   BPen.gvt       = 1235.5;

   BPen_body.pot_event_body_struct.Event_Time       = BPen.gvt - 739.0;
   BPen_body.pot_event_body_struct.Event_Lat        = 28.73;
   BPen_body.pot_event_body_struct.Event_Lon        = 111.07;

   strcpy (BPen_body.pot_event_body_struct.Event_Country,     "China");
   strcpy (BPen_body.pot_event_body_struct.Event_Site,        "Hough Ling");
   strcpy (BPen_body.pot_event_body_struct.Event_Status,      "Quick Alert");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 140.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_POT_EVENT;
   BPen.gvt       = 1235.9;

   BPen_body.pot_event_body_struct.Event_Time       = BPen.gvt - 739.0;
   BPen_body.pot_event_body_struct.Event_Lat        = 28.73;
   BPen_body.pot_event_body_struct.Event_Lon        = 111.07;

   strcpy (BPen_body.pot_event_body_struct.Event_Country,     "China");
   strcpy (BPen_body.pot_event_body_struct.Event_Site,        "Hough Ling");
   strcpy (BPen_body.pot_event_body_struct.Event_Status,      "Missile Event");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 130.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_MSL_TRK;
   BPen.gvt       = 1236.1;

   BPen_body.msl_trk_body_struct.Exp_Tgts          = 1;
   BPen_body.msl_trk_body_struct.Leth_Val          = 0.81;
   BPen_body.msl_trk_body_struct.Launch_Time       = BPen.gvt - 739.0;
   BPen_body.msl_trk_body_struct.Impact_Lat        = 28.73;
   BPen_body.msl_trk_body_struct.Impact_Lon        = 111.07;
   BPen_body.msl_trk_body_struct.Earl_Impact_Time  = BPen.gvt + 2197.0;
   BPen_body.msl_trk_body_struct.Num_Boosters      = 1;
   BPen_body.msl_trk_body_struct.Leth_Exp          = 1;

   strcpy (BPen_body.msl_trk_body_struct.Track_ID,           "11:22:33");
   strcpy (BPen_body.msl_trk_body_struct.Object_Type,        "Obj");
   strcpy (BPen_body.msl_trk_body_struct.Missile_Type,       "SS-88");
   strcpy (BPen_body.msl_trk_body_struct.Missile_Class,      "ICBM");
   strcpy (BPen_body.msl_trk_body_struct.Launch_Country,     "China");
   strcpy (BPen_body.msl_trk_body_struct.Launch_Site,        "Hough Ling");
   strcpy (BPen_body.msl_trk_body_struct.Pred_Impact_Region, "Unknown");
   strcpy (BPen_body.msl_trk_body_struct.Trk_Status,         "In Trk");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 104.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_ROE;
   BPen.gvt       = 1237.0;

   strcpy (BPen_body.roe_body_struct.ROE, ROE_CONUS);

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 130.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_MSL_TRK;
   BPen.gvt       = 1238.2;

   BPen_body.msl_trk_body_struct.Exp_Tgts          = 1;
   BPen_body.msl_trk_body_struct.Leth_Val          = 0.81;
   BPen_body.msl_trk_body_struct.Launch_Time       = BPen.gvt - 739.0;
   BPen_body.msl_trk_body_struct.Impact_Lat        = 29.47;
   BPen_body.msl_trk_body_struct.Impact_Lon        = 109.36;
   BPen_body.msl_trk_body_struct.Earl_Impact_Time  = BPen.gvt + 723.0;
   BPen_body.msl_trk_body_struct.Num_Boosters      = 1;
   BPen_body.msl_trk_body_struct.Leth_Exp          = 1;

   strcpy (BPen_body.msl_trk_body_struct.Track_ID,           "11:22:33");
   strcpy (BPen_body.msl_trk_body_struct.Object_Type,        "Obj");
   strcpy (BPen_body.msl_trk_body_struct.Missile_Type,       "SS-88");
   strcpy (BPen_body.msl_trk_body_struct.Missile_Class,      "ICBM");
   strcpy (BPen_body.msl_trk_body_struct.Launch_Country,     "China");
   strcpy (BPen_body.msl_trk_body_struct.Launch_Site,        "Hough Ling");
   strcpy (BPen_body.msl_trk_body_struct.Pred_Impact_Region, "Kansas City");
   strcpy (BPen_body.msl_trk_body_struct.Trk_Status,         "In Trk");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 108.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_TRK_ENGMT;
   BPen.gvt       = 1240.3;

   BPen_body.trk_engmt_body_struct.Cur_Eng_Num_Wea = 0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_TTI     = 0.0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_Pk      = 0.0;
   BPen_body.trk_engmt_body_struct.Eng_Opp_Remain  = 4;
   BPen_body.trk_engmt_body_struct.Task_Id         = 1101;

   strcpy (BPen_body.trk_engmt_body_struct.Eng_Status, "Pre Plan");
   strcpy (BPen_body.trk_engmt_body_struct.Track_Id,   "11:22:33");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 108.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_TRK_ENGMT;
   BPen.gvt       = 1250.4;

   BPen_body.trk_engmt_body_struct.Cur_Eng_Num_Wea = 0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_TTI     = BPen.gvt + 1800.0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_Pk      = 0.50;
   BPen_body.trk_engmt_body_struct.Eng_Opp_Remain  = 4;
   BPen_body.trk_engmt_body_struct.Task_Id         = 1101;

   strcpy (BPen_body.trk_engmt_body_struct.Eng_Status, "Planned");
   strcpy (BPen_body.trk_engmt_body_struct.Track_Id,   "11:22:33");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 108.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_TRK_ENGMT;
   BPen.gvt       = 1260.4;

   BPen_body.trk_engmt_body_struct.Cur_Eng_Num_Wea = 0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_TTI     = BPen.gvt + 1750.0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_Pk      = 0.28;
   BPen_body.trk_engmt_body_struct.Eng_Opp_Remain  = 3;
   BPen_body.trk_engmt_body_struct.Task_Id         = 1101;

   strcpy (BPen_body.trk_engmt_body_struct.Eng_Status, "Missed");
   strcpy (BPen_body.trk_engmt_body_struct.Track_Id,   "11:22:33");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 107.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_WA;
   BPen.gvt       = 1261.9;

   BPen_body.wa_body_struct.Num_GBI_Farms = 5;

   BPen_body.wa_body_struct.Farm_Val[ 0][0] = 1101;
   BPen_body.wa_body_struct.Farm_Val[ 0][1] = 5;
   BPen_body.wa_body_struct.Farm_Val[ 0][2] = 4;

   BPen_body.wa_body_struct.Farm_Val[ 1][0] = 3101;
   BPen_body.wa_body_struct.Farm_Val[ 1][1] = 2;
   BPen_body.wa_body_struct.Farm_Val[ 1][2] = 1;

   BPen_body.wa_body_struct.Farm_Val[ 2][0] = 3111;
   BPen_body.wa_body_struct.Farm_Val[ 2][1] = 12;
   BPen_body.wa_body_struct.Farm_Val[ 2][2] = 6;

   BPen_body.wa_body_struct.Farm_Val[ 3][0] = 3121;
   BPen_body.wa_body_struct.Farm_Val[ 3][1] = 50;
   BPen_body.wa_body_struct.Farm_Val[ 3][2] = 25;

   BPen_body.wa_body_struct.Farm_Val[ 4][0] = 3131;
   BPen_body.wa_body_struct.Farm_Val[ 4][1] = 80;
   BPen_body.wa_body_struct.Farm_Val[ 4][2] = 20;

   
   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 108.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_TRK_ENGMT;
   BPen.gvt       = 1262.3;

   BPen_body.trk_engmt_body_struct.Cur_Eng_Num_Wea = 0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_TTI     = BPen.gvt + 1740.0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_Pk      = 0.88;
   BPen_body.trk_engmt_body_struct.Eng_Opp_Remain  = 3;
   BPen_body.trk_engmt_body_struct.Task_Id         = 1101;

   strcpy (BPen_body.trk_engmt_body_struct.Eng_Status, "Planned");
   strcpy (BPen_body.trk_engmt_body_struct.Track_Id,   "11:22:33");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 107.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_WA;
   BPen.gvt       = 1262.8;

   BPen_body.wa_body_struct.Num_GBI_Farms = 5;

   BPen_body.wa_body_struct.Farm_Val[ 0][0] = 1101;
   BPen_body.wa_body_struct.Farm_Val[ 0][1] = 5;
   BPen_body.wa_body_struct.Farm_Val[ 0][2] = 2;

   BPen_body.wa_body_struct.Farm_Val[ 1][0] = 3101;
   BPen_body.wa_body_struct.Farm_Val[ 1][1] = 2;
   BPen_body.wa_body_struct.Farm_Val[ 1][2] = 1;

   BPen_body.wa_body_struct.Farm_Val[ 2][0] = 3111;
   BPen_body.wa_body_struct.Farm_Val[ 2][1] = 12;
   BPen_body.wa_body_struct.Farm_Val[ 2][2] = 6;

   BPen_body.wa_body_struct.Farm_Val[ 3][0] = 3121;
   BPen_body.wa_body_struct.Farm_Val[ 3][1] = 50;
   BPen_body.wa_body_struct.Farm_Val[ 3][2] = 25;

   BPen_body.wa_body_struct.Farm_Val[ 4][0] = 3131;
   BPen_body.wa_body_struct.Farm_Val[ 4][1] = 80;
   BPen_body.wa_body_struct.Farm_Val[ 4][2] = 20;

   
   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 108.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_TRK_ENGMT;
   BPen.gvt       = 1406.7;

   BPen_body.trk_engmt_body_struct.Cur_Eng_Num_Wea = 1;
   BPen_body.trk_engmt_body_struct.Cur_Eng_TTI     = 3002.3;
   BPen_body.trk_engmt_body_struct.Cur_Eng_Pk      = 0.88;
   BPen_body.trk_engmt_body_struct.Eng_Opp_Remain  = 2;
   BPen_body.trk_engmt_body_struct.Task_Id         = 1101;

   strcpy (BPen_body.trk_engmt_body_struct.Eng_Status, "Launched");
   strcpy (BPen_body.trk_engmt_body_struct.Track_Id,   "11:22:33");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 107.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_WA;
   BPen.gvt       = 1408.1;

   BPen_body.wa_body_struct.Num_GBI_Farms = 5;

   BPen_body.wa_body_struct.Farm_Val[ 0][0] = 1101;
   BPen_body.wa_body_struct.Farm_Val[ 0][1] = 4;
   BPen_body.wa_body_struct.Farm_Val[ 0][2] = 2;

   BPen_body.wa_body_struct.Farm_Val[ 1][0] = 3101;
   BPen_body.wa_body_struct.Farm_Val[ 1][1] = 2;
   BPen_body.wa_body_struct.Farm_Val[ 1][2] = 1;

   BPen_body.wa_body_struct.Farm_Val[ 2][0] = 3111;
   BPen_body.wa_body_struct.Farm_Val[ 2][1] = 12;
   BPen_body.wa_body_struct.Farm_Val[ 2][2] = 6;

   BPen_body.wa_body_struct.Farm_Val[ 3][0] = 3121;
   BPen_body.wa_body_struct.Farm_Val[ 3][1] = 50;
   BPen_body.wa_body_struct.Farm_Val[ 3][2] = 25;

   BPen_body.wa_body_struct.Farm_Val[ 4][0] = 3131;
   BPen_body.wa_body_struct.Farm_Val[ 4][1] = 80;
   BPen_body.wa_body_struct.Farm_Val[ 4][2] = 20;

   
   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode   10.  */
   /*  Build a message to the translator, set up the buffers. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.SrcID     = BtlP;
   BPen.DstID     = BMDC1_DR1;
   BPen.SCID      = Sim_Cmdr_ID;
   BPen.opcode    = OP_EXIT_SC;
   BPen.SCactive  = Sim_Cmdr_Active;
   BPen.gvt       = 1234.5;
   BPen.reserved7 = 0;
   BPen.reserved8 = 0;
   BPen.reserved9 = 0;


  
   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);


   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Messages done, remove all traces of them.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
 
}


void SC_intel_time (int day, float hours)
{
}
void SC_intel_text (char *str)
{
}
void SC_intel_src (char *str)
{
}

void SC_update_def (int defcon)
{
}

void SC_update_rp (int rp)
{
}

void SC_update_dea (char *dea)
{
}

void SC_update_gvt (float gvt)
{
}

void SC_update_hist (char *str)
{
  printf("%s\n", str);
}
















