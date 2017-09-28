
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/xdr.h>

#include "demo_opcodes.h"
#include "demo_players.h"
#include "demo_header.h"
#include "demo_msg_body.h"
#include "demo_strings.h"

#include "clips.h"

#define II_Max  12
#define Max_Src  3

extern void SC_intel_time (int day, float hours);
extern void SC_intel_text (char *str);
extern void SC_intel_src (char *str);
extern void SC_update_hist (char *str);
extern void SC_update_def (int defcon);
extern void SC_update_rp (int rp);
extern void SC_update_dea (char *dea);
extern void SC_update_gvt (float gvt);

void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
void decode_header (XDR *xdrs_de, struct header *de, int *nargs);

void translator (char *messages, char *decisions, unsigned int *xdrsize)
{
   extern int           toprttr;

     struct header        SCde, SCen;
   int                  nargs_in, nargs_out, i, call_SC, test = 0, num;

   unsigned int		four = 4, eight = 8, twelve = 12, twenty = 20;
   static   int		Entered = FALSE, ii_ix = 0;
   char   		*xdrbuf, *dp, buf[2000], *temp, line[1200];
   char			SrcNoNa[5], DstNoNa[5], SrcDoNa[5], DstDoNa[5];
   int			SrcNoNu, DstNoNu,  SrcDoNu, DstDoNu, Dummy_Opcode, rtrnSC, rtrnBP, length;
   XDR			xdrs;
   void                *bp, *msn_obj, *SimCmdr, *BattlePlanner, *msg_header, *msg_body, *msg, *gbi_farm, *info, 
                       *theClass, *wa, *phase_fact, *fact_pointer;
   DATA_OBJECT          result, return_result, return_instance, return_integer;
   

   struct DB_STATUS_BODY	SC_db_status_body, *dp_db_status_body;
   struct DEFCON_BODY  		SC_defcon_body;
   struct RP_BODY      		SC_rp_body;
   struct DEA_BODY		SC_dea_body,     	 *dp_dea_body;
   struct ROE_BODY		SC_roe_body,     	 *dp_roe_body;
   struct MSN_OBJ_BODY		SC_msn_obj_body, 	 *dp_msn_obj_body;
   struct BP_BODY	       	SC_bp_body,     	 *dp_bp_body;
   struct WA_BODY	       	SC_wa_body,		 *do_wa_body;
   struct TRK_ENGMT_BODY	SC_trk_engmt_body,       *dp_trk_engmt_body;
   struct INIT_MO_BODY		SC_init_mo_body,	 *dp_init_mo_body;
   struct INIT_BP_BODY		SC_init_bp_body,	 *dp_init_bp_body;
   struct MSL_TRK_BODY  	SC_msl_trk_body,         *dp_msl_trk_body;
   struct POT_EVENT_BODY	SC_pot_event_body,       *dp_pot_event_body;
   struct INTEL_INFO_BODY	SC_intel_info_body,      *dp_intel_info_body;

   struct RTRN_DCN_BODY 	SC_rtrn_dcn_body,	 *dp_rtrn_dcn_body;
   struct RTRN_ERR_BODY 	SC_rtrn_err_body,	 *dp_rtrn_err_body;

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

  
   if (!Entered)
   {
     InitializeCLIPS();
     Clear ();
     Load("INITIAL.CLP");
     Watch("rules");
     Watch("instances"); 
     Watch ("focus");
     Watch ("facts");
     Watch ("globals"); 
     Reset(); 
     Entered = TRUE;
   }  	 
  
   /*  Decode in the translator now.  */

   xdrmem_create (&xdrs, messages, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &SCde, &nargs_out);

   rtrnSC =  Num_2_Char (DstNoNa,  &DstNoNu, DstDoNa, &DstDoNu, &SCde.DstID);
   rtrnBP = Num_2_Char (SrcNoNa, &SrcNoNu, SrcDoNa, &SrcDoNu, &SCde.SrcID);

    if (rtrnSC == 0)
       Dummy_Opcode = SCde.opcode;
    else
       Dummy_Opcode = BAD_DESTID;



   
    switch (Dummy_Opcode)
   {

      case OP_INIT_SC :
          
    /*  Call Sim Cmdr here.  */
        call_SC = 1;
   
/*Prepare message body for SC. Initialize SC only uses header.*/
	
      msg_body = MakeInstance("(msg_body of INTEGER_PROMPT (prompt 1))"); 

      /*        SCen.opcode = OP_RTRN_ACK;  */
       
/*  Break code with an illegal opcode to check encode case statement.  */
/*       SCen.opcode = 600;  */
/*  End of test.  */

         break;

      case OP_DB_STATUS  :

         if (!xdr_int (&xdrs, &SC_db_status_body.DEFCON_Level))
            printf ("XDR_INT decode, DB DEFCON Level, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB DEFCON Level recieved is %d .\n", SC_db_status_body.DEFCON_Level);}

         if (!xdr_int (&xdrs, &SC_db_status_body.RP))
            printf ("XDR_INT decode, DB RP, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB RP recieved is %d .\n", SC_db_status_body.RP);}

         dp_db_status_body = &SC_db_status_body;

         dp          = dp_db_status_body->DEA;
         if (!xdr_string (&xdrs, &dp, eight))
            printf ("XDR_STRING decode, DB DEA, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB DEA recieved is %s .\n", SC_db_status_body.DEA);}

         dp          = dp_db_status_body->ROE;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, DB ROE, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB ROE recieved is %s .\n", SC_db_status_body.ROE);}

         dp          = dp_db_status_body->Msn_Obj_Name;
         if (!xdr_string (&xdrs, &dp, twelve))
            printf ("XDR_STRING decode, DB Msn Obj Name, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB Msn Obj Name recieved is %s .\n", SC_db_status_body.Msn_Obj_Name);}

         dp          = dp_db_status_body->Msn_Obj_Strategy;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, DB Msn Obj Strategy, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB Msn Obj Strategy recieved is %s .\n", SC_db_status_body.Msn_Obj_Strategy);}

         dp          = dp_db_status_body->Msn_Obj_Tactic;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, DB Msn Obj Tactic, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB Msn Obj Tactic recieved is %s .\n", SC_db_status_body.Msn_Obj_Tactic);}

         if (!xdr_float (&xdrs, &SC_db_status_body.Msn_Obj_Pms))
            printf ("XDR_FLOAT decode, DB Msn Obj Pms, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB Msn Obj Pms recieved is %f .\n", SC_db_status_body.Msn_Obj_Pms);}

         if (!xdr_int (&xdrs, &SC_db_status_body.Msn_Obj_Mode))
            printf ("XDR_INT decode, DB Msn Obj Mode, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB Msn Obj Mode recieved is %d .\n", SC_db_status_body.Msn_Obj_Mode);}

         if (!xdr_int (&xdrs, &SC_db_status_body.Msn_Obj_Withhold))
            printf ("XDR_INT decode, DB Msn Obj Withhold, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB Msn Obj Withhold recieved is %d .\n", SC_db_status_body.Msn_Obj_Withhold);}

         if (!xdr_int (&xdrs, &SC_db_status_body.Msn_Obj_Add_Bstr))
            printf ("XDR_INT decode, DB Msn Obj Add_Bstr, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB Msn Obj Add Bstr recieved is %d .\n", SC_db_status_body.Msn_Obj_Add_Bstr);}

         dp          = dp_db_status_body->BP_Name;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, DB BP Name, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Name recieved is %s .\n", SC_db_status_body.BP_Name);}

         dp          = dp_db_status_body->BP_Mode;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, DB BP Mode, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Mode recieved is %s .\n", SC_db_status_body.BP_Mode);}

         dp          = dp_db_status_body->BP_Tgt_Val_Cut_Off;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, DB BP Tgt Val Cut Off, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Tgt Val Cut Off recieved is %s .\n", SC_db_status_body.BP_Tgt_Val_Cut_Off);}

         dp          = dp_db_status_body->BP_Accept_Kill_Criteria;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, DB BP Accept Kill Criteria, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Accept Kill Criteria recieved is %s .\n", SC_db_status_body.BP_Accept_Kill_Criteria);}

         if (!xdr_int (&xdrs, &SC_db_status_body.BP_Override_Salvo))
            printf ("XDR_INT decode, DB BP Override Salvo, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Override Salvo recieved is %d .\n", SC_db_status_body.BP_Override_Salvo);}

         if (!xdr_int (&xdrs, &SC_db_status_body.BP_Launch_Mode))
            printf ("XDR_INT decode, DB BP Launch Mode, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Launch Mode recieved is %d .\n", SC_db_status_body.BP_Launch_Mode);}

         if (!xdr_float (&xdrs, &SC_db_status_body.BP_RV_Likelihood_Threshold))
            printf ("XDR_FLOAT decode, DB BP RV Likelihood Threshold, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP RV Likelihood Threshold recieved is %f .\n",
                                                       SC_db_status_body.BP_RV_Likelihood_Threshold);}

         if (!xdr_float (&xdrs, &SC_db_status_body.BP_Pk_Cutoff))
            printf ("XDR_FLOAT decode, DB BP Pk Cutoff, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Pk Cutoff recieved is %f .\n", SC_db_status_body.BP_Pk_Cutoff);}

         if (!xdr_int (&xdrs, &SC_db_status_body.BP_Asset_Weight_Population))
            printf ("XDR_INT decode, DB BP Asset Weight - Population, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Asset Weight - Population recieved is %d .\n",
                                                         SC_db_status_body.BP_Asset_Weight_Population);}

         if (!xdr_int (&xdrs, &SC_db_status_body.BP_Asset_Weight_Military))
            printf ("XDR_INT decode, DB BP Asset Weight - Military, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Asset Weight - Military recieved is %d .\n",
                                                         SC_db_status_body.BP_Asset_Weight_Military);}

         if (!xdr_int (&xdrs, &SC_db_status_body.BP_Asset_Weight_Self_Defense))
            printf ("XDR_INT decode, DB BP Asset Weight - Self Defense, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Asset Weight - Self Defense recieved is %d .\n",
                                                         SC_db_status_body.BP_Asset_Weight_Self_Defense);}

         if (!xdr_int (&xdrs, &SC_db_status_body.BP_Asset_Weight_NCA))
            printf ("XDR_INT decode, DB BP Asset Weight - NCA, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Asset Weight - NCA recieved is %d .\n",
                                                         SC_db_status_body.BP_Asset_Weight_NCA);}

         if (!xdr_int (&xdrs, &SC_db_status_body.BP_Asset_Weight_Industrial))
            printf ("XDR_INT decode, DB BP Asset Weight - Industrial, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DB BP Asset Weight - Industrial recieved is %d .\n",
                                                         SC_db_status_body.BP_Asset_Weight_Industrial);}

         /*  Call Sim Cmdr here.  */
		call_SC = 1;
         
/*Prepare message body for SC. */
      
     
		/* bp = MakeInstance ("(bp of BATTLE_PLAN)");

      result.type = STRING;
      result.value = AddSymbol (SC_db_status_body.BP_Name);
      DirectPutSlot (bp, "bp_name", &result);

      result.type = STRING;
      result.value = AddSymbol(SC_db_status_body.BP_Mode);
      DirectPutSlot (bp, "planner_mode", &result);

      result.type = STRING;
      result.value = AddSymbol (SC_db_status_body.BP_Tgt_Val_Cut_Off);
      DirectPutSlot (bp, "target_value_cutoff",&result);

      result.type = STRING;
      result.value = AddSymbol(SC_db_status_body.BP_Accept_Kill_Criteria);
      DirectPutSlot (bp, "accept_kill_criteria", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.BP_Override_Salvo);
      DirectPutSlot (bp, "override_salvo", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.BP_Launch_Mode);
      DirectPutSlot (bp, "launch_mode", &result);

      result.type = FLOAT;
      result.value = AddDouble (SC_db_status_body.BP_RV_Likelihood_Threshold);
      DirectPutSlot (bp, "rv_threshold", &result);

      result.type = FLOAT;
      result.value = AddDouble (SC_db_status_body.BP_Pk_Cutoff);
      DirectPutSlot (bp, "pk_cutoff", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.BP_Asset_Weight_Population);
      DirectPutSlot (bp, "weight_population", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.BP_Asset_Weight_Military); 
      DirectPutSlot (bp, "weight_military", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.BP_Asset_Weight_Self_Defense);
      DirectPutSlot (bp, "weight_selfdefense", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.BP_Asset_Weight_NCA);
      DirectPutSlot (bp, "weight_ncauthority", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.BP_Asset_Weight_Industrial); 
      DirectPutSlot (bp, "weight_industrial", &result);
 
 msn_obj = MakeInstance ("(msn_obj of MSN_OBJ)");

      result.type = STRING;
      result.value = AddSymbol (SC_db_status_body.Msn_Obj_Name);
      DirectPutSlot (msn_obj, "msn_obj_name", &result);

      result.type = STRING;
      result.value = AddSymbol (SC_db_status_body.Msn_Obj_Strategy);
      DirectPutSlot (msn_obj, "strategy", &result);

      result.type = STRING;
      result.value = AddSymbol (SC_db_status_body.Msn_Obj_Tactic);
      DirectPutSlot (msn_obj, "tactic", &result);

      result.type = FLOAT;
      result.value = AddDouble (SC_db_status_body.Msn_Obj_Pms);
      DirectPutSlot (msn_obj, "pms", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.Msn_Obj_Mode); 
      DirectPutSlot (msn_obj, "mode", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.Msn_Obj_Withhold); 
      DirectPutSlot (msn_obj, "withhold", &result);

      result.type = INTEGER;
      result.value = AddLong (SC_db_status_body.Msn_Obj_Add_Bstr); 
      DirectPutSlot (msn_obj, "add_bstrs", &result);
		*/
 msg_body = MakeInstance ("(msg_body of STATE)");
	  
     result.type = INTEGER;
     result.value = AddLong (SC_db_status_body.DEFCON_Level);
     DirectPutSlot (msg_body, "DEFCON", &result);

     result.type = INTEGER;
     result.value = AddLong (SC_db_status_body.RP);
     DirectPutSlot (msg_body, "RP", &result);

     result.type = STRING;
     result.value = AddSymbol (SC_db_status_body.DEA);
     DirectPutSlot (msg_body, "DEA", &result);

     result.type = STRING;
     result.value = AddSymbol (SC_db_status_body.ROE);
     DirectPutSlot (msg_body, "ROE",&result);

     result.type = STRING;
     result.value = AddSymbol (SC_db_status_body.Msn_Obj_Name);
     DirectPutSlot (msg_body, "Msn_Obj_Name", &result);

     result.type = STRING;
     result.value = AddSymbol (SC_db_status_body.BP_Name);
     DirectPutSlot (msg_body, "BP_Name", &result);

     /*	SCen.opcode = OP_RTRN_ACK;  */
	break;

      case OP_DEFCON  :

         if (!xdr_int (&xdrs, &SC_defcon_body.DEFCON))
            printf ("XDR_INT decode, Current DEFCON, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DEFCON recieved is %d .\n", SC_defcon_body.DEFCON);}

         /*  Call Sim Cmdr here.  */
	 call_SC = 1;
	 
	 msg_body = MakeInstance("(msg_body of INTEGER_PROMPT)");
	 
	 result.type = INTEGER;
	 result.value = AddLong (SC_defcon_body.DEFCON);
	 DirectPutSlot (msg_body, "prompt", &result);


	 /*  SCen.opcode = OP_RTRN_DCN;  

         SC_rtrn_dcn_body.DEFCON  = 1;
         SC_rtrn_dcn_body.RP      = 2;
         strcpy (SC_rtrn_dcn_body.DEA, DEA_HOLD);
         strcpy (SC_rtrn_dcn_body.ROE, ROE_NA);
         strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_CUR);
         strcpy (SC_rtrn_dcn_body.BP, BP_GEN_UMB);       */

         break;

      case OP_RP      :

         if (!xdr_int (&xdrs, &SC_rp_body.RP))
            printf ("XDR_INT decode, Current RP, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("RP recieved is %d .\n", SC_rp_body.RP);}

         /*  Call Sim Cmdr here.  */
       
	call_SC = 1;

	 msg_body = MakeInstance ("(msg_body of INTEGER_PROMPT)");
	 
	 result.type = INTEGER;
	 result.value = AddLong (SC_rp_body.RP);
	 DirectPutSlot (msg_body, "prompt", &result);


	 /*   SCen.opcode = OP_RTRN_DCN;  

         SC_rtrn_dcn_body.DEFCON  = 1;
         SC_rtrn_dcn_body.RP      = 1;
         strcpy (SC_rtrn_dcn_body.DEA, DEA_HOLD);
         strcpy (SC_rtrn_dcn_body.ROE, ROE_NA);
         strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_CUR);
         strcpy (SC_rtrn_dcn_body.BP, BP_GEN_UMB);   */

         break;

      case OP_DEA     :

         dp_dea_body = &SC_dea_body;

         dp = dp_dea_body->DEA;
         
	 if (!xdr_string (&xdrs, &dp, eight))
            printf ("XDR_STRING decode, Current DEA, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("DEA recieved is %s .\n", SC_dea_body.DEA);}
	 
	 /*  Call Sim Cmdr here.  */
	 call_SC = 1;	

	 msg_body = MakeInstance ("(msg_body of STRING_PROMPT)");
	 
	 result.type = STRING;
	 result.value = AddSymbol (SC_dea_body.DEA);
	 DirectPutSlot (msg_body, "prompt", &result);

         
	 /*   SCen.opcode = OP_RTRN_DCN;

         SC_rtrn_dcn_body.DEFCON  = 1;
         SC_rtrn_dcn_body.RP      = 1;
         strcpy (SC_rtrn_dcn_body.DEA, DEA_FREE);
         strcpy (SC_rtrn_dcn_body.ROE, ROE_NA);
         strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_CUR);
         strcpy (SC_rtrn_dcn_body.BP, BP_GEN_UMB);        */

         break;

      case OP_ROE     :

         dp_roe_body = &SC_roe_body;

         dp          = dp_roe_body->ROE;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Current ROE, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("ROE recieved is %s .\n", SC_roe_body.ROE);}
         /*  Call Sim Cmdr here.  */
	 
	 call_SC = 1;
	
	 msg_body = MakeInstance ("(msg_body of STRING_PROMPT)");
	 
	 result.type = STRING;
	 result.value = AddSymbol (SC_roe_body.ROE);
	 DirectPutSlot (msg_body, "prompt", &result);

         
	 /*   SCen.opcode = OP_RTRN_DCN;

         SC_rtrn_dcn_body.DEFCON  = 1;
         SC_rtrn_dcn_body.RP      = 1;
         strcpy (SC_rtrn_dcn_body.DEA, DEA_FREE);
         strcpy (SC_rtrn_dcn_body.ROE, ROE_CONUS);
         strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_CUR);
         strcpy (SC_rtrn_dcn_body.BP, BP_AR_DEF);        */

         break;

      case OP_MSN_OBJ     :

         dp_msn_obj_body = &SC_msn_obj_body;

         dp          = dp_msn_obj_body->Msn_Obj;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Current Msn Obj, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Msn Obj recieved is %s .\n", SC_msn_obj_body.Msn_Obj);}

         /*  Call Sim Cmdr here.  */
		
	 call_SC = 1;
	
         msg_body = MakeInstance("(msg_body of STRING_PROMPT)");
	 
	 result.type = STRING;
	 result.value = AddSymbol (SC_msn_obj_body.Msn_Obj);
	 DirectPutSlot (msg_body, "prompt", &result);

         
	 /*   SCen.opcode = OP_RTRN_DCN;

         SC_rtrn_dcn_body.DEFCON  = 1;
         SC_rtrn_dcn_body.RP      = 1;
         strcpy (SC_rtrn_dcn_body.DEA, DEA_FREE);
         strcpy (SC_rtrn_dcn_body.ROE, ROE_CONUS);
         strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_ALT);
         strcpy (SC_rtrn_dcn_body.BP, BP_AR_DEF);         */

         break;

      case OP_BP     :

        dp_bp_body = &SC_bp_body;

         dp          = dp_bp_body->BP;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Current BP, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("BP recieved is %s .\n", SC_bp_body.BP);}

 /*  Call Sim Cmdr here.  */
	call_SC = 1;
	
        msg_body = MakeInstance ("(msg_body of STRING_PROMPT)");
        
	result.type = STRING;
	result.value = AddSymbol (SC_bp_body.BP);
	DirectPutSlot (msg_body, "prompt", &result);

         
	/*	SCen.opcode = OP_RTRN_DCN;

         SC_rtrn_dcn_body.DEFCON  = 1;
         SC_rtrn_dcn_body.RP      = 1;
         strcpy (SC_rtrn_dcn_body.DEA, DEA_FREE);
         strcpy (SC_rtrn_dcn_body.ROE, ROE_CONUS);
         strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_CUR);
         strcpy (SC_rtrn_dcn_body.BP, BP_AR_DEF);         */

         break;

      case OP_WA  :

         if (!xdr_int (&xdrs, &SC_wa_body.Num_GBI_Farms))
            printf ("XDR_INT decode, Update Number of GBI Farms, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Update Num GBI Farms recieved is %d .\n", SC_wa_body.Num_GBI_Farms);}
 
	 
	 theClass = FindDefclass ("GBI_FARM"); 
	 gbi_farm = GetNextInstanceInClass (theClass, NULL);
	
	 
         for (i = 0; i < SC_wa_body.Num_GBI_Farms; i++)
         {
            if (!xdr_int (&xdrs, &SC_wa_body.Farm_Val[i][0]))
               printf ("XDR_INT decode, Update row %d GBI number, returns error.\n", i);
            else
               {nargs_out += 1;
                if (toprttr == 1)
                   printf ("Recieved row %d GBI number %d", i, SC_wa_body.Farm_Val[i][0]);}

            if (!xdr_int (&xdrs, &SC_wa_body.Farm_Val[i][1]))
               printf ("XDR_INT decode, Update row %d GBIs on loc, returns error.\n", i);
            else
               {nargs_out += 1;
                if (toprttr == 1)
                   printf (", GBIs on loc is %d",  SC_wa_body.Farm_Val[i][1]);}

            if (!xdr_int (&xdrs, &SC_wa_body.Farm_Val[i][2]))
               printf ("XDR_INT decode, Update row %d GBIs on hold, returns error.\n", i);
            else
               {nargs_out += 1;
                if (toprttr == 1)
                   printf (", and GBIs on hold is %d .\n", SC_wa_body.Farm_Val[i][2]);}

	    result.type = INTEGER;
	    result.value = AddLong (SC_wa_body.Farm_Val[i][0]);
	    DirectPutSlot (gbi_farm, "id", &result);		

	    result.type = INTEGER;
	    result.value = AddLong (SC_wa_body.Farm_Val[i][1]);
	    DirectPutSlot (gbi_farm, "weapons", &result);

	    result.type = INTEGER;
	    result.value = AddLong (SC_wa_body.Farm_Val[i][2]);
	    DirectPutSlot (gbi_farm, "weapons_held", &result);

	    if (i== 0)
	    { msg_body = MakeInstance ("(msg_body of GBI_FARM)");
	    
	      result.type = INTEGER;
	      result.value = AddLong (SC_wa_body.Farm_Val[i][0]);
	      DirectPutSlot (msg_body, "id", &result);		

	      result.type = INTEGER;
	      result.value = AddLong (SC_wa_body.Farm_Val[i][1]);
	      DirectPutSlot (msg_body, "weapons", &result);

	      result.type = INTEGER;
	      result.value = AddLong (SC_wa_body.Farm_Val[i][2]);
	      DirectPutSlot (msg_body, "weapons_held", &result);
	    }  
	   
	    if (gbi_farm != NULL)
	      gbi_farm = GetNextInstanceInClass (theClass, gbi_farm);
	      
	    
	}
	
	
         /*  Call Sim Cmdr here.  */
		call_SC = 1;
		/*   SCen.opcode = OP_RTRN_ACK;  */ 

         break;

      case OP_TRK_ENGMT  :

         dp_trk_engmt_body = &SC_trk_engmt_body;

         dp          = dp_trk_engmt_body->Eng_Status;
         if (!xdr_string (&xdrs, &dp, twelve))
            printf ("XDR_STRING decode, Engagement Status, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Engagement Status recieved is %s .\n", SC_trk_engmt_body.Eng_Status);}

         if (!xdr_int (&xdrs, &SC_trk_engmt_body.Cur_Eng_Num_Wea))
            printf ("XDR_INT decode, Cur Eng Num Weapons, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Cur Eng Num Weapons recieved is %d .\n", SC_trk_engmt_body.Cur_Eng_Num_Wea);}

         if (!xdr_float (&xdrs, &SC_trk_engmt_body.Cur_Eng_TTI))
            printf ("XDR_FLOAT decode, Cur Eng Time To Intercept, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Cur Eng Time To Intercept recieved is %f .\n", SC_trk_engmt_body.Cur_Eng_TTI);}

         if (!xdr_float (&xdrs, &SC_trk_engmt_body.Cur_Eng_Pk))
            printf ("XDR_FLOAT decode, Cur Eng Pk, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Cur Eng Pk recieved is %f .\n", SC_trk_engmt_body.Cur_Eng_Pk);}

         if (!xdr_int (&xdrs, &SC_trk_engmt_body.Eng_Opp_Remain))
            printf ("XDR_INT decode, Eng Opportunities Remaining, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Eng Opportunities Remaining recieved is %d .\n", SC_trk_engmt_body.Eng_Opp_Remain);}

         dp          = dp_trk_engmt_body->Track_Id;
         if (!xdr_string (&xdrs, &dp, twelve))
            printf ("XDR_STRING decode, Track ID, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track ID recieved is %s .\n", SC_trk_engmt_body.Track_Id);}

         if (!xdr_int (&xdrs, &SC_trk_engmt_body.Task_Id))
            printf ("XDR_INT decode, Weapon Task ID, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Weapon Task ID recieved is %d .\n", SC_trk_engmt_body.Task_Id);}

         /*  Call Sim Cmdr here.  */
		call_SC = 1;

	msg_body = MakeInstance ("(msg_body of ENGAGEMENT)");

	result.type = STRING;
	result.value = AddSymbol (SC_trk_engmt_body.Eng_Status);
	DirectPutSlot (msg_body, "eng_status", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_trk_engmt_body.Cur_Eng_Num_Wea);
	DirectPutSlot (msg_body, "cur_eng_num_weapons", &result);

	result.type = FLOAT;
	result.value = AddDouble (SC_trk_engmt_body.Cur_Eng_TTI);
	DirectPutSlot (msg_body, "cur_eng_tti", &result);

	result.type = FLOAT;
	result.value = AddDouble (SC_trk_engmt_body.Cur_Eng_Pk);
	DirectPutSlot (msg_body, "cur_eng_pk", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_trk_engmt_body.Eng_Opp_Remain);
	DirectPutSlot (msg_body, "eng_opp_rem", &result);

	result.type = STRING;
	result.value = AddSymbol (SC_trk_engmt_body.Track_Id);
	DirectPutSlot (msg_body, "tgt_trk_id", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_trk_engmt_body.Task_Id);
	DirectPutSlot (msg_body, "gbi_task_id", &result);
	

	/*      if (strcmp (SC_trk_engmt_body.Eng_Status, "Missed") == 0)
            {
               SC_rtrn_dcn_body.DEFCON  = 1;
               SC_rtrn_dcn_body.RP      = 1;
               strcpy (SC_rtrn_dcn_body.DEA, DEA_FREE);
               strcpy (SC_rtrn_dcn_body.ROE, ROE_CONUS);
               strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_ALT);
               strcpy (SC_rtrn_dcn_body.BP, BP_AR_DEF);

               SCen.opcode = OP_RTRN_DCN;
            }

         else if (strcmp (SC_trk_engmt_body.Eng_Status, "Killed") == 0)
            {
               SC_rtrn_dcn_body.DEFCON  = 3;
               SC_rtrn_dcn_body.RP      = 2;
               strcpy (SC_rtrn_dcn_body.DEA, DEA_HOLD);
               strcpy (SC_rtrn_dcn_body.ROE, ROE_NA);
               strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_CUR);
               strcpy (SC_rtrn_dcn_body.BP, BP_GEN_UMB);

               SCen.opcode = OP_RTRN_DCN;
            }

         else
	 SCen.opcode = OP_RTRN_ACK;   */

         break;

      case OP_MSL_TRK  :

         dp_msl_trk_body = &SC_msl_trk_body;

         dp          = dp_msl_trk_body->Track_ID;
         if (!xdr_string (&xdrs, &dp, twelve))
            printf ("XDR_STRING decode, Missile Track ID, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Missile Track ID recieved is %s .\n", SC_msl_trk_body.Track_ID);}

         dp          = dp_msl_trk_body->Object_Type;
         if (!xdr_string (&xdrs, &dp, eight))
            printf ("XDR_STRING decode, Track Object Type, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Object Type recieved is %s .\n", SC_msl_trk_body.Object_Type);}

         dp          = dp_msl_trk_body->Missile_Type;
         if (!xdr_string (&xdrs, &dp, eight))
            printf ("XDR_STRING decode, Track Missile Type, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Missile Type recieved is %s .\n", SC_msl_trk_body.Missile_Type);}

         dp          = dp_msl_trk_body->Missile_Class;
         if (!xdr_string (&xdrs, &dp, eight))
            printf ("XDR_STRING decode, Track Missile Class, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Missile Class recieved is %s .\n", SC_msl_trk_body.Missile_Class);}

         if (!xdr_int (&xdrs, &SC_msl_trk_body.Exp_Tgts))
            printf ("XDR_INT decode, Track Expected Targets, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Expected Targets recieved is %d .\n", SC_msl_trk_body.Exp_Tgts);}

         if (!xdr_float (&xdrs, &SC_msl_trk_body.Leth_Val))
            printf ("XDR_FLOAT decode, Track Lethality Value, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Lethality Value recieved is %f .\n", SC_msl_trk_body.Leth_Val);}

         if (!xdr_float (&xdrs, &SC_msl_trk_body.Launch_Time))
            printf ("XDR_FLOAT decode, Track Launch Time, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Launch Time recieved is %f .\n", SC_msl_trk_body.Launch_Time);}

         dp          = dp_msl_trk_body->Launch_Country;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Track Launch Country, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Launch Country recieved is %s .\n", SC_msl_trk_body.Launch_Country);}

         dp          = dp_msl_trk_body->Launch_Site;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Track Launch Site, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Launch Site recieved is %s .\n", SC_msl_trk_body.Launch_Site);}

         if (!xdr_float (&xdrs, &SC_msl_trk_body.Impact_Lat))
            printf ("XDR_FLOAT decode, Track Impact Lat, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Impact Lat recieved is %f .\n", SC_msl_trk_body.Impact_Lat);}

         if (!xdr_float (&xdrs, &SC_msl_trk_body.Impact_Lon))
            printf ("XDR_FLOAT decode, Track Impact Long, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Impact Long recieved is %f .\n", SC_msl_trk_body.Impact_Lon);}

         if (!xdr_float (&xdrs, &SC_msl_trk_body.Earl_Impact_Time))
            printf ("XDR_FLOAT decode, Track Earliest Impact Time, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Earliest Impact Time recieved is %f .\n", SC_msl_trk_body.Earl_Impact_Time);}

         dp          = dp_msl_trk_body->Pred_Impact_Region;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Track Predicted Impact Region, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Predicted Impact Region recieved is %s .\n", SC_msl_trk_body.Pred_Impact_Region);}

         if (!xdr_int (&xdrs, &SC_msl_trk_body.Num_Boosters))
            printf ("XDR_INT decode, Track Number of Boosters, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Number of Boosters recieved is %d .\n", SC_msl_trk_body.Num_Boosters);}

         if (!xdr_int (&xdrs, &SC_msl_trk_body.Leth_Exp))
            printf ("XDR_INT decode, Track Lethals Expected, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Lethals Expected recieved is %d .\n", SC_msl_trk_body.Leth_Exp);}

         dp          = dp_msl_trk_body->Trk_Status;
         if (!xdr_string (&xdrs, &dp, eight))
            printf ("XDR_STRING decode, Track Track Status, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Track Track Status recieved is %s .\n", SC_msl_trk_body.Trk_Status);}

         /*  Call Sim Cmdr here.  */
	 call_SC = 1;

	 msg_body = MakeInstance ("(msg_body of MISSILE_TRACK)");

	 result.type = STRING;
	 result.value = AddSymbol (SC_msl_trk_body.Track_ID);
	 DirectPutSlot (msg_body, "id", &result);
	 
	 result.type = STRING;
	 result.value = AddSymbol (SC_msl_trk_body.Object_Type);
	 DirectPutSlot (msg_body, "obj_type", &result);

	 result.type = STRING;
	 result.value = AddSymbol (SC_msl_trk_body.Missile_Type);
	 DirectPutSlot (msg_body, "missile_type", &result);

	 result.type = STRING;
	 result.value = AddSymbol(SC_msl_trk_body.Missile_Class);
	 DirectPutSlot (msg_body, "missile_class", &result);

	 result.type = INTEGER;
	 result.value = AddLong (SC_msl_trk_body.Exp_Tgts);
	 DirectPutSlot (msg_body, "exp_targets", &result);

	 result.type = FLOAT;
	 result.value = AddDouble (SC_msl_trk_body.Leth_Val);
	 DirectPutSlot (msg_body, "leth_val", &result);

	 result.type = FLOAT;
	 result.value = AddDouble (SC_msl_trk_body.Launch_Time);
	 DirectPutSlot (msg_body, "launch_time", &result);

	 result.type = STRING;
	 result.value = AddSymbol(SC_msl_trk_body.Launch_Country);
	 DirectPutSlot (msg_body, "launch_country", &result);

	 result.type = STRING;
	 result.value = AddSymbol (SC_msl_trk_body.Launch_Site);
	 DirectPutSlot (msg_body, "launch_site", &result);

	 result.type = FLOAT;
	 result.value = AddDouble (SC_msl_trk_body.Impact_Lat);
	 DirectPutSlot (msg_body, "imp_lat", &result);

	 result.type = FLOAT;
	 result.value = AddDouble (SC_msl_trk_body.Impact_Lon);
	 DirectPutSlot (msg_body, "imp_long", &result);

	 result.type = FLOAT;
	 result.value = AddDouble (SC_msl_trk_body.Earl_Impact_Time);
	 DirectPutSlot (msg_body, "earliest_imp_time", &result);

	 result.type = STRING;
	 result.value = AddSymbol (SC_msl_trk_body.Pred_Impact_Region);
	 DirectPutSlot (msg_body, "predicted_imp_reg", &result);


	 result.type = INTEGER;
	 result.value = AddLong (SC_msl_trk_body.Num_Boosters);
	 DirectPutSlot (msg_body, "num_bstrs", &result);

	 result.type = INTEGER;
	 result.value = AddLong (SC_msl_trk_body.Leth_Exp);
	 DirectPutSlot (msg_body, "lethals_exp", &result);

	 result.type = STRING;
	 result.value = AddSymbol (SC_msl_trk_body.Trk_Status);
	 DirectPutSlot (msg_body, "track_status", &result);

	 /*  if (SCde.gvt > 1237.0)
            SCen.opcode = OP_RTRN_ACK;
         else
            {
               SCen.opcode = OP_RTRN_DCN;

               SC_rtrn_dcn_body.DEFCON  = 1;
               SC_rtrn_dcn_body.RP      = 1;
               strcpy (SC_rtrn_dcn_body.DEA, DEA_FREE);
               strcpy (SC_rtrn_dcn_body.ROE, ROE_NA);
               strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_CUR);
               strcpy (SC_rtrn_dcn_body.BP, BP_AR_DEF);
	       }                                                 */

         break;

      case OP_POT_EVENT  :

         if (!xdr_float (&xdrs, &SC_pot_event_body.Event_Time))
            printf ("XDR_FLOAT decode, Event Time, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Event Time recieved is %f .\n", SC_pot_event_body.Event_Time);}

         dp_pot_event_body = &SC_pot_event_body;

         dp          = dp_pot_event_body->Event_Country;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Event Country, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Event Country recieved is %s .\n", SC_pot_event_body.Event_Country);}

         dp          = dp_pot_event_body->Event_Site;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Event Site, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Event Site recieved is %s .\n", SC_pot_event_body.Event_Site);}

         if (!xdr_float (&xdrs, &SC_pot_event_body.Event_Lat))
            printf ("XDR_FLOAT decode, Event Lat, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Event Lat recieved is %f .\n", SC_pot_event_body.Event_Lat);}

         if (!xdr_float (&xdrs, &SC_pot_event_body.Event_Lon))
            printf ("XDR_FLOAT decode, Event Long, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Event Long recieved is %f .\n", SC_pot_event_body.Event_Lon);}

         dp          = dp_pot_event_body->Event_Status;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Event Status, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Event Status recieved is %s .\n", SC_pot_event_body.Event_Status);}

         /*  Call Sim Cmdr here.  */
	 call_SC = 1;

	 msg_body = MakeInstance ("(msg_body of POTENTIAL_EVENT)");

	 result.type = FLOAT;
	 result.value = AddDouble (SC_pot_event_body.Event_Time);
	 DirectPutSlot (msg_body, "event_time", &result);

	 result.type = STRING;
	 result.value = AddSymbol(SC_pot_event_body.Event_Country);
	 DirectPutSlot (msg_body, "event_country", &result);

	 result.type = STRING;
	 result.value = AddSymbol (SC_pot_event_body.Event_Site);
	 DirectPutSlot (msg_body, "event_site", &result);

	 result.type = FLOAT;
	 result.value = AddDouble (SC_pot_event_body.Event_Lat);
	 DirectPutSlot (msg_body, "event_lat", &result);

	 result.type = FLOAT;
	 result.value = AddDouble (SC_pot_event_body.Event_Lon);
	 DirectPutSlot (msg_body, "event_long", &result);

	 result.type = STRING;
	 result.value = AddSymbol (SC_pot_event_body.Event_Status);
	 DirectPutSlot (msg_body, "event_status", &result);

	 /*  if (SCde.gvt > 1237.0)
            SCen.opcode = OP_RTRN_ACK;
         else
            {
               SCen.opcode = OP_RTRN_DCN;

               SC_rtrn_dcn_body.DEFCON  = 1;
               SC_rtrn_dcn_body.RP      = 1;
               strcpy (SC_rtrn_dcn_body.DEA, DEA_FREE);
               strcpy (SC_rtrn_dcn_body.ROE, ROE_NA);
               strcpy (SC_rtrn_dcn_body.Msn_Obj, MSN_OBJ_CUR);
               strcpy (SC_rtrn_dcn_body.BP, BP_AR_DEF);
	       }                                                 */

         break;

      case OP_INTEL_INFO  :

         /* Obtain the Intel_Info indexed by ii_ix here. */

         /*  Call Sim Cmdr here.  */
	 call_SC = 1;

	 msg_body = MakeInstance ("(msg_body of INTEL_MSG)");

	 result.type = INTEGER;
	 result.value = AddLong (II_Day [ii_ix]);
	 DirectPutSlot (msg_body, "day" , &result); 
	 
	 
	 result.type = FLOAT;
	 result.value = AddDouble (II_Hrs [ii_ix]);
	 DirectPutSlot (msg_body,"time" , &result); 

	 SC_intel_time (II_Day [ii_ix], II_Hrs [ii_ix]);

	for (i = 0; i < II_Nam_Num [ii_ix]; i++)
	  {  
	    info = MakeInstance ("(info of INTEL_INFO)");

	    result.type = STRING;
	    result.value = AddSymbol (II_Nam_Src [ii_ix] [i]);
	    DirectPutSlot (info, "source_type", &result); 
	 
	    result.type = INTEGER;
	    result.value = AddLong (II_Nam_Loc [ii_ix] [i]);
	    DirectPutSlot (info, "source_num", &result); 

	    result.type = FLOAT;
	    result.value = AddDouble (II_Val [ii_ix] [i]);
	    DirectPutSlot (info, "value", &result); 

	    result.type = FLOAT;
	    result.value = AddDouble (II_Per [ii_ix] [i]);
	    DirectPutSlot (info, "persistence", &result);

	    return_result.type = INSTANCE_ADDRESS;
	    return_result.value = msg_body;
	    
       	    Send (&return_result, "put-next", "[info]", &return_instance);  
	  }  
	
	sprintf (line, "%-16s  %10.4f  %10.4f\n%-16s  %10.4f  %10.4f\n%-16s  %10.4f  %10.4f\n",
		 II_Nam_Src[ii_ix][0], II_Val[ii_ix][0], II_Per[ii_ix][0],
		 II_Nam_Src[ii_ix][1], II_Val[ii_ix][1], II_Per[ii_ix][1],
		 II_Nam_Src[ii_ix][2], II_Val[ii_ix][2], II_Per[ii_ix][2]);
	SC_intel_src (line);

	sprintf (line, "%s\n%s\n%s\n%s\n%s\n", II_Txt[ii_ix][0], II_Txt[ii_ix][1], II_Txt[ii_ix][2],
		 II_Txt[ii_ix][3], II_Txt[ii_ix][4]);
	SC_intel_text (line);
	  
	ii_ix += 1;
	break;

      case OP_INIT_MO  :

         if (!xdr_int (&xdrs, &SC_init_mo_body.DEFCON_Level))
            printf ("XDR_INT decode, Init Msn Obj DEFCON Level, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj DEFCON Level recieved is %d .\n", SC_init_mo_body.DEFCON_Level);}

         if (!xdr_int (&xdrs, &SC_init_mo_body.RP))
            printf ("XDR_INT decode, Init Msn Obj RP, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj RP recieved is %d .\n", SC_init_mo_body.RP);}

         dp_init_mo_body = &SC_init_mo_body;

         dp          = dp_init_mo_body->DEA;
         if (!xdr_string (&xdrs, &dp, eight))
            printf ("XDR_STRING decode, Init Msn Obj DEA, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj DEA recieved is %s .\n", SC_init_mo_body.DEA);}

         dp          = dp_init_mo_body->ROE;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Init Msn Obj ROE, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj ROE recieved is %s .\n", SC_init_mo_body.ROE);}

         dp          = dp_init_mo_body->Msn_Obj_Name;
         if (!xdr_string (&xdrs, &dp, twelve))
            printf ("XDR_STRING decode, Init Msn Obj Name, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj Name recieved is %s .\n", SC_init_mo_body.Msn_Obj_Name);}

         dp          = dp_init_mo_body->Msn_Obj_Strategy;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Init Msn Obj Strategy, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj Strategy recieved is %s .\n", SC_init_mo_body.Msn_Obj_Strategy);}

         dp          = dp_init_mo_body->Msn_Obj_Tactic;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Init Msn Obj Tactic, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj Tactic recieved is %s .\n", SC_init_mo_body.Msn_Obj_Tactic);}

         if (!xdr_float (&xdrs, &SC_init_mo_body.Msn_Obj_Pms))
            printf ("XDR_FLOAT decode, Init Msn Obj Pms, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj Pms recieved is %f .\n", SC_init_mo_body.Msn_Obj_Pms);}

         if (!xdr_int (&xdrs, &SC_init_mo_body.Msn_Obj_Mode))
            printf ("XDR_INT decode, Init Msn Obj Mode, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj Mode recieved is %d .\n", SC_init_mo_body.Msn_Obj_Mode);}

         if (!xdr_int (&xdrs, &SC_init_mo_body.Msn_Obj_Withhold))
            printf ("XDR_INT decode, Init Msn Obj Withhold, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj Withhold recieved is %d .\n", SC_init_mo_body.Msn_Obj_Withhold);}

         if (!xdr_int (&xdrs, &SC_init_mo_body.Msn_Obj_Add_Bstr))
            printf ("XDR_INT decode, Init Msn Obj Add_Bstr, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Msn Obj Add Bstr recieved is %d .\n", SC_init_mo_body.Msn_Obj_Add_Bstr);}

         /*  Call Sim Cmdr here.  */
	  call_SC = 1;
		
          msn_obj = MakeInstance ("(msn_obj  of MSN_OBJ)"); 
	  
	  result.type = STRING;
	  result.value = AddSymbol (SC_init_mo_body.Msn_Obj_Name);
	  DirectPutSlot (msn_obj, "msn_obj_name", &result);

	  result.type = STRING;
	  result.value = AddSymbol (SC_init_mo_body.Msn_Obj_Strategy);
	  DirectPutSlot (msn_obj, "strategy", &result);

	  result.type = STRING;
	  result.value = AddSymbol (SC_init_mo_body.Msn_Obj_Tactic);
	  DirectPutSlot (msn_obj, "tactic", &result);

	  result.type = FLOAT;
	  result.value = AddDouble (SC_init_mo_body.Msn_Obj_Pms);
	  DirectPutSlot (msn_obj, "pms", &result);

	  result.type = INTEGER;
	  result.value = AddLong (SC_init_mo_body.Msn_Obj_Mode);
	  DirectPutSlot (msn_obj, "mode", &result);

	  result.type = INTEGER;
	  result.value = AddLong (SC_init_mo_body.Msn_Obj_Withhold);
	  DirectPutSlot (msn_obj, "withhold", &result);

	  result.type = INTEGER;
	  result.value = AddLong (SC_init_mo_body.Msn_Obj_Add_Bstr);
	  DirectPutSlot (msn_obj, "add_bstrs", &result);
		
	  	
	  /*   SCen.opcode = OP_RTRN_ACK;   */

         break;

      case OP_INIT_BP  :

         if (!xdr_int (&xdrs, &SC_init_bp_body.DEFCON_Level))
            printf ("XDR_INT decode, Init BP DEFCON Level, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP DEFCON Level recieved is %d .\n", SC_init_bp_body.DEFCON_Level);}

         if (!xdr_int (&xdrs, &SC_init_bp_body.RP))
            printf ("XDR_INT decode, Init BP RP, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP RP recieved is %d .\n", SC_init_bp_body.RP);}

         dp_init_bp_body = &SC_init_bp_body;

         dp          = dp_init_bp_body->DEA;
         if (!xdr_string (&xdrs, &dp, eight))
            printf ("XDR_STRING decode, Init BP DEA, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP DEA recieved is %s .\n", SC_init_bp_body.DEA);}

         dp          = dp_init_bp_body->ROE;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Init BP ROE, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP ROE recieved is %s .\n", SC_init_bp_body.ROE);}

         dp          = dp_init_bp_body->BP_Name;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Init BP Name, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Name recieved is %s .\n", SC_init_bp_body.BP_Name);}

         dp          = dp_init_bp_body->BP_Mode;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Init BP Mode, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Mode recieved is %s .\n", SC_init_bp_body.BP_Mode);}

         dp          = dp_init_bp_body->BP_Tgt_Val_Cut_Off;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Init BP Tgt Val Cut Off, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Tgt Val Cut Off recieved is %s .\n", SC_init_bp_body.BP_Tgt_Val_Cut_Off);}

         dp          = dp_init_bp_body->BP_Accept_Kill_Criteria;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING decode, Init BP Accept Kill Criteria, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Accept Kill Criteria recieved is %s .\n", SC_init_bp_body.BP_Accept_Kill_Criteria);}

         if (!xdr_int (&xdrs, &SC_init_bp_body.BP_Override_Salvo))
            printf ("XDR_INT decode, Init BP Override Salvo, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Override Salvo recieved is %d .\n", SC_init_bp_body.BP_Override_Salvo);}

         if (!xdr_int (&xdrs, &SC_init_bp_body.BP_Launch_Mode))
            printf ("XDR_INT decode, Init BP Launch Mode, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Launch Mode recieved is %d .\n", SC_init_bp_body.BP_Launch_Mode);}

         if (!xdr_float (&xdrs, &SC_init_bp_body.BP_RV_Likelihood_Threshold))
            printf ("XDR_FLOAT decode, Init BP RV Likelihood Threshold, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP RV Likelihood Threshold recieved is %f .\n",
                                                       SC_init_bp_body.BP_RV_Likelihood_Threshold);}

         if (!xdr_float (&xdrs, &SC_init_bp_body.BP_Pk_Cutoff))
            printf ("XDR_FLOAT decode, Init BP Pk Cutoff, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Pk Cutoff recieved is %f .\n", SC_init_bp_body.BP_Pk_Cutoff);}

         if (!xdr_int (&xdrs, &SC_init_bp_body.BP_Asset_Weight_Population))
            printf ("XDR_INT decode, Init BP Asset Weight - Population, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Asset Weight - Population recieved is %d .\n",
                                                         SC_init_bp_body.BP_Asset_Weight_Population);}

         if (!xdr_int (&xdrs, &SC_init_bp_body.BP_Asset_Weight_Military))
            printf ("XDR_INT decode, Init BP Asset Weight - Military, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Asset Weight - Military recieved is %d .\n",
                                                         SC_init_bp_body.BP_Asset_Weight_Military);}

         if (!xdr_int (&xdrs, &SC_init_bp_body.BP_Asset_Weight_Self_Defense))
            printf ("XDR_INT decode, Init BP Asset Weight - Self Defense, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Asset Weight - Self Defense recieved is %d .\n",
                                                         SC_init_bp_body.BP_Asset_Weight_Self_Defense);}

         if (!xdr_int (&xdrs, &SC_init_bp_body.BP_Asset_Weight_NCA))
            printf ("XDR_INT decode, Init BP Asset Weight - NCA, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Asset Weight - NCA recieved is %d .\n",
                                                         SC_init_bp_body.BP_Asset_Weight_NCA);}

         if (!xdr_int (&xdrs, &SC_init_bp_body.BP_Asset_Weight_Industrial))
            printf ("XDR_INT decode, Init BP Asset Weight - Industrial, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init BP Asset Weight - Industrial recieved is %d .\n",
                                                         SC_init_bp_body.BP_Asset_Weight_Industrial);}

      /*  Save Battle Plan in file so available on reset.  */
	call_SC = 1;
 
	bp = MakeInstance ("(bp  of BATTLE_PLAN)"); 
             
	result.type = STRING;
	result.value = AddSymbol (SC_init_bp_body.BP_Name);
	DirectPutSlot (bp, "bp_name", &result);

	result.type = STRING;
	result.value = AddSymbol (SC_init_bp_body.BP_Mode);
	DirectPutSlot (bp, "planner_mode", &result);

	result.type = STRING;
	result.value = AddSymbol (SC_init_bp_body.BP_Tgt_Val_Cut_Off);
        DirectPutSlot (bp, "target_value_cutoff", &result);

	result.type = STRING;
	result.value = AddSymbol (SC_init_bp_body.BP_Accept_Kill_Criteria);
        DirectPutSlot (bp, "accept_kill_criteria", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_init_bp_body.BP_Override_Salvo);
        DirectPutSlot (bp, "override_salvo", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_init_bp_body.BP_Launch_Mode);
        DirectPutSlot (bp, "launch_mode", &result);

	result.type = FLOAT;
	result.value = AddDouble (SC_init_bp_body.BP_RV_Likelihood_Threshold);
        DirectPutSlot (bp, "rv_threshold", &result);

	result.type = FLOAT;
	result.value = AddDouble (SC_init_bp_body.BP_Pk_Cutoff);
        DirectPutSlot (bp, "pk_cutoff", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_init_bp_body.BP_Asset_Weight_Population);
        DirectPutSlot (bp, "weight_population", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_init_bp_body.BP_Asset_Weight_Military);
        DirectPutSlot (bp, "weight_military", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_init_bp_body.BP_Asset_Weight_Self_Defense);
        DirectPutSlot (bp, "weight_selfdefense", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_init_bp_body.BP_Asset_Weight_NCA);
        DirectPutSlot (bp, "weight_ncauthority", &result);

	result.type = INTEGER;
	result.value = AddLong (SC_init_bp_body.BP_Asset_Weight_Industrial);
        DirectPutSlot (bp, "weight_industrial", &result);
	
       
	/*   SCen.opcode = OP_RTRN_ACK;    */

         break;

      case OP_INIT_WA  :

         if (!xdr_int (&xdrs, &SC_wa_body.Num_GBI_Farms))
            printf ("XDR_INT decode, Init Number of GBI Farms, returns error.\n");
         else
            {nargs_out += 1;
             if (toprttr == 1)
                printf ("Init Num GBI Farms recieved is %d .\n", SC_wa_body.Num_GBI_Farms);}

	 wa = MakeInstance ("(wa of WEAPONS)"); 
         
	 for (i = 0; i < SC_wa_body.Num_GBI_Farms; i++)
         {
            if (!xdr_int (&xdrs, &SC_wa_body.Farm_Val[i][0]))
               printf ("XDR_INT decode, Init row %d GBI number, returns error.\n", i);
            else
               {nargs_out += 1;
                if (toprttr == 1)
                   printf ("Recieved row %d GBI number %d", i, SC_wa_body.Farm_Val[i][0]);}

            if (!xdr_int (&xdrs, &SC_wa_body.Farm_Val[i][1]))
               printf ("XDR_INT decode, Init row %d GBIs on loc, returns error.\n", i);
            else
               {nargs_out += 1;
                if (toprttr == 1)
                   printf (", GBIs on loc is %d",  SC_wa_body.Farm_Val[i][1]);}

            if (!xdr_int (&xdrs, &SC_wa_body.Farm_Val[i][2]))
               printf ("XDR_INT decode, Init row %d GBIs on hold, returns error.\n", i);
            else
               {nargs_out += 1;
                if (toprttr == 1)
                   printf (", and GBIs on hold is %d .\n", SC_wa_body.Farm_Val[i][2]);}
	    
	    gbi_farm = MakeInstance ("(gbi_farm of GBI_FARM)");
	   
	    result.type = INTEGER;
	    result.value = AddLong (SC_wa_body.Farm_Val[i][0]);
	    DirectPutSlot (gbi_farm, "id", &result);		

	    result.type = INTEGER;
	    result.value = AddLong (SC_wa_body.Farm_Val[i][1]);
	    DirectPutSlot (gbi_farm, "weapons", &result);

	    result.type = INTEGER;
	    result.value = AddLong (SC_wa_body.Farm_Val[i][2]);
	    DirectPutSlot (gbi_farm, "weapons_held", &result);
	    
	   
	    return_result.type = INSTANCE_ADDRESS;
	    return_result.value = wa;
	    
	    Send (&return_result, "put-next", "[gbi_farm]", &return_instance);
	    

         }
	 
	
	 /* Call Sim Cmdr here.  */
    
	 call_SC = 1;  


	/* SCen.opcode = OP_RTRN_ACK;  */

         break;

     case OP_EXIT_SC :
          
    /*  Call Sim Cmdr here.  */
        call_SC = 1;

    /* Prepare message body for SC. Initialize SC only uses header. */
	
        msg_body = MakeInstance("(msg_body of INTEGER_PROMPT (prompt 1))"); 

       break;

  

 case BAD_DESTID     :
 
          strcpy (SC_rtrn_err_body.ERR_TXT, ERR_DESTID);
 
          SCen.opcode = OP_RTRN_ERR;
 
          break ; 
     
 default        :

         strcpy (SC_rtrn_err_body.ERR_TXT, ERR_OPCODE);

         SCen.opcode = OP_RTRN_ERR;

         break;
   }

    /*Create the Message for FZCLIPS*/

 if (call_SC == 1)
 {
     fact_pointer = FindDeftemplate ("phase_control");
     phase_fact = CreateFact (fact_pointer);
     result.type = SYMBOL;
     result.value = AddSymbol ("initialize");
     PutFactSlot (phase_fact, "phase", &result);
     Assert (phase_fact);
    
 
	SimCmdr = MakeInstance ("(SimCmdr of PLAYER)");
       
	result.type = STRING;
	result.value = AddSymbol (DstNoNa);
	DirectPutSlot (SimCmdr, "node_type", &result);

	result.type = INTEGER;
	result.value = AddLong (DstNoNu);
        DirectPutSlot (SimCmdr, "node_number", &result);

	result.type = STRING;
	result.value = AddSymbol (DstDoNa);
        DirectPutSlot (SimCmdr, "position_type", &result);

	result.type = INTEGER;
	result.value = AddLong (DstDoNu);
        DirectPutSlot (SimCmdr, "position_number", &result);

	result.type = INTEGER;
	result.value = AddLong (SCde.SCactive);
        DirectPutSlot (SimCmdr, "active", &result);
 
     BattlePlanner = MakeInstance ("(BattlePlanner of PLAYER)");

     result.type = STRING;
     result.value = AddSymbol (SrcNoNa);
     DirectPutSlot (BattlePlanner, "node_type", &result); 
									 
     result.type = INTEGER;
     result.value = AddLong (SrcNoNu);
     DirectPutSlot (BattlePlanner, "node_number", &result);

     result.type = STRING;
     result.value = AddSymbol (SrcDoNa);
     DirectPutSlot (BattlePlanner, "position_type", &result);

     result.type = INTEGER;
     result.value = AddLong (SrcDoNu);
     DirectPutSlot (BattlePlanner, "position_number", &result);

     result.type = INTEGER;
     result.value = AddLong (SCde.SCactive);
     DirectPutSlot (BattlePlanner, "active", &result);
 
    
     msg_header = MakeInstance ("(msg_header of MSG_HEADER (destination_id [SimCmdr])
					  	     (source_id [BattlePlanner]))");

     result.type = INTEGER;
     result.value = AddLong (SCde.opcode);
     DirectPutSlot (msg_header, "opcode", &result);

     result.type = FLOAT;
     result.value = AddDouble (SCde.gvt);
     DirectPutSlot (msg_header, "gvt", &result);

     msg = MakeInstance ("(msg of MESSAGE (message_header [msg_header])
					        (msg_body [msg_body]))");

     SC_update_gvt (SCde.gvt);
   
   Run (-1L);
  
 }
  
/*Feed the translator the return message from FZCLIPS  */

   
   if (toprttr == 1)
      printf ("Decoded %d fields for opcode %d.\n", nargs_out, SCde.opcode);

   /*  Message done, remove xdr set up.  */

   xdr_destroy (&xdrs);

   /*  Build a message to the battle planner, set up the buffer. */

   xdrmem_create (&xdrs, decisions, (unsigned)2000, XDR_ENCODE);

   /*Get the opcode that tells the translator the type of message.*/
 
   SetType (return_instance, INSTANCE_ADDRESS);
   SetValue (return_instance, msg_header);
  
   DirectGetSlot (return_instance.value, "opcode", &return_result);
   SCen.opcode = DOToInteger (return_result);
  
   /*Get the source id . Upon return this will be the id of an SC. */

   SetType (return_instance, INSTANCE_ADDRESS);
   SetValue (return_instance, SimCmdr);
  
   DirectGetSlot (return_instance.value, "node_type", &return_result);
   dp = DOToString (return_result);
   length = strlen (dp);
   
   for (i = 0; i < length; i++)
     SrcNoNa [i] = dp [i];

   SrcNoNa [length] = '\0';


   DirectGetSlot (return_instance.value, "node_number", &return_result);
   SrcNoNu = DOToInteger (return_result);

   DirectGetSlot (return_instance.value, "position_type", &return_result);
   dp = DOToString (return_result);
   length = strlen (dp);

   for (i = 0; i < length; i++)
    SrcDoNa [i] = dp [i];
   SrcDoNa[length] = '\0';
   
   DirectGetSlot (return_instance.value, "position_number", &return_result);
   SrcDoNu = DOToInteger (return_result);

   test = Char_2_Num (SrcNoNa, &SrcNoNu, SrcDoNa, &SrcDoNu, &SCen.SrcID);
 

  /*Get the destination  id . Upon return this will be the id of a Battle Planner. */

   SetType (return_instance, INSTANCE_ADDRESS);
   SetValue (return_instance, BattlePlanner);
  
   DirectGetSlot (return_instance.value, "node_type", &return_result);
   dp = DOToString (return_result);
   length = strlen (dp);
   
   for (i = 0; i < length; i++)
     DstNoNa [i] = dp [i];

   DstNoNa [length] = '\0';


   DirectGetSlot (return_instance.value, "node_number", &return_result);
   DstNoNu = DOToInteger (return_result);

   DirectGetSlot (return_instance.value, "position_type", &return_result);
   dp = DOToString (return_result);
   length = strlen (dp);

   for (i = 0; i < length; i++)
    DstDoNa [i] = dp [i];
   DstDoNa[length] = '\0';
   
   
   DirectGetSlot (return_instance.value, "position_number", &return_result);
   DstDoNu = DOToInteger (return_result);

   test = Char_2_Num (DstNoNa, &DstNoNu, DstDoNa, &DstDoNu, &SCen.DstID);

   SCen.SCID      = SCde.SCID;
   SCen.SCactive  = SCde.SCactive;
   SCen.gvt       = SCde.gvt;
   SCen.reserved7 = SCde.reserved7;
   SCen.reserved8 = SCde.reserved8;
   SCen.reserved9 = SCde.reserved9;

   encode_header (&xdrs, &SCen, &nargs_in);

   switch (SCen.opcode)
   {
      case OP_RTRN_ACK :
	SetType (return_instance, INSTANCE_ADDRESS);
	SetValue (return_instance, msg);
	
	DirectGetSlot (return_instance.value, "msg_body", &return_result);
	Send (&return_result, "get-prompt", NULL, &result);
       
	printf ("An Ack was received the prompt was %d\n", DOToInteger(result)); 
	printf (" An Ack was received from SC. \n");
	break;

      case OP_RTRN_DCN :  
	
	SetType (return_instance, INSTANCE_ADDRESS);
	SetValue (return_instance, msg);
	DirectGetSlot (return_instance.value, "msg_body", &return_result);

	Send (&return_result, "get-DEFCON", NULL, &result);
	SC_rtrn_dcn_body.DEFCON = DOToInteger (result);
	SC_update_def (SC_rtrn_dcn_body.DEFCON);
	
	Send (&return_result, "get-RP", NULL, &result);
	SC_rtrn_dcn_body.RP = DOToInteger (result);
	SC_update_rp (SC_rtrn_dcn_body.RP);

	Send (&return_result, "get-DEA", NULL, &result);
	dp = DOToString (result);
	length = strlen (dp);

	for (i = 0; i < length; i++)
	  SC_rtrn_dcn_body.DEA[i] = dp [i];
	SC_rtrn_dcn_body.DEA[length] = '\0';
        SC_update_dea (SC_rtrn_dcn_body.DEA);

        Send (&return_result, "get-ROE", NULL, &result);
	dp = DOToString (result);
	length = strlen (dp);

	for (i = 0; i < length; i++)
	  SC_rtrn_dcn_body.ROE[i] = dp [i];
	SC_rtrn_dcn_body.ROE[length] = '\0';
      
         Send (&return_result, "get-Msn_Obj_Name", NULL, &result);
	dp = DOToString (result);
	length = strlen (dp);

	for (i = 0; i < length; i++)
	  SC_rtrn_dcn_body.Msn_Obj [i] = dp [i];
	SC_rtrn_dcn_body.Msn_Obj [length] = '\0';
       
        Send (&return_result, "get-BP_Name", NULL, &result);
	dp = DOToString (result);
	length = strlen (dp);

	for (i = 0; i < length; i++)
	  SC_rtrn_dcn_body.BP [i] = dp [i];
	SC_rtrn_dcn_body.BP [length] = '\0';
               

         if (!xdr_int (&xdrs, &(SC_rtrn_dcn_body.DEFCON)))
            printf ("XDR_INT encode, New DEFCON, returns error.\n");
         else
            {nargs_in += 1;}

         if (!xdr_int (&xdrs, &(SC_rtrn_dcn_body.RP)))
            printf ("XDR_INT encode, New RP, returns error.\n");
         else
            {nargs_in += 1;}

         dp_rtrn_dcn_body = &SC_rtrn_dcn_body;

         dp               = dp_rtrn_dcn_body->DEA;
         if (!xdr_string (&xdrs, &dp, eight))
            printf ("XDR_STRING encode, New DEA, returns error.\n");
         else
            {nargs_in += 1;}

         dp               = dp_rtrn_dcn_body->ROE;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING encode, New ROE, returns error.\n");
         else
            {nargs_in += 1;}

         dp               = dp_rtrn_dcn_body->Msn_Obj;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING encode, New Msn Obj, returns error.\n");
         else
            {nargs_in += 1;}

         dp               = dp_rtrn_dcn_body->BP;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING encode, New BP, returns error.\n");
         else
            {nargs_in += 1;}

         break;

      case OP_RTRN_ERR :

         dp_rtrn_err_body = &SC_rtrn_err_body;

         dp               = dp_rtrn_err_body->ERR_TXT;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING encode, opcode error, returns error.\n");
         else
            {nargs_in += 1;}

         break;

      default          :
         xdr_destroy (&xdrs);
         xdrmem_create (&xdrs, decisions, (unsigned)2000, XDR_ENCODE);
         SCen.opcode    = OP_RTRN_ERR;
         encode_header (&xdrs, &SCen, &nargs_in);

         strcpy (SC_rtrn_err_body.ERR_TXT, ERR_UNK);

         dp_rtrn_err_body = &SC_rtrn_err_body;

         dp               = dp_rtrn_err_body->ERR_TXT;
         if (!xdr_string (&xdrs, &dp, twenty))
            printf ("XDR_STRING encode, unexpected error, returns error.\n");
         else
            {nargs_in += 1;}

         break;
   }
 

   if (toprttr == 1)
      printf ("Encoded %d fields for opcode %d.\n", nargs_in, SCen.opcode);

   *xdrsize = xdr_getpos (&xdrs);

   if (toprttr == 1)
      printf ("The xdrsize is %d.\n", *xdrsize);

     
}

    

     

UserFunctions ( )
{
  extern double replan ();
  extern void *roe_assess ();
  extern int TraceOn(), TraceOff();

  DefineFunction2 ("replan", 'd', PTIF replan, "replan", "55u");
  DefineFunction2 ("roe_assess", 's', PTIF roe_assess, "roe_assess", "22d");
  DefineFunction ("tron", 'b', TraceOn, "TraceOn");
  DefineFunction ("troff", 'b', TraceOff, "TraceOff");
}

