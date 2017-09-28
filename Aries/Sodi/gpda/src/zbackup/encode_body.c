#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/xdr.h>

#include "demo_opcodes.h"
#include "demo_msg_body.h"
#include "demo_strings.h"

int encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs)
{
   extern int		toprttr;

   unsigned int		four = 4, eight = 8, twelve = 12, twenty = 20;
   char   		*dpc;
   int			*dpi, return_val, i;

   struct SEND_ALL	*dp_all;

   /*  Encode the body based on the opcode.  */

   return_val = 0;

   dp_all = (struct SEND_ALL *) en;

   switch (*opcode)
   {

      case OP_INIT_SC	:
         break;

      case OP_DB_STATUS	:

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.DEFCON_Level)))
            {printf ("XDR_INT encode, DB DEFCON Level, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.RP)))
            {printf ("XDR_INT encode, DB RP, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->db_status_body_struct.DEA);
         if (!xdr_string (xdrs_en, &dpc, eight))
            {printf ("XDR_STRING encode, DB DEA, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->db_status_body_struct.ROE);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, DB ROE, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->db_status_body_struct.Msn_Obj_Name);
         if (!xdr_string (xdrs_en, &dpc, twelve))
            {printf ("XDR_STRING encode, DB Msn Obj Name, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->db_status_body_struct.Msn_Obj_Strategy);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, DB Msn Obj Strategy, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->db_status_body_struct.Msn_Obj_Tactic);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, DB Msn Obj Tactic, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->db_status_body_struct.Msn_Obj_Pms)))
            {printf ("XDR_FLOAT encode, DB Msn Obj Pms, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.Msn_Obj_Mode)))
            {printf ("XDR_INT encode, DB Msn Obj Mode, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.Msn_Obj_Withhold)))
            {printf ("XDR_INT encode, DB Msn Obj Withhold, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.Msn_Obj_Add_Bstr)))
            {printf ("XDR_INT encode, DB Msn Obj Add Bstr, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->db_status_body_struct.BP_Name);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, DB BP Name, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->db_status_body_struct.BP_Mode);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, DB BP Mode, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->db_status_body_struct.BP_Tgt_Val_Cut_Off);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, DB BP Tgt Val Cut Off, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->db_status_body_struct.BP_Accept_Kill_Criteria);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, DB BP Accept Kill Criteria, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.BP_Override_Salvo)))
            {printf ("XDR_INT encode, DB BP Override Salvo, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.BP_Launch_Mode)))
            {printf ("XDR_INT encode, DB BP Launch Mode, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->db_status_body_struct.BP_RV_Likelihood_Threshold)))
            {printf ("XDR_FLOAT encode, DB BP RV Likelihood Threshold, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->db_status_body_struct.BP_Pk_Cutoff)))
            {printf ("XDR_FLOAT encode, DB BP Pk Cutoff, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.BP_Asset_Weight_Population)))
            {printf ("XDR_INT encode, DB BP Asset weight - Population, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.BP_Asset_Weight_Military)))
            {printf ("XDR_INT encode, DB BP Asset weight - Military, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.BP_Asset_Weight_Self_Defense)))
            {printf ("XDR_INT encode, DB BP Asset weight - Self Defense, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.BP_Asset_Weight_NCA)))
            {printf ("XDR_INT encode, DB BP Asset weight - NCA, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->db_status_body_struct.BP_Asset_Weight_Industrial)))
            {printf ("XDR_INT encode, DB BP Asset weight - Industrial, returns error.\n");}
         else
            {*nargs += 1;}
 
         break;

      case OP_DEFCON	:

         if (!xdr_int (xdrs_en, &(dp_all->defcon_body_struct.DEFCON)))
            {printf ("XDR_INT encode, Current DEFCON, returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_RP	:

         if (!xdr_int (xdrs_en, &(dp_all->rp_body_struct.RP)))
            {printf ("XDR_INT encode, Current RP, returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_DEA	:

         dpc = (char *) &(dp_all->dea_body_struct.DEA);
         if (!xdr_string (xdrs_en, &dpc, eight))
            {printf ("XDR_STRING encode, Current DEA, returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_ROE	:

         dpc = (char *) &(dp_all->roe_body_struct.ROE);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Current ROE, returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_MSN_OBJ	:

         dpc = (char *) &(dp_all->msn_obj_body_struct.Msn_Obj);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Current Mission Objective, returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_BP	:

         dpc = (char *) &(dp_all->bp_body_struct.BP);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Current BP, returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_WA	:
      case OP_INIT_WA	:

         if (!xdr_int (xdrs_en, &(dp_all->wa_body_struct.Num_GBI_Farms)))
            {printf ("XDR_INT encode, Number of GBI Farms, returns error.\n");
             return_val = 1;                                                  }
         else
            {*nargs += 1;}

         if (return_val == 0 && dp_all->wa_body_struct.Num_GBI_Farms > MAX_FARMS)
            {printf ("%d is too many GBI farms.\n", dp_all->wa_body_struct.Num_GBI_Farms);
             return_val = 1;                                                          }

         if (return_val == 0)
         {
            for (i = 0; i < dp_all->wa_body_struct.Num_GBI_Farms; i++)
            {
               if (!xdr_int (xdrs_en, &(dp_all->wa_body_struct.Farm_Val[i][0])))
                  {printf ("XDR_INT encode, row %d farm number, returns error.\n", i);}
               else
                  {*nargs += 1;}

               if (!xdr_int (xdrs_en, &(dp_all->wa_body_struct.Farm_Val[i][1])))
                  {printf ("XDR_INT encode, row %d farm weapons, returns error.\n", i);}
               else
                  {*nargs += 1;}

               if (!xdr_int (xdrs_en, &(dp_all->wa_body_struct.Farm_Val[i][2])))
                  {printf ("XDR_INT encode, row %d farm GBIs on hold, returns error.\n", i);}
               else
                  {*nargs += 1;}
            }
         }

         break;

      case OP_TRK_ENGMT	:

         dpc = (char *) &(dp_all->trk_engmt_body_struct.Eng_Status);
         if (!xdr_string (xdrs_en, &dpc, twelve))
            {printf ("XDR_STRING encode, Engagement Status, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->trk_engmt_body_struct.Cur_Eng_Num_Wea)))
            {printf ("XDR_INT encode, Cur Eng Num Weapons, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->trk_engmt_body_struct.Cur_Eng_TTI)))
            {printf ("XDR_FLOAT encode, Cur Eng Time To Intercept, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->trk_engmt_body_struct.Cur_Eng_Pk)))
            {printf ("XDR_FLOAT encode, Cur Eng Pk, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->trk_engmt_body_struct.Eng_Opp_Remain)))
            {printf ("XDR_INT encode, Init Eng Opportunities Remaining, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->trk_engmt_body_struct.Track_Id);
         if (!xdr_string (xdrs_en, &dpc, twelve))
            {printf ("XDR_STRING encode, Hostile Track ID, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->trk_engmt_body_struct.Task_Id)))
            {printf ("XDR_INT encode, Weapon Task ID returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_INIT_MO	:

         if (!xdr_int (xdrs_en, &(dp_all->init_mo_body_struct.DEFCON_Level)))
            {printf ("XDR_INT encode, Init Msn Obj DEFCON Level, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_mo_body_struct.RP)))
            {printf ("XDR_INT encode, Init Msn Obj RP, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_mo_body_struct.DEA);
         if (!xdr_string (xdrs_en, &dpc, eight))
            {printf ("XDR_STRING encode, Init Msn Obj DEA, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_mo_body_struct.ROE);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Init Msn Obj ROE, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_mo_body_struct.Msn_Obj_Name);
         if (!xdr_string (xdrs_en, &dpc, twelve))
            {printf ("XDR_STRING encode, Init Msn Obj Name, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_mo_body_struct.Msn_Obj_Strategy);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Init Msn Obj Strategy, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_mo_body_struct.Msn_Obj_Tactic);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Init Msn Obj Tactic, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->init_mo_body_struct.Msn_Obj_Pms)))
            {printf ("XDR_FLOAT encode, Init Msn Obj Pms, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_mo_body_struct.Msn_Obj_Mode)))
            {printf ("XDR_INT encode, Init Msn Obj Mode, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_mo_body_struct.Msn_Obj_Withhold)))
            {printf ("XDR_INT encode, Init Msn Obj Withhold, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_mo_body_struct.Msn_Obj_Add_Bstr)))
            {printf ("XDR_INT encode, Init Msn Obj Add Bstr, returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_INIT_BP	:

         if (!xdr_int (xdrs_en, &(dp_all->init_bp_body_struct.DEFCON_Level)))
            {printf ("XDR_INT encode, Init BP DEFCON Level, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_bp_body_struct.RP)))
            {printf ("XDR_INT encode, Init BP RP, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_bp_body_struct.DEA);
         if (!xdr_string (xdrs_en, &dpc, eight))
            {printf ("XDR_STRING encode, Init BP DEA, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_bp_body_struct.ROE);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Init BP ROE, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_bp_body_struct.BP_Name);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Init BP Name, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_bp_body_struct.BP_Mode);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Init BP Mode, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_bp_body_struct.BP_Tgt_Val_Cut_Off);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Init BP Tgt Val Cut Off, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->init_bp_body_struct.BP_Accept_Kill_Criteria);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Init BP Accept Kill Criteria, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_bp_body_struct.BP_Override_Salvo)))
            {printf ("XDR_INT encode, Init BP Override Salvo, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_bp_body_struct.BP_Launch_Mode)))
            {printf ("XDR_INT encode, Init BP Launch Mode, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->init_bp_body_struct.BP_RV_Likelihood_Threshold)))
            {printf ("XDR_FLOAT encode, Init BP RV Likelihood Threshold, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->init_bp_body_struct.BP_Pk_Cutoff)))
            {printf ("XDR_FLOAT encode, Init BP Pk Cutoff, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_bp_body_struct.BP_Asset_Weight_Population)))
            {printf ("XDR_INT encode, Init BP Asset weight - Population, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_bp_body_struct.BP_Asset_Weight_Military)))
            {printf ("XDR_INT encode, Init BP Asset weight - Military, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_bp_body_struct.BP_Asset_Weight_Self_Defense)))
            {printf ("XDR_INT encode, Init BP Asset weight - Self Defense, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_bp_body_struct.BP_Asset_Weight_NCA)))
            {printf ("XDR_INT encode, Init BP Asset weight - NCA, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->init_bp_body_struct.BP_Asset_Weight_Industrial)))
            {printf ("XDR_INT encode, Init BP Asset weight - Industrial, returns error.\n");}
         else
            {*nargs += 1;}
 
         break;

      case OP_MSL_TRK	:

         dpc = (char *) &(dp_all->msl_trk_body_struct.Track_ID);
         if (!xdr_string (xdrs_en, &dpc, twelve))
            {printf ("XDR_STRING encode, Hostile Missile Track ID, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->msl_trk_body_struct.Object_Type);
         if (!xdr_string (xdrs_en, &dpc, eight))
            {printf ("XDR_STRING encode, Track Object Type, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->msl_trk_body_struct.Missile_Type);
         if (!xdr_string (xdrs_en, &dpc, eight))
            {printf ("XDR_STRING encode, Track Missile Type, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->msl_trk_body_struct.Missile_Class);
         if (!xdr_string (xdrs_en, &dpc, eight))
            {printf ("XDR_STRING encode, Track Missile Class, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->msl_trk_body_struct.Exp_Tgts)))
            {printf ("XDR_INT encode, Track Expected Targets, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->msl_trk_body_struct.Leth_Val)))
            {printf ("XDR_FLOAT encode, Track Lethality Value, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->msl_trk_body_struct.Launch_Time)))
            {printf ("XDR_FLOAT encode, Track Launch Time, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->msl_trk_body_struct.Launch_Country);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Track Launch Country, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->msl_trk_body_struct.Launch_Site);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Track Launch Site, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->msl_trk_body_struct.Impact_Lat)))
            {printf ("XDR_FLOAT encode, Track Impact Lat, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->msl_trk_body_struct.Impact_Lon)))
            {printf ("XDR_FLOAT encode, Track Impact Long, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->msl_trk_body_struct.Earl_Impact_Time)))
            {printf ("XDR_FLOAT encode, Track Earliest Impact Time, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->msl_trk_body_struct.Pred_Impact_Region);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Track Predicated Impact Region, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->msl_trk_body_struct.Num_Boosters)))
            {printf ("XDR_INT encode, Track Number of Boosters, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_int (xdrs_en, &(dp_all->msl_trk_body_struct.Leth_Exp)))
            {printf ("XDR_INT encode, Track Lethals Expected, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->msl_trk_body_struct.Trk_Status);
         if (!xdr_string (xdrs_en, &dpc, eight))
            {printf ("XDR_STRING encode, Track Track Status, returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_POT_EVENT	:

         if (!xdr_float (xdrs_en, &(dp_all->pot_event_body_struct.Event_Time)))
            {printf ("XDR_FLOAT encode, Event Time, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->pot_event_body_struct.Event_Country);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Event Country, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->pot_event_body_struct.Event_Site);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Event Site, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->pot_event_body_struct.Event_Lat)))
            {printf ("XDR_FLOAT encode, Event Lat, returns error.\n");}
         else
            {*nargs += 1;}

         if (!xdr_float (xdrs_en, &(dp_all->pot_event_body_struct.Event_Lon)))
            {printf ("XDR_FLOAT encode, Event Long, returns error.\n");}
         else
            {*nargs += 1;}

         dpc = (char *) &(dp_all->pot_event_body_struct.Event_Status);
         if (!xdr_string (xdrs_en, &dpc, twenty))
            {printf ("XDR_STRING encode, Event Status, returns error.\n");}
         else
            {*nargs += 1;}

         break;

      case OP_INTEL_INFO :
         break;

      default		:
         printf ("Body encoder passed an illegal opcode of %d .\n", *opcode);
         return_val = 1;
         break;

   }

   if (toprttr == 1)
      printf ("Encoded %d fields for opcode %d.\n", *nargs, *opcode);

   return return_val;

}

