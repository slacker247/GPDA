#define MAX_FARMS	100
#define Max_Src         3

/*  Definations of message body contents by opcode  */


/*  OP_INIT_SC     0    Initialize the Simulated Commander              */

    struct INIT_SC_BODY {
				int		DUMMY;
                        };


/*  OP_ACK         1    ACK from Battle Manager                         */

    /*  Fields TBD, message not played at this time.  */


/*  OP_NACK        2    NACK from Battle Manager                        */

    /*  Fields TBD, message not played at this time.  */


/*  OP_ DB_STATUS  100  Pass current BM status of 6 factors             */

    struct DB_STATUS_BODY {
				int		DEFCON_Level;
				int		RP;
				char		DEA[8];
				char		ROE[20];
				char		Msn_Obj_Name[12];
				char		Msn_Obj_Strategy[20];
				char		Msn_Obj_Tactic[20];
				float		Msn_Obj_Pms;
				int		Msn_Obj_Mode;
				int		Msn_Obj_Withhold;
				int		Msn_Obj_Add_Bstr;
				char		BP_Name[20];
				char		BP_Mode[20];
				char		BP_Tgt_Val_Cut_Off[20];
				char		BP_Accept_Kill_Criteria[20];
				int		BP_Override_Salvo;
				int		BP_Launch_Mode;
				float		BP_RV_Likelihood_Threshold;
				float		BP_Pk_Cutoff;
				int		BP_Asset_Weight_Population;
				int		BP_Asset_Weight_Military;
				int		BP_Asset_Weight_Self_Defense;
				int		BP_Asset_Weight_NCA;
				int		BP_Asset_Weight_Industrial;
                          };


/*  OP_DEFCON      101  BM current Defense Condition status             */

    struct DEFCON_BODY {
				int		DEFCON;
                       };


/*  OP_RP          102  BM current Readyness Posture status             */

    struct RP_BODY {
				int		RP;
                   };


/*  OP_DEA         103  BM current Defense Engagement Authority status  */

    struct DEA_BODY {
				char		DEA[8];
                    };


/*  OP_ROE         104  BM current Rules Of Engagement status           */

    struct ROE_BODY {
				char		ROE[20];
                    };


/*  OP_MSN_OBJ     105  BM current Mission Objectives status            */

    struct MSN_OBJ_BODY {
				char		Msn_Obj[20];
                        };


/*  OP_BP          106  BM current Battle Plan status                   */

    struct BP_BODY {
				char		BP[20];
                   };


/*  OP_WA          107  BM current Weapons Available status             */

    struct WA_BODY {
				int		Num_GBI_Farms;
				int		Farm_Val[MAX_FARMS][3];
                   };     
                           /*  Farm_Val[n][0] is GBI Farm Number         */
                           /*  Farm_Val[n][1] is number of GBIs on farm  */
                           /*  Farm_Val[n][2] is number of GBIs on hold  */


/*  OP_TRK_ENGMT   108  Track Engagement Summary                        */

    struct TRK_ENGMT_BODY {
				char		Eng_Status[12];
				int		Cur_Eng_Num_Wea;
                                float		Cur_Eng_TTI;
				float		Cur_Eng_Pk;
				int		Eng_Opp_Remain;
                                char            Track_Id[10];
                                int             Task_Id;
                          };


/*  OP_MSL_TRK   130  Hostile Missile Track data                    */

    struct MSL_TRK_BODY   {
				char		Track_ID[10];
				char		Object_Type[8];
				char		Missile_Type[8];
				char		Missile_Class[6];
				int		Exp_Tgts;
				float		Leth_Val;
				float		Launch_Time;
				char		Launch_Country[20];
				char		Launch_Site[20];
				float		Impact_Lat;            /*  negative is south  */
				float		Impact_Lon;            /*  negative is west  */
				float		Earl_Impact_Time;
				char		Pred_Impact_Region[20];
				int		Num_Boosters;
				int		Leth_Exp;
				char		Trk_Status[8];
                          };

/*  OP_POT_EVENT   140  Potential Event Alert/Update                    */

    struct POT_EVENT_BODY {
				float		Event_Time;
				char		Event_Country[20];
				char		Event_Site[20];
				float		Event_Lat;             /*  negative is south  */
				float		Event_Lon;             /*  negative is west  */
				char		Event_Status[20];
                          };

/*  OP_INTEL_INFO   150  Intelligence Report data                       */

    struct INTEL_INFO_BODY {
                                int             II_Day;
                                float           II_Hrs;
                                int             II_Nam_Num;
                                char            II_Txt[5][51];
                                char            II_Nam_Src[Max_Src][3];
                                int             II_Nam_Loc[Max_Src]; 
                                float           II_Val[Max_Src];
                                float           II_Per[Max_Src];
 
                           };


/*  OP_INIT_MO     301  Initialize Mission Objective + 4                */

    struct INIT_MO_BODY {
				int		DEFCON_Level;
				int		RP;
				char		DEA[8];
				char		ROE[20];
				char		Msn_Obj_Name[12];
				char		Msn_Obj_Strategy[20];
				char		Msn_Obj_Tactic[20];
				float		Msn_Obj_Pms;
				int		Msn_Obj_Mode;
				int		Msn_Obj_Withhold;
				int		Msn_Obj_Add_Bstr;
                        };


/*  OP_INIT_BP     302  Initialize Battle Plan + 4                      */

    struct INIT_BP_BODY {
				int		DEFCON_Level;
				int		RP;
				char		DEA[8];
				char		ROE[20];
				char		BP_Name[20];
				char		BP_Mode[20];
				char		BP_Tgt_Val_Cut_Off[20];
				char		BP_Accept_Kill_Criteria[20];
				int		BP_Override_Salvo;
				int		BP_Launch_Mode;
				float		BP_RV_Likelihood_Threshold;
				float		BP_Pk_Cutoff;
				int		BP_Asset_Weight_Population;
				int		BP_Asset_Weight_Military;
				int		BP_Asset_Weight_Self_Defense;
				int		BP_Asset_Weight_NCA;
				int		BP_Asset_Weight_Industrial;
                        };


/*  OP_INIT_WA     303  Initialize Weapons Available                    */

    /*  Use structure body described in OP_WA (107).  */


/*  Structure holding send structures for the general encode.  */

    struct SEND_ALL {
    		struct	INIT_SC_BODY		init_sc_body_struct;
    		struct	DB_STATUS_BODY		db_status_body_struct;
    		struct	DEFCON_BODY		defcon_body_struct;
		struct	RP_BODY			rp_body_struct;
		struct	DEA_BODY		dea_body_struct;
		struct	ROE_BODY		roe_body_struct;
		struct	MSN_OBJ_BODY		msn_obj_body_struct;
		struct	BP_BODY			bp_body_struct;
		struct	WA_BODY			wa_body_struct;
		struct	TRK_ENGMT_BODY		trk_engmt_body_struct;
		struct	INIT_MO_BODY		init_mo_body_struct;
		struct	INIT_BP_BODY		init_bp_body_struct;
		struct	MSL_TRK_BODY		msl_trk_body_struct;
		struct	POT_EVENT_BODY		pot_event_body_struct;
		struct	INTEL_INFO_BODY		intel_info_body_struct;
                    };


/*  OP_RTRN_DCN    501  Decisions on the 6 factors                      */

    struct RTRN_DCN_BODY {
				int		DEFCON;
				int		RP;
				char		DEA[8];
				char		ROE[20];
				char		Msn_Obj[20];
				char		BP[20];
                         };


/*  OP_RTRN_ACK    502  Sim Cmdr ACK, no new decisions                  */

    struct RTRN_ACK_BODY {
				int		DUMMY;
                         };


/*  OP_RTRN_ERR    503  Sim Cmdr NACK, some error found                 */

    struct RTRN_ERR_BODY {
				char		ERR_TXT[20];
                         };

/*  Structure holding return structures for the general decode.  */

    struct RTRN_ALL {
    		struct	RTRN_DCN_BODY		dcn_body_struct;
    		struct	RTRN_ACK_BODY		ack_body_struct;
    		struct	RTRN_ERR_BODY		err_body_struct;

                    };

/*  end of include  */



