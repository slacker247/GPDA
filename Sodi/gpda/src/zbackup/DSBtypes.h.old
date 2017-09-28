
/* Creates, types, and holds the variables used by both the D_S_Driver and the
        Dempster-Shafer algorithm, d_s_algo.c.  */

/*   Evid     means Evidence
     Hypot    means Hypotheses
     Plaus    means Plausibility
     Rec      means Record
     Bpa      means Basic Probability Assignment
     */

/* Variables typed as "short" are boleans, take on values True and False. */

#define True  1
#define False 0
#define A_Max 101
#define Max_Group_Level 7
#define Max_Group_Id    4
#define Max_Hypot_Used  5
#define Max_Assoc_Used  4
#define Number_Names    17

struct Evid_Bpa_Rec { int     Array_Count;
                      int     Evid_Name;
                      float   Bpa_In;
                      float   Bpa_Number;
                      float   Bpa_Belief;
                      float   Bpa_Plaus;
                      float   SecPastEpoc; };

struct Group_Evid_Rec { struct Evid_Bpa_Rec   Evid_Info[A_Max];
                        int                   Group_Count;     };

struct Retrieved_Names_Record { char     Name[21];
                                int      Number;
                                int      Length;   };

extern struct Group_Evid_Rec Group_Evid_Rec_Array[Max_Group_Level][Max_Group_Id];
extern struct Group_Evid_Rec History[Max_Assoc_Used];
extern struct Retrieved_Names_Record   Name_List[Number_Names];

void DS_Algo(int, float, float);
void DS_AlgoInit();
void DS_AlgoGetResult(int ix, int iy, float results[]);
