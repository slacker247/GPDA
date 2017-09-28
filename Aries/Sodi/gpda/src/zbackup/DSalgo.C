#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DSBtypes.h"

/* Creates, types, and holds the variables used by the Dempster-Shafer
        subroutine ds_algo.c and the subroutines it calls.

     Evid     means Evidence
     Hypot    means Hypotheses
     Plaus    means Plausibility
     Rec      means Record
     Bpa      means Basic Probability Assignment
*/
short           Quick_Test;
short           Error_Found_Flag;
short           Debug_Test = False;
int             Number_Of_Hypot;
int             True_Count[A_Max];
int             Stored_Hypot_Count;
int             Added_Hypot;
int             Subtracted_Hypot;
float           Temp_Total;
float           EtoH[Max_Hypot_Used][Max_Group_Level];
float           HtoA[Max_Assoc_Used][Max_Hypot_Used];

struct Evid_Bpa_Rec            Input_Hypot[3];
struct Evid_Bpa_Rec            Evid_Bpa_Rec_Array[A_Max];

struct Group_Evid_Rec          Group_Evid_Rec_Array[Max_Group_Level][Max_Group_Id];
struct Group_Evid_Rec          History[Max_Assoc_Used];
struct Retrieved_Names_Record  Name_List[Number_Names];

void Process_EHA (void);
void Fusion_Prt  (int, int, float);
void NewGrp_Prt  (int, int, int);
void In_Bpa      (void);
void Norm_Bpa    (void);
void Pla_Hyp     (void);
void Bel_Hyp     (void);
void Raw_Bpa     (int, int);


void DS_Algo (int Num_O_Name, float Bpa_4_Name, float Time_Entry)
{
  int         Ix, Jx;
  float       EtoH_A[Max_Hypot_Used][Max_Group_Level] = 
                 {{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
                  {0.0, 0.7, 0.7, 1.0, 1.0, 0.7, 1.0},
                  {0.0, 0.0, 0.4, 1.0, 1.0, 0.0, 0.0},
                  {0.0, 1.0, 1.0, 0.5, 0.5, 1.0, 1.0},
                  {0.0, 1.0, 1.0, 0.5, 0.5, 1.0, 1.0} };

  float       HtoA_A[Max_Assoc_Used][Max_Hypot_Used] =
                 {{0.0, 0.0, 0.0, 0.0, 0.0},
                  {0.0, 0.8, 0.9, 1.0, 0.3},
                  {0.0, 0.0, 1.0, 0.0, 1.0},
                  {0.0, 0.5, 0.7, 0.0, 1.0} };


   for (Ix = 0; Ix < Max_Hypot_Used; Ix++)
   {
     for (Jx = 0; Jx < Max_Group_Level; Jx++)
       EtoH[Ix][Jx] = EtoH_A[Ix][Jx];
   }

   for (Ix = 0; Ix < Max_Assoc_Used; Ix++)
   {
     for (Jx = 0; Jx < Max_Hypot_Used; Jx++)
       HtoA[Ix][Jx] = HtoA_A[Ix][Jx];
   }

   Stored_Hypot_Count = 1 + Group_Evid_Rec_Array[Num_O_Name][3].Group_Count;

   if (Stored_Hypot_Count < A_Max)
   {
        /* Update number of evidence entries in Group Array [][][]. */
     Group_Evid_Rec_Array[Num_O_Name][3].Group_Count += 1;
   }
   else
   {
     printf ("Group Evid_Info full, name table ok, data not saved.\n");
     Stored_Hypot_Count -= 1;
   }

      /* Enter name number, Bpa, time to Group Array; & other values. */
   Group_Evid_Rec_Array[Num_O_Name][3].
                           Evid_Info[Stored_Hypot_Count].Evid_Name =
                                               Name_List[Num_O_Name + 3].Number;
   Group_Evid_Rec_Array[Num_O_Name][3].
                              Evid_Info[Stored_Hypot_Count].Bpa_In = Bpa_4_Name;
   Group_Evid_Rec_Array[Num_O_Name][3].
                         Evid_Info[Stored_Hypot_Count].SecPastEpoc = Time_Entry;
   Group_Evid_Rec_Array[Num_O_Name][3].
                         Evid_Info[Stored_Hypot_Count].Array_Count = 1;
   Group_Evid_Rec_Array[Num_O_Name][3].
                          Evid_Info[Stored_Hypot_Count].Bpa_Number = 0.0;
   Group_Evid_Rec_Array[Num_O_Name][3].
                          Evid_Info[Stored_Hypot_Count].Bpa_Belief = 0.0;
   Group_Evid_Rec_Array[Num_O_Name][3].
                           Evid_Info[Stored_Hypot_Count].Bpa_Plaus = 0.0;

      /* Run an update to the EHA Process. */

   if (Debug_Test == True)
     NewGrp_Prt (Num_O_Name, 3, Stored_Hypot_Count);
   Process_EHA();

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Process_EHA (void)
{
  short   Have_Evid;
  int     E_Ix, H_Ix, A_Ix;
  int     T_Ix;
  int     Hist_Count;
  float   Max_Time;
  float   Sum;
  float   Norm_Count;

  Have_Evid = False;
  for (E_Ix = 1; E_Ix < Max_Group_Level; E_Ix++)
  {
       /* Only fuse if there is new evidence. */
    if (Group_Evid_Rec_Array[E_Ix][3].Group_Count              <=
        Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].Array_Count    )

      Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].Bpa_In      = -2.0;
      
    else
    {
      Have_Evid        = True;
      Quick_Test       = False;
      Error_Found_Flag = False;

      Stored_Hypot_Count = Group_Evid_Rec_Array[E_Ix][3].Group_Count;
      Raw_Bpa   (E_Ix, 3);

      Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].Array_Count =
                                                             Stored_Hypot_Count;
      Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].Evid_Name   =
                                                Evid_Bpa_Rec_Array[3].Evid_Name;
      Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].Bpa_In      = 2.0;
      Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].Bpa_Number  =
                                               Evid_Bpa_Rec_Array[3].Bpa_Number;
      Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].Bpa_Belief  =
                                               Evid_Bpa_Rec_Array[3].Bpa_Belief;
      Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].Bpa_Plaus   =
                                                Evid_Bpa_Rec_Array[3].Bpa_Plaus;

      Max_Time = -1.0;
      for (T_Ix = 1; T_Ix <= Stored_Hypot_Count; T_Ix++)
      {
        if (Max_Time < Group_Evid_Rec_Array[E_Ix][3].Evid_Info[T_Ix].
                                                                   SecPastEpoc)
          Max_Time = Group_Evid_Rec_Array[E_Ix][3].Evid_Info[T_Ix].SecPastEpoc;
      }
      Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].SecPastEpoc = Max_Time;

      if (Debug_Test == True)
        Fusion_Prt(3, Stored_Hypot_Count, Max_Time);
    }
  }

  if (Have_Evid == False)
    printf ("     No new evidence.\n\n");
  else
  {
       /* Matrix multiply to get a new hypotheses data set. */
    for (H_Ix = 1; H_Ix < Max_Hypot_Used; H_Ix++)
    {
      Norm_Count =  0.0;
      Max_Time   = -1.0;
      Sum        =  0.0;
      for (E_Ix = 1; E_Ix < Max_Group_Level; E_Ix++)
      {
           /* Use if array element not zero and this evidence is new. */
        if (EtoH[H_Ix][E_Ix]                                  > 0.0  && 
            Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].Bpa_In > 0.0     )
        {
          Sum += EtoH[H_Ix][E_Ix] * Group_Evid_Rec_Array[E_Ix][3].
                                                        Evid_Info[0].Bpa_Number;
          Norm_Count += 1.0;
          if (Max_Time < Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].SecPastEpoc)
            Max_Time = Group_Evid_Rec_Array[E_Ix][3].Evid_Info[0].SecPastEpoc;
        }
      }
      if (Norm_Count > 0.5)
      {
           /* Normalize the Bpa and save the data set. */
        Sum = Sum / Norm_Count;

        Stored_Hypot_Count = 1 + Group_Evid_Rec_Array[H_Ix][2].Group_Count;
         if (Stored_Hypot_Count < A_Max)
         {
             /* Update number of hypotheses entries in Group Array [][][]. */
           Group_Evid_Rec_Array[H_Ix][2].Group_Count += 1;
         }
         else
         {
           printf ("Group Evid_Info full, name table ok, data not saved.\n");
           Stored_Hypot_Count -= 1;
           break;
         }
              /* Enter name number, Bpa, time to Group Array; & other values. */
         Group_Evid_Rec_Array[H_Ix][2].
                             Evid_Info[Stored_Hypot_Count].Evid_Name =
                                                     Name_List[H_Ix + 9].Number;
         Group_Evid_Rec_Array[H_Ix][2].
                                Evid_Info[Stored_Hypot_Count].Bpa_In = Sum;
         Group_Evid_Rec_Array[H_Ix][2].
                           Evid_Info[Stored_Hypot_Count].SecPastEpoc = Max_Time;
         Group_Evid_Rec_Array[H_Ix][2].
                           Evid_Info[Stored_Hypot_Count].Array_Count = 1;
         Group_Evid_Rec_Array[H_Ix][2].
                            Evid_Info[Stored_Hypot_Count].Bpa_Number = 0.0;
         Group_Evid_Rec_Array[H_Ix][2].
                            Evid_Info[Stored_Hypot_Count].Bpa_Belief = 0.0;
         Group_Evid_Rec_Array[H_Ix][2].
                             Evid_Info[Stored_Hypot_Count].Bpa_Plaus = 0.0;

         if (Debug_Test == True)
           NewGrp_Prt (H_Ix, 2, Stored_Hypot_Count);
      }
    }

    for (H_Ix = 1; H_Ix < Max_Hypot_Used; H_Ix++)
    {
         /* Only fuse if there is new hypotheses. */
      if (Group_Evid_Rec_Array[H_Ix][2].Group_Count              <=
          Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].Array_Count    )

        Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].Bpa_In      = -2.0;
      
      else
      {
        Quick_Test       = False;
        Error_Found_Flag = False;

        Stored_Hypot_Count = Group_Evid_Rec_Array[H_Ix][2].Group_Count;
        Raw_Bpa   (H_Ix, 2);

        Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].Array_Count =
                                                             Stored_Hypot_Count;
        Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].Evid_Name   =
                                                Evid_Bpa_Rec_Array[3].Evid_Name;
        Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].Bpa_In      = 2.0;
        Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].Bpa_Number  =
                                               Evid_Bpa_Rec_Array[3].Bpa_Number;
        Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].Bpa_Belief  =
                                               Evid_Bpa_Rec_Array[3].Bpa_Belief;
        Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].Bpa_Plaus   =
                                                Evid_Bpa_Rec_Array[3].Bpa_Plaus;

        Max_Time = -1.0;
        for (T_Ix = 1; T_Ix <= Stored_Hypot_Count; T_Ix++)
        {
          if (Max_Time < Group_Evid_Rec_Array[H_Ix][2].Evid_Info[T_Ix].
                                                                   SecPastEpoc)
            Max_Time = Group_Evid_Rec_Array[H_Ix][2].Evid_Info[T_Ix].SecPastEpoc;
        }
        Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].SecPastEpoc = Max_Time;

        if (Debug_Test == True)
          Fusion_Prt(2, Stored_Hypot_Count, Max_Time);
      }
    }

       /* Matrix multiply to get a new assessment data set. */
    for (A_Ix = 1; A_Ix < Max_Assoc_Used; A_Ix++)
    {
      Norm_Count =  0.0;
      Max_Time   = -1.0;
      Sum        =  0.0;
      for (H_Ix = 1; H_Ix < Max_Hypot_Used; H_Ix++)
      {
           /* Use if array element not zero and this hypotheses is new. */
        if (HtoA[A_Ix][H_Ix]                                  > 0.0  && 
            Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].Bpa_In > 0.0     )
        {
          Sum += HtoA[A_Ix][H_Ix] * Group_Evid_Rec_Array[H_Ix][3].
                                                        Evid_Info[0].Bpa_Number;
          Norm_Count += 1.0;
          if (Max_Time < Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].SecPastEpoc)
            Max_Time = Group_Evid_Rec_Array[H_Ix][2].Evid_Info[0].SecPastEpoc;
        }
      }
      if (Norm_Count > 0.5)
      {
           /* Normalize the Bpa and save the data set. */
        Sum = Sum / Norm_Count;

        Stored_Hypot_Count = 1 + Group_Evid_Rec_Array[A_Ix][1].Group_Count;
         if (Stored_Hypot_Count < A_Max)
         {
             /* Update number of Assessment entries in Group Array [][][]. */
           Group_Evid_Rec_Array[A_Ix][1].Group_Count += 1;
         }
         else
         {
           printf ("Group Evid_Info full, name table ok, data not saved.\n");
           Stored_Hypot_Count -= 1;
           break;
         }
              /* Enter name number, Bpa, time to Group Array; & other values. */
         Group_Evid_Rec_Array[A_Ix][1].
                             Evid_Info[Stored_Hypot_Count].Evid_Name =
                                                    Name_List[A_Ix + 13].Number;
         Group_Evid_Rec_Array[A_Ix][1].
                                Evid_Info[Stored_Hypot_Count].Bpa_In = Sum;
         Group_Evid_Rec_Array[A_Ix][1].
                           Evid_Info[Stored_Hypot_Count].SecPastEpoc = Max_Time;
         Group_Evid_Rec_Array[A_Ix][1].
                           Evid_Info[Stored_Hypot_Count].Array_Count = 1;
         Group_Evid_Rec_Array[A_Ix][1].
                            Evid_Info[Stored_Hypot_Count].Bpa_Number = 0.0;
         Group_Evid_Rec_Array[A_Ix][1].
                            Evid_Info[Stored_Hypot_Count].Bpa_Belief = 0.0;
         Group_Evid_Rec_Array[A_Ix][1].
                             Evid_Info[Stored_Hypot_Count].Bpa_Plaus = 0.0;

         if (Debug_Test == True)
           NewGrp_Prt (A_Ix, 1, Stored_Hypot_Count);
      }
    }

    for (A_Ix = 1; A_Ix < Max_Assoc_Used; A_Ix++)
    {
         /* Only fuse if there is new Assessments. */
      if (Group_Evid_Rec_Array[A_Ix][1].Group_Count              <=
          Group_Evid_Rec_Array[A_Ix][1].Evid_Info[0].Array_Count    )

        Group_Evid_Rec_Array[A_Ix][1].Evid_Info[0].Bpa_In      = -2.0;
      
      else
      {
        Quick_Test       = False;
        Error_Found_Flag = False;

        Stored_Hypot_Count = Group_Evid_Rec_Array[A_Ix][1].Group_Count;
        Raw_Bpa   (A_Ix, 1);

        Group_Evid_Rec_Array[A_Ix][1].Evid_Info[0].Array_Count =
                                                             Stored_Hypot_Count;
        Group_Evid_Rec_Array[A_Ix][1].Evid_Info[0].Evid_Name   =
                                                Evid_Bpa_Rec_Array[3].Evid_Name;
        Group_Evid_Rec_Array[A_Ix][1].Evid_Info[0].Bpa_In      = 2.0;
        Group_Evid_Rec_Array[A_Ix][1].Evid_Info[0].Bpa_Number  =
                                               Evid_Bpa_Rec_Array[3].Bpa_Number;
        Group_Evid_Rec_Array[A_Ix][1].Evid_Info[0].Bpa_Belief  =
                                               Evid_Bpa_Rec_Array[3].Bpa_Belief;
        Group_Evid_Rec_Array[A_Ix][1].Evid_Info[0].Bpa_Plaus   =
                                                Evid_Bpa_Rec_Array[3].Bpa_Plaus;

        Max_Time = -1.0;
        for (T_Ix = 1; T_Ix <= Stored_Hypot_Count; T_Ix++)
        {
          if (Max_Time < Group_Evid_Rec_Array[A_Ix][1].Evid_Info[T_Ix].
                                                                   SecPastEpoc)
            Max_Time = Group_Evid_Rec_Array[A_Ix][1].Evid_Info[T_Ix].SecPastEpoc;
        }
        Group_Evid_Rec_Array[A_Ix][1].Evid_Info[0].SecPastEpoc = Max_Time;

        if (Debug_Test == True)
          Fusion_Prt(1, Stored_Hypot_Count, Max_Time);

           /* Store the latest data set for a history plot. */
        History[A_Ix].Group_Count += 1;
        Hist_Count = History[A_Ix].Group_Count;

        History[A_Ix].Evid_Info[Hist_Count].Array_Count = 1;
        History[A_Ix].Evid_Info[Hist_Count].Evid_Name   =
                                                Evid_Bpa_Rec_Array[3].Evid_Name;
        History[A_Ix].Evid_Info[Hist_Count].Bpa_In      = 0.0;
        History[A_Ix].Evid_Info[Hist_Count].Bpa_Number  =
                                               Evid_Bpa_Rec_Array[3].Bpa_Number;
        History[A_Ix].Evid_Info[Hist_Count].Bpa_Belief  =
                                               Evid_Bpa_Rec_Array[3].Bpa_Belief;
        History[A_Ix].Evid_Info[Hist_Count].Bpa_Plaus   =
                                                Evid_Bpa_Rec_Array[3].Bpa_Plaus;
        History[A_Ix].Evid_Info[Hist_Count].SecPastEpoc = Max_Time;
      }
    }

  }

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Fusion_Prt (int Group_Level, int Set_Count, float Most_Time)
{
   int   ix;
   int   ixf;
   short found;
   char  Name_Used[21];

   printf ("Index  Arrays            Evid_Name   Bpa_In    Bpa_Number    ");
   printf ("Bpa_Belief   Bpa_Plaus\n");
   for (ix = 1; ix <= (Set_Count + 2); ix++)
   {
      printf ("%4d %5d", ix, Evid_Bpa_Rec_Array[ix].Array_Count);

      found = False;
      for (ixf = 1; ixf < Number_Names; ixf++)
      {
        if (Evid_Bpa_Rec_Array[ix].Evid_Name == Name_List[ixf].Number)
        {
          printf ("    %20s", Name_List[ixf].Name);
          if (ix == 3)
            strcpy (Name_Used, Name_List[ixf].Name);
          found = True;
          break;
        }
      }

      if (found == False)
        printf ("%18d      ", Evid_Bpa_Rec_Array[ix].Evid_Name);

      printf ("  %f    ", Evid_Bpa_Rec_Array[ix].Bpa_In);

      printf ("%f      %f     %f\n", Evid_Bpa_Rec_Array[ix].Bpa_Number,
                                     Evid_Bpa_Rec_Array[ix].Bpa_Belief,
                                     Evid_Bpa_Rec_Array[ix].Bpa_Plaus  );
   }

   printf ("Fused values for %s ", Name_Used);
   if      (Group_Level == 3)
     printf ("Evidence, ");
   else if (Group_Level == 2)
     printf ("Hypotheses, ");
   else
     printf ("Assessment, ");

   printf ("%d data set(s) used, last time is %6.1f sec. \n\n",
                                                          Set_Count, Most_Time);

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void NewGrp_Prt (int GI, int GL, int Ix)
{
   int   ixf;
   short found;
   char  Name_Used[21];

   printf ("Index  Arrays            Evid_Name   Bpa_In    Bpa_Number    ");
   printf ("Bpa_Belief   Bpa_Plaus\n");
   printf ("%4d %5d", Ix,
                        Group_Evid_Rec_Array[GI][GL].Evid_Info[Ix].Array_Count);

   found = False;
   for (ixf = 1; ixf < Number_Names; ixf++)
   {
     if (Group_Evid_Rec_Array[GI][GL].
                              Evid_Info[Ix].Evid_Name == Name_List[ixf].Number)
     {
       printf ("    %20s", Name_List[ixf].Name);
       strcpy (Name_Used, Name_List[ixf].Name);
       found = True;
       break;
     }
   }

   if (found == False)
     printf ("%18d      ",
                          Group_Evid_Rec_Array[GI][GL].Evid_Info[Ix].Evid_Name);

   printf ("  %f    %f      %f     %f\n",
                        Group_Evid_Rec_Array[GI][GL].Evid_Info[Ix].Bpa_In,
                        Group_Evid_Rec_Array[GI][GL].Evid_Info[Ix].Bpa_Number,
                        Group_Evid_Rec_Array[GI][GL].Evid_Info[Ix].Bpa_Belief,
                        Group_Evid_Rec_Array[GI][GL].Evid_Info[Ix].Bpa_Plaus  );

   printf ("New data set for %s ", Name_Used);
   if      (GL == 3)
     printf ("Evidence, ");
   else if (GL == 2)
     printf ("Hypotheses, ");
   else
     printf ("Assessment, ");

   printf ("time is %6.1f sec. \n\n",
                        Group_Evid_Rec_Array[GI][GL].Evid_Info[Ix].SecPastEpoc);

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void In_Bpa (void)
{
  Evid_Bpa_Rec_Array[2].Evid_Name   = 10;
  Evid_Bpa_Rec_Array[1].Evid_Name   = 11;
  Evid_Bpa_Rec_Array[2].Bpa_Number  = 1.0;
  Evid_Bpa_Rec_Array[1].Bpa_Number  = 0.0;
  Evid_Bpa_Rec_Array[2].Array_Count = 1;
  Evid_Bpa_Rec_Array[1].Array_Count = 1;
  
  Evid_Bpa_Rec_Array[2].Bpa_Belief  = 0.0;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Norm_Bpa (void)
{
  int    Norm_Count;

  Temp_Total = 0.0;
  if (Debug_Test == True)
    printf ("  NORM_BPA   1  Null = %f.\n", Evid_Bpa_Rec_Array[1].Bpa_Number);

  for (Norm_Count = 2; Norm_Count <= Number_Of_Hypot; Norm_Count++)
  {
    Evid_Bpa_Rec_Array[Norm_Count].Bpa_Number =
                             Evid_Bpa_Rec_Array[Norm_Count].Bpa_Number /
                                       (1.0 - Evid_Bpa_Rec_Array[1].Bpa_Number);
    Temp_Total += Evid_Bpa_Rec_Array[Norm_Count].Bpa_Number;
    if (Debug_Test == True)
      printf ("  NORM_BPA %3d %5d = %f.\n", Norm_Count,
                                     Evid_Bpa_Rec_Array[Norm_Count].Evid_Name,
                                     Evid_Bpa_Rec_Array[Norm_Count].Bpa_Number);
  }

  Evid_Bpa_Rec_Array[1].Bpa_Number = 0.0;
  if (Debug_Test == True)
    printf ("  NORM_BPA     Total = %f.\n", Temp_Total);

     /* This section checks that the norm total is 1.0. */
  if (abs(Temp_Total - 1.0) > 1.0 )
    Error_Found_Flag = True;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Pla_Hyp (void)
{
  float  Temp_Number;
  int    Item_Count;
  int    Bpa_Count;

  for (Item_Count = 1; Item_Count <= Number_Of_Hypot; Item_Count++)
  {
    if (Debug_Test == True)
      printf ("Pla_Hyp Item_Count = %d.\n", Item_Count);
    if      (Evid_Bpa_Rec_Array[Item_Count].Evid_Name == 10)
      Evid_Bpa_Rec_Array[Item_Count].Bpa_Plaus = 1.0;
    else if (Evid_Bpa_Rec_Array[Item_Count].Evid_Name == 11)
      Evid_Bpa_Rec_Array[Item_Count].Bpa_Plaus = 0.0;
    else
    {
      Temp_Number = 0.0;
      for (Bpa_Count = Number_Of_Hypot; Bpa_Count >= 1; Bpa_Count--)
      {
        if (Debug_Test == True)
        {
          printf ("  Pla_Hyp Bpa_Count = %d.\n", Bpa_Count);
          printf ("  Evidence_Name     = %d.\n",
                                      Evid_Bpa_Rec_Array[Bpa_Count].Evid_Name);
        }
        if (Item_Count != Bpa_Count                        &&
            Evid_Bpa_Rec_Array[Bpa_Count].Evid_Name != 10       )
        {
          if (Debug_Test == True)
            printf ("    Before Temp_Number = %f.\n", Temp_Number);
          Temp_Number = Temp_Number + Evid_Bpa_Rec_Array[Bpa_Count].Bpa_Belief;
          if (Debug_Test == True)
            printf ("    After  Temp_Number = %f.\n", Temp_Number);
        }
      }
      Evid_Bpa_Rec_Array[Item_Count].Bpa_Plaus = 1.0 - Temp_Number;
    }
  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Bel_Hyp (void)
{
  int     Item_Count;

  for (Item_Count = 1; Item_Count <= Number_Of_Hypot; Item_Count++)
  {
    if (Debug_Test == True)
      printf ("Bel_Hyp Item_Count = %d.\n", Item_Count);
    if (Evid_Bpa_Rec_Array[Item_Count].Evid_Name == 10)
      Evid_Bpa_Rec_Array[Item_Count].Bpa_Belief = 1.0;
    else
      Evid_Bpa_Rec_Array[Item_Count].Bpa_Belief =
                              Evid_Bpa_Rec_Array[Item_Count].Bpa_Belief +
                              Evid_Bpa_Rec_Array[Item_Count].Bpa_Number;
  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Raw_Bpa (int Group_Id, int Group_Level)
{
  struct Evid_Bpa_Rec  Temp_Hypot[A_Max];
  int                  Matched_Count_Array[A_Max];
  short                Matched_Hypot_Array[A_Max];

  /* The following are "for" indices. */
  int   Display_Count;
  int   Hypot_Count;
  int   Input_Count;
  int   Raw_Count;
  int   Copy_Count;
  int   Item_Count;
  int   All_Count;
  int   Offset_Index;
  int   Idx;

  In_Bpa ();

  Number_Of_Hypot = 2;

  if (Debug_Test == True)
  {
    printf ("Null Name is %d, All Name is %d.\n",
                                              Evid_Bpa_Rec_Array[1].Evid_Name,
                                              Evid_Bpa_Rec_Array[2].Evid_Name );
    printf ("Group_Id = %d, Group_Level = %d.\n", Group_Id, Group_Level);
    printf ("Stored_Hypot_Count = %d.\n", Stored_Hypot_Count);
  }

   for (Display_Count = 1; Display_Count <= Stored_Hypot_Count; Display_Count++)
   {
     Added_Hypot    = 0;
     Input_Hypot[1] = Group_Evid_Rec_Array[Group_Id][Group_Level].
                                                       Evid_Info[Display_Count];
     Input_Hypot[2].Evid_Name   = 10;
     Input_Hypot[2].Array_Count = 1;
     Input_Hypot[2].Bpa_Number  = 1.0 - Input_Hypot[1].Bpa_In;
     Input_Hypot[2].Bpa_In      = 1.0 - Input_Hypot[1].Bpa_In;

        /* Hypotheses: */
     for (Hypot_Count = 1; Hypot_Count <= Number_Of_Hypot; Hypot_Count++)
     {
       Matched_Hypot_Array[Hypot_Count] = False;
       Temp_Hypot[Hypot_Count]          = Evid_Bpa_Rec_Array[Hypot_Count];

       if (Evid_Bpa_Rec_Array[Hypot_Count].Evid_Name ==
                                                       Input_Hypot[1].Evid_Name)
         Matched_Hypot_Array[Hypot_Count] = True;
     }

       /* Input_Count 1 and 2 are for New_Hypot and top row of matrix. */
     if (Debug_Test == True)
     {
       for (Idx = 1; Idx <= Number_Of_Hypot; Idx++)
         printf ("Raw Bpa for %d = %f.\n", Evid_Bpa_Rec_Array[Idx].Evid_Name,
                                           Evid_Bpa_Rec_Array[Idx].Bpa_Number);
     }

     for (Input_Count = 1; Input_Count <= 2; Input_Count++)
     {
       for (Raw_Count = 1; Raw_Count <= Number_Of_Hypot; Raw_Count++)
       {
         if      (Input_Hypot[Input_Count].Evid_Name != 10)
         {
           if      (Evid_Bpa_Rec_Array[Raw_Count].Evid_Name == 10)
           {
             if (Input_Count != 2)
             {
               Added_Hypot += 1;
               Offset_Index = Number_Of_Hypot + Added_Hypot;
               Evid_Bpa_Rec_Array[Offset_Index].Array_Count =
                                           Input_Hypot[Input_Count].Array_Count;
               Evid_Bpa_Rec_Array[Offset_Index].Evid_Name   =
                                             Input_Hypot[Input_Count].Evid_Name;
               Evid_Bpa_Rec_Array[Offset_Index].Bpa_In      = 0.0;
               Evid_Bpa_Rec_Array[Offset_Index].Bpa_Number  =
                                 Temp_Hypot[Raw_Count].Bpa_Number *
                                                Input_Hypot[Input_Count].Bpa_In;
               Evid_Bpa_Rec_Array[Offset_Index].Bpa_Belief  = 0.0;
               Evid_Bpa_Rec_Array[Offset_Index].Bpa_Plaus   = 0.0;
             }
             else
               Evid_Bpa_Rec_Array[2].Bpa_Number =
                             Temp_Hypot[Raw_Count].Bpa_Number *
                                                Input_Hypot[Input_Count].Bpa_In;
           }
           else if (Evid_Bpa_Rec_Array[Raw_Count].Evid_Name == 11 ||
                    Matched_Hypot_Array[Raw_Count] == False          )
           {
             if (Debug_Test == True)
               printf ("Null before = %f.\n", Evid_Bpa_Rec_Array[1].Bpa_Number);
             Evid_Bpa_Rec_Array[1].Bpa_Number +=
                               Temp_Hypot[Raw_Count].Bpa_Number *
                                                Input_Hypot[Input_Count].Bpa_In;
             if (Debug_Test == True)
               printf ("Null after  = %f.\n", Evid_Bpa_Rec_Array[1].Bpa_Number);
           }
           else
           {
             Added_Hypot += 1;
             Offset_Index = Number_Of_Hypot + Added_Hypot;
             Evid_Bpa_Rec_Array[Offset_Index].Array_Count = 1;
             Evid_Bpa_Rec_Array[Offset_Index].Evid_Name   =
                                                Temp_Hypot[Raw_Count].Evid_Name;
             Evid_Bpa_Rec_Array[Offset_Index].Bpa_In      = 0.0;
             Evid_Bpa_Rec_Array[Offset_Index].Bpa_Number  =
                                Temp_Hypot[Raw_Count].Bpa_Number *
                                                Input_Hypot[Input_Count].Bpa_In;
             Evid_Bpa_Rec_Array[Offset_Index].Bpa_Belief  = 0.0;
             Evid_Bpa_Rec_Array[Offset_Index].Bpa_Plaus   = 0.0;
           }
         }
         else if (Raw_Count != 1)
           Evid_Bpa_Rec_Array[Raw_Count].Bpa_Number =
                              Temp_Hypot[Raw_Count].Bpa_Number *
                                                Input_Hypot[Input_Count].Bpa_In;
       }
     }
     Number_Of_Hypot += Added_Hypot;

       /* This section checks for alike Bpa names and combines them. */

     Subtracted_Hypot = 0;
     for (Copy_Count = 1; Copy_Count <= Number_Of_Hypot; Copy_Count++)
       Temp_Hypot[Copy_Count] = Evid_Bpa_Rec_Array[Copy_Count];

     if (Debug_Test == True)
       printf ("Null before = %f.\n", Temp_Hypot[1].Bpa_Number);

     for (Item_Count = 1; Item_Count <= Number_Of_Hypot; Item_Count++)
     {
       for (All_Count = 1; All_Count <= Number_Of_Hypot; All_Count++)
       {
         if (Evid_Bpa_Rec_Array[All_Count].Evid_Name != 12)
         {
           Matched_Count_Array[All_Count] = 0;
           if (Debug_Test == True)
             printf ("Names - %d, %d - %d, %d.\n",
                                        Temp_Hypot[Item_Count].Evid_Name,
                                 Evid_Bpa_Rec_Array[All_Count].Evid_Name,
                                                         Item_Count, All_Count);

           if (Temp_Hypot[Item_Count].Evid_Name        ==
               Evid_Bpa_Rec_Array[All_Count].Evid_Name    &&
                               Item_Count != All_Count       )
           {
             Matched_Count_Array[All_Count] += 1;
             /* Exit For */
           }
           if (Debug_Test == True)
             printf ("Match Count %d - %d.\n",
                             Matched_Count_Array[All_Count], Subtracted_Hypot);

           if (Matched_Count_Array[All_Count]             ==
               Evid_Bpa_Rec_Array[All_Count ].Array_Count    && 
               Matched_Count_Array[All_Count]             ==
               Evid_Bpa_Rec_Array[Item_Count].Array_Count        ) 
           {
             /* deleted item. */
             if (Debug_Test == True)
               printf ("During = %d - %d.\n", All_Count, Item_Count);

             Temp_Hypot[Item_Count].Bpa_Number      +=
                                      Evid_Bpa_Rec_Array[All_Count].Bpa_Number;
             Evid_Bpa_Rec_Array[All_Count].Evid_Name = 12;
             Temp_Hypot[All_Count].Evid_Name         = 12;
             Subtracted_Hypot                       += 1;
           }
         }
       }
     }

     if (Debug_Test == True)
     {
       printf ("Null after  = %f.\n", Temp_Hypot[1].Bpa_Number);
       printf ("Number of %d - %d.\n", Number_Of_Hypot, Subtracted_Hypot);
     }
     Number_Of_Hypot -= Subtracted_Hypot;
     if (Debug_Test == True)
       printf ("Null before = %f.\n", Evid_Bpa_Rec_Array[1].Bpa_Number);

     for (Item_Count = 1; Item_Count <= Number_Of_Hypot; Item_Count++)
       Evid_Bpa_Rec_Array[Item_Count] = Temp_Hypot[Item_Count];

     if (Debug_Test == True)
       printf ("Null after  = %f.\n", Evid_Bpa_Rec_Array[1].Bpa_Number);

     /* End alike names. */

     if (Debug_Test == True)
     {
       for (Idx = 1; Idx <= Number_Of_Hypot; Idx++)
         printf ("Raw Bpa = %f.\n", Evid_Bpa_Rec_Array[Idx].Bpa_Number);
     }

     Norm_Bpa ();
   }

   if      (Error_Found_Flag == False && Quick_Test == False) 
   {
     Bel_Hyp ();
     Pla_Hyp ();
   }
   else if (Error_Found_Flag == True && Quick_Test == True) 
     Error_Found_Flag = False;

}

void
DS_AlgoGetResult(int ix, int iy, float results[])
{
/*   ix       input      Evidence index
 *   iy       input      Return results wanted
 *                        = 3 => Evidence
 *                        = 2 => Hypothesis
 *                        = 1 => Assessment
 *   results  output     Calculated results
 */

   results[0] = Group_Evid_Rec_Array[ix][iy].Evid_Info[0].Bpa_In;
   results[1] = Group_Evid_Rec_Array[ix][iy].Evid_Info[0].Bpa_Number;
   results[2] = Group_Evid_Rec_Array[ix][iy].Evid_Info[0].Bpa_Belief;
   results[3] = Group_Evid_Rec_Array[ix][iy].Evid_Info[0].Bpa_Plaus;
   results[4] = Group_Evid_Rec_Array[ix][iy].Evid_Info[0].SecPastEpoc;
}

void
DS_AlgoInit()
{
   char    Data_Source = 'a';
   int     Num_O_Name;
   int     Group_Id, Group_Level;
   int     Gp_Id_Idx, Gp_Lvl_Ix;
   int     Ix, Iy;
   float   Bpa_4_Name;
   float   Time_Entry;
   char    N_Chr[Number_Names][21] = {"N/A", "All", "Null", "12", "SBIRS",
                                      "RECON", "Political", "Intel", "Radar",
                                      "FireControl", "NewForeignLaunch",
                                      "Anticipated", "FriendlyImpact",
                                      "SpaceLaunch", "Hostile", "Deliberate",
                                      "Space"                                 };

   int     N_Num[Number_Names] = { 0,  10,  11,  12, 101, 102, 103, 104, 105,
                                      106, 201, 202, 203, 204, 301, 302, 303  };

   int     N_Len[Number_Names] = { 3,  3,  4,  2,  5,  5,  8,  5,  5, 11, 16,
                                                      11, 14, 11,  7, 10,  5  };

   for (Ix = 0; Ix < Number_Names; Ix++)
   {
     N_Len[Ix] = strlen(N_Chr[Ix]);
     strcpy (Name_List[Ix].Name, N_Chr[Ix]);
     Name_List[Ix].Number = N_Num[Ix];
     Name_List[Ix].Length = N_Len[Ix];
   }

   for (Gp_Id_Idx = 1; Gp_Id_Idx < Max_Group_Id; Gp_Id_Idx++)
   {
     for (Gp_Lvl_Ix = 1; Gp_Lvl_Ix < Max_Group_Level; Gp_Lvl_Ix++)
     {
       Group_Evid_Rec_Array[Gp_Id_Idx][Gp_Lvl_Ix].Group_Count   = 0;
       Group_Evid_Rec_Array[Gp_Id_Idx][Gp_Lvl_Ix].Evid_Info[0].
                                                    Array_Count = 0;
       Group_Evid_Rec_Array[Gp_Id_Idx][Gp_Lvl_Ix].Evid_Info[0].
                                                   Bpa_Number = 0.0;
     }
   }

   for (Iy = 1; Iy < Max_Assoc_Used; Iy++) {
      for (Ix=0; Ix<A_Max; Ix++) {
         History[Iy].Evid_Info[Ix].Array_Count = 0;
         History[Iy].Evid_Info[Ix].Evid_Name = 0;
         History[Iy].Evid_Info[Ix].Bpa_In = 0.0;
         History[Iy].Evid_Info[Ix].Bpa_Number = 0.0;
         History[Iy].Evid_Info[Ix].Bpa_Belief = 0.0;
         History[Iy].Evid_Info[Ix].Bpa_Plaus = 0.0;
         History[Iy].Evid_Info[Ix].SecPastEpoc = 0.0;
     }
     History[Iy].Group_Count = 0;
   }

   Added_Hypot = 0;
   Subtracted_Hypot = 0;
   Stored_Hypot_Count = 0;
   Number_Of_Hypot = 0;

   Quick_Test = 0;
   Error_Found_Flag = 0;
   Debug_Test = 0;

   Temp_Total = 0.0;

   for (Ix=0; Ix<A_Max; Ix++) True_Count[Ix] = 0;

   for (Ix=0; Ix<Max_Assoc_Used; Ix++)
      for (Iy=0; Iy<Max_Hypot_Used; Iy++)
         HtoA[Ix][Iy] = 0.0;

   for (Ix=0; Ix<Max_Hypot_Used; Ix++)
      for (Iy=0; Iy<Max_Group_Level; Iy++)
         EtoH[Ix][Iy] = 0.0;

   for (Ix=0; Ix<3; Ix++) {
      Input_Hypot[Ix].Array_Count = 0;
      Input_Hypot[Ix].Evid_Name = 0;
      Input_Hypot[Ix].Bpa_In = 0.0;
      Input_Hypot[Ix].Bpa_Number = 0.0;
      Input_Hypot[Ix].Bpa_Belief = 0.0;
      Input_Hypot[Ix].Bpa_Plaus = 0.0;
      Input_Hypot[Ix].SecPastEpoc = 0.0;
   }

   for (Ix=0; Ix<A_Max; Ix++) {
      Evid_Bpa_Rec_Array[Ix].Array_Count = 0;
      Evid_Bpa_Rec_Array[Ix].Evid_Name = 0;
      Evid_Bpa_Rec_Array[Ix].Bpa_In = 0.0;
      Evid_Bpa_Rec_Array[Ix].Bpa_Number = 0.0;
      Evid_Bpa_Rec_Array[Ix].Bpa_Belief = 0.0;
      Evid_Bpa_Rec_Array[Ix].Bpa_Plaus = 0.0;
      Evid_Bpa_Rec_Array[Ix].SecPastEpoc = 0.0;
   }

}
