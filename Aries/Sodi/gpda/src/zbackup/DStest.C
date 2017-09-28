#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DStypes.h"

void Results_Prt  (void);
void History_Prt  (void);


main ()
{
   char    Data_Source = 'a';
   int     Num_O_Name;
   int     Group_Id, Group_Level;
   int     Gp_Id_Idx, Gp_Lvl_Ix;
   int     Ix;
   float   Bpa_4_Name;
   float   Time_Entry;

   DS_AlgoInit();

      /* Decide whether to use Canned or Dynamically entered data. */
   while (Data_Source != 'C' && Data_Source != 'D')
   {
     printf ("Enter D for dynamically enterd data, C for canned.  ");
     scanf ("%c", &Data_Source);
     if      (Data_Source == 'c')
       Data_Source = 'C';
     else if (Data_Source == 'd')
       Data_Source = 'D';
   }

   if (Data_Source == 'C')
   {
      /* Have decided to use Canned Data. */

       /* printf ("\nUpdate at    3.7 seconds.\n\n"); */
     DS_Algo (1, 0.95, 3.7);

       /* printf ("\nUpdate at   34.3 seconds.\n\n"); */
     DS_Algo (3, 0.75, 34.3);

       /* printf ("\nUpdate at   56.4 seconds.\n\n"); */
     DS_Algo (1, 0.65, 56.4);

       /* printf ("\nUpdate at   78.3 seconds.\n\n"); */
     DS_Algo (5, 0.85, 78.3);

       /* printf ("\nUpdate at   80.2 seconds.\n\n"); */
     DS_Algo (2, 0.65, 80.2);

       /* printf ("\nUpdate at  132.9 seconds.\n\n"); */
     DS_Algo (5, 0.72, 132.9);

       /* printf ("\nUpdate at  171.9 seconds.\n\n"); */
     DS_Algo (5, 0.67, 171.9);

       /* printf ("\nUpdate at  174.1 seconds.\n\n"); */
     DS_Algo (4, 0.56, 174.1);

/* Data sets, for future use.

        printf ("\nUpdate at  238.7 seconds.\n\n");
     DS_Algo (6, 0.25, 238.7);

       printf ("\nUpdate at  272.4 seconds.\n\n");
     DS_Algo (2, 0.30, 272.4);

       printf ("\nUpdate at  343.7 seconds.\n\n");
     DS_Algo (3, 0.90, 343.7);

       printf ("\nUpdate at  343.7 seconds.\n\n");
     DS_Algo (4, 0.73, 343.7);

       printf ("\nUpdate at  462.2 seconds.\n\n");
     DS_Algo (4, 0.97, 462.2);

       printf ("\nUpdate at  473.0 seconds.\n\n");
     DS_Algo (6, 0.55, 473.0);

       printf ("\nUpdate at  526.2 seconds.\n\n");
     DS_Algo (3, 0.92, 526.2);

       printf ("\nUpdate at  654.3 seconds.\n\n");
     DS_Algo (6, 0.65, 654.3);

       printf ("\nUpdate at  754.1 seconds.\n\n");
     DS_Algo (2, 0.89, 754.1);

       printf ("\nUpdate at 1477.6 seconds.\n\n");
     DS_Algo (1, 0.70, 1477.6);
*/

   }

   if (Data_Source == 'D')
   {
      /* Decided to use Dynamically entered data. */

     while (Data_Source != 'Q')
     {

       /* Enter data dynamitaclly. */

       Num_O_Name = 0;
       while (Num_O_Name < 1 || Num_O_Name >= Max_Group_Level)
       {
         printf ("Enter the number of the following evidence data to add:\n");
         printf ("1 = SBIRS, 2 = RECON, 3 = Political,\n");
         printf ("4 = Intel, 5 = Radar, 6 = FireControl.                  ");
         scanf ("%d", &Num_O_Name);
       }

       Bpa_4_Name = 2.0;
       while (Bpa_4_Name < 0.0 || Bpa_4_Name > 1.0)
       {
         printf ("Enter the Bpa value (range 0.0 to 1.0).                 ");
         scanf ("%f", &Bpa_4_Name);
       }

       Time_Entry = -1.0;
       while (Time_Entry < 0.0)
       {
         printf ("Enter the number of seconds after epoch for this data.  ");
         scanf ("%f", &Time_Entry);
       }

          /* Add another evidence data set, run the EHA process. */

       printf ("\n");
       DS_Algo (Num_O_Name, Bpa_4_Name, Time_Entry);

       Data_Source = 'a';
       while (Data_Source != 'Q' && Data_Source != 'N')
       {
         printf ("Enter Q to Quit, N for New evidence data.  ");
         /* scanf ("%c", &Data_Source); */
         Data_Source = getchar();
         Data_Source = getchar();
         if      (Data_Source == 'q')
	   Data_Source = 'Q';
         else if (Data_Source == 'n')
           Data_Source = 'N';
       }
     }
   }
   Results_Prt ();
   History_Prt ();

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void Results_Prt (void)
{
   int   ix;
   int   ixf;
   short Found;

   printf ("\n\n                  Current fused values for Evidence.\n");

   printf ("Arrays        Evidence_Name   Fused   Bpa_Number   ");
   printf ("Bpa_Belief   Bpa_Plaus   Time\n");
   for (ix = 1; ix < Max_Group_Level; ix++)
   {
      printf ("%4d", Group_Evid_Rec_Array[ix][3].Evid_Info[0].Array_Count);

      Found = False;
      for (ixf = 1; ixf < Number_Names; ixf++)
      {
        if (Group_Evid_Rec_Array[ix][3].Evid_Info[0].Evid_Name ==
                                                         Name_List[ixf].Number)
        {
          printf ("   %20s", Name_List[ixf].Name);
          Found = True;
          break;
        }
      }

      if (Found == False)
        printf ("   %20s", "Unused");

      if (Group_Evid_Rec_Array[ix][3].Evid_Info[0].Bpa_In > 0.0)
        printf ("    Yes");
      else
        printf ("     No");

      printf ("     %f     %f    %f  %6.1f\n",
                         Group_Evid_Rec_Array[ix][3].Evid_Info[0].Bpa_Number,
                         Group_Evid_Rec_Array[ix][3].Evid_Info[0].Bpa_Belief,
                         Group_Evid_Rec_Array[ix][3].Evid_Info[0].Bpa_Plaus,
                         Group_Evid_Rec_Array[ix][3].Evid_Info[0].SecPastEpoc );
   }

   printf ("\n                  Current fused values for Hypotheses.\n");

   printf ("Arrays      Hypotheses_Name   Fused   Bpa_Number   ");
   printf ("Bpa_Belief   Bpa_Plaus   Time\n");
   for (ix = 1; ix < Max_Hypot_Used; ix++)
   {
      printf ("%4d", Group_Evid_Rec_Array[ix][2].Evid_Info[0].Array_Count);

      Found = False;
      for (ixf = 1; ixf < Number_Names; ixf++)
      {
        if (Group_Evid_Rec_Array[ix][2].Evid_Info[0].Evid_Name ==
                                                         Name_List[ixf].Number)
        {
          printf ("   %20s", Name_List[ixf].Name);
          Found = True;
          break;
        }
      }

      if (Found == False)
        printf ("   %20s", "Unused");

      if (Group_Evid_Rec_Array[ix][2].Evid_Info[0].Bpa_In > 0.0)
        printf ("    Yes");
      else
        printf ("     No");

      printf ("     %f     %f    %f  %6.1f\n",
                         Group_Evid_Rec_Array[ix][2].Evid_Info[0].Bpa_Number,
                         Group_Evid_Rec_Array[ix][2].Evid_Info[0].Bpa_Belief,
                         Group_Evid_Rec_Array[ix][2].Evid_Info[0].Bpa_Plaus,
                         Group_Evid_Rec_Array[ix][2].Evid_Info[0].SecPastEpoc );
   }

   printf ("\n                  Current fused values for Assessment.\n");

   printf ("Arrays      Assessment_Name   Fused   Bpa_Number   ");
   printf ("Bpa_Belief   Bpa_Plaus   Time\n");
   for (ix = 1; ix < Max_Assoc_Used; ix++)
   {
      printf ("%4d", Group_Evid_Rec_Array[ix][1].Evid_Info[0].Array_Count);

      Found = False;
      for (ixf = 1; ixf < Number_Names; ixf++)
      {
        if (Group_Evid_Rec_Array[ix][1].Evid_Info[0].Evid_Name ==
                                                         Name_List[ixf].Number)
        {
          printf ("   %20s", Name_List[ixf].Name);
          Found = True;
          break;
        }
      }

      if (Found == False)
        printf ("   %20s", "Unused");

      if (Group_Evid_Rec_Array[ix][1].Evid_Info[0].Bpa_In > 0.0)
        printf ("    Yes");
      else
        printf ("     No");

      printf ("     %f     %f    %f  %6.1f\n",
                         Group_Evid_Rec_Array[ix][1].Evid_Info[0].Bpa_Number,
                         Group_Evid_Rec_Array[ix][1].Evid_Info[0].Bpa_Belief,
                         Group_Evid_Rec_Array[ix][1].Evid_Info[0].Bpa_Plaus,
                         Group_Evid_Rec_Array[ix][1].Evid_Info[0].SecPastEpoc );
   }

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

void History_Prt (void)
{
   int   Ix, Jx;
   int   ixf;
   char  Name_Used[21];

   printf ("\n");
   for (Ix = 1; Ix < Max_Assoc_Used; Ix++)
   {
     printf ("              History data sets for ");
     for (ixf = (Number_Names - 3); ixf < Number_Names; ixf++)
     {
       if (History[Ix].Evid_Info[1].Evid_Name == Name_List[ixf].Number)
       {
         printf ("%20s Assessment.\n", Name_List[ixf].Name);
         strcpy (Name_Used, Name_List[ixf].Name);
         break;
       }
     }
     printf ("Index      Assessment _Name    Bpa_Number    ");
     printf ("Bpa_Belief   Bpa_Plaus    Time\n");

     for (Jx = 1; Jx <= History[Ix].Group_Count; Jx++)
     {
       printf ("%4d ", Jx);
       printf ("  %20s ", Name_Used);


       printf ("    %f      %f     %f  %6.1f\n",
                        History[Ix].Evid_Info[Jx].Bpa_Number,
                        History[Ix].Evid_Info[Jx].Bpa_Belief,
                        History[Ix].Evid_Info[Jx].Bpa_Plaus,
                        History[Ix].Evid_Info[Jx].SecPastEpoc);
     }
     printf ("\n");
   }

}

