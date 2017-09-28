#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "c2n_names.h"

int  Char_2_Num (char *Node_Na, int *Node_Nu, char *Doer_Na, int *Doer_Nu, int *Number)
{

   int			mm, hh, tt, uu;

   for (mm = 0; mm < 2; mm++)
      {
         if (strcmp (Node_Na, CBtlGuy[mm]) == 0)
            {
               *Number = mm;
               return 0;
            }
      }

   hh = 9;
   for (mm = 0; mm < 4; mm++)
      {
         if (strcmp (Node_Na, CNodes[mm]) == 0)
            {
               hh = mm;
               break;
            }
      }

   if (hh == 9)
      return 10;

   hh = 9;
   if (mm == 0)
      {
         for (tt = 0; tt < 2; tt++)
            {
               if (strcmp (Doer_Na, CTop_Doers[tt]) == 0)
                  {
                     hh = tt;
                     break;
                  }
            }
        if (hh == 9)
           return 11;
      }
   else
      {
         for (tt = 0; tt < 4; tt++)
            {
               if (strcmp (Doer_Na, CMid_Doers[tt]) == 0)
                  {
                     hh = tt;
                     break;
                  }
            }
        if (hh == 9)
           return 12;
      }

   if (*Node_Nu < 0 || *Node_Nu > 9)
      return 13;
   else
      hh = *Node_Nu;

   if (*Doer_Nu < 0 || *Doer_Nu > 9)
      return 14;
   else
      uu = *Doer_Nu;

   *Number = 1000 * mm + hh * 100 + tt * 10 + uu;
   return 0;

}

