#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "n2c_names.h"

int  Num_2_Char (char *Node_Na, int *Node_Nu, char *Doer_Na, int *Doer_Nu, int *Number)
{

   int			rtrn;
   int			mm, hh, tt, uu;

   rtrn = 0;

   if (*Number < 10)
      {
         if (*Number < 0 || *Number > 1)
            rtrn = 1;
         else
            {
               strcpy (Node_Na, NBtlGuy[*Number]);
               *Node_Nu = 0;
               strcpy (Doer_Na, "   ");
               *Doer_Nu = 0;
            }
      }
   else
      {
         uu = *Number % 10;
         tt = *Number / 10;
         hh = tt / 10;
         tt = tt - hh * 10;
         mm = hh / 10;
         hh = hh - mm * 10;

         if (mm < 0 || mm > 3)
            rtrn = 2;
         else if (mm == 0 && (tt < 0 || tt > 1))
            rtrn = 3;
         else if (tt < 0 || tt > 3)
            rtrn = 4;
         else
            {
               strcpy (Node_Na, NNodes[mm]);

               if (mm == 0)
                  strcpy (Doer_Na, NTop_Doers[tt]);
               else
                  strcpy (Doer_Na, NMid_Doers[tt]);

               *Node_Nu = hh;
               *Doer_Nu = uu;
            }
         }
   return rtrn;

}

