#include "clips.h"
#include <stdio.h>
#include <math.h>

double replan ()
{
  int targets, weapons, count, i, first, salvo, M;
  double pk, sum, product, q, coefficient;

  if (ArgCountCheck ("replan", EXACTLY, 5) == -1)
    {
     printf ("Error in passing variables to replan.\n"); 
     return (-1.0);
     
    }

  printf ("Number of variables passed ok.\n");
 
  targets = RtnLong (1);
  pk = RtnDouble (2);
  weapons = RtnLong (3);
  first = RtnLong (4);
  salvo = RtnLong (5);
  
  printf ("The variables passed to replan are: targets %d\t pk %f wa %d.\n", targets, pk, weapons); 


  if (((weapons + 1 - targets) % salvo) != 0)
    {
      M = (weapons + 1 - targets) / salvo + 1;
      count = targets - M;
      printf ("The number count is %d.\n", count);
    }  
  else
    {
      M = (weapons + 1 - targets) / salvo;
      count = targets - M;
      printf ("The number count is %d.\n", count);
    }  

  if (M < 0)
   {
     printf ("There are not enough GBI's for a single miss.  Better Replan.\n");
     return (1.0);
   }  
  else
    {
      sum = 0.0;
      q = 1.0 - pk;
      coefficient = 1;
      product = 1;
      if (pk != 0)
	product = pow (q, targets + 1) / pk;
      else 
	{
	  printf ("Probability of kill is zero. Better replan.\n");
	  return (1.0);
	}  
      if (count < 0) sum = pow (q, weapons);
      if (count == 0) return 0.5;

      for (i = 0; i < count + 1; i++)
	{
	  if (i > 0)
	    coefficient = coefficient * (targets - (i-1)) / (double) i;
	  product = coefficient * product /q * pk;
	  sum = sum + product;
	  product = product / coefficient;
	  
	}
      printf ("Replan probability that wa are insufficient is %f.\n", sum);
      return (sum);
    }  
}






