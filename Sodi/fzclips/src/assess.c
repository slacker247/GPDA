#include "clips.h"
#include <stdio.h>

double replan ()
{
  void *value;
  int targets, weapons, count, i, first, salvo;
  double pk, sum, product, q;

  if (ArgCountCheck ("replan", EXACTLY, 5) == -1)
    {
      print ("Error in passing parameters to replan.\n");
      return (AddDouble (-1.0));
    }

  targets = RtnLong (1);
  pk = RtnDouble (2);
  weapons = RtnLong (3);
  first = RtnLong (4);
  salvo = RtnLong (5);

  if (((weapons + 1 - targets) % salvo) != 0)
     count = (weapons + 1 - targets) / salvo + 1;
  else
    count = (weapons + 1 - targets) / salvo;

  sum = 0.0;
  q = 1 - pk;
  product = pow (q, count + 1) / pk;

  for (i = 0; i < count + 1; i++)
    {
      product = product /q * pk;
      sum = sum + product;
    }

  return (sum);
}

