
/* A fused with B = C.  Given C and A, this *
 * program figures out what B should be.    */


#include <stdio.h>
#include <math.h>

#define  EPSLON  0.00001  /* Larger = all B's closer to C */
#define  b       0
#define  u       1
#define  d       2



void adjust(float *B, float *U, float *D)
{
  /* When B + D == 1.0, U often becomes less than zero    *
   * (eg. -0.000000096857) due to round off error.  Lower *
   * B and D each by half the error.  It sometimes takes  *
   * several iterations to make U == 0.0.                 */

  if (fabs(*B - (int) *B) < EPSLON)
    *B = (int) *B;
  if (fabs(*D - (int) *D) < EPSLON)
    *D = (int) *D;
  *U = 1.0 - *B - *D;

  if (*U < 0.0)
  {
    while (*U != 0.0)
    {
      *B = *B + *U / 2.0;
      *D = *D + *U / 2.0;
      *U = 1.0 - *B - *D;
    }
  }
}



main()
{
  int    it;
  float  i, j;
  float  A[3];
  float  B[3];
  float  C[3];
  float  Ainv[3];
  float  AB[3];
  float  N;


  for (it = 0; it < 2; it++)
  {
    if (it)
    {
      C[b] = 0.95;
      C[u] = 0.05;
      C[d] = 0;
    }
    else
    {
      C[b] = 0.95;
      C[u] = 0;
      C[d] = 0.05;
    }

    printf("\n\nC = [%5.3f  %5.3f  %5.3f]\n\n\n", C[b], C[u], C[d]);


    printf ("           A        Fused With         B             ="
            "            C\n\n"
            "[   B      U      D   ]   [    B       U       D   ]"
            "   [    B       U      D   ]\n"
            "-----------------------   --------------------------"
            "   -------------------------\n");


    for (i = 0.0; i <= 1.0 + EPSLON; i += 0.1)
    {
      for (j = 0.0; j <= 1.0 + EPSLON; j += 0.1)
      {
        if (i + j > (1.0 + EPSLON))
          break;

        /* Set up the A = [b u d] vector so that b + u + d = 1.0. */

        A[b] = i;
        A[d] = j;
        A[u] = 1.0 - A[b] - A[d];
        adjust(&A[b], &A[u], &A[d]);

        /* Make U nonzero to prevent future divide by zero. */
        if (A[u] == 0.0)
        {
          if (A[b] < EPSLON / 2.0)
            A[d] = A[d] - EPSLON;
          else if (A[d] < EPSLON / 2.0)
            A[b] = A[b] - EPSLON;
          else
          {
            A[b] = A[b] - (EPSLON / 2.0);
            A[d] = A[d] - (EPSLON / 2.0);
          }

          A[u] = 1.0 - A[b] - A[d];
        }


        /* Compute the inverse of the vector. */

        Ainv[u] = 1.0 / (A[u] - (A[b] * A[d]) / (A[b] + A[u]) -
                         (A[b] * A[d]) / (A[d] + A[u]));

        /* Prevent divide by zero. */
        if (fabs(Ainv[u]) < EPSLON)
        {
          printf("%f,%f -- Ainv[u] = %f\n", i, j, Ainv[u]);
          continue;
        }

        Ainv[b] = -A[b] / (A[b] + A[u]) * Ainv[u];
        Ainv[d] = -A[d] / (A[d] + A[u]) * Ainv[u];


        /* Fuse Ainv with C to get B. */

        N = 1.0  -  Ainv[b] * C[d]  -  Ainv[d] * C[b];
        B[b] = (Ainv[b] * C[b]  +  Ainv[b] * C[u]  +  Ainv[u] * C[b]) / N;
        B[d] = (Ainv[d] * C[d]  +  Ainv[d] * C[u]  +  Ainv[u] * C[d]) / N;
        B[u] = 1.0 - B[b] - B[d];


        /* Check result -- fuse A with B, result should be equal to C. */

        N = 1.0  -  A[b] * B[d]  -  A[d] * B[b];
        AB[b] = (A[b] * B[b]  +  A[b] * B[u]  +  A[u] * B[b]) / N;
        AB[d] = (A[d] * B[d]  +  A[d] * B[u]  +  A[u] * B[d]) / N;
        AB[u] = 1.0 - AB[b] - AB[d];

        if (((AB[b] - C[b]) < EPSLON) && ((AB[u] - C[u]) < EPSLON) &&
            ((AB[d] - C[d]) < EPSLON))
        {
          printf ("[%6.3f  %5.3f %6.3f ]   [%7.3f %7.3f %7.3f ]"
                  "   [%7.3f  %6.3f %6.3f ]\n",
                  A[b], A[u], A[d], B[b], B[u], B[d], AB[b], AB[u], AB[d]);
        }
        else
        {
          printf ("[%6.3f  %5.3f %6.3f ] * [%7.3f %7.3f %7.3f ]"
                  " * [%7.3f  %6.3f %6.3f ]\n",
                  A[b], A[u], A[d], B[b], B[u], B[d], AB[b], AB[u], AB[d]);
          printf ("  -----  -----  -----        -----   -----   -----"
                  "        -----   -----  -----\n");
        }
      }

      printf("\n");
    }

    printf("\n");
  }
}
