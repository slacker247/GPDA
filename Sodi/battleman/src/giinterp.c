/* giinterp.f -- translated by f2c (version 19960717).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

doublereal giinterp_(x1, x2, y1, y2, x)
real *x1, *x2, *y1, *y2, *x;
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    static real slp;



/*-----------------------------------------------------------------------
-------*/

/*  Description: */
/*   Performs linear interpolation. The two interpolation points are speci
fied*/
/*   by (x1,y1) and (x2,y2). The independent variable is x and the interpo
lated*/
/*    value is returned in giinterp. */


/*--------------------------- Calling Arguments -------------------------
-------*/

/*     Name           I/O  Type  Description (range) */
/*     ----           ---  ----  ------------------- */
/* i   int   x - coordinate of first interpolation po */
/* i   int   x - coordinate of second interpolation p */
/* i   int   y - coord of first interpolation point */
/* i   int   y - coord of second interpolation point */

/*-----------------------------------------------------------------------
-------*/
/*  BEGIN EXECUTABLE CODE */
/* i   int   independent variable */

/*-----------------------------------------------------------------------
-------*/
/*  Code body */
/*-----------------------------------------------------------------------
-------*/

/* slope of curve */
    slp = (*y2 - *y1) / (*x2 - *x1);
    ret_val = *y1 + slp * (*x - *x1);

    return ret_val;
} /* giinterp_ */

