/* gisrchar.f -- translated by f2c (version 19960717).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

integer gisrchar_(num, array, value)
integer *num;
real *array, *value;
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    static integer i__;




/*-----------------------------------------------------------------------
-------*/

/*  Description */
/*   This function returns the index of the lower array element of the pai
r*/
/*    which bracket 'value'. It is assumed that the values in 'array' are 
*/
/*    monotonically increasing. */


/*--------------------------- Calling Arguments -------------------------
-------*/

/*     Name              I/O    Description (range) */
/*     ----              ---    ------------------- */
/* i     number of elements in the array to search */
/* i     array of real values to search */

/*-----------------------------------------------------------------------
-------*/
/*  BEGIN EXECUTABLE CODE */

/* i     target value to search for */

/*-----------------------------------------------------------------------
-------*/
/*  Code body */
/*-----------------------------------------------------------------------
-------*/

/* loop counter */
    /* Parameter adjustments */
    --array;

    /* Function Body */
    if (*value < array[1]) {
	ret_val = 1;
	return ret_val;
    }
    if (*value >= array[*num]) {
	ret_val = *num - 1;
	return ret_val;
    }

    i__1 = *num;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (*value < array[i__]) {
	    ret_val = i__ - 1;
	    return ret_val;
	}
/* L100: */
    }

    return ret_val;
} /* gisrchar_ */

