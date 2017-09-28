/* gilnchaz.f -- translated by f2c (version 19960717).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int gilnchaz_(site1, site2, site3, pip1, pip2, pip3, laz)
real *site1, *site2, *site3, *pip1, *pip2, *pip3, *laz;
{
    /* System generated locals */
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(), acos();

    /* Local variables */
    static real v1mag, v2mag, cosaz, v1[3], v2[3], pi;


/*-----------------------------------------------------------------------
-------*/

/*  Description: */
/*  This function computes the launch azimuth from site to pip. */
/*  Because the ECI - Z axis always points toward zero azimuth, the true 
*/
/* lat and lon points are not important. The value returned is in radians,
*/
/*  where North = 0.0 and East = PI/2. */


/*-----------------------------------------------------------------------
-------*/
/*  BEGIN EXECUTABLE CODE */


/* cross product of site and pip vectors */
/* cross product of site and ECI z-axis */
/* magnitude of v1 */
/* magnitude of v2 */
/* cosine of launch azimuth */

/*-----------------------------------------------------------------------
--*/
/*  Code body */
/*-----------------------------------------------------------------------
--*/
/*     ------------------------------------------------- */
/*     - Compute cross product of site and pip vectors - */
/*     ------------------------------------------------- */
    v1[0] = *site2 * *pip3 - *site3 * *pip2;
    v1[1] = *site3 * *pip1 - *site1 * *pip3;
    v1[2] = *site1 * *pip2 - *site2 * *pip1;
/*     ------------------------------ */
/*     - Magnitude of cross product - */
/*     ------------------------------ */
/* Computing 2nd power */
    r__1 = v1[0];
/* Computing 2nd power */
    r__2 = v1[1];
/* Computing 2nd power */
    r__3 = v1[2];
    v1mag = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
/*     ------------------------------------------ */
/*     - Cross product of site and ECI - Z axis - */
/*     ------------------------------------------ */
    v2[0] = *site2;
    v2[1] = -(*site1);
    v2[2] = (float)0.;
/*     ------------------------------ */
/*     - Magnitude of cross product - */
/*     ------------------------------ */
/* Computing 2nd power */
    r__1 = v2[0];
/* Computing 2nd power */
    r__2 = v2[1];
/* Computing 2nd power */
    r__3 = v2[2];
    v2mag = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
/*     ------------------------------------------ */
/*     - Case where site is North or South pole - */
/*     ------------------------------------------ */
    if (v2mag == (float)0.) {
	*laz = pi;
	if (*site3 < (float)0.) {
	    *laz = (float)0.;
	}
	return 0;
    }
/*     ------------------------------------------- */
/*     -- Case where target is directly overhead - */
/*     ------------------------------------------- */
    if (v1mag == (float)0.) {
	*laz = (float)0.;
	return 0;
    }
    cosaz = (v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2]) / (v1mag * v2mag);
    if (dabs(cosaz) > (float)1.) {
	if (cosaz > (float)1.) {
	    cosaz = (float)1.;
	}
	if (cosaz < (float)-1.) {
	    cosaz = (float)-1.;
	}
    }
    *laz = acos(cosaz);
    if (v1[2] < (float)0.) {
	*laz = -(*laz);
    }
    return 0;
} /* gilnchaz_ */

