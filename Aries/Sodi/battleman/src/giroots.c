/* giroots.f -- translated by f2c (version 19960717).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer iout;
    real pi, twopi, radtodeg, rearth, we, radius, gmu, ppgtol, dphase, 
	    sqrt_mu__, degtorad;
    doublereal current_time__;
    real war_time__[50];
    integer war_time_increment__, num_threats__, num_assets__, num_modules__, 
	    num_pebbles__, num_gbis__, num_weapon_sites__;
    real threat_state__[300]	/* was [50][6] */, threat_time__[50];
    integer threat_class__[50];
    char threat_type__[400], threat_impact__[400];
    logical threat_play__[50], pebble[38];
    real asset_id__[1751], asset_lat__[1751], asset_lon__[1751], 
	    asset_class__[1751], asset_value__[21012]	/* was [1751][12] */, 
	    asset_xyz__[5253]	/* was [1751][3] */;
    integer asset_hardns3__[1751];
    real xold_obj_state__[300]	/* was [50][6] */, xnew_obj_state__[300]	
	    /* was [50][6] */;
} shared_;

#define shared_1 shared_

struct {
    real orbalt[24], orbecc[24], orbinc[24], orblan[24], orbaop[24], angv[24],
	     rot_1__[24], rot_2__[24], rot_3__[24], rot_4__[24], rot_5__[24], 
	    rot_6__[24];
} odata_;

#define odata_1 odata_

struct {
    real xmaly[24], sataop[24], site_xyz__[114]	/* was [38][3] */, gbi_lat__[
	    38], gbi_lon__[38], gbi_pk__[38];
    integer gbi_id__[38], gbi_gbis__[38];
    char gbi_name__[608];
} sdata_;

#define sdata_1 sdata_

struct {
    real trntry[50];
} thtdat_;

#define thtdat_1 thtdat_

struct {
    integer wta_matrix__[1900]	/* was [38][50] */;
    real wta_time__[1900]	/* was [38][50] */;
    integer cost[2500]	/* was [50][50] */, map[50], threat_id__[50];
} wta_solver__;

#define wta_solver__1 wta_solver__

struct {
    integer bid[50];
} wta_plans__;

#define wta_plans__1 wta_plans__

struct {
    logical pebbles_on__, tpt, switch__, done;
} logicals_;

#define logicals_1 logicals_

struct {
    real burn_time__, k_x__, c_x__, k_y__, c_y__, c_vx__[6], c_vy__[6], 
	    kx_t__[8], ky_t__[8], rmax[500], mustepan, gi_lops_delay__, 
	    min_alt__, max_alt__, wpn_endoacq__, pkang[10], pkvel[10], pkill[
	    100]	/* was [10][10] */;
    integer num_pkang__, num_pkvel__;
    real dtc;
    integer num_tc__;
    real max_range__, total_delay__, jeff_time__[50];
} tpt_codes__;

#define tpt_codes__1 tpt_codes__

/* Subroutine */ int giroots_(range, alt, angle_ret__, tcoast_ret__, nfound)
real *range, *alt, *angle_ret__, *tcoast_ret__;
integer *nfound;
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Builtin functions */
    double sin(), cos();

    /* Local variables */
    static real abig, tbig, thet, timx[2], timy[2];
    static integer i__, k;
    static real r__, angle[2], x, y, theta, thetb, dthet, astep, timxa, timya,
	     txend[2], tyend[2], cross;
    static integer nstep;
    static real th;
    extern /* Subroutine */ int giquad_();
    static real tx[2], ty[2], tcoast[2];
    static integer status;
    static real acc, end[2], tha, thb, psi, txa, tya, tol, txb, tyb;



/*-----------------------------------------------------------------------
-------*/

/*  Description: */
/*    This subroutine attempts to find roots for the interceptor position 
*/
/*    equations given input values of range and altitude. For any input */
/*    values of range and altitude, there will be either 0 or 2 roots to 
*/
/*    the equations. The array 'angle' contains the two elevation angles 
*/
/*    and 'tcoast' contains the two coast time values on output. */

/*--------------------------- Procedures Called -------------------------
-------*/

/*     Name                   Description */
/*     ----                   ----------- */


/*  parameters */

/*     MAX_NUM_ASSETS     ...    maximum number of assets */
/*     MAX_NUM_THREATS    ...    maximum number of threats */
/*     MAX_NUM_RINGS      ...    maximum number of pebble rings */
/*     MAX_NUM_SATS       ...    maximum number of pebbles per ring */
/*     MAX_NUM_PEBBLES    ...    maximun number of pebbles */
/*    MAX_NUM_GBIS       ...    maximum number of ground based intercepter
s*/
/*     MAX_NUM_SITES      ...    maximum number of weapon sites */
/*     MAX_NUM_WEAPONS    ...    maximum number of weapons */
/*     MAX_NUM_SHOTS      ...    maximum number of shots per GBI farm */



/*  impact_prediction */
/*  variables needed by tpt codes */

/*--------------------------- Calling Arguments -------------------------
-------*/

/*     Name             I/O    Description (range) */
/*     ----             ---    ------------------- */
/* GBI quadratic equation solver */
/* i     range to intercept (km) */
/* i     altitude above sea level (km) */
/* o     two burnout elevation angles (rad) */
/* o     two coast time values (sec) */

/*-----------------------------------------------------------------------
-------*/
/*  BEGIN EXECUTABLE CODE */
/* o     number of roots found */
/* position in cartesian coord */
/* 'longitude' angle */
/* distance from earth center */
/* time values returned from giquad */
/* angle values */
/* angle values */
/* angle tolerance for endpoint locations */
/* angle step size */
/* angle step size */
/* angle tolerance for solution location */
/* curve endpoint locations */
/* time values */
/* time values */
/* time values */
/* time values at first curve endpoint */
/* curve crossing indicator */
/* temporary angle value */
/* temporary time value */
/* loop indexes */
/* number of angle steps */

/*-----------------------------------------------------------------------
-------*/
/*  Code body */
/*-----------------------------------------------------------------------
-------*/
/* status returned from giquad */
    tol = shared_1.degtorad * (float).02;
/* angle tolerance */
    acc = shared_1.degtorad * (float).2;
    astep = shared_1.degtorad * (float)2.;
/*     -------------------------------------- */
/*     - Initialize returned values to zero - */
/*     -------------------------------------- */
/* angle step size */
    angle[0] = (float)0.;
    angle[1] = (float)0.;
    tcoast[0] = (float)0.;
    tcoast[1] = (float)0.;
    *nfound = 0;
/*     ---------------------------- */
/*     - Convert range,alt to X,Y - */
/*     ---------------------------- */
    r__ = *alt + shared_1.rearth;
    psi = *range / shared_1.rearth;
    x = r__ * sin(psi);
    y = r__ * cos(psi);

/*    ===================================================================
======*/
/*     --------------------------------------------------------------- */
/*     - Find the lower and upper boundaries of curves.              - */
/*     - Start at theta = 0 and step up until first boundary         - */
/*     - is found, then step down from 90.0 to find second endpoint. - */
/*     - Boundary is located when both the range and altitude curves - */
/*     - have two solutions retured from giquad.                     - */
/*     --------------------------------------------------------------- */
    *nfound = 0;
    nstep = (integer) (shared_1.pi * (float).5 / astep) + 1;
    for (k = 1; k <= 2; ++k) {
	if (k == 1) {
	    theta = (float)0.;
	    dthet = astep;
	} else {
	    theta = shared_1.pi / (float)2.;
	    dthet = -astep;
	}
/*        -------------------------------------------- */
/*        - Step through angles until curves overlap - */
/*        -------------------------------------------- */
	i__1 = nstep;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    thetb = theta + dthet;
	    giquad_(&thetb, &x, &y, tx, ty, &status);
	    if (status == 1) {
		goto L200;
	    }
	    theta = thetb;
/* L100: */
	}
	goto L50;
/*        --------------------------------------------------- */
/*        - Perform bisection to refine the boundary value  - */
/*        --------------------------------------------------- */
L200:
	thet = (theta + thetb) / (float)2.;
	giquad_(&thet, &x, &y, tx, ty, &status);
	if (status == 1) {
	    if ((r__1 = theta - thetb, dabs(r__1)) <= tol) {
		goto L300;
	    }
	    thetb = thet;
	} else {
	    theta = thet;
	}
/* **** ADDED */
	if ((r__1 = theta - thetb, dabs(r__1)) <= tol) {
	    goto L300;
	}
/* **** */
	goto L200;
L300:
	end[k - 1] = thet;
	if (k == 1) {
	    txend[0] = tx[0];
	    txend[1] = tx[1];
	    tyend[0] = ty[0];
	    tyend[1] = ty[1];
	}
/* L150: */
    }
    nstep = (integer) ((end[1] - end[0]) / astep) + 1;
/*    ===================================================================
======*/
/*     ---------------------------------------------------------- */
/*     - Search within the boundaries determined above for the  - */
/*     - crossing point of the range and alititude curves.      - */
/*     ---------------------------------------------------------- */
    for (k = 1; k <= 2; ++k) {
	theta = end[0];
	timxa = txend[0];
	timya = tyend[k - 1];
/*        ----------------------------------------- */
/*        - Step through angles between endpoints - */
/*        - until a curve crossing is found       - */
/*        ----------------------------------------- */
	i__1 = nstep;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    thetb = theta + astep;
	    if (thetb > end[1]) {
		thetb = end[1];
	    }
	    giquad_(&thetb, &x, &y, timx, timy, &status);
	    cross = (timx[0] - timy[k - 1]) * (timxa - timya);
/*           ---------------------------------- */
/*           - Perform bisection to find root - */
/*           ---------------------------------- */
	    if (cross < (float)0.) {
		tha = theta;
		thb = thetb;
		txa = timxa;
		tya = timya;
		txb = timx[0];
		tyb = timy[k - 1];
L500:
		if ((r__1 = tha - thb, dabs(r__1)) <= acc) {
		    ++(*nfound);
/*              -----------------------------------------
---------------------*/
/*              - Use linear interpolation to refine angle
 and time solution -*/
/*              - Make sure time solution is still positiv
e                  -*/
/*              -----------------------------------------
---------------------*/
		    angle[*nfound - 1] = tha + (tya - txa) * (thb - tha) / (
			    txb - txa - tyb + tya);
		    tcoast[*nfound - 1] = tya + (tyb - tya) / (thb - tha) * (
			    angle[*nfound - 1] - tha);
		    if (tcoast[*nfound - 1] < (float)0.) {
			--(*nfound);
		    }
		    if (*nfound == 2) {
			goto L50;
		    }
		    goto L600;
		}
		th = (tha + thb) / (float)2.;
		giquad_(&th, &x, &y, tx, ty, &status);
		cross = (tx[0] - ty[k - 1]) * (txa - tya);
		if (cross < (float)0.) {
		    thb = th;
		    txb = tx[0];
		    tyb = ty[k - 1];
		} else {
		    tha = th;
		    txa = tx[0];
		    tya = ty[k - 1];
		}
		goto L500;
	    }
L600:
	    theta = thetb;
/* L400: */
	}
/* L700: */
    }

/*    ===================================================================
======*/
L50:
/*     -------------------------------------------------- */
/*     - Make sure that the smaller coast time and its  - */
/*     - associated angle is first solution in array    - */
/*     -------------------------------------------------- */
    if (*nfound != 2) {
	goto L51;
    }
    if (tcoast[0] > tcoast[1]) {
	abig = angle[0];
	tbig = tcoast[0];
	angle[0] = angle[1];
	tcoast[0] = tcoast[1];
	angle[1] = abig;
	tcoast[1] = tbig;
    }

L51:
    *tcoast_ret__ = tcoast[0];
    *angle_ret__ = angle[0];
    return 0;
} /* giroots_ */

