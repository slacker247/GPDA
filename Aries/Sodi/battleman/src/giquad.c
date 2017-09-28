/* giquad.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int giquad_(theta, x, y, tx, ty, real_sol__)
real *theta, *x, *y, *tx, *ty;
integer *real_sol__;
{
    /* System generated locals */
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double cos(), sin(), sqrt();

    /* Local variables */
    static real temp;
    static integer i__;
    static real quanx, quany, x0, y0, ax, bx, ay, by, v0x, v0y;


/*-----------------------------------------------------------------------
-------*/

/*  Description: */
/*    This subroutine solves the two quadratic equations fOR tx and ty, */
/*    given a point (X,Y) in the cartesian launch point coORdinate system,
 */
/*    a value fOR burnout elevation angle, and the mode. */

/*--------------------------- Calling Arguments -------------------------
-------*/

/*     Name             I/O  Type  Description (range) */
/*     ----             ---  ----  ------------------- */


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
/* i   real  burnout elevation angle (rad) */
/* i   real  X cartesian coORd (km) */
/* i   real  Y cartesian coORd (km) */
/* o   real  early tx value (late tx not valid) */
/* o   real  early and late ty values */
/* o   int   real_sol = 1 indicates real solution */
/* local intermediate variables */

/*-----------------------------------------------------------------------
-------*/
/*  BEGIN EXECUTABLE CODE */

/* local intermediate variables */
/* x and y discriminant */
/*-----------------------------------------------------------------------
-------*/
/*  Code body */
/*-----------------------------------------------------------------------
-------*/
/*    ----------------------------------------- */
/*    - Initialize returned variables to zero - */
/*    ----------------------------------------- */
    /* Parameter adjustments */
    --ty;
    --tx;

    /* Function Body */
    for (i__ = 1; i__ <= 2; ++i__) {
	tx[i__] = (float)0.;
	ty[i__] = (float)0.;
/* L100: */
    }
/*   Make sure earth radius offset is accounted fOR */
    x0 = tpt_codes__1.c_x__ * cos(*theta) + tpt_codes__1.k_x__;
/* Computing 2nd power */
    r__1 = sin(*theta);
/* Computing 3rd power */
    r__2 = sin(*theta), r__3 = r__2;
    v0x = tpt_codes__1.c_vx__[0] + tpt_codes__1.c_vx__[1] * *theta + 
	    tpt_codes__1.c_vx__[2] * cos(*theta) + tpt_codes__1.c_vx__[3] * 
	    cos(*theta) * sin(*theta) + tpt_codes__1.c_vx__[4] * cos(*theta) *
	     (r__1 * r__1) + tpt_codes__1.c_vx__[5] * cos(*theta) * (r__3 * (
	    r__2 * r__2));
/* Computing 2nd power */
    r__1 = *theta;
/* Computing 3rd power */
    r__2 = *theta, r__3 = r__2;
    bx = v0x + tpt_codes__1.kx_t__[0] + tpt_codes__1.kx_t__[1] * *theta + 
	    tpt_codes__1.kx_t__[2] * (r__1 * r__1) + tpt_codes__1.kx_t__[3] * 
	    (r__3 * (r__2 * r__2));
/* Computing 2nd power */
    r__1 = *theta;
/* Computing 3rd power */
    r__2 = *theta, r__3 = r__2;
    ax = tpt_codes__1.kx_t__[4] + tpt_codes__1.kx_t__[5] * *theta + 
	    tpt_codes__1.kx_t__[6] * (r__1 * r__1) + tpt_codes__1.kx_t__[7] * 
	    (r__3 * (r__2 * r__2));
    y0 = tpt_codes__1.c_y__ * sin(*theta) + tpt_codes__1.k_y__;
/* Computing 2nd power */
    r__1 = sin(*theta);
/* Computing 3rd power */
    r__2 = sin(*theta), r__3 = r__2;
/* Computing 4th power */
    r__4 = sin(*theta), r__4 *= r__4;
    v0y = tpt_codes__1.c_vy__[0] * sin(*theta) + tpt_codes__1.c_vy__[1] * (
	    r__1 * r__1) + tpt_codes__1.c_vy__[2] * (r__3 * (r__2 * r__2)) + 
	    tpt_codes__1.c_vy__[3] * (r__4 * r__4);
/* Computing 2nd power */
    r__1 = *theta;
/* Computing 3rd power */
    r__2 = *theta, r__3 = r__2;
    by = v0y + tpt_codes__1.ky_t__[0] + tpt_codes__1.ky_t__[1] * *theta + 
	    tpt_codes__1.ky_t__[2] * (r__1 * r__1) + tpt_codes__1.ky_t__[3] * 
	    (r__3 * (r__2 * r__2));
/* Computing 2nd power */
    r__1 = *theta;
/* Computing 3rd power */
    r__2 = *theta, r__3 = r__2;
    ay = tpt_codes__1.ky_t__[4] + tpt_codes__1.ky_t__[5] * *theta + 
	    tpt_codes__1.ky_t__[6] * (r__1 * r__1) + tpt_codes__1.ky_t__[7] * 
	    (r__3 * (r__2 * r__2));
/*     ------------------------------------------- */
/*     - Calculate the discriminant fOR both the - */
/*     - range and time curves                   - */
/*     ------------------------------------------- */
/* Computing 2nd power */
    r__1 = bx;
    quanx = r__1 * r__1 - ax * (float)4. * (x0 - *x * (float)1e3);
/* Computing 2nd power */
    r__1 = by;
    quany = r__1 * r__1 - ay * (float)4. * (y0 - *y * (float)1e3);
/*     --------------------------------------------- */
/*     - If either discriminant is less than zero, - */
/*     - the curves  don't intersect and hence no  - */
/*     - feasible solution at this range           - */
/*     --------------------------------------------- */
    if (quanx < (float)0. || quany < (float)0.) {
	*real_sol__ = 0;
	return 0;
    }
    *real_sol__ = 1;
/*     ------------------------------------------- */
/*     - Calculate the two range time solutions  - */
/*     - and put the smallest value in the first - */
/*     - position.                               - */
/*     ------------------------------------------- */
    tx[1] = (-bx - sqrt(quanx)) * (float).5 / ax;
    tx[2] = (-bx + sqrt(quanx)) * (float).5 / ax;
    if (tx[1] > tx[2]) {
	temp = tx[2];
	tx[2] = tx[1];
	tx[1] = temp;
    }
/*     ---------------------------------------------- */
/*     - Calculate the two aLTitude time solutions  - */
/*     - and put the smallest value in the first    - */
/*     - position.                                  - */
/*     ---------------------------------------------- */
    ty[1] = (-by - sqrt(quany)) * (float).5 / ay;
    ty[2] = (-by + sqrt(quany)) * (float).5 / ay;
    if (ty[1] > ty[2]) {
	temp = ty[2];
	ty[2] = ty[1];
	ty[1] = temp;
    }
/*     ---------------------------------------------------- */
/*     - If any time solution is negative, assume no good - */
/*     - solution exists                                  - */
/*     ---------------------------------------------------- */
    if (tx[1] < (float)0. || tx[2] < (float)0. || ty[1] < (float)0. || ty[2] <
	     (float)0.) {
	*real_sol__ = 0;
    }
    return 0;
} /* giquad_ */

