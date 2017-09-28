/* get_data.f -- translated by f2c (version 19960717).
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

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__4 = 4;

/* Subroutine */ int getdata_()
{
    /* System generated locals */
    integer i__1, i__2;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle(), f_open(), s_rsle(), e_rsle(), 
	    f_clos();

    /* Local variables */
    static real earthatm, a;
    static integer i__, j, k;
    static real r__, t, theta;
    extern /* Subroutine */ int gipos_();
    static real a0, max_coast__, t0, r0, r1;
    static integer mx_num_tc__;
    static real tc, dt;
    static integer in;
    static real dtheta;
    static integer num_angles__;
    static real max_tof__;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 6, 0, 0, 0 };
    static cilist io___21 = { 0, 0, 0, 0, 0 };
    static cilist io___22 = { 0, 0, 0, 0, 0 };
    static cilist io___23 = { 0, 0, 0, 0, 0 };
    static cilist io___24 = { 0, 0, 0, 0, 0 };
    static cilist io___26 = { 0, 0, 0, 0, 0 };


/*-----------------------------------------------------------------------
-------*/

/*  Description: */
/*   This subroutine initializes the interceptor flyout data for later use
*/
/*    by the feasibility routine. The following operations are performed: 
*/
/*      -Determines the min and max intercept altitude. */
/*      -Determines the maximum TOF for each. */
/*      -Generates a curve of maximum range versus coast time. */

/*   Note: In this subroutine, all "time" variables refer to the intercept
or*/
/*          coast time in seconds, unless specified otherwise. */

/*-----------------------------------------------------------------------
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

/* GBW interceptor position routine */
/* altitude values */
/* time values */
/* angle value */
/* range values */
/* coast time */
/* time increment for determination of amax a */
/* maximum overall coast time */
/* i=angle index, j=time index */

/*-----------------------------------------------------------------------
-------*/
/*  Code body */
/*-----------------------------------------------------------------------
-------*/
/* The maximum num tc */
    earthatm = (float)1.014352515;
    num_angles__ = 45;
    mx_num_tc__ = 0;
/* debug loop counter */
    dt = (float)2.;
/* time increment */
    theta = shared_1.pi * (float).5;
/* Use theta = 90.0 deg to get max altitude */
    dtheta = theta / num_angles__;
/* -----------------------------------------------------------------------
 */
/* - Get minimum altitudes for interceptors */
/* -----------------------------------------------------------------------
 */
/* angle increment */
    tpt_codes__1.min_alt__ = shared_1.rearth * (earthatm - (float)1.) / (
	    float)1e3;
/*-----------------------------------------------------------------------
-------*/
/*                                                                        
    -*/
/*      DETERMINE:                                                        
              -*/
/*      max_alt = maximum altitude for each type,needed by gifeasbl       
 -*/
/*      max_tof = maximum TOF for each type,needed by EP                  
 -*/
/*      max_coast  = maximum overall coast time,needed to compute num_tc  
    -*/
/*                                                                        
    -*/
/*-----------------------------------------------------------------------
-------*/
/* top of atmosphere in */
    s_wsle(&io___7);
    do_lio(&c__9, &c__1, " TPT option on ", 15L);
    e_wsle();
/*        ------------------------ */
/*        - Initialize variables - */
/*        ------------------------ */
    max_coast__ = (float)0.;
    tpt_codes__1.max_alt__ = (float)0.;
    t = (float)0.;
    t0 = (float)0.;
    a0 = (float)0.;
L200:
    t += dt;
    gipos_(&theta, &t, &r__, &a);
    if (a > tpt_codes__1.max_alt__) {
	tpt_codes__1.max_alt__ = a;
    }
    if (a < tpt_codes__1.min_alt__ && a < a0) {
	max_tof__ = t0 + dt / (a - a0) * (tpt_codes__1.min_alt__ - a0);
	if (max_tof__ > max_coast__) {
	    max_coast__ = max_tof__;
	}
	max_tof__ += tpt_codes__1.burn_time__;
	goto L250;
    }
    t0 = t;
    a0 = a;
    goto L200;
L250:
    tpt_codes__1.num_tc__ = (integer) (max_coast__ / tpt_codes__1.dtc);
    mx_num_tc__ = tpt_codes__1.num_tc__;
    if (max_coast__ == (float)0.) {
	tpt_codes__1.num_tc__ = 1;
	max_tof__ = (float)0.;
    }

    i__1 = tpt_codes__1.num_tc__;
    for (j = 1; j <= i__1; ++j) {
	tc = tpt_codes__1.dtc * j;
	a0 = (float)0.;
	r0 = (float)0.;
	tpt_codes__1.rmax[j - 1] = (float)0.;
	for (i__ = num_angles__; i__ >= 1; --i__) {
	    theta = i__ * dtheta;
	    gipos_(&theta, &tc, &r__, &a);
	    if (a < tpt_codes__1.min_alt__) {
		if (i__ == num_angles__) {
		    tpt_codes__1.rmax[j - 1] = (float)0.;
		    goto L300;
		} else {
		    r1 = r0 + (r__ - r0) / (a - a0) * (tpt_codes__1.min_alt__ 
			    - a0);
		    if (r1 > tpt_codes__1.rmax[j - 1]) {
			tpt_codes__1.rmax[j - 1] = r1;
		    }
		    goto L300;
		}
	    }
	    if (r__ > tpt_codes__1.rmax[j - 1]) {
		tpt_codes__1.rmax[j - 1] = r__;
	    }
	    r0 = r__;
	    a0 = a;
/* L400: */
	}
L300:
	;
    }
    tpt_codes__1.rmax[0] = (float)0.;
/*        ------------------------------ */
/*        -  Determine maximum range   - */
/*        ------------------------------ */
    tpt_codes__1.max_range__ = (float)0.;
    i__1 = tpt_codes__1.num_tc__;
    for (j = 1; j <= i__1; ++j) {
	if (tpt_codes__1.rmax[j - 1] > tpt_codes__1.max_range__) {
	    tpt_codes__1.max_range__ = tpt_codes__1.rmax[j - 1];
	}
/* L600: */
    }
/*  read in tables */
    in = 1;
    o__1.oerr = 0;
    o__1.ounit = in;
    o__1.ofnmlen = 7;
    o__1.ofnm = "tpt.dat";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = "formatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    io___21.ciunit = in;
    s_rsle(&io___21);
    do_lio(&c__3, &c__1, (char *)&tpt_codes__1.num_pkang__, (ftnlen)sizeof(
	    integer));
    e_rsle();
    io___22.ciunit = in;
    s_rsle(&io___22);
    do_lio(&c__3, &c__1, (char *)&tpt_codes__1.num_pkvel__, (ftnlen)sizeof(
	    integer));
    e_rsle();
    io___23.ciunit = in;
    s_rsle(&io___23);
    i__1 = tpt_codes__1.num_pkang__;
    for (j = 1; j <= i__1; ++j) {
	do_lio(&c__4, &c__1, (char *)&tpt_codes__1.pkang[j - 1], (ftnlen)
		sizeof(real));
    }
    e_rsle();
    io___24.ciunit = in;
    s_rsle(&io___24);
    i__1 = tpt_codes__1.num_pkvel__;
    for (j = 1; j <= i__1; ++j) {
	do_lio(&c__4, &c__1, (char *)&tpt_codes__1.pkvel[j - 1], (ftnlen)
		sizeof(real));
    }
    e_rsle();
    i__1 = tpt_codes__1.num_pkang__;
    for (k = 1; k <= i__1; ++k) {
	io___26.ciunit = in;
	s_rsle(&io___26);
	i__2 = tpt_codes__1.num_pkvel__;
	for (j = 1; j <= i__2; ++j) {
	    do_lio(&c__4, &c__1, (char *)&tpt_codes__1.pkill[k + j * 10 - 11],
		     (ftnlen)sizeof(real));
	}
	e_rsle();
/* L650: */
    }
    cl__1.cerr = 0;
    cl__1.cunit = in;
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
} /* getdata_ */

/* Subroutine */ int gipos_(theta, t, r__, a)
real *theta, *t, *r__, *a;
{
    /* System generated locals */
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double cos(), sin(), atan(), sqrt();

    /* Local variables */
    static real x, y, x0, y0, ax, bx, ay, by, v0x, v0y;



/*-----------------------------------------------------------------------
-------*/

/*  Description: */
/*   This subroutine computes the range and altitude from the launch point
*/
/*   to the interceptor, given the mode, burnout elevation angle, and coas
t*/
/*    time. */

/*--------------------------- Calling Arguments -------------------------
-------*/

/*     Name           I/O  Type  Description (range) */
/*     ----           ---  ----  ------------------- */


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
/* i   real  burnout elevation angle in X,Y system (r */
/* i   real  coast time (seconds) */
/* o   real  range (km) */

/*-----------------------------------------------------------------------
-------*/
/*  BEGIN EXECUTABLE CODE */
/*-----------------------------------------------------------------------
-------*/
/* o   real  altitude above sea level (km) */
/* interceptor position (km) */
/* interceptor burnout position (km) */
/* interceptor burnout velocity (km/sec) */
/* local intermediate variables */

/*-----------------------------------------------------------------------
-------*/
/*  Code body */
/*-----------------------------------------------------------------------
-------*/
/* local intermediate variables */
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
    ax = tpt_codes__1.kx_t__[4] + tpt_codes__1.kx_t__[5] * *theta + 
	    tpt_codes__1.kx_t__[6] * (r__1 * r__1) + tpt_codes__1.kx_t__[7] * 
	    (r__3 * (r__2 * r__2));
/* Computing 2nd power */
    r__1 = *theta;
/* Computing 3rd power */
    r__2 = *theta, r__3 = r__2;
    bx = v0x + tpt_codes__1.kx_t__[0] + tpt_codes__1.kx_t__[1] * *theta + 
	    tpt_codes__1.kx_t__[2] * (r__1 * r__1) + tpt_codes__1.kx_t__[3] * 
	    (r__3 * (r__2 * r__2));
/* Computing 2nd power */
    r__1 = *t;
    x = (x0 + bx * *t + ax * (r__1 * r__1)) / (float)1e3;
/*     ----------------------------------------- */
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
/* Computing 2nd power */
    r__1 = *t;
    y = (y0 + by * *t + ay * (r__1 * r__1)) / (float)1e3;
/*     ------------------------------------------ */
    *r__ = atan(x / y) * shared_1.rearth / (float)1e3;
/* Computing 2nd power */
    r__1 = x;
/* Computing 2nd power */
    r__2 = y;
    *a = sqrt(r__1 * r__1 + r__2 * r__2) - shared_1.rearth / (float)1e3;
    return 0;
} /* gipos_ */

