/* fly_threat.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int fly_threat__()
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(), cos(), sin();

    /* Local variables */
    static real alfa[50], time_dif__[50], f, g;
    static integer i__;
    static real x, y, z__, alpsq[50], alpsy[50], s0[50], s1[50], s2[50], s3[
	    50], fd, gd, ft[50], rm[50];
    static logical threat_off__[50];
    static real rm0[50], dif, alt, tol, psy[50], ro_1__[50], ro_2__[50], sig0[
	    50], ro_3__[50], vo_1__[50], vo_2__[50], vo_3__[50];



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
    i__1 = shared_1.num_threats__;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (shared_1.threat_play__[i__ - 1]) {
	    ro_1__[i__ - 1] = shared_1.threat_state__[i__ - 1];
	    ro_2__[i__ - 1] = shared_1.threat_state__[i__ + 49];
	    ro_3__[i__ - 1] = shared_1.threat_state__[i__ + 99];
	    vo_1__[i__ - 1] = shared_1.threat_state__[i__ + 149];
	    vo_2__[i__ - 1] = shared_1.threat_state__[i__ + 199];
	    vo_3__[i__ - 1] = shared_1.threat_state__[i__ + 249];

	    time_dif__[i__ - 1] = shared_1.war_time__[i__ - 1] - 
		    shared_1.threat_time__[i__ - 1];
/* Computing 2nd power */
	    r__1 = ro_1__[i__ - 1];
/* Computing 2nd power */
	    r__2 = ro_2__[i__ - 1];
/* Computing 2nd power */
	    r__3 = ro_3__[i__ - 1];
	    rm0[i__ - 1] = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
	    sig0[i__ - 1] = ro_1__[i__ - 1] * vo_1__[i__ - 1] + ro_2__[i__ - 
		    1] * vo_2__[i__ - 1] + ro_3__[i__ - 1] * vo_3__[i__ - 1];
/* Computing 2nd power */
	    r__1 = vo_1__[i__ - 1];
/* Computing 2nd power */
	    r__2 = vo_2__[i__ - 1];
/* Computing 2nd power */
	    r__3 = vo_3__[i__ - 1];
	    alfa[i__ - 1] = r__1 * r__1 + r__2 * r__2 + r__3 * r__3 - 
		    shared_1.gmu * (float)2. / rm0[i__ - 1];
/* Computing 2nd power */
	    r__1 = time_dif__[i__ - 1];
/* Computing 3rd power */
	    r__2 = rm0[i__ - 1], r__3 = r__2;
	    psy[i__ - 1] = time_dif__[i__ - 1] / rm0[i__ - 1] - sig0[i__ - 1] 
		    * (r__1 * r__1) / (r__3 * (r__2 * r__2) * (float)2.);
	    alpsq[i__ - 1] = sqrt(-alfa[i__ - 1]);
	}
/* L100: */
    }
    i__1 = shared_1.num_threats__;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (shared_1.threat_play__[i__ - 1]) {
	    tol = shared_1.ppgtol * (float)2.;
L105:
	    if (tol > shared_1.ppgtol) {
		alpsy[i__ - 1] = psy[i__ - 1] * alpsq[i__ - 1];
		s0[i__ - 1] = cos(alpsy[i__ - 1]);
		s1[i__ - 1] = sin(alpsy[i__ - 1]) / alpsq[i__ - 1];
		s2[i__ - 1] = (s0[i__ - 1] - (float)1.) / alfa[i__ - 1];
		s3[i__ - 1] = (s1[i__ - 1] - psy[i__ - 1]) / alfa[i__ - 1];
		ft[i__ - 1] = rm0[i__ - 1] * s1[i__ - 1] + sig0[i__ - 1] * s2[
			i__ - 1] + shared_1.gmu * s3[i__ - 1];
		rm[i__ - 1] = rm0[i__ - 1] * s0[i__ - 1] + sig0[i__ - 1] * s1[
			i__ - 1] + shared_1.gmu * s2[i__ - 1];
		dif = time_dif__[i__ - 1] - ft[i__ - 1];
		psy[i__ - 1] += dif / rm[i__ - 1];
		tol = dabs(dif);
		goto L105;
	    }
	}
/* L200: */
    }
    i__1 = shared_1.num_threats__;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (shared_1.threat_play__[i__ - 1]) {
	    threat_off__[i__ - 1] = FALSE_;
	    f = (float)1. - shared_1.gmu * s2[i__ - 1] / rm0[i__ - 1];
	    fd = -shared_1.gmu * s1[i__ - 1] / (rm[i__ - 1] * rm0[i__ - 1]);
	    g = ft[i__ - 1] - shared_1.gmu * s3[i__ - 1];
	    gd = (float)1. - shared_1.gmu * s2[i__ - 1] / rm[i__ - 1];
	    x = ro_1__[i__ - 1] * f + vo_1__[i__ - 1] * g;
	    y = ro_2__[i__ - 1] * f + vo_2__[i__ - 1] * g;
	    z__ = ro_3__[i__ - 1] * f + vo_3__[i__ - 1] * g;
/* Computing 2nd power */
	    r__1 = x;
/* Computing 2nd power */
	    r__2 = y;
/* Computing 2nd power */
	    r__3 = z__;
	    alt = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
	    if (alt > shared_1.rearth) {
		shared_1.threat_state__[i__ - 1] = x;
		shared_1.threat_state__[i__ + 49] = y;
		shared_1.threat_state__[i__ + 99] = z__;
		shared_1.threat_state__[i__ + 149] = ro_1__[i__ - 1] * fd + 
			vo_1__[i__ - 1] * gd;
		shared_1.threat_state__[i__ + 199] = ro_2__[i__ - 1] * fd + 
			vo_2__[i__ - 1] * gd;
		shared_1.threat_state__[i__ + 249] = ro_3__[i__ - 1] * fd + 
			vo_3__[i__ - 1] * gd;
		shared_1.threat_time__[i__ - 1] = shared_1.war_time__[i__ - 1]
			;
	    } else {
		threat_off__[i__ - 1] = TRUE_;
	    }
	}
/* L300: */
    }
    logicals_1.done = TRUE_;
    i__1 = shared_1.num_threats__;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (shared_1.threat_play__[i__ - 1] && threat_off__[i__ - 1]) {
/* Computing 2nd power */
	    r__1 = shared_1.threat_state__[i__ - 1];
/* Computing 2nd power */
	    r__2 = shared_1.threat_state__[i__ + 49];
/* Computing 2nd power */
	    r__3 = shared_1.threat_state__[i__ + 99];
	    alt = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
	    shared_1.threat_play__[i__ - 1] = FALSE_;
/*           print 99, i, threat_time (i), alt */
	}
	if (shared_1.threat_play__[i__ - 1]) {
	    logicals_1.done = FALSE_;
	}
/* L400: */
    }
/* L99: */
    return 0;
} /* fly_threat__ */

