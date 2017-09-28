/* feas.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int feas_()
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(), cos(), sin(), acos();

    /* Local variables */
    static real probability_of_kill__[1900]	/* was [38][50] */;
    static integer jmax[1900]	/* was [38][50] */, ndum;
    static real tdum, miss, rint, xalt, tsol[1900]	/* was [38][50] */;
    static integer nsol, prev;
    extern /* Subroutine */ int gilnchaz_();
    static real site1, site2, rint0, rmax0, site3[50];
    static integer i__, j, k, m;
    static real r__, clang, angle[2], x_ecr__[100], y_ecr__[100], z_ecr__[100]
	    , clvel, ddist[1900]	/* was [38][50] */, tcmin[1900]	/* 
	    was [38][50] */, tcmax[1900]	/* was [38][50] */, probk, 
	    state, cross, max_coast__, troot, tc, ri[50], tl, xr, angdum[2], 
	    angsol[1900]	/* was [38][50] */;
    static integer nfound;
    static real tc0, troot10, tcross, tl1, tl2;
    static integer status[1900]	/* was [38][50] */;
    static real arg, alt[50], laz, xyl[38];
    extern /* Subroutine */ int giprobk_(), gistate_();
    static real t_state__[6], rintmin[1900]	/* was [38][50] */, rintmax[
	    1900]	/* was [38][50] */;
    static integer found_tcmin__[1900]	/* was [38][50] */;
    static real threat_save__[600]	/* was [100][6] */;
    extern /* Subroutine */ int giroots_();



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
    for (j = 1; j <= i__1; ++j) {
	x_ecr__[j - 1] = shared_1.threat_state__[j - 1];
	y_ecr__[j - 1] = shared_1.threat_state__[j + 49];
	z_ecr__[j - 1] = shared_1.threat_state__[j + 99];
	threat_save__[j - 1] = shared_1.threat_state__[j - 1];
	threat_save__[j + 99] = shared_1.threat_state__[j + 49];
	threat_save__[j + 199] = shared_1.threat_state__[j + 99];
	threat_save__[j + 299] = shared_1.threat_state__[j + 149];
	threat_save__[j + 399] = shared_1.threat_state__[j + 199];
	threat_save__[j + 499] = shared_1.threat_state__[j + 249];
	i__2 = shared_1.num_weapon_sites__;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* Computing 2nd power */
	    r__1 = sdata_1.site_xyz__[i__ - 1] - x_ecr__[j - 1];
/* Computing 2nd power */
	    r__2 = sdata_1.site_xyz__[i__ + 37] - y_ecr__[j - 1];
/* Computing 2nd power */
	    r__3 = sdata_1.site_xyz__[i__ + 75] - z_ecr__[j - 1];
	    ddist[i__ + j * 38 - 39] = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 *
		     r__3);
/* L1: */
	}
/* L2: */
    }
    shared_1.rearth /= (float)1e3;
    i__1 = shared_1.num_threats__;
    for (j = 1; j <= i__1; ++j) {
	shared_1.threat_state__[j - 1] = x_ecr__[j - 1] / (float)1e3;
	shared_1.threat_state__[j + 49] = y_ecr__[j - 1] / (float)1e3;
	shared_1.threat_state__[j + 99] = z_ecr__[j - 1] / (float)1e3;
	shared_1.threat_state__[j + 149] /= (float)1e3;
	shared_1.threat_state__[j + 199] /= (float)1e3;
	shared_1.threat_state__[j + 249] /= (float)1e3;
/* Computing 2nd power */
	r__1 = shared_1.threat_state__[j - 1];
/* Computing 2nd power */
	r__2 = shared_1.threat_state__[j + 49];
/* Computing 2nd power */
	r__3 = shared_1.threat_state__[j + 99];
	ri[j - 1] = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
	alt[j - 1] = ri[j - 1] - shared_1.rearth;
	if (logicals_1.switch__) {
	    tpt_codes__1.jeff_time__[j - 1] = shared_1.threat_time__[j - 1];
	}
/* L10: */
    }
    logicals_1.switch__ = FALSE_;
    i__1 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = shared_1.num_threats__;
	for (j = 1; j <= i__2; ++j) {
	    if (shared_1.threat_play__[j - 1]) {
		status[i__ + j * 38 - 39] = 1;
	    } else {
		status[i__ + j * 38 - 39] = 10;
	    }
	    tcmax[i__ + j * 38 - 39] = shared_1.threat_time__[j - 1] - (
		    tpt_codes__1.jeff_time__[j - 1] + 
		    tpt_codes__1.total_delay__);
	    tcmin[i__ + j * 38 - 39] = tpt_codes__1.dtc;
/* L5: */
	}
    }
    i__2 = shared_1.num_threats__;
    for (j = 1; j <= i__2; ++j) {
	i__1 = shared_1.num_weapon_sites__;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (tcmax[i__ + j * 38 - 39] < tpt_codes__1.dtc) {
		status[i__ + j * 38 - 39] = 20;
	    }
/* L40: */
	}
	if (alt[j - 1] > tpt_codes__1.max_alt__) {
	    i__1 = shared_1.num_weapon_sites__;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		status[i__ + j * 38 - 39] = 30;
/* L20: */
	    }
	}
	if (alt[j - 1] < tpt_codes__1.min_alt__) {
	    i__1 = shared_1.num_weapon_sites__;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		status[i__ + j * 38 - 39] = 40;
/* L30: */
	    }
	}
/* L50: */
    }
/*     call Status_out (status, 50) */
/*  compute range to intercept at smallest coast time */
    i__2 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	xyl[i__ - 1] = cos(sdata_1.gbi_lat__[i__ - 1]);
	site3[i__ - 1] = sin(sdata_1.gbi_lat__[i__ - 1]);
/* L60: */
    }
    i__2 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = shared_1.num_threats__;
	for (j = 1; j <= i__1; ++j) {
	    if (status[i__ + j * 38 - 39] == 1) {
		tl1 = shared_1.threat_time__[j - 1] - (tcmin[i__ + j * 38 - 
			39] + tpt_codes__1.total_delay__);
		arg = sdata_1.gbi_lon__[i__ - 1] + shared_1.we * tl1 + 
			tpt_codes__1.mustepan;
		site1 = xyl[i__ - 1] * cos(arg);
		site2 = xyl[i__ - 1] * sin(arg);
		rintmin[i__ + j * 38 - 39] = shared_1.rearth * acos((site1 * 
			shared_1.threat_state__[j - 1] + site2 * 
			shared_1.threat_state__[j + 49] + site3[i__ - 1] * 
			shared_1.threat_state__[j + 99]) / ri[j - 1]);
		tl2 = tpt_codes__1.jeff_time__[j - 1] + 
			tpt_codes__1.total_delay__;
		arg = sdata_1.gbi_lon__[i__ - 1] + shared_1.we * tl2 + 
			tpt_codes__1.mustepan;
		site1 = xyl[i__ - 1] * cos(arg);
		site2 = xyl[i__ - 1] * sin(arg);
		rintmax[i__ + j * 38 - 39] = shared_1.rearth * acos((site1 * 
			shared_1.threat_state__[j - 1] + site2 * 
			shared_1.threat_state__[j + 49] + site3[i__ - 1] * 
			shared_1.threat_state__[j + 99]) / ri[j - 1]);
	    }
/* L70: */
	}
/* L80: */
    }
/*  perform initial check to see if target is in range */
    i__2 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = shared_1.num_threats__;
	for (j = 1; j <= i__1; ++j) {
	    if (status[i__ + j * 38 - 39] == 1) {
		if (rintmin[i__ + j * 38 - 39] > tpt_codes__1.max_range__ && 
			rintmax[i__ + j * 38 - 39] > tpt_codes__1.max_range__)
			 {
		    status[i__ + j * 38 - 39] = 50;
		}
	    }
/* L90: */
	}
/* L100: */
    }
/*     call Status_out (status, 100) */
/*  find valid coast time window for all threats and gbis */
    i__2 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = shared_1.num_threats__;
	for (j = 1; j <= i__1; ++j) {
	    if (status[i__ + j * 38 - 39] == 1) {
/* Computing MIN */
		i__3 = tpt_codes__1.num_tc__, i__4 = (integer) (tcmax[i__ + j 
			* 38 - 39] / tpt_codes__1.dtc);
		jmax[i__ + j * 38 - 39] = min(i__3,i__4);
		tcmax[i__ + j * 38 - 39] = jmax[i__ + j * 38 - 39] * 
			tpt_codes__1.dtc;
		found_tcmin__[i__ + j * 38 - 39] = 0;
	    }
/* L120: */
	}
/* L130: */
    }
    i__2 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = shared_1.num_threats__;
	for (j = 1; j <= i__1; ++j) {
	    if (status[i__ + j * 38 - 39] == 1) {
		rint0 = (float)0.;
		rmax0 = (float)0.;
		i__3 = jmax[i__ + j * 38 - 39];
		for (k = 1; k <= i__3; ++k) {
		    tc = tpt_codes__1.dtc * k;
		    tl = shared_1.threat_time__[j - 1] - tc - 
			    tpt_codes__1.burn_time__;
		    arg = sdata_1.gbi_lon__[i__ - 1] + shared_1.we * tl + 
			    tpt_codes__1.mustepan;
		    site1 = xyl[i__ - 1] * cos(arg);
		    site2 = xyl[i__ - 1] * sin(arg);
/*  compute range from launch site to threat */
		    rint = shared_1.rearth * acos((site1 * 
			    shared_1.threat_state__[j - 1] + site2 * 
			    shared_1.threat_state__[j + 49] + site3[i__ - 1] *
			     shared_1.threat_state__[j + 99]) / ri[j - 1]);
/*  check if rint curve crosses rmax curve */
		    cross = (tpt_codes__1.rmax[k - 1] - rint) * (rmax0 - 
			    rint0);
		    if (cross < (float)0.) {
/*  interpolate time value where curves cross */
			tcross = tc - tpt_codes__1.dtc + tpt_codes__1.dtc * (
				rint0 - rmax0) / (tpt_codes__1.rmax[k - 1] - 
				rmax0 - rint + rint0);
			if (found_tcmin__[i__ + j * 38 - 39] == 0) {
			    found_tcmin__[i__ + j * 38 - 39] = 1;
			    tcmin[i__ + j * 38 - 39] = tcross;
			} else {
			    tcmax[i__ + j * 38 - 39] = tcross;
			    goto L145;
			}
		    }
		    rint0 = rint;
		    rmax0 = tpt_codes__1.rmax[k - 1];
/* L140: */
		}
/*  if pairs never cross the pairs are not feasible */
		if (found_tcmin__[i__ + j * 38 - 39] == 0) {
		    status[i__ + j * 38 - 39] = 60;
		    goto L150;
		}
/*  check minimum coast time against the amount of time availa
ble */
L145:
		max_coast__ = shared_1.threat_time__[j - 1] - (
			tpt_codes__1.jeff_time__[j - 1] + 
			tpt_codes__1.total_delay__);
		if (tcmin[i__ + j * 38 - 39] > max_coast__) {
		    status[i__ + j * 38 - 39] = 70;
		    goto L150;
		}
	    }
L150:
	    ;
	}
/* L160: */
    }
/*     call Status_out (status, 160) */
    i__2 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = shared_1.num_threats__;
	for (j = 1; j <= i__1; ++j) {
	    if (status[i__ + j * 38 - 39] == 1) {
		prev = 0;
		nsol = 0;
		troot10 = (float)0.;
		tc0 = (float)0.;
		jmax[i__ + j * 38 - 39] = (integer) ((tcmax[i__ + j * 38 - 39]
			 - tcmin[i__ + j * 38 - 39]) / tpt_codes__1.dtc) + 2;
		i__3 = jmax[i__ + j * 38 - 39];
		for (k = 1; k <= i__3; ++k) {
		    tc = tcmin[i__ + j * 38 - 39] + (k - 1) * 
			    tpt_codes__1.dtc;
/* coast time ( */
		    tl = shared_1.threat_time__[j - 1] - (tc + 
			    tpt_codes__1.burn_time__);
/* launch time ( */
		    arg = sdata_1.gbi_lon__[i__ - 1] + shared_1.we * tl + 
			    tpt_codes__1.mustepan;
/* earth rotation ang */
		    site1 = xyl[i__ - 1] * cos(arg);
		    site2 = xyl[i__ - 1] * sin(arg);
		    rint = shared_1.rearth * acos((site1 * 
			    shared_1.threat_state__[j - 1] + site2 * 
			    shared_1.threat_state__[j + 49] + site3[i__ - 1] *
			     shared_1.threat_state__[j + 99]) / ri[j - 1]);
		    giroots_(&rint, &alt[j - 1], angle, &troot, &nfound);
		    xr = (shared_1.rearth + alt[j - 1]) * sin(rint / 
			    shared_1.rearth);
		    xalt = (shared_1.rearth + alt[j - 1]) * cos(rint / 
			    shared_1.rearth);
		    if (nfound == 0) {
			prev = 0;
			goto L180;
		    }
		    if (prev == 0) {
			goto L175;
		    }
		    cross = (tc - troot) * (tc0 - troot10);
		    if (cross < (float)0.) {
			r__ = rint0 + (troot10 - tc0) * (rint - rint0) / (
				tpt_codes__1.dtc - troot + troot10);
			giroots_(&r__, &alt[j - 1], angdum, &tdum, &ndum);
/*             --------------------------------------
-------------------------*/
/*             - Check that coast time is not longer t
han maximum coast time -*/
/*             - or less than minimum coast time, if s
o, try a later time    -*/
/*             --------------------------------------
-------------------------*/
			if (tdum > tpt_codes__1.num_tc__ * tpt_codes__1.dtc) {
			    goto L175;
			}
			if (tdum < tpt_codes__1.dtc + 
				tpt_codes__1.wpn_endoacq__) {
			    goto L175;
			}
			nsol = 1;
			tsol[i__ + j * 38 - 39] = tdum;
			angsol[i__ + j * 38 - 39] = angdum[0];
			goto L190;
/* have found the early solu */
		    }
L175:
		    prev = 1;
		    rint0 = rint;
		    tc0 = tc;
		    troot10 = troot;
L180:
		    ;
		}
		if (nsol < 1) {
		    status[i__ + j * 38 - 39] = 80;
		}
	    }
L190:
	    ;
	}
/* L200: */
    }
/*     call Status_out (status, 200) */
/*        ------------------------------------------------------------- */
/*        - Determine the launch site ECI position and launch azimuth - */
/*        ------------------------------------------------------------- */
    i__2 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = shared_1.num_threats__;
	for (j = 1; j <= i__1; ++j) {
	    if (status[i__ + j * 38 - 39] == 1) {
		tl = shared_1.threat_time__[j - 1] - tsol[i__ + j * 38 - 39] 
			- tpt_codes__1.burn_time__;
		arg = sdata_1.gbi_lon__[i__ - 1] + shared_1.we * tl + 
			tpt_codes__1.mustepan;
		site1 = xyl[i__ - 1] * cos(arg);
		site2 = xyl[i__ - 1] * sin(arg);
		gilnchaz_(&site1, &site2, &site3[i__ - 1], &
			shared_1.threat_state__[j - 1], &
			shared_1.threat_state__[j + 49], &
			shared_1.threat_state__[j + 99], &laz);
/*        ----------------------------------- */
/*        - Determine IV state at intercept - */
/*        ----------------------------------- */
		gistate_(&i__, &tl, &laz, &angsol[i__ + j * 38 - 39], &
			shared_1.threat_time__[j - 1], &state);
/*        ------------------------------------------- */
/*        - Compute estimated Pk for pair:          - */
/*        - Use Pk table for RVs, since we wouldn't - */
/*        - be bothering if we thought              - */
/*        - the target was a penaid!                - */
/*        ------------------------------------------- */
		for (m = 1; m <= 6; ++m) {
		    t_state__[m - 1] = shared_1.threat_state__[j + m * 50 - 
			    51];
/* L205: */
		}
		giprobk_(&j, &state, t_state__, &probk, &clang, &clvel, &miss)
			;
/*  determine IV state at intercept */
		probability_of_kill__[i__ + j * 38 - 39] = ((float)1. - probk)
			 * (float)100.;
	    }
/* L210: */
	}
/* L220: */
    }
    i__2 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = shared_1.num_threats__;
	for (j = 1; j <= i__1; ++j) {
	    if (status[i__ + j * 38 - 39] == 1) {
		if (probability_of_kill__[i__ + j * 38 - 39] < (real) 
			wta_solver__1.wta_matrix__[i__ + j * 38 - 39]) {
		    wta_solver__1.wta_matrix__[i__ + j * 38 - 39] = 
			    probability_of_kill__[i__ + j * 38 - 39];
		    wta_solver__1.wta_time__[i__ + j * 38 - 39] = 
			    shared_1.threat_time__[j - 1];
		}
	    }
/* L230: */
	}
/* L240: */
    }
    shared_1.rearth *= (float)1e3;
    i__2 = shared_1.num_threats__;
    for (j = 1; j <= i__2; ++j) {
	shared_1.threat_state__[j - 1] = threat_save__[j - 1];
	shared_1.threat_state__[j + 49] = threat_save__[j + 99];
	shared_1.threat_state__[j + 99] = threat_save__[j + 199];
	shared_1.threat_state__[j + 149] = threat_save__[j + 299];
	shared_1.threat_state__[j + 199] = threat_save__[j + 399];
	shared_1.threat_state__[j + 249] = threat_save__[j + 499];
/* L250: */
    }
    return 0;
} /* feas_ */

/* Subroutine */ int status_out__(status, k)
integer *status, *k;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle();

    /* Local variables */
    static integer i__, j;

    /* Fortran I/O blocks */
    static cilist io___57 = { 0, 6, 0, 0, 0 };
    static cilist io___59 = { 0, 6, 0, 0, 0 };
    static cilist io___61 = { 0, 6, 0, 0, 0 };




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
    /* Parameter adjustments */
    status -= 39;

    /* Function Body */
    s_wsle(&io___57);
    do_lio(&c__9, &c__1, " status after ", 14L);
    do_lio(&c__3, &c__1, (char *)&(*k), (ftnlen)sizeof(integer));
    e_wsle();
    i__1 = shared_1.num_threats__;
    for (j = 1; j <= i__1; ++j) {
	s_wsle(&io___59);
	i__2 = shared_1.num_weapon_sites__;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&status[i__ + j * 38], (ftnlen)
		    sizeof(integer));
	}
	e_wsle();
/* L10: */
    }
    s_wsle(&io___61);
    e_wsle();
    return 0;
} /* status_out__ */

