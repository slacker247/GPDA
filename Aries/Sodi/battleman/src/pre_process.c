/* pre_process.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int pre_process__()
{
    /* Format strings */
    static char fmt_13[] = "(i2,9(1x,i7))";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle(), s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static integer idiv, site[38], min_dist__, i__, j, k, max_value__;

    /* Fortran I/O blocks */
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___4 = { 0, 6, 0, fmt_13, 0 };




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
    for (i__ = 1; i__ <= 38; ++i__) {
	site[i__ - 1] = 0;
/* L5: */
    }
    s_wsle(&io___3);
    do_lio(&c__9, &c__1, "No. of weapon sites is ", 23L);
    do_lio(&c__3, &c__1, (char *)&shared_1.num_weapon_sites__, (ftnlen)sizeof(
	    integer));
    e_wsle();
    i__1 = shared_1.num_weapon_sites__;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L6: */
	s_wsfe(&io___4);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	i__2 = shared_1.num_threats__;
	for (j = 1; j <= i__2; ++j) {
	    do_fio(&c__1, (char *)&wta_solver__1.wta_matrix__[i__ + j * 38 - 
		    39], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }
    k = 1;
    i__2 = shared_1.num_threats__;
    for (j = 1; j <= i__2; ++j) {
/*  find which site has the best shot at threat j, save in cost and ma
p */
	min_dist__ = 9000000;
	i__1 = shared_1.num_weapon_sites__;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (wta_solver__1.wta_matrix__[i__ + j * 38 - 39] < min_dist__) {
		min_dist__ = wta_solver__1.wta_matrix__[i__ + j * 38 - 39];
		k = i__;
	    }
/* L10: */
	}
/*  weapon site k has best shot at threat j */
	wta_solver__1.map[j - 1] = k;
	++site[k - 1];
/* save wta_matrix row in cost and if weapon site k is a pebble zero i
t out*/
/*  if weapon site k has more than MAX_NUM_SHOTS zero it out */
	i__1 = shared_1.num_threats__;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    wta_solver__1.cost[j + i__ * 50 - 51] = 
		    wta_solver__1.wta_matrix__[k + i__ * 38 - 39];
/* L20: */
	}
	if (shared_1.pebble[k - 1] || site[k - 1] == 35) {
	    i__1 = shared_1.num_threats__;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		wta_solver__1.wta_matrix__[k + i__ * 38 - 39] = 9000000;
/* L30: */
	    }
	}
/* L40: */
    }
/* set up COST matrix for auction */
    max_value__ = 0;
    i__2 = shared_1.num_threats__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = shared_1.num_threats__;
	for (j = 1; j <= i__1; ++j) {
	    if (wta_solver__1.cost[i__ + j * 50 - 51] > max_value__) {
		max_value__ = wta_solver__1.cost[i__ + j * 50 - 51];
	    }
/* L50: */
	}
/* L60: */
    }
/* Computing MAX */
    i__2 = (integer) (max_value__ * (float).1);
    idiv = max(i__2,1);
    i__2 = shared_1.num_threats__;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i__1 = shared_1.num_threats__;
	for (j = 1; j <= i__1; ++j) {
	    wta_solver__1.cost[i__ + j * 50 - 51] = (max_value__ - 
		    wta_solver__1.cost[i__ + j * 50 - 51]) / idiv + 5;
/* L70: */
	}
/* L80: */
    }
    return 0;
} /* pre_process__ */

