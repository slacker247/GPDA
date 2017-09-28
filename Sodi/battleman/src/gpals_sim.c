/* gpals_sim.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int gpalssim_()
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int feas_(), read_threats__();
    static integer i__, itime;
    extern /* Subroutine */ int conin1_(), matrix_(), fly_threat__(), wta_(), 
	    expected_damage__(), engagement_plan__(), pre_process__();

/* GPALS simulation */
/*      real tmp (2) */
/*  set up i/o file numbers and constants, then read in data and process 
*/
/*      call Initial */
/*      call Read_data */
/*      if (tpt) call Get_data */
/*      do 30 k = 1, 1 */


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
    read_threats__();
/*       tbegin = etime (tmp) */
    if (logicals_1.pebbles_on__) {
	conin1_();
    }
    i__1 = shared_1.war_time_increment__;
    for (itime = 100; i__1 < 0 ? itime >= 2000 : itime <= 2000; itime += i__1)
	     {
	for (i__ = 1; i__ <= 50; ++i__) {
	    if (shared_1.threat_play__[i__ - 1]) {
		shared_1.war_time__[i__ - 1] += shared_1.war_time_increment__;
	    }
/* L10: */
	}
/*          if (pebbles_on) call Orbit */
	fly_threat__();
/* set up feasibility matrix */
	if (logicals_1.tpt) {
	    feas_();
	} else {
	    matrix_();
	}
	if (logicals_1.done) {
	    goto L25;
	}
/* L20: */
    }
L25:
/*  compute expected damage from impact points */
    expected_damage__();
/*  weapon-target allocation */
    pre_process__();
    wta_();
    engagement_plan__();
/*        tend = etime (tmp) */
/*        print *, ' elapsed time = ', tend - tbegin */
/* 	 print *, ' ' */
/* L30: */
    return 0;
} /* gpalssim_ */
