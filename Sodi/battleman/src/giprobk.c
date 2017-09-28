/* giprobk.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int giprobk_(j, iv_state__, targ_state__, pk, closing_ang__, 
	closing_vel__, miss_dis__)
integer *j;
real *iv_state__, *targ_state__, *pk, *closing_ang__, *closing_vel__, *
	miss_dis__;
{
    /* System generated locals */
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(), acos();

    /* Local variables */
    static integer iang, ivel;
    static real sdot;
    extern integer gisrchar_();
    extern doublereal giinterp_();
    static real ivmag, pk1, pk2, targmag;



/*-----------------------------------------------------------------------
-------*/

/*  Description: */
/*     This routine computes the nominal probability of kill. Given the */
/*     interceptor and target states at a particular time, the routine */
/*     computes closing angle and closing velocity, and then uses these */
/*     to interpolate a value of Pk from the user-specified table. If the 
*/
/*     computed closing angle or velocity is outside the range of the */
/*     table, then the Pk will be set to zero. */
/*    The integer index 'table' refers to the Pk table which will be used:
*/

/*        1 - exo mode,   RV */
/*        4 - exo mode,   Penaid */


/*--------------------------- Calling Arguments -------------------------
-------*/

/*     Name                   I/O  Type  Description (range) */
/*     ----                   ---  ----  ------------------- */


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
/* i   real interceptor state vec (ECI pos an */
/* o   real probability of kill */
/* o   real closing angle */
/* o   real closing velocity */

/*-----------------------------------------------------------------------
-------*/
/*  BEGIN EXECUTABLE CODE */
/*     ----------------------------------- */
/*     - Framework and IFG include files - */
/*     ----------------------------------- */
/* o   real miss distance (interceptor-target */
/* GBI interpolation index */
/* velocity index */

/* angle index */

/* interpolated value */
/* target state vector */
/* magnitude of IV velocity */
/* magnitude of target velocity */

/*-----------------------------------------------------------------------
-------*/
/*  Code body */
/*-----------------------------------------------------------------------
-------*/
/*     -------------------------------- */
/*     - Print up the targ_state array - */
/*     -------------------------------- */
/*     ------------------------------------------ */
/*     - Compute the distance from IV to target - */
/*     ------------------------------------------ */
/* intermediate pk values */
    /* Parameter adjustments */
    --targ_state__;
    --iv_state__;

    /* Function Body */
/* Computing 2nd power */
    r__1 = iv_state__[1] - targ_state__[1];
/* Computing 2nd power */
    r__2 = iv_state__[2] - targ_state__[2];
/* Computing 2nd power */
    r__3 = iv_state__[3] - targ_state__[3];
    *miss_dis__ = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
/*     -------------------------------- */
/*     - Compute the closing velocity - */
/*     -------------------------------- */
/* Computing 2nd power */
    r__1 = iv_state__[4] - targ_state__[4];
/* Computing 2nd power */
    r__2 = iv_state__[5] - targ_state__[5];
/* Computing 2nd power */
    r__3 = iv_state__[6] - targ_state__[6];
    *closing_vel__ = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
/*     ----------------------------- */
/*    - Compute the closing angle Jim: the value of PI and DEGTORAD are in
 fwconst.com -*/
/*     ----------------------------- */
/* Computing 2nd power */
    r__1 = iv_state__[4];
/* Computing 2nd power */
    r__2 = iv_state__[5];
/* Computing 2nd power */
    r__3 = iv_state__[6];
    ivmag = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
/* Computing 2nd power */
    r__1 = targ_state__[4];
/* Computing 2nd power */
    r__2 = targ_state__[5];
/* Computing 2nd power */
    r__3 = targ_state__[6];
    targmag = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
    sdot = iv_state__[4] * targ_state__[4] + iv_state__[5] * targ_state__[5] 
	    + iv_state__[6] * targ_state__[6];
    *closing_ang__ = (shared_1.pi - acos(sdot / (ivmag * targmag))) / 
	    shared_1.degtorad;
/*     -------------------------------------------------------- */
/*     - If closing angle or closing velocity are outside the - */
/*     - ranges in the table, then the Pk is zero.            - */
/*     -------------------------------------------------------- */
    if (*closing_ang__ < tpt_codes__1.pkang[0] || *closing_ang__ > 
	    tpt_codes__1.pkang[tpt_codes__1.num_pkang__ - 1] || *
	    closing_vel__ < tpt_codes__1.pkvel[0] || *closing_vel__ > 
	    tpt_codes__1.pkvel[tpt_codes__1.num_pkvel__ - 1]) {
	*pk = (float)0.;
	return 0;
    }
/*     -------------------------------------------------------------------
 */
/*     - Perform double linear interpolation to get the nominal Pk value -
 */
/*     -------------------------------------------------------------------
 */
    iang = gisrchar_(&tpt_codes__1.num_pkang__, tpt_codes__1.pkang, 
	    closing_ang__);
    ivel = gisrchar_(&tpt_codes__1.num_pkvel__, tpt_codes__1.pkvel, 
	    closing_vel__);

    pk1 = giinterp_(&tpt_codes__1.pkang[iang - 1], &tpt_codes__1.pkang[iang], 
	    &tpt_codes__1.pkill[iang + ivel * 10 - 11], &tpt_codes__1.pkill[
	    iang + 1 + ivel * 10 - 11], closing_ang__);

    pk2 = giinterp_(&tpt_codes__1.pkang[iang - 1], &tpt_codes__1.pkang[iang], 
	    &tpt_codes__1.pkill[iang + (ivel + 1) * 10 - 11], &
	    tpt_codes__1.pkill[iang + 1 + (ivel + 1) * 10 - 11], 
	    closing_ang__);

    *pk = giinterp_(&tpt_codes__1.pkvel[ivel - 1], &tpt_codes__1.pkvel[ivel], 
	    &pk1, &pk2, closing_vel__);

    if (*pk < (float)0.) {
	*pk = (float)0.;
    }
    if (*pk > (float)1.) {
	*pk = (float)1.;
    }

    return 0;
} /* giprobk_ */

