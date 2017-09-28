/* initial.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int initial_()
{
    /* Builtin functions */
    double atan(), sqrt();

/*  constants */


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
    shared_1.pi = atan((float)1.) * (float)4.;
    shared_1.twopi = shared_1.pi * (float)2.;
    shared_1.rearth = (float)6371011.;
/*     rearth        = 6378145.0 */
    shared_1.we = (float)7.29211585e-5;
    shared_1.gmu = (float)3.9860068e14;
    shared_1.ppgtol = (float)1e-5;
    shared_1.degtorad = shared_1.pi / (float)180.;
    shared_1.radtodeg = (float)180. / shared_1.pi;
    tpt_codes__1.mustepan = (float)0.;
    tpt_codes__1.gi_lops_delay__ = (float)5.;
    tpt_codes__1.burn_time__ = (float)77.;
    tpt_codes__1.total_delay__ = tpt_codes__1.burn_time__ + 
	    tpt_codes__1.gi_lops_delay__;
    tpt_codes__1.wpn_endoacq__ = (float)5.;
    shared_1.dphase = (float)0.;
    shared_1.sqrt_mu__ = sqrt(shared_1.gmu);
    tpt_codes__1.dtc = (float)5.;
    shared_1.radius = (float)1e4;
/*  run time parameters */
    shared_1.war_time_increment__ = 30;
    logicals_1.pebbles_on__ = FALSE_;
    logicals_1.tpt = FALSE_;
/*  values need by tpt codes */
    tpt_codes__1.c_x__ = (float)152503.;
    tpt_codes__1.k_x__ = (float)2693.52;
    tpt_codes__1.c_y__ = (float)168696.13058375;
    tpt_codes__1.k_y__ = (float)6383196.7252063;
    tpt_codes__1.c_vx__[0] = (float)-196927.;
    tpt_codes__1.c_vx__[1] = (float)125368.;
    tpt_codes__1.c_vx__[2] = (float)198241.;
    tpt_codes__1.c_vx__[3] = (float)-100293.2;
    tpt_codes__1.c_vx__[4] = (float)42834.6;
    tpt_codes__1.c_vx__[5] = (float)-9130.12;
    tpt_codes__1.c_vy__[0] = (float)3381.5;
    tpt_codes__1.c_vy__[1] = (float)9808.6;
    tpt_codes__1.c_vy__[2] = (float)-10965.5;
    tpt_codes__1.c_vy__[3] = (float)4065.7;
    tpt_codes__1.kx_t__[0] = (float)-131.4;
    tpt_codes__1.kx_t__[1] = (float)1034.7;
    tpt_codes__1.kx_t__[2] = (float)-1167.9;
    tpt_codes__1.kx_t__[3] = (float)355.3;
    tpt_codes__1.kx_t__[4] = (float)-1.3;
    tpt_codes__1.kx_t__[5] = (float).51;
    tpt_codes__1.kx_t__[6] = (float).5;
    tpt_codes__1.kx_t__[7] = (float)-.18;
    tpt_codes__1.ky_t__[0] = (float)-26.9;
    tpt_codes__1.ky_t__[1] = (float)-423.9;
    tpt_codes__1.ky_t__[2] = (float)259.2;
    tpt_codes__1.ky_t__[3] = (float)-58.3;
    tpt_codes__1.ky_t__[4] = (float)-5.3;
    tpt_codes__1.ky_t__[5] = (float)4.2;
    tpt_codes__1.ky_t__[6] = (float)-2.8;
    tpt_codes__1.ky_t__[7] = (float).63;
    return 0;
} /* initial_ */

