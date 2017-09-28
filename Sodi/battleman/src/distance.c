/* distance.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int distance_(ddist, n, xyz, ndim, m)
real *ddist;
integer *n;
real *xyz;
integer *ndim, *m;
{
    /* System generated locals */
    integer ddist_dim1, ddist_offset, xyz_dim1, xyz_offset, i__1, i__2;
    real r__1, r__2, r__3;

    /* Builtin functions */
    double cos(), sin(), sqrt();

    /* Local variables */
    static integer i__, j;
    static real x_ecr__, y_ecr__, z_ecr__;



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
    ddist_dim1 = *n;
    ddist_offset = ddist_dim1 + 1;
    ddist -= ddist_offset;
    xyz_dim1 = *ndim;
    xyz_offset = xyz_dim1 + 1;
    xyz -= xyz_offset;

    /* Function Body */
    i__1 = shared_1.num_threats__;
    for (j = 1; j <= i__1; ++j) {
	if (shared_1.threat_play__[j - 1]) {
	    x_ecr__ = cos(shared_1.we * shared_1.threat_time__[j - 1]) * 
		    shared_1.threat_state__[j - 1] + sin(shared_1.we * 
		    shared_1.threat_time__[j - 1]) * shared_1.threat_state__[
		    j + 49];
	    y_ecr__ = -sin(shared_1.we * shared_1.threat_time__[j - 1]) * 
		    shared_1.threat_state__[j - 1] + cos(shared_1.we * 
		    shared_1.threat_time__[j - 1]) * shared_1.threat_state__[
		    j + 49];
	    z_ecr__ = shared_1.threat_state__[j + 99];
	    i__2 = *m;
	    for (i__ = 1; i__ <= i__2; ++i__) {
/* Computing 2nd power */
		r__1 = xyz[i__ + xyz_dim1] - x_ecr__;
/* Computing 2nd power */
		r__2 = xyz[i__ + (xyz_dim1 << 1)] - y_ecr__;
/* Computing 2nd power */
		r__3 = xyz[i__ + xyz_dim1 * 3] - z_ecr__;
		ddist[i__ + j * ddist_dim1] = sqrt(r__1 * r__1 + r__2 * r__2 
			+ r__3 * r__3);
/* L10: */
	    }
	}
/* L20: */
    }
    return 0;
} /* distance_ */

