/* gistate.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int gistate_(i__, tlnch, az, theta, time, state)
integer *i__;
real *tlnch, *az, *theta, *time, *state;
{
    /* System generated locals */
    real r__1, r__2, r__3, r__4, r__5, r__6, r__7;

    /* Builtin functions */
    double cos(), sin();

    /* Local variables */
    static real t, x0, y0, rotmat[9]	/* was [3][3] */, v0x, v0y, lat, vel[
	    3], lon, pos[3];
    extern /* Subroutine */ int mxv_();


/*-----------------------------------------------------------------------
-------*/

/*  Description: */
/*   This subroutine computes the GBI interceptor position and velocity in
*/
/*    ECI coordinates. */


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

/*     Name              I/O    Description (range) */
/*     ----              ---    ------------------- */
/* Cray intrinsic matrix multiplication routi */
/* i     simtime at which launch occurs */
/* i     launch azimuth (rad, PI/2 = East) */
/* i     burnout elevation angle in LPS system (ra */
/* i     simtime at which IV position is desired */

/*-----------------------------------------------------------------------
-------*/
/*  BEGIN EXECUTABLE CODE */
/*     --------------------------- */
/*     - GBI model include files - */
/*     --------------------------- */
/* o     IV ECI 6-state vector */
/* launch site latitude (rad) */
/* launch site longitude at time (rad, 0 = EC */
/* rotation matrix from LPS to ECI coordinate */
/* IV position in LPS coord */
/* IV velocity in LPS coord */

/* coast time (sec) */
/* interceptor burnout position (km) */

/*-----------------------------------------------------------------------
-------*/
/*  Code body */
/*-----------------------------------------------------------------------
-------*/

/* interceptor burnout velocity (km/sec) */
    /* Parameter adjustments */
    --state;

    /* Function Body */
    tpt_codes__1.mustepan = (float)0.;
    lat = sdata_1.gbi_lat__[*i__ - 1];
    lon = -(sdata_1.gbi_lon__[*i__ - 1] + shared_1.we * *tlnch + 
	    tpt_codes__1.mustepan);
/*    -------------------------------------------------------------------
---*/
/*    - Set up the rotation matrix from LPS to ECI                        
 -*/
/*    - Can zero out second column because the y component is always zero.
 -*/
/*    -------------------------------------------------------------------
---*/
    rotmat[0] = cos(lon) * cos(lat);
    rotmat[3] = (float)0.;
    rotmat[6] = sin(lon) * sin(*az) - cos(lon) * sin(lat) * cos(*az);
    rotmat[1] = -sin(lon) * cos(lat);
    rotmat[4] = (float)0.;
    rotmat[7] = cos(lon) * sin(*az) + sin(lon) * sin(lat) * cos(*az);
    rotmat[2] = sin(lat);
    rotmat[5] = (float)0.;
    rotmat[8] = cos(lat) * cos(*az);
/*     ---------------------------------------- */
/*     - Compute IV LPS position and velocity - */
/*     ---------------------------------------- */
    t = *time - *tlnch - tpt_codes__1.burn_time__;

/* coast time */
    x0 = tpt_codes__1.c_x__ * cos(*theta) + tpt_codes__1.k_x__;
/*   --------------------------------------------------------------------
-----------*/
/*   - Adjust coefficients to convert to earth surface coordinate system(e
arth_rad).*/
/*   --------------------------------------------------------------------
-----------*/
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
/* Computing 2nd power */
    r__4 = t;
/* Computing 2nd power */
    r__5 = *theta;
/* Computing 3rd power */
    r__6 = *theta, r__7 = r__6;
    pos[2] = x0 + v0x * t + t * (tpt_codes__1.kx_t__[0] + tpt_codes__1.kx_t__[
	    1] * *theta + tpt_codes__1.kx_t__[2] * (r__1 * r__1) + 
	    tpt_codes__1.kx_t__[3] * (r__3 * (r__2 * r__2))) + r__4 * r__4 * (
	    tpt_codes__1.kx_t__[4] + tpt_codes__1.kx_t__[5] * *theta + 
	    tpt_codes__1.kx_t__[6] * (r__5 * r__5) + tpt_codes__1.kx_t__[7] * 
	    (r__7 * (r__6 * r__6)));
/* Computing 2nd power */
    r__1 = *theta;
/* Computing 3rd power */
    r__2 = *theta, r__3 = r__2;
/* Computing 2nd power */
    r__4 = *theta;
/* Computing 3rd power */
    r__5 = *theta, r__6 = r__5;
    vel[2] = v0x + (tpt_codes__1.kx_t__[0] + tpt_codes__1.kx_t__[1] * *theta 
	    + tpt_codes__1.kx_t__[2] * (r__1 * r__1) + tpt_codes__1.kx_t__[3] 
	    * (r__3 * (r__2 * r__2))) + t * (float)2. * (tpt_codes__1.kx_t__[
	    4] + tpt_codes__1.kx_t__[5] * *theta + tpt_codes__1.kx_t__[6] * (
	    r__4 * r__4) + tpt_codes__1.kx_t__[7] * (r__6 * (r__5 * r__5)));
/*     ----------------------- */
/*     - Convert meters to km. */
/*     ----------------------- */
    x0 /= (float)1e3;
    v0x /= (float)1e3;
    pos[2] /= (float)1e3;
    vel[2] /= (float)1e3;

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
/* Computing 2nd power */
    r__4 = t;
/* Computing 2nd power */
    r__5 = *theta;
/* Computing 3rd power */
    r__6 = *theta, r__7 = r__6;
    pos[0] = y0 + v0y * t + t * (tpt_codes__1.ky_t__[0] + tpt_codes__1.ky_t__[
	    1] * *theta + tpt_codes__1.ky_t__[2] * (r__1 * r__1) + 
	    tpt_codes__1.ky_t__[3] * (r__3 * (r__2 * r__2))) + r__4 * r__4 * (
	    tpt_codes__1.ky_t__[4] + tpt_codes__1.ky_t__[5] * *theta + 
	    tpt_codes__1.ky_t__[6] * (r__5 * r__5) + tpt_codes__1.ky_t__[7] * 
	    (r__7 * (r__6 * r__6)));
/* Computing 2nd power */
    r__1 = *theta;
/* Computing 3rd power */
    r__2 = *theta, r__3 = r__2;
/* Computing 2nd power */
    r__4 = *theta;
/* Computing 3rd power */
    r__5 = *theta, r__6 = r__5;
    vel[0] = v0y + (tpt_codes__1.ky_t__[0] + tpt_codes__1.ky_t__[1] * *theta 
	    + tpt_codes__1.ky_t__[2] * (r__1 * r__1) + tpt_codes__1.ky_t__[3] 
	    * (r__3 * (r__2 * r__2))) + t * (float)2. * (tpt_codes__1.ky_t__[
	    4] + tpt_codes__1.ky_t__[5] * *theta + tpt_codes__1.ky_t__[6] * (
	    r__4 * r__4) + tpt_codes__1.ky_t__[7] * (r__6 * (r__5 * r__5)));
    y0 /= (float)1e3;
    v0y /= (float)1e3;
    pos[0] /= (float)1e3;
    vel[0] /= (float)1e3;

    pos[1] = (float)0.;
    vel[1] = (float)0.;
/*     ----------------------------------------------------- */
/*     - Perform matrix multiplication to get ECI position - */
/*     ----------------------------------------------------- */
    mxv_(rotmat, pos, &state[1]);
/*     ----------------------------------------------------- */
/*     - Perform matrix multiplication to get ECI velocity - */
/*     ----------------------------------------------------- */
    mxv_(rotmat, vel, &state[4]);
    return 0;
} /* gistate_ */

/* Subroutine */ int mxv_(a, b, c__)
real *a, *b, *c__;
{
    /* Parameter adjustments */
    --c__;
    --b;
    a -= 4;

    /* Function Body */
    c__[1] = a[4] * b[1] + a[7] * b[2] + a[10] * b[3];
    c__[2] = a[5] * b[1] + a[8] * b[2] + a[11] * b[3];
    c__[3] = a[6] * b[1] + a[9] * b[2] + a[12] * b[3];
    return 0;
} /* mxv_ */

