/* expected_damage.f -- translated by f2c (version 19960717).
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
static integer c__1751 = 1751;

/* Subroutine */ int expected_damage__()
{
    /* Format strings */
    static char fmt_99[] = "(7x,i4,4x,f7.2,2x,f7.2,2x,f9.2)";
    static char fmt_98[] = "(\002 asset \002,i4,\002 targeted by threat \002\
,i4)";

    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2, r__3;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle();
    double sqrt(), asin(), atan2();
    integer s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    extern /* Subroutine */ int distance_();
    static integer i__, j;
    static real ddist[87550]	/* was [1751][50] */, threat_lat__, 
	    threat_lon__, alt;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, 0, 0 };
    static cilist io___2 = { 0, 6, 0, 0, 0 };
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___8 = { 0, 6, 0, fmt_99, 0 };
    static cilist io___10 = { 0, 6, 0, 0, 0 };
    static cilist io___12 = { 0, 6, 0, fmt_98, 0 };


/*  compute expected damage */


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
    s_wsle(&io___1);
    do_lio(&c__9, &c__1, " ", 1L);
    e_wsle();
    s_wsle(&io___2);
    do_lio(&c__9, &c__1, " ***** expected damage", 22L);
    e_wsle();
/*  compute alt, lat, long for threats */
    s_wsle(&io___3);
    do_lio(&c__9, &c__1, " expected impact lat      lon      time", 39L);
    e_wsle();
/*  convert x,y,z of threat to lat lon */
    i__1 = shared_1.num_threats__;
    for (j = 1; j <= i__1; ++j) {
	shared_1.threat_play__[j - 1] = TRUE_;
/* Computing 2nd power */
	r__1 = shared_1.threat_state__[j - 1];
/* Computing 2nd power */
	r__2 = shared_1.threat_state__[j + 49];
/* Computing 2nd power */
	r__3 = shared_1.threat_state__[j + 99];
	alt = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
	threat_lat__ = asin(shared_1.threat_state__[j + 99] / alt) * 
		shared_1.radtodeg;
	threat_lon__ = atan2(shared_1.threat_state__[j + 49], 
		shared_1.threat_state__[j - 1]) * shared_1.radtodeg - 
		shared_1.threat_time__[j - 1] * shared_1.we * 
		shared_1.radtodeg;
	if (threat_lon__ < (float)-180.) {
	    threat_lon__ += (float)360.;
	}
	if (threat_lon__ > (float)180.) {
	    threat_lon__ += (float)-360.;
	}
	s_wsfe(&io___8);
	do_fio(&c__1, (char *)&wta_solver__1.threat_id__[j - 1], (ftnlen)
		sizeof(integer));
	do_fio(&c__1, (char *)&threat_lat__, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&threat_lon__, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&shared_1.threat_time__[j - 1], (ftnlen)sizeof(
		real));
	e_wsfe();
/* L10: */
    }
    distance_(ddist, &c__1751, shared_1.asset_xyz__, &c__1751, &
	    shared_1.num_assets__);
/*  asset is threatened if within radius of expected impact */
    s_wsle(&io___10);
    do_lio(&c__9, &c__1, "  threatened sites ", 19L);
    e_wsle();
    i__1 = shared_1.num_assets__;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = shared_1.num_threats__;
	for (j = 1; j <= i__2; ++j) {
	    if (ddist[i__ + j * 1751 - 1752] < shared_1.radius) {
		s_wsfe(&io___12);
		do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&wta_solver__1.threat_id__[j - 1], (
			ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* L20: */
	}
/* L30: */
    }
    return 0;
} /* expected_damage__ */

