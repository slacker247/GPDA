/* fortsetup.f -- translated by f2c (version 19960717).
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

/* Subroutine */ int threatinit_(index, misstype, plaunch, pimpact, tlaunch, 
	limpact)
integer *index;
doublereal *misstype, *plaunch, *pimpact;
real *tlaunch, *limpact;
{
    /* System generated locals */
    static doublereal equiv_0[1];

    /* Builtin functions */
    /* Subroutine */ int s_copy();

    /* Local variables */
    static integer i__;
#define itemp (equiv_0)
#define chtemp ((char *)equiv_0)
    static char chtype[8];
    static integer ind;





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

    ind = *index + 1;
    if (ind > 50) {
	return 0;
    }

    *itemp = *misstype;
    s_copy(chtype, "        ", 8L, 8L);
    for (i__ = 1; i__ <= 8; ++i__) {
	if (*(unsigned char *)&chtemp[i__ - 1] == '\0') {
	    goto L110;
	}
	*(unsigned char *)&chtype[i__ - 1] = *(unsigned char *)&chtemp[i__ - 
		1];
    }
L110:
    s_copy(shared_1.threat_type__ + (ind - 1 << 3), chtype, 8L, 8L);

    *itemp = *pimpact;
    s_copy(chtype, "        ", 8L, 8L);
    for (i__ = 1; i__ <= 8; ++i__) {
	if (*(unsigned char *)&chtemp[i__ - 1] == '\0') {
	    goto L120;
	}
	*(unsigned char *)&chtype[i__ - 1] = *(unsigned char *)&chtemp[i__ - 
		1];
    }
L120:
    s_copy(shared_1.threat_impact__ + (ind - 1 << 3), chtype, 8L, 8L);

/*     write(*,*) threat_type(ind), '   ', threat_impact(ind) */
    return 0;
} /* threatinit_ */

#undef chtemp
#undef itemp


/* Subroutine */ int threatsetup_(x1, x2, x3, v1, v2, v3, thetime, 
	start_time__, ntracks, id, ithrt)
doublereal *x1, *x2, *x3, *v1, *v2, *v3, *thetime, *start_time__;
integer *ntracks, *id, *ithrt;
{


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
    shared_1.war_time__[*ithrt - 1] = *thetime;
    shared_1.num_threats__ = *ntracks;
    shared_1.threat_state__[*ithrt - 1] = *x1 * (float)1e3;
    shared_1.threat_state__[*ithrt + 49] = *x2 * (float)1e3;
    shared_1.threat_state__[*ithrt + 99] = *x3 * (float)1e3;
    shared_1.threat_state__[*ithrt + 149] = *v1 * (float)1e3;
    shared_1.threat_state__[*ithrt + 199] = *v2 * (float)1e3;
    shared_1.threat_state__[*ithrt + 249] = *v3 * (float)1e3;
/*     print *, ' in threat_setup ',x1, x2, x3, v1, v2, v3, */
/*    *                               start_time, thetime */
    shared_1.current_time__ = *start_time__;
    shared_1.threat_time__[*ithrt - 1] = *thetime;
    wta_solver__1.threat_id__[*ithrt - 1] = *id;
    shared_1.threat_play__[*ithrt - 1] = TRUE_;
    return 0;
} /* threatsetup_ */

/* Subroutine */ int settpt_()
{


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
    logicals_1.tpt = TRUE_;
    return 0;
} /* settpt_ */

/* Subroutine */ int resettpt_()
{


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
    logicals_1.tpt = FALSE_;
    return 0;
} /* resettpt_ */

/* Subroutine */ int initthreatplay_()
{
    static integer i__, j, n;



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
    for (n = 1; n <= 50; ++n) {
	shared_1.threat_play__[n - 1] = FALSE_;
/* L10: */
    }
    logicals_1.switch__ = TRUE_;
    for (i__ = 1; i__ <= 38; ++i__) {
	for (j = 1; j <= 50; ++j) {
	    wta_solver__1.wta_matrix__[i__ + j * 38 - 39] = 9000000;
	    wta_solver__1.wta_time__[i__ + j * 38 - 39] = (float)0.;
/* L30: */
	}
/* L40: */
    }
    return 0;
} /* initthreatplay_ */

/* Subroutine */ int getnassign_(n)
integer *n;
{


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
    *n = shared_1.num_threats__;
    return 0;
} /* getnassign_ */

/* Subroutine */ int getassign_(i__, thrt, weap, etime)
integer *i__, *thrt, *weap;
doublereal *etime;
{


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
    *thrt = wta_solver__1.threat_id__[wta_plans__1.bid[*i__ - 1] - 1];
    *weap = wta_solver__1.map[*i__ - 1];
    *etime = wta_solver__1.wta_time__[wta_solver__1.map[*i__ - 1] + 
	    wta_plans__1.bid[*i__ - 1] * 38 - 39];
    return 0;
} /* getassign_ */

