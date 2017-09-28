/* engagement_plan.f -- translated by f2c (version 19960717).
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

struct {
    integer plan_threats__;
    char plan_lines__[6000];
} planout_;

#define planout_1 planout_

/* Table of constant values */

static integer c__1 = 1;
static real c_b31 = (float).67;
static integer c__3 = 3;

/* Subroutine */ int engagement_plan__()
{
    /* Initialized data */

    static char chtrack[8+1] = "In_Trk  ";
    static char chengage[8+1] = "Engaged ";
    static logical killed[50] = { FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_ };

    /* Format strings */
    static char fmt_79[] = "(\002Number of threats processed is \002,i4,\002\
 at time \002,f10.2,/)";
    static char fmt_84[] = "(60x,15x,\002Track Engagement View\002)";
    static char fmt_80[] = "(\002 Trk     Obj    Msl    Exp   Leth     Tgt\
 \002,\002    Pre Imp   T to\002)";
    static char fmt_81[] = "(\002 ID     Type    Type   Tgts  Value    Typ\
e\002,\002    Loc.     Impact\002)";
    static char fmt_89[] = "(\002----  -------- ------ ----  ------  -------\
- \002,\002--------  ------\002,\002 -------- -------- -------- ---  ----\
-\002,\002 -----   ----\002)";
    static char fmt_99[] = "(4x,i3,\002 (pebble)\002,1x,i4,5x,f10.2)";
    static char fmt_91[] = "(i4,2x,a8,2x,a6,1x,i4,2x,f6.2,4x,a8,1x,a8,2x,i2\
,\002:\002,i2.2,1x)";
    static char fmt_92[] = "(\002|\002,1x,a8,1x,a8,1x,a8,1x,i3,2x,i2,\002\
:\002,i2.2,f6.3,3x,1x,i2,1x)";
    static char fmt_97[] = "(a64,a52)";

    /* System generated locals */
    address a__1[3];
    integer i__1, i__2, i__3[3];
    real r__1;

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();
    /* Subroutine */ int s_copy();
    integer s_wsfi(), e_wsfi();
    /* Subroutine */ int s_cat();

    /* Local variables */
    static integer igbi;
    static real tcurrent;
    static integer i__, id, second, minute;
    static real tintercept;
    static char chpart1[64], chpart2[52];
    static real tti, timpact;

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 6, 0, fmt_79, 0 };
    static cilist io___6 = { 0, 6, 0, fmt_84, 0 };
    static cilist io___7 = { 0, 6, 0, fmt_80, 0 };
    static cilist io___8 = { 0, 6, 0, fmt_81, 0 };
    static cilist io___9 = { 0, 6, 0, fmt_89, 0 };
    static cilist io___13 = { 0, 6, 0, fmt_99, 0 };
    static icilist io___20 = { 0, chpart1, 0, fmt_91, 64, 1 };
    static icilist io___22 = { 0, chpart2, 0, fmt_92, 52, 1 };
    static cilist io___23 = { 0, 6, 0, fmt_97, 0 };



/*  engagement plan */



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



/*      print *, ' ' */
/*      print *, ' ***** engagement plan as follows:' */
/*      print *, '    weapon id   threat id    time' */
    tcurrent = shared_1.current_time__;
    planout_1.plan_threats__ = shared_1.num_threats__;
    s_wsfe(&io___5);
    do_fio(&c__1, (char *)&shared_1.num_threats__, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&tcurrent, (ftnlen)sizeof(real));
    e_wsfe();
    s_wsfe(&io___6);
    e_wsfe();
    s_wsfe(&io___7);
    e_wsfe();
    s_wsfe(&io___8);
    e_wsfe();
    s_wsfe(&io___9);
    e_wsfe();
    i__1 = shared_1.num_threats__;
    for (i__ = 1; i__ <= i__1; ++i__) {
	id = wta_solver__1.threat_id__[wta_plans__1.bid[i__ - 1] - 1];
	igbi = wta_solver__1.map[i__ - 1];
	if (wta_solver__1.map[i__ - 1] > 0) {
	    if (wta_solver__1.map[i__ - 1] <= shared_1.num_pebbles__) {
		s_wsfe(&io___13);
		do_fio(&c__1, (char *)&wta_solver__1.map[i__ - 1], (ftnlen)
			sizeof(integer));
		do_fio(&c__1, (char *)&id, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&wta_solver__1.wta_time__[
			wta_solver__1.map[i__ - 1] + wta_plans__1.bid[i__ - 1]
			 * 38 - 39], (ftnlen)sizeof(real));
		e_wsfe();
	    } else {
/* 	         print 98, map(i), id, wta_time(map (i), bid (i)) 
*/
		tintercept = wta_solver__1.wta_time__[wta_solver__1.map[i__ - 
			1] + wta_plans__1.bid[i__ - 1] * 38 - 39];
		timpact = shared_1.threat_time__[wta_plans__1.bid[i__ - 1] - 
			1];

/*                write(*,*) Tcurrent, Tintercept, Timpact */
		s_copy(chtrack, "In_Trk", 8L, 6L);
		s_copy(chengage, "Engaged", 8L, 7L);
		if (tcurrent < timpact) {
		    tti = timpact - tcurrent;
		    minute = (r__1 = tti / (float)60., (integer) dabs(r__1));
		    second = (r__1 = tti - minute * (float)60., (integer) 
			    dabs(r__1));
		} else {
		    if (! killed[wta_plans__1.bid[i__ - 1] - 1]) {
			s_copy(chtrack, "Impact", 8L, 6L);
		    }
		    minute = 0;
		    second = 0;
		}
		s_wsfi(&io___20);
		do_fio(&c__1, (char *)&id, (ftnlen)sizeof(integer));
		do_fio(&c__1, "Obj", 3L);
		do_fio(&c__1, shared_1.threat_type__ + (wta_plans__1.bid[i__ 
			- 1] - 1 << 3), 8L);
		do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c_b31, (ftnlen)sizeof(real));
		do_fio(&c__1, "Tgt", 3L);
		do_fio(&c__1, shared_1.threat_impact__ + (wta_plans__1.bid[
			i__ - 1] - 1 << 3), 8L);
		do_fio(&c__1, (char *)&minute, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&second, (ftnlen)sizeof(integer));
		e_wsfi();

		if (tcurrent < tintercept) {
		    tti = tintercept - tcurrent;
		    minute = (r__1 = tti / (float)60., (integer) dabs(r__1));
		    second = (r__1 = tti - minute * (float)60., (integer) 
			    dabs(r__1));
		} else {
		    killed[wta_plans__1.bid[i__ - 1] - 1] = TRUE_;
		    s_copy(chengage, "Killed", 8L, 6L);
		    s_copy(chtrack, "Killed", 8L, 6L);
		    minute = 0;
		    second = 0;
		}
		s_wsfi(&io___22);
		do_fio(&c__1, chtrack, 8L);
		do_fio(&c__1, sdata_1.gbi_name__ + (igbi - 1 << 4), 16L);
		do_fio(&c__1, chengage, 8L);
		do_fio(&c__1, (char *)&sdata_1.gbi_gbis__[igbi - 1], (ftnlen)
			sizeof(integer));
		do_fio(&c__1, (char *)&minute, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&second, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&sdata_1.gbi_pk__[igbi - 1], (ftnlen)
			sizeof(real));
		i__2 = (r__1 = (timpact - (float)200. - tcurrent) / (float)
			400., (integer) dabs(r__1));
		do_fio(&c__1, (char *)&i__2, (ftnlen)sizeof(integer));
		e_wsfi();

		s_wsfe(&io___23);
		do_fio(&c__1, chpart1, 64L);
		do_fio(&c__1, chpart2, 52L);
		e_wsfe();
/* Writing concatenation */
		i__3[0] = 64, a__1[0] = chpart1;
		i__3[1] = 52, a__1[1] = chpart2;
		i__3[2] = 1, a__1[2] = "\000";
		s_cat(planout_1.plan_lines__ + (i__ - 1) * 120, a__1, i__3, &
			c__3, 120L);
	    }
	}
/* L10: */
    }
    return 0;
/* 64 + next size MUST <= 119 */
/* 52 */
/* L98: */
} /* engagement_plan__ */

integer getnthreats_()
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static real get_n_threats__;



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

    get_n_threats__ = (real) planout_1.plan_threats__;
    return ret_val;
} /* getnthreats_ */

/* Subroutine */ int getplanline_(index, chline, chline_len)
integer *index;
char *chline;
ftnlen chline_len;
{
    /* Builtin functions */
    /* Subroutine */ int s_copy();



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

    s_copy(chline, planout_1.plan_lines__ + (*index - 1) * 120, 120L, 120L);
    return 0;
} /* getplanline_ */

