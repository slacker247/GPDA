/* read_data.f -- translated by f2c (version 19991025).
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

static integer c__1 = 1;
static integer c__4 = 4;
static integer c__3 = 3;
static integer c__9 = 9;

/* Subroutine */ int readdat_()
{
    /* Format strings */
    static char fmt_99[] = "(i4,1x,i2,1x,2(f8.3,1x),4a1,1x,13(f6.3,1x),i1,1x\
,i1)";

    /* System generated locals */
    integer i__1;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open(), s_rsfe(), do_fio(), e_rsfe();
    double sin(), cos();
    integer f_clos(), s_rsle(), do_lio(), e_rsle();

    /* Local variables */
    static integer iorb, idum;
    static real xlat, valu[12], xlon, asset_vu__;
    static integer i__, j, k;
    static real x, y, z__;
    static integer modid[2];
    static real cvinc, cvlan;
    static integer kvgid;
    static real cvaop, cvalt, cvaop_old__;
    static integer nsats;
    static real s1;
    static integer id, in, iclass;
    static real cvlans;
    static integer icvinv;
    static char string[1*4];
    static real dum;
    static integer idx;

    /* Fortran I/O blocks */
    static cilist io___3 = { 0, 0, 1, fmt_99, 0 };
    static cilist io___22 = { 0, 0, 1, 0, 0 };
    static cilist io___32 = { 0, 0, 1, 0, 0 };




/*  parameters */

/*     MAX_NUM_ASSETS     ...    maximum number of assets */
/*     MAX_NUM_THREATS    ...    maximum number of threats */
/*     MAX_NUM_RINGS      ...    maximum number of pebble rings */
/*     MAX_NUM_SATS       ...    maximum number of pebbles per ring */
/*     MAX_NUM_PEBBLES    ...    maximun number of pebbles */
/*     MAX_NUM_GBIS       ...    maximum number of ground based intercepters */
/*     MAX_NUM_SITES      ...    maximum number of weapon sites */
/*     MAX_NUM_WEAPONS    ...    maximum number of weapons */
/*     MAX_NUM_SHOTS      ...    maximum number of shots per GBI farm */



/*  impact_prediction */
/*  variables needed by tpt codes */
    in = 1;
    o__1.oerr = 0;
    o__1.ounit = in;
    o__1.ofnmlen = 11;
    o__1.ofnm = "greenta.dat";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = "formatted";
    o__1.oblnk = 0;
    f_open(&o__1);
/*     write(*,*) "Read_data: Reading asset data from greenta.dat" */
    shared_1.num_assets__ = 0;
    for (i__ = 1; i__ <= 1751; ++i__) {
	io___3.ciunit = in;
	i__1 = s_rsfe(&io___3);
	if (i__1 != 0) {
	    goto L25;
	}
	i__1 = do_fio(&c__1, (char *)&id, (ftnlen)sizeof(integer));
	if (i__1 != 0) {
	    goto L25;
	}
	i__1 = do_fio(&c__1, (char *)&iclass, (ftnlen)sizeof(integer));
	if (i__1 != 0) {
	    goto L25;
	}
	i__1 = do_fio(&c__1, (char *)&xlat, (ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L25;
	}
	i__1 = do_fio(&c__1, (char *)&xlon, (ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L25;
	}
	for (k = 1; k <= 4; ++k) {
	    i__1 = do_fio(&c__1, string + (k - 1), (ftnlen)1);
	    if (i__1 != 0) {
		goto L25;
	    }
	}
	for (k = 0; k <= 11; ++k) {
	    i__1 = do_fio(&c__1, (char *)&valu[k], (ftnlen)sizeof(real));
	    if (i__1 != 0) {
		goto L25;
	    }
	}
	i__1 = do_fio(&c__1, (char *)&dum, (ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L25;
	}
	for (k = 1; k <= 2; ++k) {
	    i__1 = do_fio(&c__1, (char *)&modid[k - 1], (ftnlen)sizeof(
		    integer));
	    if (i__1 != 0) {
		goto L25;
	    }
	}
	i__1 = e_rsfe();
	if (i__1 != 0) {
	    goto L25;
	}
	shared_1.num_assets__ = i__;
	asset_vu__ = (real) (*(unsigned char *)&string[0] * 10 + *(unsigned 
		char *)&string[1]);
	shared_1.asset_hardns3__[i__ - 1] = *(unsigned char *)&string[3];
	shared_1.asset_id__[i__ - 1] = (real) id;
	shared_1.asset_lat__[i__ - 1] = xlat;
	shared_1.asset_lon__[i__ - 1] = xlon;
	shared_1.asset_class__[i__ - 1] = (real) iclass;
	xlat = ((float)90. - xlat) / shared_1.radtodeg;
	xlon /= shared_1.radtodeg;
	s1 = sin(xlat);
	x = s1 * cos(xlon);
	y = s1 * sin(xlon);
	z__ = cos(xlat);
	shared_1.asset_xyz__[i__ - 1] = shared_1.rearth * x;
	shared_1.asset_xyz__[i__ + 1750] = shared_1.rearth * y;
	shared_1.asset_xyz__[i__ + 3501] = shared_1.rearth * z__;
	for (j = 0; j <= 11; ++j) {
	    shared_1.asset_value__[i__ + j * 1751 - 1] = valu[j];
/* L10: */
	}
/* L20: */
    }
L25:
    cl__1.cerr = 0;
    cl__1.cunit = in;
    cl__1.csta = 0;
    f_clos(&cl__1);
    nsats = 0;
    cvlans = (float)-999.;
    for (i__ = 1; i__ <= 514; ++i__) {
	shared_1.pebble[i__ - 1] = FALSE_;
/* L30: */
    }
    shared_1.num_pebbles__ = 0;
    iorb = 0;
    if (logicals_1.pebbles_on__) {
	o__1.oerr = 0;
	o__1.ounit = in;
	o__1.ofnmlen = 13;
	o__1.ofnm = "evconst10.dat";
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = "formatted";
	o__1.oblnk = 0;
	f_open(&o__1);
/*     write(*,*) "Read_data: Reading pebble data from evconst10.dat" */
	for (i__ = 1; i__ <= 514; ++i__) {
	    io___22.ciunit = in;
	    i__1 = s_rsle(&io___22);
	    if (i__1 != 0) {
		goto L55;
	    }
	    i__1 = do_lio(&c__4, &c__1, (char *)&cvalt, (ftnlen)sizeof(real));
	    if (i__1 != 0) {
		goto L55;
	    }
	    i__1 = do_lio(&c__4, &c__1, (char *)&cvinc, (ftnlen)sizeof(real));
	    if (i__1 != 0) {
		goto L55;
	    }
	    i__1 = do_lio(&c__4, &c__1, (char *)&cvaop, (ftnlen)sizeof(real));
	    if (i__1 != 0) {
		goto L55;
	    }
	    i__1 = do_lio(&c__4, &c__1, (char *)&cvlan, (ftnlen)sizeof(real));
	    if (i__1 != 0) {
		goto L55;
	    }
	    i__1 = do_lio(&c__3, &c__1, (char *)&icvinv, (ftnlen)sizeof(
		    integer));
	    if (i__1 != 0) {
		goto L55;
	    }
	    i__1 = do_lio(&c__3, &c__1, (char *)&idx, (ftnlen)sizeof(integer))
		    ;
	    if (i__1 != 0) {
		goto L55;
	    }
	    i__1 = do_lio(&c__3, &c__1, (char *)&kvgid, (ftnlen)sizeof(
		    integer));
	    if (i__1 != 0) {
		goto L55;
	    }
	    i__1 = do_lio(&c__3, &c__1, (char *)&idum, (ftnlen)sizeof(integer)
		    );
	    if (i__1 != 0) {
		goto L55;
	    }
	    i__1 = e_rsle();
	    if (i__1 != 0) {
		goto L55;
	    }
	    if (kvgid >= 80000000) {
		++nsats;
		shared_1.pebble[nsats - 1] = TRUE_;
		if (cvlans != cvlan) {
		    ++iorb;
		    cvlans = cvlan;
		    cvaop_old__ = cvaop;
		    odata_1.orbalt[nsats - 1] = cvalt;
		    odata_1.orbecc[nsats - 1] = (float)0.;
		    odata_1.orbinc[nsats - 1] = cvinc;
		    odata_1.orblan[nsats - 1] = cvlan;
		    odata_1.orbaop[nsats - 1] = cvaop;
		} else {
		    odata_1.orbalt[nsats - 1] = odata_1.orbalt[nsats - 2];
		    odata_1.orbecc[nsats - 1] = odata_1.orbecc[nsats - 2];
		    odata_1.orbinc[nsats - 1] = odata_1.orbinc[nsats - 2];
		    odata_1.orblan[nsats - 1] = odata_1.orblan[nsats - 2];
		    odata_1.orbaop[nsats - 1] = odata_1.orbaop[nsats - 2];
		}
		sdata_1.sataop[nsats - 1] = (cvaop - cvaop_old__) * 
			shared_1.degtorad;
	    }
/* L50: */
	}
L55:
	shared_1.num_pebbles__ = nsats;
	cl__1.cerr = 0;
	cl__1.cunit = in;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }

/*       ----- Read gbi information ----- */

    in = 4;
    o__1.oerr = 0;
    o__1.ounit = in;
    o__1.ofnmlen = 7;
    o__1.ofnm = "gbi.dat";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = "formatted";
    o__1.oblnk = 0;
    f_open(&o__1);
/*     write(*,*) "Read_data: Reading gbi data from gbi.dat" */
    shared_1.num_gbis__ = 0;
    for (i__ = 1; i__ <= 14; ++i__) {
	io___32.ciunit = in;
	i__1 = s_rsle(&io___32);
	if (i__1 != 0) {
	    goto L65;
	}
	i__1 = do_lio(&c__3, &c__1, (char *)&sdata_1.gbi_id__[i__ - 1], (
		ftnlen)sizeof(integer));
	if (i__1 != 0) {
	    goto L65;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&xlat, (ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L65;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&xlon, (ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L65;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&sdata_1.gbi_pk__[i__ - 1], (
		ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L65;
	}
	i__1 = do_lio(&c__3, &c__1, (char *)&sdata_1.gbi_gbis__[i__ - 1], (
		ftnlen)sizeof(integer));
	if (i__1 != 0) {
	    goto L65;
	}
	i__1 = do_lio(&c__9, &c__1, sdata_1.gbi_name__ + (i__ - 1 << 4), (
		ftnlen)16);
	if (i__1 != 0) {
	    goto L65;
	}
	i__1 = e_rsle();
	if (i__1 != 0) {
	    goto L65;
	}
	sdata_1.gbi_lat__[i__ - 1] = xlat * shared_1.degtorad;
	sdata_1.gbi_lon__[i__ - 1] = xlon * shared_1.degtorad;
	++nsats;
	++shared_1.num_gbis__;
	shared_1.pebble[nsats - 1] = FALSE_;
	xlat = ((float)90. - xlat) / shared_1.radtodeg;
	xlon /= shared_1.radtodeg;
	s1 = sin(xlat);
	x = s1 * cos(xlon);
	y = s1 * sin(xlon);
	z__ = cos(xlat);
	sdata_1.site_xyz__[nsats - 1] = shared_1.rearth * x;
	sdata_1.site_xyz__[nsats + 37] = shared_1.rearth * y;
	sdata_1.site_xyz__[nsats + 75] = shared_1.rearth * z__;
/* L60: */
    }
L65:
    cl__1.cerr = 0;
    cl__1.cunit = in;
    cl__1.csta = 0;
    f_clos(&cl__1);

/*       ----- Tell the world what we read ----- */

    shared_1.num_weapon_sites__ = shared_1.num_pebbles__ + 
	    shared_1.num_gbis__;
/*     write (*, *)  ' *********** summary of simulation **********' */
/*     write (*, 98) iorb */
/*     write (*, 94) num_assets */
/*     write (*, 97) num_pebbles */
/*     write (*, 96) num_gbis */
/*     write (*, 95) MAX_NUM_SHOTS */
/* L95: */
/* L94: */
/* L96: */
/* L97: */
/* L98: */
    return 0;
} /* readdat_ */

