/** Header file generated with fdesign on Tue Sep  5 13:11:04 2000.**/

#ifndef FD_fixinput_h_
#define FD_fixinput_h_

/** Callbacks, globals and object handlers **/
extern void repairCB(FL_OBJECT *, long);
extern void FIXexitCB(FL_OBJECT *, long);
extern void genweightCB(FL_OBJECT *, long);
extern void genconstraintCB(FL_OBJECT *, long);
extern void threatconstraintCB(FL_OBJECT *, long);
extern void assetconstraintCB(FL_OBJECT *, long);
extern void cbpnoneCB(FL_OBJECT *, long);
extern void fixselectCB(FL_OBJECT *, long);
extern void genmenuCB(FL_OBJECT *, long);
extern void genselectCB(FL_OBJECT *, long);
extern void genbrowserCB(FL_OBJECT *, long);
extern void cbpcoaviewCB(FL_OBJECT *, long);
extern void changesCB(FL_OBJECT *, long);
extern void controlparmsCB(FL_OBJECT *, long);
extern void userviewCB(FL_OBJECT *, long);
extern void fixviewCB(FL_OBJECT *, long);

extern void cbpnoneCB(FL_OBJECT *, long);
extern void caseCB(FL_OBJECT *, long);
extern void caseinfoCB(FL_OBJECT *, long);

extern void viewdoneCB(FL_OBJECT *, long);
extern void viewbrowserCB(FL_OBJECT *, long);

extern void cbpnoneCB(FL_OBJECT *, long);
extern void cbpmapexitCB(FL_OBJECT *, long);
extern void cbpnonecb(FL_OBJECT *, long);
extern void cbpmapcoaCB(FL_OBJECT *, long);

extern void cbpnoneCB(FL_OBJECT *, long);
extern void cbpcoaexitCB(FL_OBJECT *, long);
extern void cbpscoreCB(FL_OBJECT *, long);
extern void cbpcoamapCB(FL_OBJECT *, long);
extern void jcsordersCB(FL_OBJECT *, long);
extern void cbpengageCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *fixinput;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *fix_caseid;
	FL_OBJECT *gen_thumb;
	FL_OBJECT *gen_weight;
	FL_OBJECT *gen_constraint;
	FL_OBJECT *threat_constraint;
	FL_OBJECT *asset_constraint;
	FL_OBJECT *indice_selected;
	FL_OBJECT *gen_weighting;
	FL_OBJECT *repair_worth;
	FL_OBJECT *gen_menu;
	FL_OBJECT *threat_menu;
	FL_OBJECT *asset_menu;
	FL_OBJECT *gen_select;
	FL_OBJECT *indice_browser;
	FL_OBJECT *cbp_view;
	FL_OBJECT *repair_add;
	FL_OBJECT *repair_rde;
	FL_OBJECT *controlgrp;
	FL_OBJECT *viewgrp;
	FL_OBJECT *user_view[1];
} FD_fixinput;

extern FD_fixinput * create_form_fixinput(void);
typedef struct {
	FL_FORM *fixinfo;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *info_ok;
	FL_OBJECT *info_cancel;
	FL_OBJECT *info_caseid;
	FL_OBJECT *info_desc;
	FL_OBJECT *info_prep;
	FL_OBJECT *info_bilt;
	FL_OBJECT *info_last;
	FL_OBJECT *info_used;
	FL_OBJECT *info_percent;
	FL_OBJECT *info_low;
	FL_OBJECT *info_med;
	FL_OBJECT *info_high;
} FD_fixinfo;

extern FD_fixinfo * create_form_fixinfo(void);
typedef struct {
	FL_FORM *fixview;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *view_browser;
} FD_fixview;

extern FD_fixview * create_form_fixview(void);
typedef struct {
	FL_FORM *fixmapview;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *cbpmap_caseid;
	FL_OBJECT *pix_sbirs;
	FL_OBJECT *pix_dsp;
	FL_OBJECT *pix_uewr;
	FL_OBJECT *pix_sensor;
	FL_OBJECT *pix_xbr;
	FL_OBJECT *gbi_count;
	FL_OBJECT *icbm_count;
	FL_OBJECT *threat_count;
	FL_OBJECT *pix_bomber;
} FD_fixmapview;

extern FD_fixmapview * create_form_fixmapview(void);
typedef struct {
	FL_FORM *fixcoaview;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *cbpcoa_caseid;
	FL_OBJECT *verttext;
	FL_OBJECT *coa_response;
	FL_OBJECT *coa_top[3];
	FL_OBJECT *coasub_assets[4];
	FL_OBJECT *coasub_threat[4];
	FL_OBJECT *coasub_mission[3];
	FL_OBJECT *cbpcoa_score[1];
	FL_OBJECT *coa_warnname[3];
	FL_OBJECT *coa_warnscore[3];
	FL_OBJECT *coa_score[9];
} FD_fixcoaview;

extern FD_fixcoaview * create_form_fixcoaview(void);

#endif /* FD_fixinput_h_ */
