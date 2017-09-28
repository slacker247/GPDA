/** Header file generated with fdesign on Wed Oct  8 13:06:11 2003.**/

#ifndef FD_cbpinput_h_
#define FD_cbpinput_h_

/** Callbacks, globals and object handlers **/
extern void CBPnoneCB(FL_OBJECT *, long);
extern void caseCB(FL_OBJECT *, long);
extern void jcsordersCB(FL_OBJECT *, long);
extern void CBPexitCB(FL_OBJECT *, long);
extern void genweightCB(FL_OBJECT *, long);
extern void genselectCB(FL_OBJECT *, long);
extern void coaviewCB(FL_OBJECT *, long);
extern void catselectCB(FL_OBJECT *, long);
extern void goallistCB(FL_OBJECT *, long);
extern void controlparmsCB(FL_OBJECT *, long);
extern void genbrowserCB(FL_OBJECT *, long);
extern void morekeysCB(FL_OBJECT *, long);
extern void lesskeysCB(FL_OBJECT *, long);

extern void CBPnoneCB(FL_OBJECT *, long);
extern void caseCB(FL_OBJECT *, long);
extern void caseinfoCB(FL_OBJECT *, long);

extern void viewdoneCB(FL_OBJECT *, long);
extern void viewbrowserCB(FL_OBJECT *, long);

extern void CBPnoneCB(FL_OBJECT *, long);
extern void COAexitCB(FL_OBJECT *, long);

extern void CBPnoneCB(FL_OBJECT *, long);
extern void COAexitCB(FL_OBJECT *, long);
extern void cbpscoreCB(FL_OBJECT *, long);
extern void jcsordersCB(FL_OBJECT *, long);
extern void caseselectCB(FL_OBJECT *, long);

extern void COAexitCB(FL_OBJECT *, long);
extern void RDEnoneCB(FL_OBJECT *, long);
extern void jcsordersCB(FL_OBJECT *, long);
extern void CBPnoneCB(FL_OBJECT *, long);
extern void rdehelpCB(FL_OBJECT *, long);
extern void rdeactionCB(FL_OBJECT *, long);
extern void rdetextCB(FL_OBJECT *, long);


extern void CBPnoneCB(FL_OBJECT *, long);
extern void COAexitCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *cbpinput;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *cbp_caseid;
	FL_OBJECT *cbp_save;
	FL_OBJECT *cbp_exec;
	FL_OBJECT *gen_weighting;
	FL_OBJECT *jcs_orders;
	FL_OBJECT *view_order;
	FL_OBJECT *gen_select;
	FL_OBJECT *user_default;
	FL_OBJECT *cbp_view;
	FL_OBJECT *gen_thumb;
	FL_OBJECT *category;
	FL_OBJECT *cbp_criteria;
	FL_OBJECT *indice_browser;
	FL_OBJECT *cbp_goallist[4];
	FL_OBJECT *cbp_threatlist[4];
	FL_OBJECT *cbp_assetlist[4];
	FL_OBJECT *cbp_cats[3];
	FL_OBJECT *morekeys[3][3];
	FL_OBJECT *lesskeys[3][3];
} FD_cbpinput;

extern FD_cbpinput * create_form_cbpinput(void);
typedef struct {
	FL_FORM *caseinfo;
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
} FD_caseinfo;

extern FD_caseinfo * create_form_caseinfo(void);
typedef struct {
	FL_FORM *cbpview;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *view_browser;
} FD_cbpview;

extern FD_cbpview * create_form_cbpview(void);
typedef struct {
	FL_FORM *cbpmapview;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *cbpmap_caseid;
	FL_OBJECT *pix_icon[16];
	FL_OBJECT *pix_count[16];
} FD_cbpmapview;

extern FD_cbpmapview * create_form_cbpmapview(void);
typedef struct {
	FL_FORM *cbpcoaview;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *cbpcoa_caseid;
	FL_OBJECT *verttext;
	FL_OBJECT *cbp_case_select;
	FL_OBJECT *cbp_case_active;
	FL_OBJECT *coa_top[3];
	FL_OBJECT *coasub_assets[4];
	FL_OBJECT *coasub_threat[4];
	FL_OBJECT *coa_class[4];
	FL_OBJECT *cbp_score[46];
	FL_OBJECT *coa_score[8];
	FL_OBJECT *case_name[8];
} FD_cbpcoaview;

extern FD_cbpcoaview * create_form_cbpcoaview(void);
typedef struct {
	FL_FORM *cbprdeinput;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *vu_jcs_warn;
	FL_OBJECT *rde_jcswo;
	FL_OBJECT *cbprde_caseid;
	FL_OBJECT *ro_browser;
	FL_OBJECT *rde_jcscoa[8];
	FL_OBJECT *rde_coaper[8];
} FD_cbprdeinput;

extern FD_cbprdeinput * create_form_cbprdeinput(void);
typedef struct {
	FL_FORM *cbprdehelp;
	void *vdata;
	char *cdata;
	long  ldata;
} FD_cbprdehelp;

extern FD_cbprdehelp * create_form_cbprdehelp(void);
typedef struct {
	FL_FORM *cbprdetext;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *cbprdedisclaim;
} FD_cbprdetext;

extern FD_cbprdetext * create_form_cbprdetext(void);

#endif /* FD_cbpinput_h_ */
