/** Header file generated with fdesign on Fri Mar 14 10:19:36 2003.**/

#ifndef FD_bmc3_h_
#define FD_bmc3_h_

/** Callbacks, globals and object handlers **/
extern void signoffCB(FL_OBJECT *, long);
extern void BMCnoneCB(FL_OBJECT *, long);
extern void dirstatusCB(FL_OBJECT *, long);
extern void wpnchartCB(FL_OBJECT *, long);
extern void missummaryCB(FL_OBJECT *, long);
extern void map2dCB(FL_OBJECT *, long);
extern void nothingCB(FL_OBJECT *, long);
extern void nmdweapsumCB(FL_OBJECT *, long);
extern void sdfweapsumCB(FL_OBJECT *, long);
extern void NMDinjectCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);
extern void browseexitCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *bmc3;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *clocktime;
	FL_OBJECT *classification;
	FL_OBJECT *sum_label;
	FL_OBJECT *plan_val;
	FL_OBJECT *defcon_label;
	FL_OBJECT *rp_label;
	FL_OBJECT *dea_label;
	FL_OBJECT *roe_label;
	FL_OBJECT *msncon_label;
	FL_OBJECT *msnobj_label;
	FL_OBJECT *plan_label;
	FL_OBJECT *sdf_chart;
	FL_OBJECT *map2d;
	FL_OBJECT *nmd_chart;
	FL_OBJECT *projectname;
	FL_OBJECT *nmd_weap_menu;
	FL_OBJECT *sdf_weap_menu;
	FL_OBJECT *msnobj_val;
	FL_OBJECT *bmc_inject;
	FL_OBJECT *defcon_val;
	FL_OBJECT *rp_val;
	FL_OBJECT *dea_val;
	FL_OBJECT *roe_val;
	FL_OBJECT *msncon_val;
	FL_OBJECT *summ_label[8];
	FL_OBJECT *summ_value[8];
} FD_bmc3;

extern FD_bmc3 * create_form_bmc3(void);
typedef struct {
	FL_FORM *bmcbrowse;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *bmc_browse;
} FD_bmcbrowse;

extern FD_bmcbrowse * create_form_bmcbrowse(void);

#endif /* FD_bmc3_h_ */
