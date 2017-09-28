/** Header file generated with fdesign on Fri Apr 28 13:17:43 2000.**/

#ifndef FD_bmc3_h_
#define FD_bmc3_h_

/** Callbacks, globals and object handlers **/
extern void signoffCB(FL_OBJECT *, long);
extern void nothingCB(FL_OBJECT *, long);
extern void intel_browserCB(FL_OBJECT *, long);
extern void lockCB(FL_OBJECT *, long);
extern void wpnchartCB(FL_OBJECT *, long);
extern void trackmenuCB(FL_OBJECT *, long);
extern void optionmenuCB(FL_OBJECT *, long);
extern void cbpnoneCB(FL_OBJECT *, long);
extern void missummaryCB(FL_OBJECT *, long);
extern void actwindowsCB(FL_OBJECT *, long);
extern void warfogCB(FL_OBJECT *, long);

extern void loginCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);

extern void positionCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *bmc3;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *defcon_val;
	FL_OBJECT *sim_history;
	FL_OBJECT *intel_browser;
	FL_OBJECT *node_val;
	FL_OBJECT *plan_val;
	FL_OBJECT *rp_val;
	FL_OBJECT *dea_val;
	FL_OBJECT *roe_val;
	FL_OBJECT *wpn_chart;
	FL_OBJECT *track_menu;
	FL_OBJECT *option_menu;
	FL_OBJECT *sum_tot_rvs;
	FL_OBJECT *sum_tot_tgt;
	FL_OBJECT *sum_tgt_kill;
	FL_OBJECT *sum_cur_tgt;
	FL_OBJECT *sum_leakers;
	FL_OBJECT *sum_tgt_trk;
	FL_OBJECT *sum_tgt_eng;
	FL_OBJECT *sum_tgt_not;
	FL_OBJECT *act_windows;
	FL_OBJECT *fogofwar;
} FD_bmc3;

extern FD_bmc3 * create_form_bmc3(void);
typedef struct {
	FL_FORM *login;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *username;
	FL_OBJECT *password;
} FD_login;

extern FD_login * create_form_login(void);
typedef struct {
	FL_FORM *cmocnode;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *positions;
	FL_OBJECT *centers;
	FL_OBJECT *position[14];
} FD_cmocnode;

extern FD_cmocnode * create_form_cmocnode(void);

#endif /* FD_bmc3_h_ */
