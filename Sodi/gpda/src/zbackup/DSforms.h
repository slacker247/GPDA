/** Header file generated with fdesign on Mon Feb 26 14:11:48 2001.**/

#ifndef FD_input_h_
#define FD_input_h_

/** Callbacks, globals and object handlers **/
extern void DSBexitCB(FL_OBJECT *, long);
extern void input_saveCB(FL_OBJECT *, long);
extern void input_loadCB(FL_OBJECT *, long);
extern void input_netwCB(FL_OBJECT *, long);
extern void input_assessCB(FL_OBJECT *, long);
extern void input_modCB(FL_OBJECT *, long);
extern void input_addCB(FL_OBJECT *, long);
extern void input_delCB(FL_OBJECT *, long);
extern void DSBnoneCB(FL_OBJECT *, long);
extern void assess_tunitCB(FL_OBJECT *, long);
extern void input_menuCB(FL_OBJECT *, long);
extern void input_browserCB(FL_OBJECT *, long);
extern void executeCB(FL_OBJECT *, long);
extern void input_caseidCB(FL_OBJECT *, long);
extern void resetCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void assess_exitCB(FL_OBJECT *, long);
extern void assess_tunitCB(FL_OBJECT *, long);
extern void assess_issueCB(FL_OBJECT *, long);
extern void assess_selectCB(FL_OBJECT *, long);
extern void populateCB(FL_OBJECT *, long);
extern void evidenceCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void netw_exitCB(FL_OBJECT *, long);
extern void populateCB(FL_OBJECT *, long);
extern void netw_beliefCB(FL_OBJECT *, long);
extern void netw_plauseCB(FL_OBJECT *, long);
extern void netw_unknownCB(FL_OBJECT *, long);
extern void netw_disbeliefCB(FL_OBJECT *, long);

extern void add_getvalCB(FL_OBJECT *, long);
extern void add_sourceCB(FL_OBJECT *, long);
extern void add_acceptCB(FL_OBJECT *, long);
extern void assadd_cancelCB(FL_OBJECT *, long);
extern void DSBnoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *input;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *save_but;
	FL_OBJECT *load_but;
	FL_OBJECT *netw_but;
	FL_OBJECT *assm_but;
	FL_OBJECT *mod_but;
	FL_OBJECT *add_but;
	FL_OBJECT *del_but;
	FL_OBJECT *startime_text;
	FL_OBJECT *sdate_text;
	FL_OBJECT *top_sep;
	FL_OBJECT *timestep_text;
	FL_OBJECT *asstype_menu;
	FL_OBJECT *input_browser;
	FL_OBJECT *input_exec_but;
	FL_OBJECT *input_caseid;
	FL_OBJECT *assess_reset_but;
	FL_OBJECT *inputtunit[4];
	FL_OBJECT *inputtuint[3];
} FD_input;

extern FD_input * create_form_input(void);
typedef struct {
	FL_FORM *assess;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *conf_plot;
	FL_OBJECT *caseid_title;
	FL_OBJECT *case_id;
	FL_OBJECT *stime_text;
	FL_OBJECT *ctime_text;
	FL_OBJECT *sdate_text;
	FL_OBJECT *cdate_text;
	FL_OBJECT *timestep_text;
	FL_OBJECT *issue_but;
	FL_OBJECT *asstype_text;
	FL_OBJECT *assess_type;
	FL_OBJECT *best_title;
	FL_OBJECT *best_value;
	FL_OBJECT *evid_title;
	FL_OBJECT *ass_accid_but;
	FL_OBJECT *ass_anom_but;
	FL_OBJECT *ass_other_but;
	FL_OBJECT *ass_belief_text;
	FL_OBJECT *ass_host_but;
	FL_OBJECT *ass_graf_bot;
	FL_OBJECT *time_group;
	FL_OBJECT *propagate_button;
	FL_OBJECT *belief_pie;
	FL_OBJECT *disbelief_pie;
	FL_OBJECT *issue_group;
	FL_OBJECT *ass_browser;
	FL_OBJECT *assesstunit[4];
	FL_OBJECT *assess_disbelief[5];
	FL_OBJECT *assess_belief[5];
	FL_OBJECT *assm_text[4];
	FL_OBJECT *assess_round[6];
} FD_assess;

extern FD_assess * create_form_assess(void);
typedef struct {
	FL_FORM *network;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *hypot_text;
	FL_OBJECT *ass_text;
	FL_OBJECT *case_id;
	FL_OBJECT *assess_type;
	FL_OBJECT *ass_title;
	FL_OBJECT *evid_text;
	FL_OBJECT *stime_text;
	FL_OBJECT *ctime_text;
	FL_OBJECT *sdate_text;
	FL_OBJECT *cdate_text;
	FL_OBJECT *net_top_sep;
	FL_OBJECT *net_bot_sep;
	FL_OBJECT *propagate_button;
	FL_OBJECT *hyp_title[4];
	FL_OBJECT *src_title[6];
	FL_OBJECT *netw_text[3];
	FL_OBJECT *netw_src_belief[7];
	FL_OBJECT *netw_src_plause[7];
	FL_OBJECT *netw_src_unknown[7];
	FL_OBJECT *netw_hyp_unknown[5];
	FL_OBJECT *netw_hyp_plause[5];
	FL_OBJECT *netw_hyp_belief[5];
	FL_OBJECT *netw_ass_unknown[4];
	FL_OBJECT *netw_ass_disbelief[4];
	FL_OBJECT *netw_ass_belief[4];
} FD_network;

extern FD_network * create_form_network(void);
typedef struct {
	FL_FORM *assessadd;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *add_confid;
	FL_OBJECT *add_time;
	FL_OBJECT *add_src_choice;
	FL_OBJECT *add_accept;
	FL_OBJECT *assadd_cancel;
	FL_OBJECT *add_lat;
	FL_OBJECT *add_long;
	FL_OBJECT *add_alt;
	FL_OBJECT *add_desc;
	FL_OBJECT *add_plause;
	FL_OBJECT *add_source;
} FD_assessadd;

extern FD_assessadd * create_form_assessadd(void);

#endif /* FD_input_h_ */
