/** Header file generated with fdesign on Thu Nov  1 15:08:46 2001.**/

#ifndef FD_input_h_
#define FD_input_h_

/** Callbacks, globals and object handlers **/
extern void DSBnoneCB(FL_OBJECT *, long);
extern void DSBexitCB(FL_OBJECT *, long);
extern void input_saveCB(FL_OBJECT *, long);
extern void input_loadCB(FL_OBJECT *, long);
extern void input_displayCB(FL_OBJECT *, long);
extern void input_modCB(FL_OBJECT *, long);
extern void input_addCB(FL_OBJECT *, long);
extern void input_delCB(FL_OBJECT *, long);
extern void assess_tunitCB(FL_OBJECT *, long);
extern void input_menuCB(FL_OBJECT *, long);
extern void executeCB(FL_OBJECT *, long);
extern void resetCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void DSBexitCB(FL_OBJECT *, long);
extern void assess_tunitCB(FL_OBJECT *, long);
extern void assess_issueCB(FL_OBJECT *, long);
extern void populateCB(FL_OBJECT *, long);
extern void evidenceCB(FL_OBJECT *, long);
extern void assess_explainCB(FL_OBJECT *, long);
extern void assess_selectCB(FL_OBJECT *, long);
extern void input_addCB(FL_OBJECT *, long);
extern void input_modCB(FL_OBJECT *, long);
extern void input_delCB(FL_OBJECT *, long);
extern void executeCB(FL_OBJECT *, long);
extern void resetCB(FL_OBJECT *, long);
extern void input_displayCB(FL_OBJECT *, long);
extern void input_loadCB(FL_OBJECT *, long);
extern void input_saveCB(FL_OBJECT *, long);
extern void input_menuCB(FL_OBJECT *, long);
extern void plotolddataCB(FL_OBJECT *, long);
extern void assess_ctlCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);
extern void dseditorCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void addchoiceCB(FL_OBJECT *, long);
extern void add_acceptCB(FL_OBJECT *, long);
extern void dsbynameCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);
extern void dscontrolCB(FL_OBJECT *, long);
extern void dsimscrollCB(FL_OBJECT *, long);
extern void dsimdelayCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void neteditCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);
extern void populateCB(FL_OBJECT *, long);
extern void netdrawCB(FL_OBJECT *, long);
extern void neteddoneCB(FL_OBJECT *, long);
extern void assess_ctlCB(FL_OBJECT *, long);

extern void netedboxCB(FL_OBJECT *, long);
extern void DSBnoneCB(FL_OBJECT *, long);
extern void netedhelpCB(FL_OBJECT *, long);
extern void neteddoneCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void netedboxCB(FL_OBJECT *, long);
extern void neteddoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *input;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *load_hint;
	FL_OBJECT *exec_hint;
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
	FL_OBJECT *best_title;
	FL_OBJECT *best_value;
	FL_OBJECT *ass_belief_text;
	FL_OBJECT *time_group;
	FL_OBJECT *prop_but;
	FL_OBJECT *belief_pie;
	FL_OBJECT *disbelief_pie;
	FL_OBJECT *input_browser;
	FL_OBJECT *graf_group;
	FL_OBJECT *add_but;
	FL_OBJECT *mod_but;
	FL_OBJECT *del_but;
	FL_OBJECT *exec_but;
	FL_OBJECT *reset_but;
	FL_OBJECT *load_but;
	FL_OBJECT *save_but;
	FL_OBJECT *asstype_menu;
	FL_OBJECT *prev_but;
	FL_OBJECT *assess_level;
	FL_OBJECT *degrade;
	FL_OBJECT *assesstunit[4];
	FL_OBJECT *assess_disbelief[7];
	FL_OBJECT *assess_belief[7];
	FL_OBJECT *assm_text[6];
	FL_OBJECT *assess_graph[6];
	FL_OBJECT *ds_match[6];
} FD_assess;

extern FD_assess * create_form_assess(void);
typedef struct {
	FL_FORM *dsnetedit;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *case_id;
	FL_OBJECT *assess_type;
	FL_OBJECT *ass_title;
	FL_OBJECT *stime_text;
	FL_OBJECT *ctime_text;
	FL_OBJECT *sdate_text;
	FL_OBJECT *cdate_text;
	FL_OBJECT *net_top_sep;
	FL_OBJECT *src_box;
	FL_OBJECT *dsed_belief;
	FL_OBJECT *dsed_disbelief;
	FL_OBJECT *dsed_unknown;
	FL_OBJECT *dsed_lower[6];
	FL_OBJECT *dsed_top[6];
	FL_OBJECT *dsed_label[2];
	FL_OBJECT *dsed_top_grp[6];
	FL_OBJECT *dsed_top_b[6];
	FL_OBJECT *dsed_top_d[6];
	FL_OBJECT *dsed_top_u[6];
	FL_OBJECT *dsed_lower_grp[6];
	FL_OBJECT *dsed_lower_b[6];
	FL_OBJECT *dsed_lower_d[6];
	FL_OBJECT *dsed_lower_u[6];
} FD_dsnetedit;

extern FD_dsnetedit * create_form_dsnetedit(void);
typedef struct {
	FL_FORM *assessadd;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *add_confid;
	FL_OBJECT *add_time;
	FL_OBJECT *add_src_choice;
	FL_OBJECT *assadd_cancel;
	FL_OBJECT *add_lat;
	FL_OBJECT *add_long;
	FL_OBJECT *add_alt;
	FL_OBJECT *add_desc;
	FL_OBJECT *add_plause;
	FL_OBJECT *add_source;
	FL_OBJECT *add_duration;
	FL_OBJECT *belief_by_name;
	FL_OBJECT *disbelief_by_name;
} FD_assessadd;

extern FD_assessadd * create_form_assessadd(void);
typedef struct {
	FL_FORM *dshelpevid;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *ds_help_line1;
	FL_OBJECT *evid_desc;
	FL_OBJECT *ds_help_line2;
	FL_OBJECT *belief_desc;
	FL_OBJECT *out_desc;
	FL_OBJECT *explain_belief;
	FL_OBJECT *disbelief_desc;
	FL_OBJECT *explain_disbelief;
	FL_OBJECT *ds_help_line4;
	FL_OBJECT *ds_help_line3;
	FL_OBJECT *explain_unknown;
	FL_OBJECT *linkhelpname;
} FD_dshelpevid;

extern FD_dshelpevid * create_form_dshelpevid(void);
typedef struct {
	FL_FORM *dsexplain;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *ds_explain;
	FL_OBJECT *dsex_stop;
	FL_OBJECT *dsex_step;
	FL_OBJECT *dsex_play;
	FL_OBJECT *dsex_back;
	FL_OBJECT *dsex_rev;
	FL_OBJECT *dsex_rew;
	FL_OBJECT *dsex_end;
	FL_OBJECT *ds_image;
	FL_OBJECT *dsim_vscroll;
	FL_OBJECT *dsim_hscroll;
	FL_OBJECT *dsim_percent;
	FL_OBJECT *dsim_delay;
	FL_OBJECT *ds_frameno[3];
} FD_dsexplain;

extern FD_dsexplain * create_form_dsexplain(void);
typedef struct {
	FL_FORM *network;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *level4_title;
	FL_OBJECT *level3_title;
	FL_OBJECT *level2_title;
	FL_OBJECT *case_id;
	FL_OBJECT *assess_type;
	FL_OBJECT *sdate_text;
	FL_OBJECT *cdate_text;
	FL_OBJECT *prop_button;
	FL_OBJECT *net_show;
	FL_OBJECT *net_effective;
	FL_OBJECT *net_compare;
	FL_OBJECT *externet[6][6];
	FL_OBJECT *src_title[6];
	FL_OBJECT *src_chart[6];
	FL_OBJECT *hyp_chart[6];
	FL_OBJECT *hyp1_chart[6];
	FL_OBJECT *hyp3_chart[6];
	FL_OBJECT *con_title[6];
	FL_OBJECT *con_chart[6];
	FL_OBJECT *level_label[6];
	FL_OBJECT *hyp_title[6];
	FL_OBJECT *hyp1_title[6];
	FL_OBJECT *hyp2_chart[6];
	FL_OBJECT *hyp2_title[6];
	FL_OBJECT *hyp3_title[6];
	FL_OBJECT *directin[6][6];
	FL_OBJECT *override[6][6];
} FD_network;

extern FD_network * create_form_network(void);
typedef struct {
	FL_FORM *netedlink;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *netednodename;
	FL_OBJECT *neted_bw_from[7];
	FL_OBJECT *netedfrom[7];
	FL_OBJECT *netedto[7];
	FL_OBJECT *neted_bw_to[7];
	FL_OBJECT *neted_i_link[7];
	FL_OBJECT *neted_o_link[7];
	FL_OBJECT *neted_dw_from[7];
	FL_OBJECT *neted_dw_to[7];
} FD_netedlink;

extern FD_netedlink * create_form_netedlink(void);
typedef struct {
	FL_FORM *netednode;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *neted_learn_grp;
	FL_OBJECT *neted_link_grp;
	FL_OBJECT *netednodename;
	FL_OBJECT *neted_range;
	FL_OBJECT *neted_local;
	FL_OBJECT *neted_global;
	FL_OBJECT *netedbelief;
	FL_OBJECT *neteddisbelief;
	FL_OBJECT *neted_b_value;
	FL_OBJECT *neted_d_value;
	FL_OBJECT *neted_link_btn[4];
} FD_netednode;

extern FD_netednode * create_form_netednode(void);

#endif /* FD_input_h_ */
