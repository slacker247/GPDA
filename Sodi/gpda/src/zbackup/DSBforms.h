/** Header file generated with fdesign on Tue Apr  8 09:18:22 2003.**/

#ifndef FD_assess_h_
#define FD_assess_h_

/** Callbacks, globals and object handlers **/
extern void DSBnoneCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);
extern void assess_issueCB(FL_OBJECT *, long);
extern void executeCB(FL_OBJECT *, long);
extern void assess_explainCB(FL_OBJECT *, long);
extern void assess_selectCB(FL_OBJECT *, long);
extern void input_addCB(FL_OBJECT *, long);
extern void input_modCB(FL_OBJECT *, long);
extern void input_delCB(FL_OBJECT *, long);
extern void resetCB(FL_OBJECT *, long);
extern void input_displayCB(FL_OBJECT *, long);
extern void input_loadCB(FL_OBJECT *, long);
extern void input_saveCB(FL_OBJECT *, long);
extern void input_menuCB(FL_OBJECT *, long);
extern void plotolddataCB(FL_OBJECT *, long);
extern void graphlevelCB(FL_OBJECT *, long);
extern void evidenceCB(FL_OBJECT *, long);
extern void assess_ctlCB(FL_OBJECT *, long);
extern void optionCB(FL_OBJECT *, long);
extern void dsbmissionCB(FL_OBJECT *, long);

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
extern void netdrawCB(FL_OBJECT *, long);
extern void net_menuCB(FL_OBJECT *, long);
extern void assess_ctlCB(FL_OBJECT *, long);
extern void dsbmissionCB(FL_OBJECT *, long);
extern void input_menuCB(FL_OBJECT *, long);

extern void netedboxCB(FL_OBJECT *, long);
extern void DSBnoneCB(FL_OBJECT *, long);
extern void netedhelpCB(FL_OBJECT *, long);
extern void neteddoneCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void netedboxCB(FL_OBJECT *, long);
extern void neteddoneCB(FL_OBJECT *, long);

extern void neteditorCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void budtableCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);

extern void exitCB(FL_OBJECT *, long);
extern void DSBnoneCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);

extern void exitCB(FL_OBJECT *, long);
extern void assess_storyCB(FL_OBJECT *, long);
extern void DSBnoneCB(FL_OBJECT *, long);
extern void storyeditCB(FL_OBJECT *, long);
extern void storyfileCB(FL_OBJECT *, long);

extern void assess_tunitCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void assess_ctlCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void optexitCB(FL_OBJECT *, long);

extern void DSBnoneCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);
extern void assess_issueCB(FL_OBJECT *, long);
extern void assess_explainCB(FL_OBJECT *, long);

extern void storyBrowserCB(FL_OBJECT *, long);

extern void exitCB(FL_OBJECT *, long);
extern void DSBnoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
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
	FL_OBJECT *issue_but;
	FL_OBJECT *best_title;
	FL_OBJECT *best_value;
	FL_OBJECT *ass_belief_text;
	FL_OBJECT *prop_but;
	FL_OBJECT *belief_pie;
	FL_OBJECT *disbelief_pie;
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
	FL_OBJECT *assess_best;
	FL_OBJECT *assess_newnode;
	FL_OBJECT *assess_newdata;
	FL_OBJECT *assess_newmsn;
	FL_OBJECT *input_browser;
	FL_OBJECT *assess_import;
	FL_OBJECT *assess_summ;
	FL_OBJECT *menu_mission;
	FL_OBJECT *dsb_story;
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
	FL_OBJECT *dsed_belief;
	FL_OBJECT *dsed_disbelief;
	FL_OBJECT *dsed_unknown;
	FL_OBJECT *src_box;
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
	FL_OBJECT *add_lvl_choice;
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
	FL_OBJECT *net_frame;
	FL_OBJECT *assess_type;
	FL_OBJECT *sdate_text;
	FL_OBJECT *cdate_text;
	FL_OBJECT *net_canvas;
	FL_OBJECT *nettab_menu;
	FL_OBJECT *netops_menu;
	FL_OBJECT *netanal_menu;
	FL_OBJECT *net_compare;
	FL_OBJECT *net_show;
	FL_OBJECT *menu_file;
	FL_OBJECT *menu_mission;
	FL_OBJECT *dsb_story;
	FL_OBJECT *nethelp_menu;
	FL_OBJECT *externet[6][6];
	FL_OBJECT *node_chart[6][6];
	FL_OBJECT *level_label[6];
	FL_OBJECT *node_label[6][6];
	FL_OBJECT *directin[6][6];
	FL_OBJECT *override[6][6];
	FL_OBJECT *net_toomany[6];
	FL_OBJECT *bestpath[6][6];
	FL_OBJECT *worstpath[6][6];
	FL_OBJECT *leastpath[6][6];
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
typedef struct {
	FL_FORM *neteditor;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *netedit_level;
	FL_OBJECT *netedit_node;
	FL_OBJECT *netedit_belief;
	FL_OBJECT *netedit_disbelief;
	FL_OBJECT *netedit_name;
	FL_OBJECT *netedit_b;
	FL_OBJECT *netedit_d;
	FL_OBJECT *netedit_bweight;
	FL_OBJECT *netedit_onodes;
	FL_OBJECT *netedit_inodes;
	FL_OBJECT *netedit_dweight;
} FD_neteditor;

extern FD_neteditor * create_form_neteditor(void);
typedef struct {
	FL_FORM *budtable;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *bud_label;
	FL_OBJECT *bud_cur_belief;
	FL_OBJECT *bud_old_belief;
	FL_OBJECT *bud_del_belief;
	FL_OBJECT *bud_cur_disbelief;
	FL_OBJECT *bud_old_disbelief;
	FL_OBJECT *bud_del_disbelief;
} FD_budtable;

extern FD_budtable * create_form_budtable(void);
typedef struct {
	FL_FORM *dsbstats;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *dsb_stat_niters;
	FL_OBJECT *dsb_stat_bchange;
	FL_OBJECT *dsb_stat_dchange;
	FL_OBJECT *dsb_stat_constraint;
	FL_OBJECT *dsb_stat_learn;
	FL_OBJECT *dsb_stat_berror;
	FL_OBJECT *dsb_stat_derror;
	FL_OBJECT *dsb_stat_l;
	FL_OBJECT *dsb_stat_meanl;
} FD_dsbstats;

extern FD_dsbstats * create_form_dsbstats(void);
typedef struct {
	FL_FORM *dsbstory;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *story_btotal;
	FL_OBJECT *story_dtotal;
	FL_OBJECT *story_utotal;
	FL_OBJECT *story_label[6];
	FL_OBJECT *story_belief[6];
	FL_OBJECT *story_disbelief[6];
	FL_OBJECT *story_unknown[6];
} FD_dsbstory;

extern FD_dsbstory * create_form_dsbstory(void);
typedef struct {
	FL_FORM *netsnap;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *net_limage;
	FL_OBJECT *net_rimage;
	FL_OBJECT *net_ltext;
	FL_OBJECT *net_rtext;
} FD_netsnap;

extern FD_netsnap * create_form_netsnap(void);
typedef struct {
	FL_FORM *completion;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *percent;
	FL_OBJECT *message;
} FD_completion;

extern FD_completion * create_form_completion(void);
typedef struct {
	FL_FORM *dsstory;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *story_details;
	FL_OBJECT *story_mission;
	FL_OBJECT *story_count;
	FL_OBJECT *story_header;
	FL_OBJECT *story_pause;
	FL_OBJECT *story_edit;
	FL_OBJECT *story_done;
	FL_OBJECT *story_file;
	FL_OBJECT *story_ident;
	FL_OBJECT *story_browse;
	FL_OBJECT *story_sort;
	FL_OBJECT *story_sortby;
	FL_OBJECT *story_sortorder;
	FL_OBJECT *story_evidence;
	FL_OBJECT *story_evidtitle;
} FD_dsstory;

extern FD_dsstory * create_form_dsstory(void);
typedef struct {
	FL_FORM *dsb_opt_tim;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *time_group;
	FL_OBJECT *assesstunit[4];
} FD_dsb_opt_tim;

extern FD_dsb_opt_tim * create_form_dsb_opt_tim(void);
typedef struct {
	FL_FORM *dsb_opt_ctl;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *prune;
	FL_OBJECT *secure;
	FL_OBJECT *diffBP;
	FL_OBJECT *auto_delta;
	FL_OBJECT *overides;
	FL_OBJECT *record;
	FL_OBJECT *fow;
} FD_dsb_opt_ctl;

extern FD_dsb_opt_ctl * create_form_dsb_opt_ctl(void);
typedef struct {
	FL_FORM *options;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *folder;
} FD_options;

extern FD_options * create_form_options(void);
typedef struct {
	FL_FORM *budgraph;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *bud_plot;
	FL_OBJECT *summary;
} FD_budgraph;

extern FD_budgraph * create_form_budgraph(void);
typedef struct {
	FL_FORM *storyBrowser;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *list;
	FL_OBJECT *input;
	FL_OBJECT *okBtn;
	FL_OBJECT *cancelBtn;
} FD_storyBrowser;

extern FD_storyBrowser * create_form_storyBrowser(void);
typedef struct {
	FL_FORM *dsbhelp;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *dsb_help;
} FD_dsbhelp;

extern FD_dsbhelp * create_form_dsbhelp(void);

#endif /* FD_assess_h_ */
