/** Header file generated with fdesign on Tue Mar 11 13:47:06 2003.**/

#ifndef FD_bmc3_h_
#define FD_bmc3_h_

/** Callbacks, globals and object handlers **/
extern void signoffCB(FL_OBJECT *, long);
extern void BMCnoneCB(FL_OBJECT *, long);
extern void lockCB(FL_OBJECT *, long);
extern void dirstatusCB(FL_OBJECT *, long);
extern void alarmCB(FL_OBJECT *, long);
extern void wpnchartCB(FL_OBJECT *, long);
extern void monitormenuCB(FL_OBJECT *, long);
extern void determineCB(FL_OBJECT *, long);
extern void coadevmenuCB(FL_OBJECT *, long);
extern void assessmenuCB(FL_OBJECT *, long);
extern void utilmenuCB(FL_OBJECT *, long);
extern void missummaryCB(FL_OBJECT *, long);
extern void actwindowsCB(FL_OBJECT *, long);
extern void map2dCB(FL_OBJECT *, long);
extern void nothingCB(FL_OBJECT *, long);
extern void detailmenuCB(FL_OBJECT *, long);
extern void opsmodeCB(FL_OBJECT *, long);
extern void showstatsCB(FL_OBJECT *, long);
extern void nmdweapsumCB(FL_OBJECT *, long);
extern void sdfweapsumCB(FL_OBJECT *, long);
extern void NMDinjectCB(FL_OBJECT *, long);
extern void snapCB(FL_OBJECT *, long);
extern void cmdlogCB(FL_OBJECT *, long);
extern void toolmenuCB(FL_OBJECT *, long);
extern void simtimeCB(FL_OBJECT *, long);
extern void filemanCB(FL_OBJECT *, long);

extern void loginCB(FL_OBJECT *, long);
extern void signoffCB(FL_OBJECT *, long);
extern void nothingCB(FL_OBJECT *, long);

extern void loginCB(FL_OBJECT *, long);
extern void signoffCB(FL_OBJECT *, long);
extern void cmdcenterCB(FL_OBJECT *, long);
extern void positionCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);
extern void TRKexitCB(FL_OBJECT *, long);
extern void TRKnoneCB(FL_OBJECT *, long);
extern void nothingCB(FL_OBJECT *, long);

extern void infoexitCB(FL_OBJECT *, long);
extern void nothingCB(FL_OBJECT *, long);

extern void nothingCB(FL_OBJECT *, long);
extern void drawdownCB(FL_OBJECT *, long);

extern void nothingCB(FL_OBJECT *, long);

extern void ENGexitCB(FL_OBJECT *, long);
extern void BMCnoneCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);
extern void alrmactionCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);
extern void SITexitCB(FL_OBJECT *, long);
extern void SITnoneCB(FL_OBJECT *, long);
extern void sitoverlayCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);


extern void BMCnoneCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);

extern void statexitCB(FL_OBJECT *, long);
extern void BMCnoneCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);
extern void bmcenvironCB(FL_OBJECT *, long);

extern void BMCnoneCB(FL_OBJECT *, long);
extern void filestatCB(FL_OBJECT *, long);
extern void bmcenvironCB(FL_OBJECT *, long);

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
	FL_OBJECT *node_position;
	FL_OBJECT *sum_label;
	FL_OBJECT *plan_val;
	FL_OBJECT *defcon_label;
	FL_OBJECT *rp_label;
	FL_OBJECT *dea_label;
	FL_OBJECT *roe_label;
	FL_OBJECT *msncon_label;
	FL_OBJECT *msnobj_label;
	FL_OBJECT *plan_label;
	FL_OBJECT *alarm_button;
	FL_OBJECT *sdf_chart;
	FL_OBJECT *monitor_menu;
	FL_OBJECT *determine_menu;
	FL_OBJECT *coadev_menu;
	FL_OBJECT *assess_menu;
	FL_OBJECT *util_menu;
	FL_OBJECT *act_windows;
	FL_OBJECT *map2d;
	FL_OBJECT *alarm_text;
	FL_OBJECT *nmd_chart;
	FL_OBJECT *runmode;
	FL_OBJECT *projectname;
	FL_OBJECT *detailed_menu;
	FL_OBJECT *bmc_choice;
	FL_OBJECT *nmd_weap_menu;
	FL_OBJECT *sdf_weap_menu;
	FL_OBJECT *bmctop_browser;
	FL_OBJECT *node_center;
	FL_OBJECT *msnobj_val;
	FL_OBJECT *bmc_inject;
	FL_OBJECT *bmc_mission;
	FL_OBJECT *bmctop_map;
	FL_OBJECT *tool_menu;
	FL_OBJECT *bmc_message;
	FL_OBJECT *bmc_canvas;
	FL_OBJECT *simtime;
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
	FL_OBJECT *cc_grp;
	FL_OBJECT *pos_grp;
	FL_OBJECT *cmdcenter[5];
	FL_OBJECT *position[11];
} FD_cmocnode;

extern FD_cmocnode * create_form_cmocnode(void);
typedef struct {
	FL_FORM *bmctrack;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *trk_browser;
	FL_OBJECT *trk_header;
	FL_OBJECT *trk_menu;
} FD_bmctrack;

extern FD_bmctrack * create_form_bmctrack(void);
typedef struct {
	FL_FORM *infodialog;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *info_text;
	FL_OBJECT *info_title;
} FD_infodialog;

extern FD_infodialog * create_form_infodialog(void);
typedef struct {
	FL_FORM *drawdown;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *red_diminish_plot;
	FL_OBJECT *blue_diminish_plot;
	FL_OBJECT *drawdown_plot;
	FL_OBJECT *drawdown_alpha;
	FL_OBJECT *drawdown_parity;
	FL_OBJECT *drawdown_offset;
	FL_OBJECT *no_odi[5];
	FL_OBJECT *with_odi[5];
	FL_OBJECT *triad[5];
} FD_drawdown;

extern FD_drawdown * create_form_drawdown(void);
typedef struct {
	FL_FORM *sitmonitor;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *sit_what;
	FL_OBJECT *sit_value;
} FD_sitmonitor;

extern FD_sitmonitor * create_form_sitmonitor(void);
typedef struct {
	FL_FORM *bmcengage;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *engage_browser;
	FL_OBJECT *engage_header;
} FD_bmcengage;

extern FD_bmcengage * create_form_bmcengage(void);
typedef struct {
	FL_FORM *bmcalarms;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *folder;
	FL_OBJECT *accept_btn;
	FL_OBJECT *defer_btn;
	FL_OBJECT *cancel_btn;
} FD_bmcalarms;

extern FD_bmcalarms * create_form_bmcalarms(void);
typedef struct {
	FL_FORM *alrmaccept;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *alrm_accepted;
} FD_alrmaccept;

extern FD_alrmaccept * create_form_alrmaccept(void);
typedef struct {
	FL_FORM *alrmdefered;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *alrm_defered;
} FD_alrmdefered;

extern FD_alrmdefered * create_form_alrmdefered(void);
typedef struct {
	FL_FORM *alrmcancel;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *alrm_canceled;
} FD_alrmcancel;

extern FD_alrmcancel * create_form_alrmcancel(void);
typedef struct {
	FL_FORM *sitaware;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *sit_canvas;
	FL_OBJECT *sit_threats;
	FL_OBJECT *sit_threat_label;
	FL_OBJECT *sit_defcon;
	FL_OBJECT *sit_roe;
	FL_OBJECT *sit_dea;
	FL_OBJECT *sit_strategy;
	FL_OBJECT *sit_riskmap;
	FL_OBJECT *sit_namap;
	FL_OBJECT *sit_leakers;
	FL_OBJECT *sit_atrisk;
	FL_OBJECT *sit_overlay[8];
	FL_OBJECT *sit_durable[6];
} FD_sitaware;

extern FD_sitaware * create_form_sitaware(void);
typedef struct {
	FL_FORM *statcomm;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *ifstats_list;
} FD_statcomm;

extern FD_statcomm * create_form_statcomm(void);
typedef struct {
	FL_FORM *statsubs;
	void *vdata;
	char *cdata;
	long  ldata;
} FD_statsubs;

extern FD_statsubs * create_form_statsubs(void);
typedef struct {
	FL_FORM *statuser;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *user_info;
} FD_statuser;

extern FD_statuser * create_form_statuser(void);
typedef struct {
	FL_FORM *stathard;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *hard_info;
} FD_stathard;

extern FD_stathard * create_form_stathard(void);
typedef struct {
	FL_FORM *statsyst;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *syst_info;
} FD_statsyst;

extern FD_statsyst * create_form_statsyst(void);
typedef struct {
	FL_FORM *bmcstatus;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *folder;
} FD_bmcstatus;

extern FD_bmcstatus * create_form_bmcstatus(void);
typedef struct {
	FL_FORM *statenvs;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *bmc_env_setting;
	FL_OBJECT *bmc_env_vars;
	FL_OBJECT *bmc_env_input;
} FD_statenvs;

extern FD_statenvs * create_form_statenvs(void);
typedef struct {
	FL_FORM *statfile;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *file_info;
	FL_OBJECT *bmc_file_folder;
	FL_OBJECT *bmc_file_name;
} FD_statfile;

extern FD_statfile * create_form_statfile(void);
typedef struct {
	FL_FORM *bmcbrowse;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *bmc_browse;
} FD_bmcbrowse;

extern FD_bmcbrowse * create_form_bmcbrowse(void);

#endif /* FD_bmc3_h_ */
