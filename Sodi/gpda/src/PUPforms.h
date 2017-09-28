/** Header file generated with fdesign on Tue Dec 10 13:25:11 2002.**/

#ifndef FD_pup_h_
#define FD_pup_h_

/** Callbacks, globals and object handlers **/
extern void PUPnoneCB(FL_OBJECT *, long);
extern void pupexecCB(FL_OBJECT *, long);
extern void pupupdateCB(FL_OBJECT *, long);
extern void pupstratCB(FL_OBJECT *, long);
extern void puphypothCB(FL_OBJECT *, long);
extern void pupmenuCB(FL_OBJECT *, long);
extern void pupmissionCB(FL_OBJECT *, long);
extern void pupstoryCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *pup;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *pup_log;
	FL_OBJECT *pup_shell;
	FL_OBJECT *pup_exec;
	FL_OBJECT *pup_belief;
	FL_OBJECT *pup_disbelief;
	FL_OBJECT *pup_bthresh;
	FL_OBJECT *pup_dthresh;
	FL_OBJECT *pup_weight;
	FL_OBJECT *pup_update;
	FL_OBJECT *pup_time;
	FL_OBJECT *pup_estimated;
	FL_OBJECT *pup_actual;
	FL_OBJECT *pup_worth;
	FL_OBJECT *pup_strategy;
	FL_OBJECT *pup_action;
	FL_OBJECT *pup_hypothesis;
	FL_OBJECT *menu_file;
	FL_OBJECT *menu_mission;
	FL_OBJECT *pup_story;
} FD_pup;

extern FD_pup * create_form_pup(void);

#endif /* FD_pup_h_ */
