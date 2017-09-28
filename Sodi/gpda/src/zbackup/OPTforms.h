/** Header file generated with fdesign on Fri Feb  2 14:32:59 2001.**/

#ifndef FD_gagraph_h_
#define FD_gagraph_h_

/** Callbacks, globals and object handlers **/
extern void gaselectCB(FL_OBJECT *, long);
extern void gaexitCB(FL_OBJECT *, long);
extern void OPTnoneCB(FL_OBJECT *, long);
extern void StopCB(FL_OBJECT *, long);
extern void ResetCB(FL_OBJECT *, long);
extern void EvolveCB(FL_OBJECT *, long);
extern void StepCB(FL_OBJECT *, long);
extern void EvolveSomeCB(FL_OBJECT *, long);
extern void OPTsetupCB(FL_OBJECT *, long);

extern void gaselectCB(FL_OBJECT *, long);
extern void OPTnoneCB(FL_OBJECT *, long);
extern void gachoiceCB(FL_OBJECT *, long);
extern void OPTinputCB(FL_OBJECT *, long);
extern void gaexitCB(FL_OBJECT *, long);
extern void gaparmsCB(FL_OBJECT *, long);

extern void OPTnoneCB(FL_OBJECT *, long);
extern void OPTexitCB(FL_OBJECT *, long);
extern void OPTinputCB(FL_OBJECT *, long);
extern void gainbuttonCB(FL_OBJECT *, long);
extern void OPTsetupCB(FL_OBJECT *, long);
extern void gainmodeCB(FL_OBJECT *, long);
extern void gasaveCB(FL_OBJECT *, long);

extern void OPTnoneCB(FL_OBJECT *, long);
extern void gaexitCB(FL_OBJECT *, long);
extern void optcommitCB(FL_OBJECT *, long);
extern void optactionCB(FL_OBJECT *, long);
extern void optconstrainCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *gagraph;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *gadata_browser;
	FL_OBJECT *ga_canvas;
	FL_OBJECT *ga_gen;
	FL_OBJECT *bitmap_stop;
	FL_OBJECT *bitmap_rew;
	FL_OBJECT *bitmap_run;
	FL_OBJECT *bitmap_step;
	FL_OBJECT *bitmap_some;
	FL_OBJECT *ga_start;
	FL_OBJECT *geneview;
	FL_OBJECT *ga_genome;
	FL_OBJECT *ga_algor;
	FL_OBJECT *gaparmview;
} FD_gagraph;

extern FD_gagraph * create_form_gagraph(void);
typedef struct {
	FL_FORM *gaparms;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *gamenu_genome;
	FL_OBJECT *gamenu_algorithm;
	FL_OBJECT *ga_geninc;
	FL_OBJECT *gamenu_mutation;
	FL_OBJECT *gamenu_xover;
	FL_OBJECT *gamenu_init;
	FL_OBJECT *gamenu_replace;
	FL_OBJECT *gamenu_select;
	FL_OBJECT *gamenu_terminate;
	FL_OBJECT *diversegrp;
	FL_OBJECT *elitismgrp;
	FL_OBJECT *gamenu_scaling;
	FL_OBJECT *gaparm[14];
} FD_gaparms;

extern FD_gaparms * create_form_gaparms(void);
typedef struct {
	FL_FORM *gainput;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *gain_coaname;
	FL_OBJECT *gain_nRO;
	FL_OBJECT *ga_goal;
	FL_OBJECT *ga_tol;
	FL_OBJECT *gain_adv;
	FL_OBJECT *gain_go;
	FL_OBJECT *gain_solutions;
	FL_OBJECT *gain_load;
	FL_OBJECT *optmodegrp;
	FL_OBJECT *gain_save;
} FD_gainput;

extern FD_gainput * create_form_gainput(void);
typedef struct {
	FL_FORM *gacommit;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *opt_reslist;
	FL_OBJECT *opt_tgtlist;
	FL_OBJECT *opt_nactions;
	FL_OBJECT *opt_restext;
	FL_OBJECT *opt_tgttext;
	FL_OBJECT *opt_rescommit;
	FL_OBJECT *opt_tgtcommit;
	FL_OBJECT *opt_goodness;
	FL_OBJECT *opt_constrain[10];
	FL_OBJECT *opt_cobstrain[4];
	FL_OBJECT *opt_contrain[7];
} FD_gacommit;

extern FD_gacommit * create_form_gacommit(void);

#endif /* FD_gagraph_h_ */
