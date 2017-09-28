/** Header file generated with fdesign on Tue Sep 10 14:44:38 2002.**/

#ifndef FD_cluster_h_
#define FD_cluster_h_

/** Callbacks, globals and object handlers **/
extern void CLUnoneCB(FL_OBJECT *, long);
extern void CLUexitCB(FL_OBJECT *, long);
extern void cluapplyCB(FL_OBJECT *, long);
extern void cluviewCB(FL_OBJECT *, long);
extern void cluresetCB(FL_OBJECT *, long);
extern void cluclassCB(FL_OBJECT *, long);
extern void cluparamCB(FL_OBJECT *, long);
extern void cluvalueCB(FL_OBJECT *, long);
extern void clunodeCB(FL_OBJECT *, long);

extern void ITELnoneCB(FL_OBJECT *, long);
extern void cluclassCB(FL_OBJECT *, long);
extern void CLUnoneCB(FL_OBJECT *, long);

extern void CLUnoneCB(FL_OBJECT *, long);
extern void clunodeCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *cluster;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *clu_waitmsg;
	FL_OBJECT *clu_donemsg;
	FL_OBJECT *message;
	FL_OBJECT *out_choice;
	FL_OBJECT *clu_lowsize;
	FL_OBJECT *clu_highsize;
	FL_OBJECT *clu_plotfn;
	FL_OBJECT *clu_subsfn;
	FL_OBJECT *param[30];
	FL_OBJECT *cluvalue[29];
} FD_cluster;

extern FD_cluster * create_form_cluster(void);
typedef struct {
	FL_FORM *itelfields;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *clu_classes;
	FL_OBJECT *clu_classfn;
	FL_OBJECT *clugrp_mode;
	FL_OBJECT *clu_subdue;
	FL_OBJECT *clu_classify;
	FL_OBJECT *clu_closest;
	FL_OBJECT *itel_fldselect[52];
} FD_itelfields;

extern FD_itelfields * create_form_itelfields(void);
typedef struct {
	FL_FORM *defnode;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *clu_mission;
	FL_OBJECT *clu_nodename;
} FD_defnode;

extern FD_defnode * create_form_defnode(void);

#endif /* FD_cluster_h_ */
