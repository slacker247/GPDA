/** Header file generated with fdesign on Thu Apr 24 14:45:45 2003.**/

#ifndef FD_itelfields_h_
#define FD_itelfields_h_

/** Callbacks, globals and object handlers **/
extern void CLUnoneCB(FL_OBJECT *, long);
extern void cluclassCB(FL_OBJECT *, long);
extern void CLUexitCB(FL_OBJECT *, long);
extern void clumissionCB(FL_OBJECT *, long);

extern void CLUnoneCB(FL_OBJECT *, long);
extern void clunodeCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *itelfields;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *cluapply;
	FL_OBJECT *clu_classes;
	FL_OBJECT *clu_classfn;
	FL_OBJECT *clu_closest;
	FL_OBJECT *menu_file;
	FL_OBJECT *menu_mission;
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

#endif /* FD_itelfields_h_ */
