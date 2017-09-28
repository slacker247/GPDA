/** Header file generated with fdesign on Thu Feb 14 12:32:39 2002.**/

#ifndef FD_ODI_h_
#define FD_ODI_h_

/** Callbacks, globals and object handlers **/
extern void datamineCB(FL_OBJECT *, long);
extern void ODInoneCB(FL_OBJECT *, long);
extern void battleCB(FL_OBJECT *, long);
extern void ODIexitCB(FL_OBJECT *, long);
extern void patriotCB(FL_OBJECT *, long);
extern void reasonerCB(FL_OBJECT *, long);
extern void engageCB(FL_OBJECT *, long);
extern void predatorCB(FL_OBJECT *, long);
extern void configedCB(FL_OBJECT *, long);
extern void plannerCB(FL_OBJECT *, long);
extern void visualCB(FL_OBJECT *, long);
extern void fusionCB(FL_OBJECT *, long);
extern void splashhelpCB(FL_OBJECT *, long);

extern void loginCB(FL_OBJECT *, long);
extern void signoffCB(FL_OBJECT *, long);
extern void nothingCB(FL_OBJECT *, long);

extern void loginCB(FL_OBJECT *, long);
extern void signoffCB(FL_OBJECT *, long);
extern void cmdcenterCB(FL_OBJECT *, long);
extern void positionCB(FL_OBJECT *, long);

extern void helpdoneCB(FL_OBJECT *, long);
extern void ODInoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *ODI;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *background;
	FL_OBJECT *battle_button;
	FL_OBJECT *quit_button;
	FL_OBJECT *visual_button;
} FD_ODI;

extern FD_ODI * create_form_ODI(void);
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
	FL_FORM *help;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *helptext;
} FD_help;

extern FD_help * create_form_help(void);

#endif /* FD_ODI_h_ */
