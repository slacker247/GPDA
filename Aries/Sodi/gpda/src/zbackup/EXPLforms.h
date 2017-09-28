/** Header file generated with fdesign on Thu Jun 12 12:42:40 2003.**/

#ifndef FD_explain_h_
#define FD_explain_h_

/** Callbacks, globals and object handlers **/
extern void EXPLnoneCB(FL_OBJECT *, long);
extern void EXPLexitCB(FL_OBJECT *, long);
extern void dscontrolCB(FL_OBJECT *, long);
extern void dsimscrollCB(FL_OBJECT *, long);
extern void dsimdelayCB(FL_OBJECT *, long);

extern void EXPLnoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *explain;
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
	FL_OBJECT *ds_frametime[6];
} FD_explain;

extern FD_explain * create_form_explain(void);
typedef struct {
	FL_FORM *expl_complete;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *percent;
	FL_OBJECT *message;
} FD_expl_complete;

extern FD_expl_complete * create_form_expl_complete(void);

#endif /* FD_explain_h_ */
