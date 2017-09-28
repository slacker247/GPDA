/** Header file generated with fdesign on Wed Apr  2 10:40:45 2003.**/

#ifndef FD_FogInput_h_
#define FD_FogInput_h_

/** Callbacks, globals and object handlers **/
extern void FOGnoneCB(FL_OBJECT *, long);
extern void fog_evidCB(FL_OBJECT *, long);
extern void FOGexitCB(FL_OBJECT *, long);
extern void fog_weightCB(FL_OBJECT *, long);
extern void fog_outcomeCB(FL_OBJECT *, long);
extern void fog_loadCB(FL_OBJECT *, long);
extern void cause_buttonCB(FL_OBJECT *, long);
extern void fog_saveCB(FL_OBJECT *, long);
extern void fog_buttonCB(FL_OBJECT *, long);

extern void fog_acceptCB(FL_OBJECT *, long);
extern void fog_cancelCB(FL_OBJECT *, long);
extern void fog_settingCB(FL_OBJECT *, long);
extern void fog_sliderCB(FL_OBJECT *, long);

extern void help_browserCB(FL_OBJECT *, long);
extern void help_doneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *FogInput;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *fog_name;
	FL_OBJECT *evid_button[28];
	FL_OBJECT *weight_button[37];
	FL_OBJECT *outcome_button[37];
	FL_OBJECT *cause_button[17];
} FD_FogInput;

extern FD_FogInput * create_form_FogInput(void);
typedef struct {
	FL_FORM *SetDialog;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *set_slider;
	FL_OBJECT *set_value;
} FD_SetDialog;

extern FD_SetDialog * create_form_SetDialog(void);
typedef struct {
	FL_FORM *HelpDialog;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *help_oper;
	FL_OBJECT *help_factor;
	FL_OBJECT *help_browser;
} FD_HelpDialog;

extern FD_HelpDialog * create_form_HelpDialog(void);

#endif /* FD_FogInput_h_ */
