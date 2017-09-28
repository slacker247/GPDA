/** Header file generated with fdesign on Tue Jun 13 09:13:43 2000.**/

#ifndef FD_ODI_h_
#define FD_ODI_h_

/** Callbacks, globals and object handlers **/
extern void nothingCB(FL_OBJECT *, long);
extern void battleCB(FL_OBJECT *, long);
extern void visualCB(FL_OBJECT *, long);
extern void configCB(FL_OBJECT *, long);
extern void scenarioCB(FL_OBJECT *, long);
extern void plannerCB(FL_OBJECT *, long);
extern void quitCB(FL_OBJECT *, long);
extern void utilitiesCB(FL_OBJECT *, long);

extern void saveCB(FL_OBJECT *, long);
extern void classCB(FL_OBJECT *, long);
extern void jtamvCB(FL_OBJECT *, long);
extern void sourceCB(FL_OBJECT *, long);
extern void infileCB(FL_OBJECT *, long);
extern void inputCB(FL_OBJECT *, long);
extern void doneCB(FL_OBJECT *, long);
extern void timeCB(FL_OBJECT *, long);
extern void saveasCB(FL_OBJECT *, long);
extern void simtimeCB(FL_OBJECT *, long);
extern void siminputCB(FL_OBJECT *, long);
extern void regionCB(FL_OBJECT *, long);
extern void regeditCB(FL_OBJECT *, long);

extern void regbrowserCB(FL_OBJECT *, long);
extern void regioneditCB(FL_OBJECT *, long);
extern void nothingCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *ODI;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *background;
	FL_OBJECT *battle_button;
	FL_OBJECT *visual_button;
	FL_OBJECT *config_button;
	FL_OBJECT *scenario_button;
	FL_OBJECT *planner_button;
	FL_OBJECT *quit_button;
	FL_OBJECT *utility_button;
} FD_ODI;

extern FD_ODI * create_form_ODI(void);
typedef struct {
	FL_FORM *config;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *save_jtamv;
	FL_OBJECT *save_input;
	FL_OBJECT *save_graph;
	FL_OBJECT *classification;
	FL_OBJECT *texfile;
	FL_OBJECT *hostid;
	FL_OBJECT *portid;
	FL_OBJECT *source;
	FL_OBJECT *vinfile;
	FL_OBJECT *delay;
	FL_OBJECT *autorun;
	FL_OBJECT *autoexit;
	FL_OBJECT *textured;
	FL_OBJECT *coverage;
	FL_OBJECT *gridlines;
	FL_OBJECT *boundary;
	FL_OBJECT *timestats;
	FL_OBJECT *simstats;
	FL_OBJECT *tracks;
	FL_OBJECT *trails;
	FL_OBJECT *tracklabel;
	FL_OBJECT *trackdrop;
	FL_OBJECT *r2links;
	FL_OBJECT *keeplinks;
	FL_OBJECT *starttime;
	FL_OBJECT *endtime;
	FL_OBJECT *saveas1;
	FL_OBJECT *saveas2;
	FL_OBJECT *saveas3;
	FL_OBJECT *p_endtime;
	FL_OBJECT *p_starttime;
	FL_OBJECT *pinfile;
	FL_OBJECT *siminfile;
	FL_OBJECT *p_cycletime;
	FL_OBJECT *p_synctime;
	FL_OBJECT *region_menu;
	FL_OBJECT *impacts;
} FD_config;

extern FD_config * create_form_config(void);
typedef struct {
	FL_FORM *regionedit;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *region_browser;
	FL_OBJECT *region_name;
	FL_OBJECT *region_lat;
	FL_OBJECT *region_lon;
	FL_OBJECT *region_alt;
	FL_OBJECT *region_fov;
	FL_OBJECT *region_azi;
	FL_OBJECT *regedgrp;
	FL_OBJECT *region_norm;
	FL_OBJECT *region_tang;
} FD_regionedit;

extern FD_regionedit * create_form_regionedit(void);

#endif /* FD_ODI_h_ */
