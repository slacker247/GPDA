/** Header file generated with fdesign on Mon Sep 24 11:24:23 2001.**/

#ifndef FD_config_h_
#define FD_config_h_

/** Callbacks, globals and object handlers **/
extern void ODInoneCB(FL_OBJECT *, long);
extern void saveCB(FL_OBJECT *, long);
extern void classCB(FL_OBJECT *, long);
extern void jtamvCB(FL_OBJECT *, long);
extern void sourceCB(FL_OBJECT *, long);
extern void infileCB(FL_OBJECT *, long);
extern void inputCB(FL_OBJECT *, long);
extern void doneCB(FL_OBJECT *, long);
extern void saveasCB(FL_OBJECT *, long);
extern void simtimeCB(FL_OBJECT *, long);
extern void siminputCB(FL_OBJECT *, long);
extern void regeditCB(FL_OBJECT *, long);
extern void regionCB(FL_OBJECT *, long);
extern void odifilesCB(FL_OBJECT *, long);

extern void regbrowserCB(FL_OBJECT *, long);
extern void regioneditCB(FL_OBJECT *, long);
extern void nothingCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *config;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *save_jtamv;
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
	FL_OBJECT *saveas1;
	FL_OBJECT *starttime;
	FL_OBJECT *pinfile;
	FL_OBJECT *siminfile;
	FL_OBJECT *cycletime;
	FL_OBJECT *synctime;
	FL_OBJECT *impacts;
	FL_OBJECT *region_choice;
	FL_OBJECT *endtime;
	FL_OBJECT *folder_icon;
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

#endif /* FD_config_h_ */
