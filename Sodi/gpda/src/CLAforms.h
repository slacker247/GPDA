/** Header file generated with fdesign on Thu Dec 19 15:10:59 2002.**/

#ifndef FD_bmcintel_h_
#define FD_bmcintel_h_

/** Callbacks, globals and object handlers **/
extern void ITELnoneCB(FL_OBJECT *, long);
extern void itelocationCB(FL_OBJECT *, long);
extern void itelsubjectCB(FL_OBJECT *, long);
extern void itelmissionCB(FL_OBJECT *, long);
extern void itelevidenceCB(FL_OBJECT *, long);
extern void CLAexitCB(FL_OBJECT *, long);
extern void clamissionCB(FL_OBJECT *, long);
extern void clabuttonCB(FL_OBJECT *, long);

extern void ITELnoneCB(FL_OBJECT *, long);
extern void claclassCB(FL_OBJECT *, long);
extern void CLAnoneCB(FL_OBJECT *, long);
extern void cladoneCB(FL_OBJECT *, long);

extern void CLAnoneCB(FL_OBJECT *, long);
extern void clunodeCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *bmcintel;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *itel_evidence;
	FL_OBJECT *intel_source;
	FL_OBJECT *itel_mission;
	FL_OBJECT *itel_source;
	FL_OBJECT *itel_belief;
	FL_OBJECT *itel_disbelief;
	FL_OBJECT *itel_sources;
	FL_OBJECT *itel_end_day;
	FL_OBJECT *itel_end_mon;
	FL_OBJECT *itel_end_year;
	FL_OBJECT *itel_tframe;
	FL_OBJECT *itel_end_hour;
	FL_OBJECT *itel_end_min;
	FL_OBJECT *itel_start_year;
	FL_OBJECT *itel_start_mon;
	FL_OBJECT *itel_start_day;
	FL_OBJECT *itel_start_hour;
	FL_OBJECT *itel_start_min;
	FL_OBJECT *itel_dur_year;
	FL_OBJECT *itel_dur_mon;
	FL_OBJECT *itel_dur_day;
	FL_OBJECT *itel_dur_hour;
	FL_OBJECT *itel_dur_min;
	FL_OBJECT *intel_browser;
	FL_OBJECT *itel_evidmode;
	FL_OBJECT *menu_file;
	FL_OBJECT *menu_mission;
	FL_OBJECT *itel_frame[7];
	FL_OBJECT *itel_field[52];
} FD_bmcintel;

extern FD_bmcintel * create_form_bmcintel(void);
typedef struct {
	FL_FORM *itelfields;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *clu_classes;
	FL_OBJECT *clu_classfn;
	FL_OBJECT *clu_closest;
	FL_OBJECT *clu_percent;
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

#endif /* FD_bmcintel_h_ */
