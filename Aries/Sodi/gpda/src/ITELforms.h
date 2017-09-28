/** Header file generated with fdesign on Tue Sep 17 11:13:48 2002.**/

#ifndef FD_bmcintel_h_
#define FD_bmcintel_h_

/** Callbacks, globals and object handlers **/
extern void ITELnoneCB(FL_OBJECT *, long);
extern void itelocationCB(FL_OBJECT *, long);
extern void ITELexitCB(FL_OBJECT *, long);
extern void itelsubjectCB(FL_OBJECT *, long);
extern void itelmissionCB(FL_OBJECT *, long);
extern void itelevidenceCB(FL_OBJECT *, long);
extern void itelbuttonCB(FL_OBJECT *, long);
extern void BMCnoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *bmcintel;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *itel_evidence;
	FL_OBJECT *intel_exit;
	FL_OBJECT *intel_source;
	FL_OBJECT *itel_mission;
	FL_OBJECT *itel_source;
	FL_OBJECT *itel_belief;
	FL_OBJECT *itel_disbelief;
	FL_OBJECT *itel_add;
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
	FL_OBJECT *itel_mode;
	FL_OBJECT *itel_evidmode;
	FL_OBJECT *itel_datamode;
	FL_OBJECT *intel_browser;
	FL_OBJECT *itel_newfile;
	FL_OBJECT *itel_openfile;
	FL_OBJECT *itep_apply;
	FL_OBJECT *itel_browse;
	FL_OBJECT *itel_nextrec;
	FL_OBJECT *itel_firstrec;
	FL_OBJECT *itel_rules;
	FL_OBJECT *itel_frame[7];
	FL_OBJECT *itel_field[52];
} FD_bmcintel;

extern FD_bmcintel * create_form_bmcintel(void);

#endif /* FD_bmcintel_h_ */
