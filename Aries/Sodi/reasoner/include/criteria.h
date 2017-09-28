#ifndef FD_criteria_h_
#define FD_criteria_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void max_threatsCB(FL_OBJECT *, long);
extern void terrainCB(FL_OBJECT *, long);
extern void slopeCB(FL_OBJECT *, long);
extern void threat_timingCB(FL_OBJECT *, long);
extern void strengthCB(FL_OBJECT *, long);
extern void timeCB(FL_OBJECT *, long);
extern void commoditiesCB(FL_OBJECT *, long);
extern void equipageCB(FL_OBJECT *, long);
extern void fuelCB(FL_OBJECT *, long);
extern void subsidiariesCB(FL_OBJECT *, long);
extern void expected_down_commCB(FL_OBJECT *, long);
extern void unexpected_down_comm(FL_OBJECT *, long);
extern void natural_elemenstCB(FL_OBJECT *, long);
extern void mechanicalCB(FL_OBJECT *, long);
extern void startCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);
extern void distanceCB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *criteria;
	void *vdata;
	long ldata;
	FL_OBJECT *max_threats;
	FL_OBJECT *terrain;
	FL_OBJECT *slope;
	FL_OBJECT *timing;
	FL_OBJECT *strength;
	FL_OBJECT *time;
	FL_OBJECT *commodities;
	FL_OBJECT *equipage;
	FL_OBJECT *fuel;
	FL_OBJECT *subsidiaries;
	FL_OBJECT *expected_down_comm;
	FL_OBJECT *unexpected_down_comm;
	FL_OBJECT *natural_elements;
	FL_OBJECT *mechanical;
	FL_OBJECT *start;
	FL_OBJECT *exit;
	FL_OBJECT *distance;
} FD_criteria;

extern FD_criteria * create_form_criteria(void);

#endif /* FD_criteria_h_ */
