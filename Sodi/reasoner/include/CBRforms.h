/** Header file generated with fdesign on Tue Sep 12 13:01:50 2000.**/

#ifndef FD_add_h_
#define FD_add_h_

/** Callbacks, globals and object handlers **/
extern void add_nowCB(FL_OBJECT *, long);
extern void add_cancelCB(FL_OBJECT *, long);
extern void add_locCB(FL_OBJECT *, long);

extern void doneCB(FL_OBJECT *, long);

extern void char_remove(FL_OBJECT *, long);
extern void char_add(FL_OBJECT *, long);
extern void char_never_mind(FL_OBJECT *, long);

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

extern void cancel_finalCB(FL_OBJECT *, long);
extern void acceptCB(FL_OBJECT *, long);
extern void start_overCB(FL_OBJECT *, long);

extern void float_inputCB(FL_OBJECT *, long);
extern void done4CB(FL_OBJECT *, long);
extern void nevermind3CB(FL_OBJECT *, long);

extern void graph1CB(FL_OBJECT *, long);
extern void graph2CB(FL_OBJECT *, long);
extern void graph3CB(FL_OBJECT *, long);
extern void graph_doneCB(FL_OBJECT *, long);

extern void buttonCB(FL_OBJECT *, long);

extern void remove_removeCB(FL_OBJECT *, long);
extern void remove_cancelCB(FL_OBJECT *, long);
extern void remove_inputCB(FL_OBJECT *, long);

extern void new_terrainCB(FL_OBJECT *, long);
extern void nevermind2(FL_OBJECT *, long);
extern void done3CB(FL_OBJECT *, long);

extern void coa1CB(FL_OBJECT *, long);
extern void coa2CB(FL_OBJECT *, long);
extern void coa3CB(FL_OBJECT *, long);
extern void computeCB(FL_OBJECT *, long);
extern void exit2CB(FL_OBJECT *, long);
extern void coa1infoCB(FL_OBJECT *, long);
extern void coa2infoCB(FL_OBJECT *, long);
extern void coa3infoCB(FL_OBJECT *, long);
extern void route1CB(FL_OBJECT *, long);
extern void route2CB(FL_OBJECT *, long);
extern void route3CB(FL_OBJECT *, long);

extern void inputCB(FL_OBJECT *, long);
extern void done2CB(FL_OBJECT *, long);
extern void nevermindCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *add;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *add_now;
	FL_OBJECT *add_cancel;
	FL_OBJECT *add_loc;
	FL_OBJECT *add_output;
} FD_add;

extern FD_add * create_form_add(void);
typedef struct {
	FL_FORM *changer;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *output;
	FL_OBJECT *done;
} FD_changer;

extern FD_changer * create_form_changer(void);
typedef struct {
	FL_FORM *char_changer;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *char_output;
	FL_OBJECT *char_remove;
	FL_OBJECT *char_add;
	FL_OBJECT *char_never_mind;
} FD_char_changer;

extern FD_char_changer * create_form_char_changer(void);
typedef struct {
	FL_FORM *criteria;
	void *vdata;
	char *cdata;
	long  ldata;
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
typedef struct {
	FL_FORM *final;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *cancel_final;
	FL_OBJECT *accept;
	FL_OBJECT *start_over;
	FL_OBJECT *text_box1;
} FD_final;

extern FD_final * create_form_final(void);
typedef struct {
	FL_FORM *float_changer;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *float_output;
	FL_OBJECT *float_input;
	FL_OBJECT *done4;
	FL_OBJECT *nevermind;
} FD_float_changer;

extern FD_float_changer * create_form_float_changer(void);
typedef struct {
	FL_FORM *graphs;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *graph1;
	FL_OBJECT *graph2;
	FL_OBJECT *graph3;
	FL_OBJECT *graph_done;
	FL_OBJECT *text1;
	FL_OBJECT *text2;
	FL_OBJECT *text3;
} FD_graphs;

extern FD_graphs * create_form_graphs(void);
typedef struct {
	FL_FORM *one;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *button;
} FD_one;

extern FD_one * create_form_one(void);
typedef struct {
	FL_FORM *remove;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *remove_remove;
	FL_OBJECT *remove_cancel;
	FL_OBJECT *remove_input;
	FL_OBJECT *remove_output;
} FD_remove;

extern FD_remove * create_form_remove(void);
typedef struct {
	FL_FORM *terrain_changer;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *terrain_output;
	FL_OBJECT *new_terrain;
	FL_OBJECT *nevermind2;
	FL_OBJECT *done3;
} FD_terrain_changer;

extern FD_terrain_changer * create_form_terrain_changer(void);
typedef struct {
	FL_FORM *test;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *coa1;
	FL_OBJECT *coa2;
	FL_OBJECT *coa3;
	FL_OBJECT *compute;
	FL_OBJECT *exit2;
	FL_OBJECT *coa1info;
	FL_OBJECT *coa2info;
	FL_OBJECT *coa3info;
	FL_OBJECT *route1;
	FL_OBJECT *route2;
	FL_OBJECT *route3;
} FD_test;

extern FD_test * create_form_test(void);
typedef struct {
	FL_FORM *time_changer;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *output1;
	FL_OBJECT *input;
	FL_OBJECT *done2;
	FL_OBJECT *nevermind;
} FD_time_changer;

extern FD_time_changer * create_form_time_changer(void);

#endif /* FD_add_h_ */
