/** Header file generated with fdesign on Mon Mar 17 10:33:44 2003.**/

#ifndef FD_tlegraph_h_
#define FD_tlegraph_h_

/** Callbacks, globals and object handlers **/
extern void TLEexitCB(FL_OBJECT *, long);
extern void TLEnoneCB(FL_OBJECT *, long);
extern void timescrollCB(FL_OBJECT *, long);
extern void eventlistCB(FL_OBJECT *, long);
extern void acceptCB(FL_OBJECT *, long);
extern void intervalCB(FL_OBJECT *, long);
extern void filterCB(FL_OBJECT *, long);
extern void timeunitCB(FL_OBJECT *, long);
extern void timescaleCB(FL_OBJECT *, long);
extern void timesourceCB(FL_OBJECT *, long);

extern void TLEnoneCB(FL_OBJECT *, long);
extern void evtexitCB(FL_OBJECT *, long);
extern void modlistCB(FL_OBJECT *, long);

extern void tle_planexitCB(FL_OBJECT *, long);
extern void TLEnoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *tlegraph;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *canvas;
	FL_OBJECT *time_scroll;
	FL_OBJECT *next_event;
	FL_OBJECT *event_timer;
	FL_OBJECT *real_time;
	FL_OBJECT *event_list;
	FL_OBJECT *tle_accept;
	FL_OBJECT *tle_auto;
	FL_OBJECT *interval;
	FL_OBJECT *filter_menu;
	FL_OBJECT *unit_menu;
	FL_OBJECT *scale_menu;
	FL_OBJECT *source_menu;
	FL_OBJECT *sim_time;
	FL_OBJECT *tle_explain;
} FD_tlegraph;

extern FD_tlegraph * create_form_tlegraph(void);
typedef struct {
	FL_FORM *tleevent;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *evt_desc;
	FL_OBJECT *evt_time;
	FL_OBJECT *evt_remain;
	FL_OBJECT *evt_runit;
	FL_OBJECT *tle_type;
	FL_OBJECT *evt_tunit;
	FL_OBJECT *tle_priority;
	FL_OBJECT *tle_value;
	FL_OBJECT *tle_repeat;
	FL_OBJECT *tle_period;
	FL_OBJECT *evt_file;
	FL_OBJECT *evt_id;
	FL_OBJECT *evt_subtype;
} FD_tleevent;

extern FD_tleevent * create_form_tleevent(void);
typedef struct {
	FL_FORM *tlereplan;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *replan_graph;
	FL_OBJECT *explain_text;
	FL_OBJECT *parms_text;
} FD_tlereplan;

extern FD_tlereplan * create_form_tlereplan(void);

#endif /* FD_tlegraph_h_ */
