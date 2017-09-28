#ifndef FD_graphs_h_
#define FD_graphs_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void graph1CB(FL_OBJECT *, long);
extern void graph2CB(FL_OBJECT *, long);
extern void graph3CB(FL_OBJECT *, long);
extern void graph_doneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *graphs;
	void *vdata;
	long ldata;
	FL_OBJECT *graph1;
	FL_OBJECT *graph2;
	FL_OBJECT *graph3;
	FL_OBJECT *graph_done;
	FL_OBJECT *text1;
	FL_OBJECT *text2;
	FL_OBJECT *text3;
} FD_graphs;

extern FD_graphs * create_form_graphs(void);

#endif /* FD_graphs_h_ */
