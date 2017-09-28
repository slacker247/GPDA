/** Header file generated with fdesign on Fri Oct  4 15:01:48 2002.**/

#ifndef FD_simgraph_h_
#define FD_simgraph_h_

/** Callbacks, globals and object handlers **/
extern void SIMnoneCB(FL_OBJECT *, long);
extern void SIMexitCB(FL_OBJECT *, long);
extern void SIMsliderCB(FL_OBJECT *, long);
extern void SIMzoomCB(FL_OBJECT *, long);
extern void SIMelevCB(FL_OBJECT *, long);
extern void SIMrefreshCB(FL_OBJECT *, long);
extern void SIMcontrolCB(FL_OBJECT *, long);
extern void SIMstartCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *simgraph;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *canvas;
	FL_OBJECT *viewpos;
	FL_OBJECT *zoomer;
	FL_OBJECT *elevation;
	FL_OBJECT *h3d_scale;
	FL_OBJECT *sim_step;
	FL_OBJECT *simgrp_mode;
	FL_OBJECT *sim_realtime;
	FL_OBJECT *sim_playback;
	FL_OBJECT *sim_results[10];
} FD_simgraph;

extern FD_simgraph * create_form_simgraph(void);

#endif /* FD_simgraph_h_ */
