/** Header file generated with fdesign on Thu Apr 11 16:03:08 2002.**/

#ifndef FD_h3dgraph_h_
#define FD_h3dgraph_h_

/** Callbacks, globals and object handlers **/
extern void H3DexitCB(FL_OBJECT *, long);
extern void H3DnoneCB(FL_OBJECT *, long);
extern void H3DsliderCB(FL_OBJECT *, long);
extern void H3DzoomCB(FL_OBJECT *, long);
extern void H3DelevCB(FL_OBJECT *, long);
extern void H3DrefreshCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *h3dgraph;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *canvas;
	FL_OBJECT *viewpos;
	FL_OBJECT *zoomer;
	FL_OBJECT *elevation;
	FL_OBJECT *h3d_scale;
} FD_h3dgraph;

extern FD_h3dgraph * create_form_h3dgraph(void);

#endif /* FD_h3dgraph_h_ */
