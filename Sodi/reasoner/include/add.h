#ifndef FD_add_h_
#define FD_add_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void add_nowCB(FL_OBJECT *, long);
extern void add_cancelCB(FL_OBJECT *, long);
extern void add_locCB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *add;
	void *vdata;
	long ldata;
	FL_OBJECT *add_now;
	FL_OBJECT *add_cancel;
	FL_OBJECT *add_loc;
	FL_OBJECT *add_output;
} FD_add;

extern FD_add * create_form_add(void);

#endif /* FD_add_h_ */
