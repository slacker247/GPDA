#ifndef FD_remove_h_
#define FD_remove_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void remove_removeCB(FL_OBJECT *, long);
extern void remove_cancelCB(FL_OBJECT *, long);
extern void remove_inputCB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *remove;
	void *vdata;
	long ldata;
	FL_OBJECT *remove_remove;
	FL_OBJECT *remove_cancel;
	FL_OBJECT *remove_input;
	FL_OBJECT *remove_output;
} FD_remove;

extern FD_remove * create_form_remove(void);

#endif /* FD_remove_h_ */
