#ifndef FD_float_changer_h_
#define FD_float_changer_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void float_inputCB(FL_OBJECT *, long);
extern void done4CB(FL_OBJECT *, long);
extern void nevermind3CB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *float_changer;
	void *vdata;
	long ldata;
	FL_OBJECT *float_output;
	FL_OBJECT *float_input;
	FL_OBJECT *done4;
	FL_OBJECT *nevermind;
} FD_float_changer;

extern FD_float_changer * create_form_float_changer(void);

#endif /* FD_float_changer_h_ */
