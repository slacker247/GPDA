#ifndef FD_changer_h_
#define FD_changer_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void doneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *changer;
	void *vdata;
	long ldata;
	FL_OBJECT *output;
	FL_OBJECT *done;
} FD_changer;

extern FD_changer * create_form_changer(void);

#endif /* FD_changer_h_ */
