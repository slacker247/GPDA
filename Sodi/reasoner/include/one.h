#ifndef FD_one_h_
#define FD_one_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void buttonCB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *one;
	void *vdata;
	long ldata;
	FL_OBJECT *button;
} FD_one;

extern FD_one * create_form_one(void);

#endif /* FD_one_h_ */
