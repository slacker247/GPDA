/** Header file generated with fdesign on Thu May 15 10:05:14 2003.**/

#ifndef FD_ODI_h_
#define FD_ODI_h_

/** Callbacks, globals and object handlers **/
extern void ODInoneCB(FL_OBJECT *, long);
extern void ODIexitCB(FL_OBJECT *, long);
extern void RHSideCB(FL_OBJECT *, long);
extern void battleCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *ODI;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *background;
	FL_OBJECT *quit_button;
	FL_OBJECT *RHSRadio;
	FL_OBJECT *rhs_button[15];
} FD_ODI;

extern FD_ODI * create_form_ODI(void);

#endif /* FD_ODI_h_ */
