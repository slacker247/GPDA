#ifndef FD_time_changer_h_
#define FD_time_changer_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void inputCB(FL_OBJECT *, long);
extern void done2CB(FL_OBJECT *, long);
extern void nevermindCB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *time_changer;
	void *vdata;
	long ldata;
	FL_OBJECT *output1;
	FL_OBJECT *input;
	FL_OBJECT *done2;
	FL_OBJECT *nevermind;
} FD_time_changer;

extern FD_time_changer * create_form_time_changer(void);

#endif /* FD_time_changer_h_ */
