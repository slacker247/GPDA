#ifndef FD_final_h_
#define FD_final_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void cancel_finalCB(FL_OBJECT *, long);
extern void acceptCB(FL_OBJECT *, long);
extern void start_overCB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *final;
	void *vdata;
	long ldata;
	FL_OBJECT *cancel_final;
	FL_OBJECT *accept;
	FL_OBJECT *start_over;
	FL_OBJECT *text_box1;
} FD_final;

extern FD_final * create_form_final(void);

#endif /* FD_final_h_ */
