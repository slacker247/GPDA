#ifndef FD_test_h_
#define FD_test_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void coa1CB(FL_OBJECT *, long);
extern void coa2CB(FL_OBJECT *, long);
extern void coa3CB(FL_OBJECT *, long);
extern void computeCB(FL_OBJECT *, long);
extern void exit2CB(FL_OBJECT *, long);
extern void coa1infoCB(FL_OBJECT *, long);
extern void coa2infoCB(FL_OBJECT *, long);
extern void coa3infoCB(FL_OBJECT *, long);
extern void route1CB(FL_OBJECT *, long);
extern void route2CB(FL_OBJECT *, long);
extern void route3CB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *test;
	void *vdata;
	long ldata;
	FL_OBJECT *coa1;
	FL_OBJECT *coa2;
	FL_OBJECT *coa3;
	FL_OBJECT *compute;
	FL_OBJECT *exit2;
	FL_OBJECT *coa1info;
	FL_OBJECT *coa2info;
	FL_OBJECT *coa3info;
	FL_OBJECT *route1;
	FL_OBJECT *route2;
	FL_OBJECT *route3;
} FD_test;

extern FD_test * create_form_test(void);

#endif /* FD_test_h_ */
