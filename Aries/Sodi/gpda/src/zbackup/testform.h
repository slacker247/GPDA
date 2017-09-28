/** Header file generated with fdesign on Thu Nov  1 10:58:05 2001.**/

#ifndef FD_test_h_
#define FD_test_h_

/** Callbacks, globals and object handlers **/


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *test;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *button[6][6];
} FD_test;

extern FD_test * create_form_test(void);

#endif /* FD_test_h_ */
