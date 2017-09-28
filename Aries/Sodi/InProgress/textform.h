/** Header file generated with fdesign on Wed Feb 11 08:44:30 2004.**/

#ifndef FD_textform_h_
#define FD_textform_h_

/** Callbacks, globals and object handlers **/


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *textform;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *text;
} FD_textform;

extern FD_textform * create_form_textform(void);

#endif /* FD_textform_h_ */
