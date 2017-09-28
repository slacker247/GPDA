/** Header file generated with fdesign on Wed Oct  2 11:52:19 2002.**/

#ifndef FD_About_Form_h_
#define FD_About_Form_h_

/** Callbacks, globals and object handlers **/
extern void aboutexitCB(FL_OBJECT *, long);
extern void BMCnoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *About_Form;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *About_Go_Back;
	FL_OBJECT *Gena;
	FL_OBJECT *about_version;
	FL_OBJECT *about_os;
	FL_OBJECT *about_date;
	FL_OBJECT *about_poc;
} FD_About_Form;

extern FD_About_Form * create_form_About_Form(void);

#endif /* FD_About_Form_h_ */
