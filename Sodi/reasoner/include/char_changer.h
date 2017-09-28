#ifndef FD_char_changer_h_
#define FD_char_changer_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void char_remove(FL_OBJECT *, long);
extern void char_add(FL_OBJECT *, long);
extern void char_never_mind(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *char_changer;
	void *vdata;
	long ldata;
	FL_OBJECT *char_output;
	FL_OBJECT *char_remove;
	FL_OBJECT *char_add;
	FL_OBJECT *char_never_mind;
} FD_char_changer;

extern FD_char_changer * create_form_char_changer(void);

#endif /* FD_char_changer_h_ */
