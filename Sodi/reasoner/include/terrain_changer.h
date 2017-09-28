#ifndef FD_terrain_changer_h_
#define FD_terrain_changer_h_
/* Header file generated with fdesign. */

/**** Callback routines ****/

extern void new_terrainCB(FL_OBJECT *, long);
extern void nevermind2(FL_OBJECT *, long);
extern void done3CB(FL_OBJECT *, long);


/**** Forms and Objects ****/

typedef struct {
	FL_FORM *terrain_changer;
	void *vdata;
	long ldata;
	FL_OBJECT *terrain_output;
	FL_OBJECT *new_terrain;
	FL_OBJECT *nevermind2;
	FL_OBJECT *done3;
} FD_terrain_changer;

extern FD_terrain_changer * create_form_terrain_changer(void);

#endif /* FD_terrain_changer_h_ */
