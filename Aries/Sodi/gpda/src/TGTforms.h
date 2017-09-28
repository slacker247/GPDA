/** Header file generated with fdesign on Fri Jun 13 11:48:25 2003.**/

#ifndef FD_targetstatus_h_
#define FD_targetstatus_h_

/** Callbacks, globals and object handlers **/
extern void TGTnoneCB(FL_OBJECT *, long);
extern void localebrowserCB(FL_OBJECT *, long);
extern void damageCB(FL_OBJECT *, long);
extern void arrivalCB(FL_OBJECT *, long);
extern void targetselectCB(FL_OBJECT *, long);
extern void tgthardCB(FL_OBJECT *, long);
extern void tgtactiveCB(FL_OBJECT *, long);
extern void tgttypeCB(FL_OBJECT *, long);
extern void targetbrowserCB(FL_OBJECT *, long);
extern void TGTexitCB(FL_OBJECT *, long);
extern void targetimageCB(FL_OBJECT *, long);

extern void TGTnoneCB(FL_OBJECT *, long);
extern void targetimageCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *targetstatus;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *tgt_hardthumb;
	FL_OBJECT *site_browser;
	FL_OBJECT *country_menu;
	FL_OBJECT *damage_p;
	FL_OBJECT *arrival_p;
	FL_OBJECT *tgt_locale;
	FL_OBJECT *tgt_country;
	FL_OBJECT *tgt_hardness;
	FL_OBJECT *tgt_sethard;
	FL_OBJECT *tgt_active;
	FL_OBJECT *tgt_delete;
	FL_OBJECT *tgt_add;
	FL_OBJECT *tgt_save;
	FL_OBJECT *tgt_mobility;
	FL_OBJECT *tgt_type;
	FL_OBJECT *tgt_value;
	FL_OBJECT *tgt_name;
	FL_OBJECT *tgt_load;
	FL_OBJECT *class_menu;
	FL_OBJECT *target_browser;
	FL_OBJECT *tgt_filter;
	FL_OBJECT *tgt_latitude;
	FL_OBJECT *tgt_longitude;
	FL_OBJECT *tgt_altitude;
	FL_OBJECT *tgt_count;
	FL_OBJECT *tgt_map;
	FL_OBJECT *tgt_imagery;
	FL_OBJECT *damage[12];
	FL_OBJECT *arrival[14];
	FL_OBJECT *tgt_pixmap[10];
} FD_targetstatus;

extern FD_targetstatus * create_form_targetstatus(void);
typedef struct {
	FL_FORM *tgtimage;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *tgt_image;
} FD_tgtimage;

extern FD_tgtimage * create_form_tgtimage(void);

#endif /* FD_targetstatus_h_ */
