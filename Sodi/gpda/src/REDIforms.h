/** Header file generated with fdesign on Fri Oct 20 11:38:17 2000.**/

#ifndef FD_readistatus_h_
#define FD_readistatus_h_

/** Callbacks, globals and object handlers **/
extern void redinoneCB(FL_OBJECT *, long);
extern void REDIexitCB(FL_OBJECT *, long);
extern void rediselectCB(FL_OBJECT *, long);
extern void redistatusCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *readistatus;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *plause_pie;
	FL_OBJECT *site_browser;
	FL_OBJECT *redi_sites;
	FL_OBJECT *redi_3D;
	FL_OBJECT *redi_select[6];
	FL_OBJECT *redi_status[59];
} FD_readistatus;

extern FD_readistatus * create_form_readistatus(void);

#endif /* FD_readistatus_h_ */
