/** Header file generated with fdesign on Fri Oct 20 10:59:41 2000.**/

#ifndef FD_ccpmanage_h_
#define FD_ccpmanage_h_

/** Callbacks, globals and object handlers **/
extern void CCPnoneCB(FL_OBJECT *, long);
extern void CCPexitCB(FL_OBJECT *, long);

extern void CCPexitCB(FL_OBJECT *, long);
extern void CCPnoneCB(FL_OBJECT *, long);

extern void CCPexitCB(FL_OBJECT *, long);
extern void CCPnoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *ccpmanage;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *pix_manage;
} FD_ccpmanage;

extern FD_ccpmanage * create_form_ccpmanage(void);
typedef struct {
	FL_FORM *ccpsurvival;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *pix_survival;
} FD_ccpsurvival;

extern FD_ccpsurvival * create_form_ccpsurvival(void);
typedef struct {
	FL_FORM *ccpattack;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *pix_attack;
} FD_ccpattack;

extern FD_ccpattack * create_form_ccpattack(void);

#endif /* FD_ccpmanage_h_ */
