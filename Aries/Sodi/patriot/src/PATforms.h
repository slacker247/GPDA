/** Header file generated with fdesign on Fri Jan 24 12:28:27 2003.**/

#ifndef FD_PATcontrol_h_
#define FD_PATcontrol_h_

/** Callbacks, globals and object handlers **/
extern void consoleCB(FL_OBJECT *, long);
extern void tabCB(FL_OBJECT *, long);
extern void situationCB(FL_OBJECT *, long);
extern void acknowCB(FL_OBJECT *, long);
extern void trackCB(FL_OBJECT *, long);
extern void systemCB(FL_OBJECT *, long);
extern void engageCB(FL_OBJECT *, long);
extern void powerCB(FL_OBJECT *, long);
extern void radiateCB(FL_OBJECT *, long);
extern void statusCB(FL_OBJECT *, long);
extern void weaponsCB(FL_OBJECT *, long);
extern void exitCB(FL_OBJECT *, long);
extern void canvasCB(FL_OBJECT *, long);
extern void globe3dCB(FL_OBJECT *, long);

extern void ECS_todCB(FL_OBJECT *, long);
extern void voidCB(FL_OBJECT *, long);
extern void countCB(FL_OBJECT *, long);
extern void LS_inventoryCB(FL_OBJECT *, long);
extern void standbyCB(FL_OBJECT *, long);
extern void ddlCB(FL_OBJECT *, long);
extern void fuelCB(FL_OBJECT *, long);
extern void operCB(FL_OBJECT *, long);
extern void brightCB(FL_OBJECT *, long);
extern void ECS_dismissCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *PATcontrol;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *console;
	FL_OBJECT *canvas;
} FD_PATcontrol;

extern FD_PATcontrol * create_form_PATcontrol(void);
typedef struct {
	FL_FORM *ECStatus;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *defcon;
	FL_OBJECT *alert;
	FL_OBJECT *ECS_method;
	FL_OBJECT *ECS_weapon;
	FL_OBJECT *ECS_adw;
	FL_OBJECT *ECS_attack;
	FL_OBJECT *ECS_cbr;
	FL_OBJECT *ECS_equip;
	FL_OBJECT *ECS_commo;
	FL_OBJECT *ECS_bright;
	FL_OBJECT *tod[6];
	FL_OBJECT *ECS_count[3];
	FL_OBJECT *LS_inventory1[8];
	FL_OBJECT *ECS_standby[8];
	FL_OBJECT *ECS_ddl[8];
	FL_OBJECT *ECS_fuel[8];
	FL_OBJECT *ECS_oper[8];
	FL_OBJECT *LS_inventory0[8];
} FD_ECStatus;

extern FD_ECStatus * create_form_ECStatus(void);

#endif /* FD_PATcontrol_h_ */
