/** Header file generated with fdesign on Mon Jun 26 08:22:39 2000.**/

#ifndef FD_Map_h_
#define FD_Map_h_

/** Callbacks, globals and object handlers **/
extern void maindoneCB(FL_OBJECT *, long);
extern void canvasCB(FL_OBJECT *, long);
extern void hawkCB(FL_OBJECT *, long);
extern void crcCB(FL_OBJECT *, long);
extern void aegisCB(FL_OBJECT *, long);
extern void jtagsCB(FL_OBJECT *, long);
extern void patriotCB(FL_OBJECT *, long);
extern void gbiCB(FL_OBJECT *, long);
extern void dspCB(FL_OBJECT *, long);
extern void sbirsCB(FL_OBJECT *, long);
extern void gbrCB(FL_OBJECT *, long);
extern void thaadCB(FL_OBJECT *, long);
extern void awacsCB(FL_OBJECT *, long);
extern void tacticalCB(FL_OBJECT *, long);
extern void cruiseCB(FL_OBJECT *, long);
extern void armynavyCB(FL_OBJECT *, long);

extern void locationCB(FL_OBJECT *, long);
extern void saveCB(FL_OBJECT *, long);
extern void cancelCB(FL_OBJECT *, long);
extern void assetnameCB(FL_OBJECT *, long);
extern void assetidCB(FL_OBJECT *, long);
extern void unitCB(FL_OBJECT *, long);
extern void defenseCB(FL_OBJECT *, long);
extern void weaponsCB(FL_OBJECT *, long);
extern void coverageCB(FL_OBJECT *, long);
extern void sensorCB(FL_OBJECT *, long);
extern void colorassetCB(FL_OBJECT *, long);
extern void scaleCB(FL_OBJECT *, long);
extern void statusCB(FL_OBJECT *, long);
extern void altitudeCB(FL_OBJECT *, long);
extern void iconCB(FL_OBJECT *, long);

extern void deglatchangeCB(FL_OBJECT *, long);
extern void minlatchangeCB(FL_OBJECT *, long);
extern void seclatchangeCB(FL_OBJECT *, long);
extern void seclonchangeCB(FL_OBJECT *, long);
extern void deglonchangeCB(FL_OBJECT *, long);
extern void minlonchangeCB(FL_OBJECT *, long);
extern void donelocationCB(FL_OBJECT *, long);
extern void northdirectionCB(FL_OBJECT *, long);
extern void eastdirectionCB(FL_OBJECT *, long);

extern void nholdchangeCB(FL_OBJECT *, long);
extern void pkillchangeCB(FL_OBJECT *, long);
extern void ngbichangeCB(FL_OBJECT *, long);
extern void gbiidchangeCB(FL_OBJECT *, long);
extern void gbitypechangeCB(FL_OBJECT *, long);
extern void weaponsdoneCB(FL_OBJECT *, long);

extern void donedefenseCB(FL_OBJECT *, long);
extern void arealabelCB(FL_OBJECT *, long);
extern void areanorthCB(FL_OBJECT *, long);
extern void areaeastCB(FL_OBJECT *, long);
extern void arealatdegCB(FL_OBJECT *, long);
extern void arealatminCB(FL_OBJECT *, long);
extern void arealatsecCB(FL_OBJECT *, long);
extern void arealongdegCB(FL_OBJECT *, long);
extern void arealonminCB(FL_OBJECT *, long);
extern void arealonsecCB(FL_OBJECT *, long);
extern void areamajorCB(FL_OBJECT *, long);
extern void areaminorCB(FL_OBJECT *, long);
extern void areaorientCB(FL_OBJECT *, long);
extern void colorCB(FL_OBJECT *, long);
extern void areafileCB(FL_OBJECT *, long);
extern void areanameCB(FL_OBJECT *, long);

extern void sensortypechangeCB(FL_OBJECT *, long);
extern void scantimechangeCB(FL_OBJECT *, long);
extern void rminchangeCB(FL_OBJECT *, long);
extern void rmaxchangeCB(FL_OBJECT *, long);
extern void rmaxlowchangeCB(FL_OBJECT *, long);
extern void rdotminchangeCB(FL_OBJECT *, long);
extern void signalchangeCB(FL_OBJECT *, long);
extern void luminositychangeCB(FL_OBJECT *, long);
extern void elevationchangeCB(FL_OBJECT *, long);
extern void azimuthchangeCB(FL_OBJECT *, long);
extern void fovhighchange(FL_OBJECT *, long);
extern void fovlowchangeCB(FL_OBJECT *, long);
extern void fixedchangeCB(FL_OBJECT *, long);
extern void loschangeCB(FL_OBJECT *, long);
extern void errorchangeCB(FL_OBJECT *, long);
extern void iconchangeCB(FL_OBJECT *, long);
extern void scalechangeCB(FL_OBJECT *, long);
extern void donesensorCB(FL_OBJECT *, long);

extern void blackCB(FL_OBJECT *, long);
extern void redCB(FL_OBJECT *, long);
extern void greenCB(FL_OBJECT *, long);
extern void yellowCB(FL_OBJECT *, long);
extern void blueCB(FL_OBJECT *, long);
extern void pinkCB(FL_OBJECT *, long);
extern void lightblueCB(FL_OBJECT *, long);
extern void whiteCB(FL_OBJECT *, long);
extern void orangeCB(FL_OBJECT *, long);
extern void purpleCB(FL_OBJECT *, long);
extern void greyCB(FL_OBJECT *, long);
extern void brownCB(FL_OBJECT *, long);

extern void selectsensortypeCB(FL_OBJECT *, long);

extern void selectdefensetypeCB(FL_OBJECT *, long);

extern void unitnameCB(FL_OBJECT *, long);
extern void typeCB(FL_OBJECT *, long);
extern void sizeCB(FL_OBJECT *, long);
extern void unittypeCB(FL_OBJECT *, long);
extern void unitdoneCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *Map;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *threats;
	FL_OBJECT *Assets;
	FL_OBJECT *maindone;
	FL_OBJECT *canvas;
	FL_OBJECT *hawk;
	FL_OBJECT *crc;
	FL_OBJECT *aegis;
	FL_OBJECT *jtags;
	FL_OBJECT *patriot;
	FL_OBJECT *gbi;
	FL_OBJECT *dsp;
	FL_OBJECT *sbirs;
	FL_OBJECT *gbr;
	FL_OBJECT *thaad;
	FL_OBJECT *awacs;
	FL_OBJECT *tactical;
	FL_OBJECT *cruise;
	FL_OBJECT *armynavy;
	FL_OBJECT *foreignair;
	FL_OBJECT *scud;
	FL_OBJECT *nodong;
	FL_OBJECT *m9;
	FL_OBJECT *ss18;
} FD_Map;

extern FD_Map * create_form_Map(void);
typedef struct {
	FL_FORM *select_change;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *location;
	FL_OBJECT *north;
	FL_OBJECT *northtext;
	FL_OBJECT *east;
	FL_OBJECT *easttext;
	FL_OBJECT *latdeg;
	FL_OBJECT *latdegtext;
	FL_OBJECT *latmin;
	FL_OBJECT *latmintext;
	FL_OBJECT *latsec;
	FL_OBJECT *latsectext;
	FL_OBJECT *londeg;
	FL_OBJECT *lonmin;
	FL_OBJECT *lonsec;
	FL_OBJECT *londegtext;
	FL_OBJECT *lonmintext;
	FL_OBJECT *lonsectext;
	FL_OBJECT *savedata;
	FL_OBJECT *cancel;
	FL_OBJECT *assetname;
	FL_OBJECT *assetid;
	FL_OBJECT *unit;
	FL_OBJECT *defense;
	FL_OBJECT *weapons;
	FL_OBJECT *coverage;
	FL_OBJECT *sensor;
	FL_OBJECT *colorasset;
	FL_OBJECT *scale;
	FL_OBJECT *status;
	FL_OBJECT *altitude;
	FL_OBJECT *icon;
	FL_OBJECT *instructions;
} FD_select_change;

extern FD_select_change * create_form_select_change(void);
typedef struct {
	FL_FORM *location;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *deglatchange;
	FL_OBJECT *minlatchange;
	FL_OBJECT *seclatchange;
	FL_OBJECT *seclonchange;
	FL_OBJECT *deglonchange;
	FL_OBJECT *minlonchange;
	FL_OBJECT *donelocation;
	FL_OBJECT *locationtext;
	FL_OBJECT *northdirection;
	FL_OBJECT *eastdirection;
} FD_location;

extern FD_location * create_form_location(void);
typedef struct {
	FL_FORM *weapons;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *nholdchange;
	FL_OBJECT *pkillchange;
	FL_OBJECT *ngbichange;
	FL_OBJECT *gbiidchange;
	FL_OBJECT *gbitypechange;
	FL_OBJECT *weaponschangetext;
	FL_OBJECT *weaponsdone;
} FD_weapons;

extern FD_weapons * create_form_weapons(void);
typedef struct {
	FL_FORM *defense;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *donedefense;
	FL_OBJECT *arealabel;
	FL_OBJECT *areanorth;
	FL_OBJECT *areaeast;
	FL_OBJECT *arealatdeg;
	FL_OBJECT *arealatmin;
	FL_OBJECT *arealatsec;
	FL_OBJECT *arealondeg;
	FL_OBJECT *arealonmin;
	FL_OBJECT *arealonsec;
	FL_OBJECT *areamajor;
	FL_OBJECT *areaminor;
	FL_OBJECT *areaorient;
	FL_OBJECT *color;
	FL_OBJECT *areafile;
	FL_OBJECT *areaname;
	FL_OBJECT *defensetextchange;
} FD_defense;

extern FD_defense * create_form_defense(void);
typedef struct {
	FL_FORM *sensors;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *sensortypechange;
	FL_OBJECT *scantimechange;
	FL_OBJECT *rminchange;
	FL_OBJECT *rmaxchange;
	FL_OBJECT *rmaxlowchange;
	FL_OBJECT *rdotminchange;
	FL_OBJECT *signalchange;
	FL_OBJECT *luminositychange;
	FL_OBJECT *elevationchange;
	FL_OBJECT *azimuthchange;
	FL_OBJECT *fovhighchange;
	FL_OBJECT *fovlowchange;
	FL_OBJECT *fixedchange;
	FL_OBJECT *loschange;
	FL_OBJECT *errorchange;
	FL_OBJECT *iconchange;
	FL_OBJECT *scalechange;
	FL_OBJECT *donesensor;
	FL_OBJECT *sensorchangetext;
} FD_sensors;

extern FD_sensors * create_form_sensors(void);
typedef struct {
	FL_FORM *color;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *black;
	FL_OBJECT *red;
	FL_OBJECT *green;
	FL_OBJECT *yellow;
	FL_OBJECT *blue;
	FL_OBJECT *pink;
	FL_OBJECT *lightblue;
	FL_OBJECT *white;
	FL_OBJECT *orange;
	FL_OBJECT *purple;
	FL_OBJECT *grey;
	FL_OBJECT *brown;
	FL_OBJECT *selectcolor;
} FD_color;

extern FD_color * create_form_color(void);
typedef struct {
	FL_FORM *selectsensor;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *selectsensortype;
} FD_selectsensor;

extern FD_selectsensor * create_form_selectsensor(void);
typedef struct {
	FL_FORM *selectdefense;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *selectdefensetype;
} FD_selectdefense;

extern FD_selectdefense * create_form_selectdefense(void);
typedef struct {
	FL_FORM *units;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *unitname;
	FL_OBJECT *type;
	FL_OBJECT *size;
	FL_OBJECT *unittype;
	FL_OBJECT *unitdone;
	FL_OBJECT *unittext;
} FD_units;

extern FD_units * create_form_units(void);

#endif /* FD_Map_h_ */
