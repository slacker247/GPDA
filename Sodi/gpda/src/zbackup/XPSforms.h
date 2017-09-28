/** Header file generated with fdesign on Fri Oct 26 10:37:29 2001.**/

#ifndef FD_xps_h_
#define FD_xps_h_

/** Callbacks, globals and object handlers **/
extern void xps_browser_cb(FL_OBJECT *, long);
extern void xps_timer_cb(FL_OBJECT *, long);
extern void XPSexitCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *xps;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *xps_browser;
	FL_OBJECT *xps_timer;
} FD_xps;

extern FD_xps * create_form_xps(void);

#endif /* FD_xps_h_ */
