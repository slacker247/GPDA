/** Header file generated with fdesign on Mon Jul  1 16:10:46 2002.**/

#ifndef FD_tcpscan_h_
#define FD_tcpscan_h_

/** Callbacks, globals and object handlers **/
extern void TCPexitCB(FL_OBJECT *, long);
extern void TCPnoneCB(FL_OBJECT *, long);
extern void tcpscanCB(FL_OBJECT *, long);
extern void tcptraceCB(FL_OBJECT *, long);
extern void tcppingCB(FL_OBJECT *, long);
extern void tcphostCB(FL_OBJECT *, long);


/**** Forms and Objects ****/
typedef struct {
	FL_FORM *tcpscan;
	void *vdata;
	char *cdata;
	long  ldata;
	FL_OBJECT *tcp_suspicious;
	FL_OBJECT *tcp_sport;
	FL_OBJECT *tcp_eport;
	FL_OBJECT *tcp_scan;
	FL_OBJECT *tcp_portsopen;
	FL_OBJECT *tcp_oportitle;
	FL_OBJECT *tcp_sportitle;
	FL_OBJECT *tcp_hostname;
	FL_OBJECT *tcp_periodic;
	FL_OBJECT *tcp_busy;
	FL_OBJECT *tcp_trace;
	FL_OBJECT *tcp_busy2;
	FL_OBJECT *tcp_busy3;
	FL_OBJECT *tcp_ping;
	FL_OBJECT *tcp_busy4;
	FL_OBJECT *tcp_host;
} FD_tcpscan;

extern FD_tcpscan * create_form_tcpscan(void);

#endif /* FD_tcpscan_h_ */
