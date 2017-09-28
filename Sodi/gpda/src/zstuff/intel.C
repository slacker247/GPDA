
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
#ifdef SC_THREADS
#include "pthread.h"
#endif
/*
 *   Include the network/socket stuff
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#include <rpc/xdr.h>
#define SIGCHILD SIGCLD 
/*
 *   Include the X-window stuff
 */
#include <Xm/MainW.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/ScrolledW.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/DrawingA.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include "X11/Xc/Led.h"
/*
 *   Include the SimCmdr stuff
 */
#include "parser.H"

#include "forms.h"
#include "IPforms.h"

#include "demo_opcodes.h"
#include "demo_header.h"
#include "demo_msg_body.h"
#include "demo_players.h"
#include "demo_strings.h"

/* --------------------------------------------------------------------- */

int        toprttr;

XtIntervalId  timeoutid;

int        msgsock;
int        sock;
int        portid;
char       *hostid;
struct sockaddr_in server, client;
int                server_len, client_len;

FD_IPcontrol  *fd_IPcontrol;
FL_OBJECT     *ipipm;

/* --------------------------------------------------------------------- */

static void timeoutCB(int itout, void *call_data);

void Net_init(int port, char *host);
void Net_write(char *buf, int bytes);
int  Net_read(char *buf, int bufsize);
void Net_close();

extern "C" void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
extern "C" void decode_header (XDR *xdrs_de, struct header *de, int *nargs);
extern "C" int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
extern "C" int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);

/* --------------------------------------------------------------------- */

main(int argc, char *argv[])
{
time_t     clock;
struct tm  *loctime;
char       *str;
C_PARSER   *lanl_bp_parser;
C_BASETYPE *parameters;

   printf("\n\n");
   printf("  -------------- NMD Intel Processor --------------\n");
   printf("                 (Version 0.1)");
   printf("\n\n");

   lanl_bp_parser = new C_PARSER("planner.par");
   parameters = lanl_bp_parser->get_basetype("parameters");
   portid = parameters->get_int("portid");
   hostid = parameters->get_string("hostid");  
   Net_init(portid, hostid);                 // Initialize the socket connections
//
// Initialize the 'FORMS' system and create all forms
//
   fl_initialize(&argc, argv, 0, 0, 0);
   fd_IPcontrol = create_form_IPcontrol();

   ipipm = fd_IPcontrol->SC_icon;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   fl_set_pixmap_file(ipipm, "SmallHead.xpm");

   fl_set_browser_fontsize(fd_IPcontrol->intel_browser, FL_NORMAL_SIZE);
   fl_set_browser_fontstyle(fd_IPcontrol->intel_browser, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontsize(fd_IPcontrol->intel_source, FL_NORMAL_SIZE);
   fl_set_browser_fontstyle(fd_IPcontrol->intel_source, FL_FIXED_STYLE|FL_BOLD_STYLE);
//
// Popup the 'Intel Processor' form and get the show going
//
   fl_show_form(fd_IPcontrol->IPcontrol, FL_PLACE_CENTER,FL_FULLBORDER, "SC Control");

   timeoutid = fl_add_timeout(1000, timeoutCB, NULL);

   fl_do_forms();
}

static void timeoutCB(int itout, void *call_data)
{
int          incount;
char         xdrbuf[2000], str[512];
char         *inbuf, *outbuf;
char         src[5], dst[5], obj[25], roe[25], dea[5];
int          scn, opcode, response, pad1, pad2, pad3, pad4;
int          Defcon, Rposture, Weapons;
static int   DialogActive = FALSE;
int          sp, dp;
float        gvt;
int          i, answer, nargs, Thread;;
XDR          xdrs;
unsigned int bytecnt, four = 4, xdrsize;
int          zero = 0, one = 1, two = 2, three = 3, five = 5, six = 6;
Widget       i_dialog;

   //
   //  Do it again in POLLRATE seconds
   //
   timeoutid = fl_add_timeout(1000, timeoutCB, NULL);
}

void
SC_intel_time(int day, float hours)
{
char   chline[16];

   sprintf(chline, "%d", day);
   fl_set_object_label(fd_IPcontrol->intel_day, chline);
   sprintf(chline, "%f", hours);
   fl_set_object_label(fd_IPcontrol->intel_hour, chline);     
}

void
SC_intel_text(char *str)
{

   fl_clear_browser(fd_IPcontrol->intel_browser);
   fl_addto_browser(fd_IPcontrol->intel_browser, str);
}

void
SC_intel_src(char *chline)
{

   fl_clear_browser(fd_IPcontrol->intel_source);   
   fl_addto_browser(fd_IPcontrol->intel_source, chline);
}

/* --------------------------------------------------------------------- */

void
Net_init(int portid, char *host)
{
  struct hostent *hp = gethostbyname(host);
  if (hp == NULL) {
    fprintf(stderr, "%s: unknown host", host); exit(2);
  }
     
  server.sin_family = AF_INET;
  server.sin_port = htons(portid);
  memmove(&server.sin_addr, hp->h_addr, hp->h_length);

  sock = socket(AF_INET, SOCK_DGRAM, 0);           /* Create socket */
  if (sock < 0) {
    perror("opening stream socket"); exit(1);
  }
  client.sin_family = AF_INET;
  client.sin_addr.s_addr = htonl(INADDR_ANY);
  client.sin_port = htons(0);

  if (bind(sock, (struct sockaddr *)&client, sizeof(client)) < 0) {
    perror("Client bind "); exit(4);
  }
  /*
  if (connect(sock, (sockaddr*) &server, sizeof(server)) < 0) {
    perror("connecting stream socket");
    exit(1);
  }
  */
}

void
Net_write(char *buf, int bytes)
{    
    server_len = sizeof(server);
    if (sendto(sock, buf, bytes, 0, (struct sockaddr *)&server, server_len) < 0) {
      perror("writing on stream socket"); close(sock); exit(1);
    }
}

int
Net_read(char *buf, int bufsize)
{
int  rval;

   memset(buf, 0, bufsize);
   rval = recvfrom(sock, buf, bufsize, 0, (struct sockaddr *)&server, &server_len); 
   if (rval < 0) {
	perror("reading stream message"); close(sock); exit(1);
   }
   if (rval == 0) {
	cout << "SimCmdr has exited." << endl;
   }
   return (rval);
}

void
Net_close()
{
  close(sock);
}

/* -------------------------- SC Control CB's -------------------------- */

void ip_exitCB(FL_OBJECT *object, long item_no)
{
int item = 0;

   exit(0);
}

void ip_browserCB(FL_OBJECT *object, long item_no)
{
int item = 0;

}

void ip_nextCB(FL_OBJECT *object, long item_no)
{
int              item = 0;
static int       opened = FALSE;
static FILE      *infile;
static int       icount = 0;

int              i,j,l,n;
int              nsource, nlines, srcid, nargs_in;
int              day;
float            hours;
float            x, y;
char             ch;
char             chline[80];
char             ipline[1280];

char             chsource[4][40];
char             chout[5][120];

int              opcode=150;
int              scn=0, response=0, pad1=0, pad2=0, pad3=0, pad4=0;
float            gvt=0.0;
int              nargs, xferbytes;
char             buf[2000], src[5], dst[5], dea[5];
char             *xdrbuf;
int              sp=BtlP, dp=BMDC1_DR1;
unsigned int     four = 4, bytecnt, xdrsize;
XDR              xdrs;
struct header    BPen, BPde;
struct SEND_ALL  BPen_body;
struct RTRN_ALL  BPde_body;

   if (!opened) {
      infile = fopen("intel.in", "r+");
      opened = TRUE;
   }

   fl_set_object_label(fd_IPcontrol->status_text, "Processing");
   icount = icount + 1;
   sprintf(chline, "%d", icount);
   fl_set_object_label(fd_IPcontrol->opcode_text, chline);
//
//   Build and send the message to the SC
//
   xdrbuf = (char *)malloc(2000); 
   xdrmem_create(&xdrs, xdrbuf, (unsigned)2000, XDR_ENCODE);

   BPen.SrcID     = BtlP;
   BPen.DstID     = BMDC1_DR1;
   BPen.SCID      = Sim_Cmdr_ID;
   BPen.opcode    = OP_INTEL_INFO;
   BPen.SCactive  = 0;
   BPen.gvt       = 0.0;
   BPen.reserved7 = 0;
   BPen.reserved8 = 0;
   BPen.reserved9 = 0;
   encode_header (&xdrs, &BPen, &nargs);
//
//   Get the next INTEL record
//
   if (fscanf(infile, "%d %f", &day, &hours) <= 0) { fclose(infile); opened = FALSE; return; }
   fscanf(infile, "%d", &nsource);         // get number of sources

   BPen_body.intel_info_body_struct.II_Day     = day;
   BPen_body.intel_info_body_struct.II_Hrs     = hours;
   BPen_body.intel_info_body_struct.II_Nam_Num = nsource;

   strcpy(chsource[0], "                          ");
   strcpy(chsource[1], "                          ");
   strcpy(chsource[2], "                          ");

   for (i=0; i<nsource; i++) {
      fgetc(infile);
      fread(chline, 1, 5, infile);
      chline[5] = '\0';
      fscanf(infile, "%d %f %f", &srcid, &x, &y);

      sprintf(ipline, "%-5s %10d   %10.4f   %10.4f", chline, srcid, x, y);
      strcpy(chsource[i], ipline);
      printf("%s\n", chsource[i]);

      strcpy (BPen_body.intel_info_body_struct.II_Nam_Src[i], chline);
      BPen_body.intel_info_body_struct.II_Nam_Loc[i]        = srcid;
      BPen_body.intel_info_body_struct.II_Val[i]            = x;
      BPen_body.intel_info_body_struct.II_Per[i]            = y;
   }

   fscanf(infile, "%d", &nlines);         // get number of lines
   fgetc(infile);                         // get newline char
   strcpy(chout[0], " ");
   strcpy(chout[1], " ");
   strcpy(chout[2], " ");
   strcpy(chout[3], " ");
   strcpy(chout[4], " ");
   for (i=0; i<nlines; i++) {
      ch = fgetc(infile); l = 0;          // get 1st char
      while (ch != '\n') {
        chline[l] = ch;
        l = l+1;
        ch = fgetc(infile);
      }
      chline[l] = '\0';
      strcpy(chout[i], chline);
   }
   for (i = 0; i < 5; i++)
    {
      strcpy (BPen_body.intel_info_body_struct.II_Txt[i], chout[i]);
    }

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   xdrsize = xdr_getpos(&xdrs);
//
//   Display the data
//
   SC_intel_time(day, hours);

   sprintf(ipline, "%s\n%s\n%s", chsource[0], chsource[1], chsource[2]);
   SC_intel_src(ipline);

   sprintf(ipline, "%s\n%s\n%s\n%s\n%s", chout[0], chout[1], chout[2], chout[3], chout[4]);
   SC_intel_text(ipline);

   fprintf(stderr, "Send request. Byte count is %d\n", xdrsize);
   Net_write(xdrbuf, xdrsize);
   xdr_destroy(&xdrs);
   free(xdrbuf);

   fl_set_object_label(fd_IPcontrol->status_text, "Waiting");
}
