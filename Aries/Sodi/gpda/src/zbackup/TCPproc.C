#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netdb.h>

#ifdef SC_THREADS
#include "pthread.h"
#endif

#include "Globals.h"
#include "isql.h"

#include "forms.h"
#include "TCPforms.h"

enum errlist
{
  BAD_ARGS,BAD_HOST,NO_IDENT,SOCK_ERR
};

int             TCPwinX, TCPwinY;
int             TCPwinW, TCPwinH;
char            TCPlabel[32];
char            *TCPitMaps;
Window          TCPwinid;

char            tcptemp[128];

FD_tcpscan      *fd_tcpscan;

void TCPinit();
void TCPshow(int xpos, int ypos, int width, int height, Window mainwinID);
void TCPexitCB(FL_OBJECT *object, long item_no);
int  TCPclose(FL_FORM *form, void *data);
void TCPnoneCB(FL_OBJECT *ob, long data);
int  portscan(char *, int, int);

extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();

/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void TCPinit()
{
int             i;

   fd_tcpscan = create_form_tcpscan();

   strcpy(TCPlabel, "Port-Scan");

   if ((TCPitMaps = getenv("BITMAPDIR")) == NULL) TCPitMaps = "../BitMaps";

   fl_set_browser_fontsize(fd_tcpscan->tcp_portsopen, FL_NORMAL_SIZE); 
   fl_set_browser_fontstyle(fd_tcpscan->tcp_portsopen, FL_FIXEDBOLD_STYLE);
   fl_set_browser_fontsize(fd_tcpscan->tcp_suspicious, FL_NORMAL_SIZE); 
   fl_set_browser_fontstyle(fd_tcpscan->tcp_suspicious, FL_FIXEDBOLD_STYLE);
   fl_set_browser_fontsize(fd_tcpscan->tcp_trace, FL_SMALL_SIZE); 
   fl_set_browser_fontstyle(fd_tcpscan->tcp_trace, FL_FIXEDBOLD_STYLE);

   fl_set_input(fd_tcpscan->tcp_hostname, "localhost");
   fl_set_input(fd_tcpscan->tcp_sport, "1");
   fl_set_input(fd_tcpscan->tcp_eport, "65000");

   return;
}

void TCPshow(int xpos, int ypos, int width, int height, Window mainwinID)
{
Window          winid;
FL_OBJECT       *ipipm;

   if(!fl_form_is_visible(fd_tcpscan->tcpscan) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      winid = fl_prepare_form_window(fd_tcpscan->tcpscan,
                                     FL_PLACE_POSITION,FL_TRANSIENT, TCPlabel);
      fl_winreshape(winid, xpos, ypos, width, height);
      fl_show_form_window(fd_tcpscan->tcpscan);
      fl_set_form_atclose(fd_tcpscan->tcpscan, TCPclose, 0);
      StoreActiveEntry(TCPlabel);
   }

   fl_set_object_label(fd_tcpscan->tcp_ping, "");

   sprintf (tcptemp, "%s/%s", TCPitMaps, "led-red.xpm");
   ipipm = fd_tcpscan->tcp_busy;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   fl_set_pixmap_file(ipipm, tcptemp);
   //
   ipipm = fd_tcpscan->tcp_busy2;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   fl_set_pixmap_file(ipipm, tcptemp);
   //
   ipipm = fd_tcpscan->tcp_busy3;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   fl_set_pixmap_file(ipipm, tcptemp);
   //
   ipipm = fd_tcpscan->tcp_busy4;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   fl_set_pixmap_file(ipipm, tcptemp);

   //tcpscanCB(NULL, 0);

   return;
}
void TCPexitCB(FL_OBJECT *object, long item_no)
{
int             item = item_no;

   fl_hide_form(fd_tcpscan->tcpscan);
   EraseActiveEntry(TCPlabel);

   FinishUp();

   return;
}

int TCPclose(FL_FORM *form, void *data)
{
long            item;

   item = (long)data;
   TCPexitCB(NULL, item);

   return(0);
}

void TCPnoneCB(FL_OBJECT *ob, long data)
{
   return;
}

/*
 *   ident-scan [v0.15]
 *   This TCP scanner has the additional functionality of retrieving
 *   the username that owns the daemon running on the specified port.
 *   It does this by by attempting to connect to a TCP port, and if it
 *   succeeds, it will send out an ident request to identd on the
 *   remote host.  I believe this to be a flaw in the design of the
 *   protocol, and if it is the developers intent to allow 'reverse' 
 *   idents, then it should have been stated clearer in the 
 *   rfc(rfc1413).
 *
 *   USES:
 *   It can be useful to determine who is running daemons on high ports 
 *   that can be security risks.  It can also be used to search for 
 *   misconfigurations such as httpd running as root, other daemons
 *   running under the wrong uids.
 *   
 *   COMPILES:  Compiles fine under Linux, BSDI and SunOS 4.1.x.
 *
 *   Dave Goldsmith
 *   <daveg@escape.com>
 *   02/11/1996   
 */

void usage(enum errlist error)
{
   switch(error) {
     case BAD_ARGS:
       sprintf(tcptemp,"Usage: ident-scan hostname [low port] [hi port]\n");
       break;
     case BAD_HOST:
       sprintf(tcptemp,"Error: Can't resolve hostname\n");
       break;
     case NO_IDENT:
       sprintf(tcptemp,"Error: 'ident' isn't running on host\n");
       break;
     case SOCK_ERR:
       sprintf(tcptemp,"Error: socket() call failed\n");
       break;
   }

   fl_show_messages(tcptemp);

   return;
}

struct hostent *fill_host(char *machine, struct hostent *host)
{

   if ((host=gethostbyname(machine))==NULL) {
     if ((host=gethostbyaddr(machine,4,AF_INET))==NULL)
        return(host);
   }

   return(host);
}
 
int portscan(char *hostname, int sport, int eport)
{
struct sockaddr_in forconnect, forport, forident;
int             i, sockfd, identfd, hiport=65000, loport=1, curport;
int             IDENTOK=0;
socklen_t       len = sizeof(forport);
struct servent  *service;
struct hostent  *host;
char            identbuf[15], recieved[85], chservice[16], chuser[16], *uid;

   strcpy(tcptemp, fl_get_input(fd_tcpscan->tcp_hostname));
   if ((host=fill_host(tcptemp, host))==NULL)
     //if ((host=fill_host(hostname, host))==NULL)
     usage(BAD_HOST);

   forconnect.sin_family=host->h_addrtype;
   forconnect.sin_addr.s_addr=*((long *)host->h_addr);
   forident.sin_family=host->h_addrtype;
   forident.sin_addr.s_addr=*((long *)host->h_addr);
   forident.sin_port=htons(113);

   if ((identfd=socket(AF_INET,SOCK_STREAM,0)) == -1) usage(SOCK_ERR);
   if ((connect(identfd,(struct sockaddr *)&forident,sizeof(forident)))!=0)
     IDENTOK = FALSE; //usage(NO_IDENT);
   close(identfd);

   if (sport > 0) loport = sport;
   if (eport > 0) hiport = eport;

   for(curport=loport; curport<=hiport; curport++) {
     for(i=0;i!=85;i++) recieved[i]='\0';
     forconnect.sin_port=htons(curport);
     if ((sockfd=socket(AF_INET,SOCK_STREAM,0))== -1)
        usage(SOCK_ERR);
     
     if (connect(sockfd,(struct sockaddr *)&forconnect,sizeof(forconnect))==0) {
       if (getsockname(sockfd,(struct sockaddr *)&forport,&len)==0)
       {
	  if (IDENTOK) {
            if ((identfd=socket(AF_INET,SOCK_STREAM,0))== -1)
               usage(SOCK_ERR);
            if (connect(identfd,(struct sockaddr *)&forident,sizeof(forident))==0)
            {
               sprintf(identbuf,"%u,%u",htons(forconnect.sin_port),
                htons(forport.sin_port));
     
               write(identfd,identbuf,strlen(identbuf)+1);
               read(identfd,recieved,80);
               recieved[strlen(recieved)-1]='\0';
               uid=strrchr(recieved,' ');
             }
	  }
          service = getservbyport(forconnect.sin_port, "tcp");
	  strcpy(chservice, (service == NULL) ? "(?????)" : service->s_name);
	  strcpy(chuser,    (IDENTOK == 0)    ? uid       : "Unknown");
          sprintf(tcptemp, "%10s [%5d] %s",
		  chservice,
		  curport,
		  "Unknown");
	  if (strcmp(chservice, "(?????)") == 0) {
	    fl_addto_browser(fd_tcpscan->tcp_suspicious, tcptemp);
	  } else {
	    fl_addto_browser(fd_tcpscan->tcp_portsopen, tcptemp);
	  }
       }
     }
     close(sockfd);
     if (IDENTOK) close(identfd);
   }

   return(0);
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void tcpscanCB(FL_OBJECT *object, long item_no)
{
int             sport, eport;
char            chname[64];
FL_OBJECT       *ipipm;

   ipipm = fd_tcpscan->tcp_busy;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (tcptemp, "%s/%s", TCPitMaps, "led-green.xpm");
   fl_set_pixmap_file(ipipm, tcptemp);

   sport = atoi(fl_get_input(fd_tcpscan->tcp_sport));
   eport = atoi(fl_get_input(fd_tcpscan->tcp_eport));

   sprintf(tcptemp, "%10s  %7s %s", "Service", " Port ", "Userid");
   fl_clear_browser(fd_tcpscan->tcp_portsopen);
   fl_set_object_label(fd_tcpscan->tcp_oportitle, tcptemp);
   fl_clear_browser(fd_tcpscan->tcp_suspicious);
   fl_set_object_label(fd_tcpscan->tcp_sportitle, tcptemp);

   //fl_set_input(fd_tcpscan->tcp_hostname, "localhost");
   //strcpy(chname, fl_get_input(fd_tcpscan->tcp_hostname));
   //strcpy(chname, "localhost");
   portscan("localhost", sport, eport);

   ipipm = fd_tcpscan->tcp_busy;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (tcptemp, "%s/%s", TCPitMaps, "led-red.xpm");
   fl_set_pixmap_file(ipipm, tcptemp);

   return;
}

void tcptraceCB(FL_OBJECT *object, long item_no)
{
FILE            *pipein, *pipeout;
int             sport, eport;
int             hopno;
char            chname[64], chip[32], cmdline[64];
char            chtemp[128]; 
FL_OBJECT       *ipipm;

   ipipm = fd_tcpscan->tcp_busy2;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (tcptemp, "%s/%s", TCPitMaps, "led-green.xpm");
   fl_set_pixmap_file(ipipm, tcptemp);

   fl_clear_browser(fd_tcpscan->tcp_trace);
   fl_set_input(fd_tcpscan->tcp_hostname, "128.83.120.155");

   strcpy(cmdline, "/usr/sbin/traceroute");
   strcat(cmdline, " ");                          // Assure separator
   strcat(cmdline, fl_get_input(fd_tcpscan->tcp_hostname));
   fprintf(stderr, "Doing: %s\n", cmdline);
   if ((pipein = popen(cmdline, "r")) == NULL)    // Start the search engine
             return;
   //
   while (1) {
     fgets(tcptemp, 120, pipein);                 // Get next line from pipe
     if (feof(pipein)) break;
     //fputs(tcptemp, stdout);                    // Echo it for now
     strsub(tcptemp, '\n', '\0');                 // Get rid of newline
     sscanf(tcptemp, "%d %s %s", &hopno, chname, chip);
     sprintf(chtemp, "%3d. %-40s %-24s", hopno, chname, chip);
     fl_addto_browser(fd_tcpscan->tcp_trace, chtemp);
   }
   pclose(pipein);

   ipipm = fd_tcpscan->tcp_busy2;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (tcptemp, "%s/%s", TCPitMaps, "led-red.xpm");
   fl_set_pixmap_file(ipipm, tcptemp);

   return;
}

void tcppingCB(FL_OBJECT *object, long item_no)
{
FILE            *pipein, *pipeout;
int             sport, eport;
int             pinged;
char            chname[64], chip[32], cmdline[64];
char            chtemp[128]; 
FL_OBJECT       *ipipm;

   ipipm = fd_tcpscan->tcp_busy3;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (tcptemp, "%s/%s", TCPitMaps, "led-green.xpm");
   fl_set_pixmap_file(ipipm, tcptemp);

   pinged = FALSE;
   fl_set_input(fd_tcpscan->tcp_hostname, "128.83.120.155");

   strcpy(cmdline, "ping -c 1");
   strcat(cmdline, " ");                          // Assure separator
   strcat(cmdline, fl_get_input(fd_tcpscan->tcp_hostname));
   if ((pipein = popen(cmdline, "r")) == NULL)    // Start the search engine
             return;
   //
   while (1) {
     fgets(tcptemp, 120, pipein);                 // Get next line from pipe
     if (feof(pipein)) break;
     //fputs(tcptemp, stdout);                    // Echo it for now
     strsub(tcptemp, '\n', '\0');                 // Get rid of newline
     if (strstr(tcptemp, "64 bytes from") == 0)
       pinged = TRUE;
   }
   pclose(pipein);

   if (pinged == 0)
     fl_set_object_label(fd_tcpscan->tcp_ping, "Failure!");
   else
     fl_set_object_label(fd_tcpscan->tcp_ping, "Success!");

   ipipm = fd_tcpscan->tcp_busy3;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (tcptemp, "%s/%s", TCPitMaps, "led-red.xpm");
   fl_set_pixmap_file(ipipm, tcptemp);

   return;
}

void tcphostCB(FL_OBJECT *object, long item_no)
{
FILE            *pipein, *pipeout;
int             sport, eport;
int             outit;
char            chname[64], chip[32], cmdline[64];
char            chtemp[128]; 
FL_OBJECT       *ipipm;

   ipipm = fd_tcpscan->tcp_busy4;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (tcptemp, "%s/%s", TCPitMaps, "led-green.xpm");
   fl_set_pixmap_file(ipipm, tcptemp);

   fl_clear_browser(fd_tcpscan->tcp_host);
   fl_set_input(fd_tcpscan->tcp_hostname, "net2.cs.utexas.edu");

   outit = FALSE;

   strcpy(cmdline, "host -l -v -t any");
   strcat(cmdline, " ");                          // Assure separator
   strcat(cmdline, fl_get_input(fd_tcpscan->tcp_hostname));
   if ((pipein = popen(cmdline, "r")) == NULL)    // Start the search engine
             return;
   //
   while (1) {
     fgets(tcptemp, 120, pipein);                 // Get next line from pipe
     if (feof(pipein)) break;                     // If no more output
     //fputs(tcptemp, stdout);                    // Echo it for now
     strsub(tcptemp, '\n', '\0');                 // Get rid of newline
     if (strstr(tcptemp, "ANSWER SECTION") == 0)
       outit = TRUE;
     if (outit == TRUE)
       fl_add_browser_line(fd_tcpscan->tcp_host, tcptemp);
   }
   pclose(pipein);

   ipipm = fd_tcpscan->tcp_busy4;
   fl_show_object(ipipm);
   fl_free_pixmap_pixmap(ipipm);
   sprintf (tcptemp, "%s/%s", TCPitMaps, "led-red.xpm");
   fl_set_pixmap_file(ipipm, tcptemp);

   return;
}
