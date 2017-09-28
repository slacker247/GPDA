
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
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

/* --------------------------------------------------------------------- */

int        toprttr;

int        msgsock;
int        sock;
int        portid;
char       hostid[16];
struct sockaddr_in server, client;
int                server_len, client_len;

/* --------------------------------------------------------------------- */

void Net_init(int port, char *host);
void Net_write(char *buf, int bytes);
int  Net_read(char *buf, int bufsize);
void Net_close();

/* --------------------------------------------------------------------- */

main(int argc, char *argv[])
{

FILE             *infile;
char             xdrbuf[180];
int              xdrsize;
char             buffer[121];
char             buf[200];
char             chmsgno[8];
char             chmark[40];
char             chmsgtyp;
char             chident[40];
char             idtext[8];
float            orient, major, minor;
int              drop, stn, incolor;
int              trkid_S, trkid_A, trkid_E;
float            Xlat, Xlon, Xalt;
float            tadilj_time;
int              n_records = 0;
int              xferbytes;

   portid = 1129;
   strcpy(hostid, "tisa3b");  
   Net_init(portid, hostid);                 // Initialize the socket connections

   infile = fopen("arcticsubs.pdu", "r+");

   while (!feof(infile)) {
      fgets(buffer, sizeof(buffer), infile);
      sscanf(buffer, "%f %c %s %d %d %d %o %f %f %f %s %d %f %f %f",
            &tadilj_time, &chmsgtyp, chmsgno,
            &trkid_S, &trkid_A, &trkid_E, &stn, &Xlat, &Xlon, &Xalt,
            chmark, &incolor, &orient, &major, &minor);
      n_records = n_records+1;
      /*
      fprintf(stderr, "[%d] %f %c %s %d %d %d %o %f %f %f %s %d %f %f %f\n",
               n_records, tadilj_time, chmsgtyp, chmsgno,
               trkid_S, trkid_A, trkid_E, stn, Xlat, Xlon, Xalt,
               chmark, incolor, orient, major, minor);
      */
      sprintf(xdrbuf, "%8s %8s %d %d %s\n", "TMDSE   ", "JTAMV   ", 0, 0, buffer);
      //fprintf(stderr, "Buffer count is %d\n", strlen(xdrbuf) );
      //fprintf(stderr, "%s\n", xdrbuf); 

      xdrsize = strlen(xdrbuf);
      fprintf(stderr, "Send request. Byte count is %d\n", xdrsize);
      Net_write(xdrbuf, xdrsize);

      xferbytes = Net_read(buf, 200);
   }

   sprintf(xdrbuf, "%c", "\0");
   Net_write(xdrbuf, 1);

   Net_close();
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
	fprintf(stderr, "SimCmdr has exited.\n");
   }
   return (rval);
}

void
Net_close()
{
  close(sock);
}

