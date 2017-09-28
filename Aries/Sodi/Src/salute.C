 /*
 *   This program decodes SALUTE E-mail messages from the SMART
 *   terminal which provides TACELINT, HUMINT and MASINT messages.
 *   The program accepts the message as input from STDIN or a file.
 *
 *   Usage: salute - | filename
 *
 *   SALUTE = Size, Activity, Location, Unit, Time, Equipment
 *
 *   COMPILES:  Compiles fine under Linux, BSDI and SunOS 4.1.x.   
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <stream.h>

enum errlist
{
  BAD_ARGS,BAD_HOST,NO_IDENT,SOCK_ERR
};

int             sock;
int             portid;
char            *hostid;
struct sockaddr_in server, client;
socklen_t       server_len, client_len; 
int             msgsock;

void
usage(enum errlist error)
{
  fprintf(stderr,"ident-scan: ");
  switch(error)
  {
    case BAD_ARGS: fprintf(stderr,"usage: ident-scan hostname [low port] [hi port]\n");
                   break;
    case BAD_HOST: fprintf(stderr,"error: cant resolve hostname\n");
                   break;
    case NO_IDENT: fprintf(stderr,"error: ident isnt running on host\n");
                   break;
    case SOCK_ERR: fprintf(stderr,"error: socket() failed\n");
                   break;
  }
  exit(-1);
}
/*
struct hostent *
fill_host(char *machine, struct hostent host)
{

  if ((host=gethostbyname(machine))==NULL)
  {
     if ((host=gethostbyaddr(machine,4,AF_INET))==NULL)
        return(host);
  }
  return(host);
}
*/
void Net_init(int portid, char *host)
{
   struct hostent *hp = gethostbyname(host);
   if (hp == NULL) {
     fprintf(stderr, "%s: unknown host", host); exit(2);
   }
 
   server.sin_family = AF_INET;
   server.sin_port = htons(portid);
   memmove(&server.sin_addr, hp->h_addr, hp->h_length);
 
   sock = socket(AF_INET, SOCK_DGRAM, 0);          /* Create socket */
   if (sock < 0) {
     perror("opening stream socket"); exit(1);
   }
   client.sin_family = AF_INET;
   client.sin_addr.s_addr = htonl(INADDR_ANY);
   client.sin_port = htons(0);
 
   if (bind(sock, (struct sockaddr *)&client, sizeof(client)) < 0) {
     perror("Client bind "); exit(4);
   }
}
void Net_write(char *buf, int bytes)
{
   server_len = sizeof(server);
   if (sendto(sock, buf, bytes, 0, (struct sockaddr *)&server, server_len) < 0) {
     perror("writing on stream socket"); close(sock); exit(1);
   }
}
 
int Net_read(char *buf, int bufsize)
{
int             rval;
 
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
 
void Net_close()
{
   close(sock);
}
 
int
main(int argc, char **argv)
{
struct sockaddr_in forconnect,forport,forident;
int             i,sockfd,identfd,len=sizeof(forport),hiport=9999,loport=1,curport;
int             nrecs;
struct servent  *service;
struct hostent  *host;
char            identbuf[15], recieved[85], *uid;
char            chmsg[128], chentity[64], chtime[32];
char            chunit[32], chequip[32], chlat[32], chlon[32];
char            *p;
FILE            *infile, *lst;
/*
**   Determine if input is file or STDIN
* 
   if ((argc<2) || (argc>2))
     usage(BAD_ARGS);

   if (argc==2) {
     if (strcmp("-", argv[1]) == 0) {
       infile = stdin;
     } else {
      infile = fopen(argv[1], "r");
     }
   }
*/

   infile = fopen("/tmp/NewMail", "r");

   lst = fopen("/tmp/salute.out", "w");

   Net_init(8127, "localhost");
/*
**   Skip the mail header
*/
   while (1) {
      fgets(chmsg, 128, infile);
      //fprintf(stderr, "Mail in line: %s\n", chmsg);
      //fflush(stderr);
      p = strtok(chmsg, " "); 
      if (strcmp(p, "Lines:") == 0) break;
   }
   fgets(chmsg, 8, infile);
/*
**   Process input message
*/
   while (1) {
      fgets(chtime, 128, infile);                 // Get time

      fgets(chmsg, 128, infile);                  // Skip a line
      fgets(chmsg, 128, infile);                  // Skip a line
      fgets(chmsg, 128, infile);                  // Skip a line

      fgets(chmsg, 128, infile);                  // Get msg ID line
      p = strtok(chmsg, "/");                     // Get 'MSGID'
      p = strtok(NULL, "/");                      // Get msg type (should be 'SALUTE')

      fgets(chmsg, 128, infile);                  // Get 1st data line
      p = strtok(chmsg, "/");                     // Get 'KSALUTE'
      p = strtok(NULL, "/");                      // Skip field
      p = strtok(NULL, "/");                      // Skip field
      p = strtok(NULL, "/");                      // Skip field
      p = strtok(NULL, "/");                      // Get number of following records
      nrecs = atoi(p);
      p = strtok(NULL, "/");                      // Get entity name
      strcpy(chunit, p);
      p = strtok(NULL, "/");                      // Get latitude
      strcpy(chlat, p);
      p = strtok(NULL, "/");                      // Get longitude
      strcpy(chlon, p);
      fprintf(lst, "S:   %d\n", nrecs);
      fprintf(lst, "A:   %s\n", "?");
      fprintf(lst, "L:   %s/%s\n", chlat, chlon);
      fprintf(lst, "U:   %s [%d]\n", chunit, nrecs);
      fprintf(lst, "T:   %s", chtime);
      //
      for (i=0; i<nrecs; i++) {
	fgets(chmsg, 128, infile);                // Get next sub-entity line
	p = strtok(chmsg, "/");                   // Get 'KSALUTE'
	p = strtok(NULL, "/");                    // Get subentity name
	strcpy(chentity, p);
	fprintf(lst, "E:   %s\n", chentity);
      }
      break;
   }

   sprintf(chmsg, "%s %d %s %f %f %s", "SMTP GPDA DRE 105 0",
	   48, chunit, 0.54, 0.6, chtime);

   Net_write(chmsg, sizeof(chmsg));

   Net_close();
}


