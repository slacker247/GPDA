#include <math.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>
#include "GR_Socket.H"

GR_Socket::GR_Socket(int SOCKPORT, long type)
{

  /* Create socket */
  const int sock = socket(AF_INET, SOCK_STREAM, 0);
  if (sock < 0) {
    perror("opening stream socket");
    exit(1);
  }

  p_sock = sock;
  p_sockport = SOCKPORT;

  struct sockaddr_in server;
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port = htons(SOCKPORT);
  if (bind(sock, (sockaddr*) &server, sizeof(server))) {
    perror("binding stream socket");
    exit(1);
  }
}

long
GR_Socket::SocketIn(char *buf)
{
int rval;
  
  /* Start accepting connections */
  listen(p_sock, 5);
  for(;;)
  {
    cout << "Waiting for SPEEDES to connect..." << endl;
    const int msgsock = accept(p_sock, 0, 0);
    if (msgsock == -1)
    {
      perror("accept");
      exit(1);
    }
  
    for(;;) {
      rval = read(msgsock, buf, 1024);
      if (rval < 0)
      {
	perror("reading stream message");
	exit(1);
      }
      if (rval == 0)
      {
	cout << "SPEEDES has exited." << endl;
	break;
      }
      write(STDOUT_FILENO, buf, rval);
      fflush(stdout);
    }
  }
  return (long)rval;
}

void
GR_Socket::SocketOut(char *buf, int bufsize)
{
    if (write(p_sock, buf, bufsize) < 0)
    {
      perror("writing on stream socket");
      exit(1);
    }
}

GR_Socket::~GR_Socket()
{
  close(p_sock);
}
