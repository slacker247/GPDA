#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stream.h>

#define TRUE 1

main()
{
  /* Create socket */
  const int sock = socket(AF_INET, SOCK_STREAM, 0);
  if (sock < 0) {
    perror("opening stream socket");
    exit(1);
  }

  struct sockaddr_in server;
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port = htons(1029);
  if (bind(sock, (sockaddr*) &server, sizeof(server))) {
    perror("binding stream socket");
    exit(1);
  }

  char buf[1024];

  /* Start accepting connections */
  listen(sock, 5);
  for(;;)
  {
    cout << "Waiting for SPEEDES to connect..." << endl;
    const int msgsock = accept(sock, 0, 0);
    if (msgsock == -1)
    {
      perror("accept");
      exit(1);
    }
  
    for(;;) {
      const int rval = read(msgsock, buf, 1024);
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
}
