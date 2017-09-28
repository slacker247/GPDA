#ifdef CROS

#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <sys/file.h>
#include <signal.h> 
#include <netinet/tcp.h>

#if defined(SUN3) || defined(SUN4) 
#include <sys/socket.h> 
#include <netdb.h>
#include <netinet/in.h>
#endif

#ifdef SGI
#include <sys/types.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#ifdef KSR1
#include <sys/types.h>
#include <sys/socket.h> 
#include <netdb.h>
#include <netinet/in.h>
#endif

#define TRUE 1


struct hostent *get_host_by_name(name)
char *name;
{
  return gethostbyname(name);
}

/************************************************************************
*	Darcy's wonder _set_nodelay_ socket calls			*
************************************************************************/
int _set_nodelay_(sockno)
  int sockno;
{
  int flag;

  flag = 1;
  if (setsockopt(	sockno,
			IPPROTO_TCP,
			TCP_NODELAY,
			(char *)&flag, sizeof(int)) < 0) {
    perror("_set_nodelay_");
    return -1;
  }
/*
  if (setsockopt(	sockno,
			SOL_SOCKET,
			SO_USELOOPBACK,
			(char *)&flag, sizeof(int)) < 0) {
    perror("_set_nodelay_");
    return -1;
  }
*/
  return 0;

}


/************************************************************************
*	blocking write							*
************************************************************************/
int socket_write(sock,buff,bytes)
int sock;
char *buff;
int bytes;
{
  int start;
  int status;

  start = 0;

/*
  printf("trying to write %d bytes to sock %d\n",bytes,sock);
*/

  while (start < bytes) {

    status = write(sock, &buff[start], bytes-start);

    if (status < 0) {
/*
      perror("writing on stream socket");
      exit(1);
*/
    }else{
      start += status;
    }

  }

/*
  printf("did it !\n");
*/

}


/************************************************************************
*	non-blocking write						*
************************************************************************/
int nb_socket_write(sock,buff,bytes)
int sock;
char *buff;
int bytes;
{
  int status;

  status = write(sock, buff, bytes);

  if (status < 0) {
    if (errno == EWOULDBLOCK) status = 0;
  }

  if (status < 0) {
/*
    perror("writing on stream socket");
    exit(1);
*/
  }

  return status;

}


/************************************************************************
*	blocking read							*
************************************************************************/
int socket_read(sock,buff,bytes)
int sock;
char *buff;
int bytes;
{
  int start;
  int status;

  start = 0;

/*
  printf("trying to read %d bytes from sock %d\n",bytes,sock);
*/

  while (start < bytes) {

    status = read(sock, &buff[start], bytes-start);

    if (status < 0) {
/*
      perror("reading on stream socket");
      exit(1);
*/
    }else{
      start += status;
    }

  }

/*
  printf("did it !\n");
*/
  return start;

}

/************************************************************************
*	non-blocking read						*
************************************************************************/
int nb_socket_read(sock,buff,bytes)
int sock;
char *buff;
int bytes;
{
  int status;

  status = read(sock, buff, bytes);

  if (status < 0) {
    if (errno == EWOULDBLOCK) status = 0;
  }

  if (status < 0) {
/*
    perror("reading on stream socket");
    exit(1);
*/
  }

  return status;

}


/************************************************************************
*	set a socket to be non-blocking					*
************************************************************************/
int set_nonblocking(p_socket)
  int p_socket;
{
        int fcntlarg;
        if ((fcntlarg = fcntl(p_socket, F_GETFL, 0)) < 0) {
/*
                perror("fcntl");
                exit(1);
*/
        }
        fcntlarg |= O_NDELAY;
        if (fcntl(p_socket, F_SETFL, fcntlarg) < 0) {
/*
                perror("fcntl");
                exit(1);
*/
        }

}

/************************************************************************
*	set a socket to be blocking					*
************************************************************************/
int set_blocking(p_socket)
  int p_socket;
{
        int fcntlarg;
        if ((fcntlarg = fcntl(p_socket, F_GETFL, 0)) < 0) {
/*
                perror("fcntl");
                exit(1);
*/
        }
        fcntlarg &= ~O_NDELAY;
        if (fcntl(p_socket, F_SETFL, fcntlarg) < 0) {
/*
                perror("fcntl");
                exit(1);
*/
        }
}

#endif
