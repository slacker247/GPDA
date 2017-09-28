/************************************************************************

*************************************************************************/

#ifndef GR_SOCKET
#define GR_SOCKET

#include "GR_Interface.H"

class GR_Socket

{
 private:
   long p_sock;
   int  p_sockport;
   
 public:
   GR_Socket (int sockport, long type);
   long SocketIn(char *buf);
   void SocketOut(char *buf, int bufsize);
   ~GR_Socket();
};

#endif
