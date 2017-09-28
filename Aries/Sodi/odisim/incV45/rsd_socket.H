// rsd_socket.H header file

#ifndef rsd_socket_object
#define rsd_socket_object

#include "event.H"
#include "rsd_socket_mess.H"

/************************************************************************
*	rsd_socket object 						*
************************************************************************/
class C_RSD_SOCKET : public C_EVENT {

  private:

    static int done;
    static int RSD_SOCKET;

    int socket;

  protected:

  public:

    C_RSD_SOCKET();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
