// graphobj.H header file

#ifndef graphobj_object
#define graphobj_object

#include "simobj.H"

/************************************************************************
*	graphobj object - graphics object for rsd			*
************************************************************************/
class C_GRAPHOBJ : public C_SIMOBJ {

  private:

    int blocking;		// blocking flag for initialization
    int socket;			// socket id for rsd

  protected:

  public:

    C_GRAPHOBJ();

    void set_blocking() {blocking = 1;}
    void reset_blocking() {blocking = 0;}
    int get_blocking() {return blocking;}
    void exchange_blocking(int &b) {EXCHANGE(blocking,b);}

    void exchange_socket(int &s) {EXCHANGE(socket,s);}
    int get_socket() {return socket;}

};


#endif
