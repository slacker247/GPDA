// kill.H header file

#ifndef kill_object
#define kill_object

#include "event.H"
#include "kill_mess.H"

/************************************************************************
*	kill object 							*
************************************************************************/
class C_KILL : public C_EVENT {

  private:

    double kill_position[3];
    double bomb_size;
    int alive;
    int alive_flag;
    int sd;

    static int done;
    static int KILL;
    static int GBI;
    static int GRAPHICS;

  protected:

  public:

    C_KILL();
    virtual void init(C_HEADER *);
    virtual void temp_process();
    virtual void exchange();
    virtual void perm_process();
    virtual void cleanup();

};


#endif
