// remove_tracks.H header file

#ifndef remove_tracks_object
#define remove_tracks_object

#include "event.H"
#include "remove_tracks_mess.H"

/************************************************************************
*	remove_tracks object 						*
************************************************************************/
class C_REMOVE_TRACKS : public C_EVENT {

  private:
    int remove_tracks_on;

  protected:

  public:

    C_REMOVE_TRACKS();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy() {return 1;}

};


#endif
