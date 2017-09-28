// fuse_tracks.H header file

#ifndef fuse_tracks_object
#define fuse_tracks_object

#include "event.H"
#include "fuse_tracks_mess.H"

/************************************************************************
*	fuse_tracks object 						*
************************************************************************/
class C_FUSE_TRACKS : public C_EVENT {

  private:

    FUSE_TRACKS_MESS *fuse_tracks_mess;
    int remove_tracks_on;

  public:

    C_FUSE_TRACKS();
    virtual void init(C_HEADER *);
    virtual void exchange();
    virtual void temp_process();
    virtual void perm_process();
    virtual void cleanup();
    virtual int lazy() {return 1;}

};


#endif
