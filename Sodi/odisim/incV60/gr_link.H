// gr_link.H header file

#ifndef gr_link_object
#define gr_link_object

#include "gr_event.H"
#include "speedes_object.H"
#include "link.H"

/************************************************************************
*	gr_link - add a link to the speedes state			*
************************************************************************/
class C_GR_LINK : public C_GR_EVENT {

  private:

    int sensor_id;		// sensor unique id
    int track_id;		// track unique id
    int link_flag;		// link flag (1 = link, 0 = unlink)

    C_LINK *unlink;		// pointer to unlink an object
    C_LINK link;		// object for describing a link

  public:

    C_GR_LINK() {;}
    void init(int sid, int tid, int color, double in, int flag);

    virtual void process();
    virtual void unprocess();

};

#endif

