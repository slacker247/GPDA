// base_scan.H header file

#ifndef bascan_object
#define bascan_object

#include "event.H"
#include "xqueue.H"
#include "ext_graphics_link_output.H"

/************************************************************************
*	base_scan object 						*
************************************************************************/
class C_BASE_SCAN : public C_EVENT {

  private:

  protected:

    int scanning;
    int seed;

    double last_tcom;
    C_XQUEUE *new_tracks;

    EXT_GRAPHICS_LINK_OUTPUT *ext_graphics_link_output;
    int size_link_output;

  public:

    C_BASE_SCAN();
    void graphics(C_XQUEUE *old_tks, C_XQUEUE *new_tks);

};


#endif
