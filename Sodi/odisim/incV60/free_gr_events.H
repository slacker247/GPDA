// free_gr_events.H header file

#ifndef free_gr_events_object
#define free_gr_events_object

#include "free_list.H"

enum {
  GR_EVENT,
  GR_ADD_EOM,
  GR_DEL_EOM,
  GR_LINK,
  GR_KILL,
  N_GR_EVENTS
};

/************************************************************************
*	free_gr_events object 						*
************************************************************************/
class C_FREE_GR_EVENTS : public C_FREE_LIST {

  private:

  protected:

  public:

    C_FREE_GR_EVENTS();

};


#endif
