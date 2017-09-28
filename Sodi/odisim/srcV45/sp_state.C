// speedes_state.C method file

#include <stdio.h>
#include "speedes_state.H"

/************************************************************************
* remove_link : remove a link from the state				*
************************************************************************/
C_LINK *C_SPEEDES_STATE::remove_link(int sensor_id, int track_id) {
  int len;
  int i;
  C_LINK *link;

  link = (C_LINK *)links.get_top();
  len = links.get_length();

  for (i=0; i<len; i++) {
    if (link->get_sensor_id() == sensor_id) {
      if (link->get_track_id() == track_id) {
        links.deque(link);
        return link;
      }
    }
    link = (C_LINK *)link->get_link();
  }

  return NULL;

}

