#include <stdio.h>
#include "speedes_graphics.H"

main() {
  C_SPEEDES_GRAPHICS *speedes_graphics;
  C_SPEEDES_STATE *speedes_state;
  C_QUEUE *objects;
  C_QUEUE *links;
  C_SPEEDES_OBJECT *object;
  C_LINK *link;
  int sid, tid;
  double gvt;
  double time;
  double X[3];
  double V[3];
  int i,j;
  int len;
  int nbad;
  int uids[10000];

  speedes_graphics = new C_SPEEDES_GRAPHICS();
  speedes_state = speedes_graphics->get_speedes_state();
  gvt = speedes_graphics->get_gvt();
  time = gvt;

  while (speedes_graphics->simulating()) {

    speedes_graphics->messages();
    speedes_graphics->process();

    if (gvt != speedes_graphics->get_gvt()) {

      gvt = speedes_graphics->get_gvt();
      time = gvt;

      objects = speedes_state->get_objects();
      links = speedes_state->get_links();

      printf("SPEEDES Graphics gvt = %f, objects = %d, links = %d\n",
	gvt, objects->get_length(), links->get_length());

//...... get position and velocity for all the objects

      nbad = 0;
      len = objects->get_length();
      object = (C_SPEEDES_OBJECT *)objects->get_top();
      for (i=0; i<len; i++) {
        object->get_pos_vel(gvt,X,V);
	if (object->get_icon() == -1) {
//	  printf("Error, bad icon -1\n");
	  nbad++;
	}

	uids[i] = object->get_unique_id();
	for (j=0; j<i; j++) {
	  if (uids[j] == uids[i]) {
	    printf("Error, multiple ids are the same %d\n",uids[i]);
	  }
	}

        object = (C_SPEEDES_OBJECT *)object->get_link();
      }
      if (nbad > 0) printf("%d bad icons at time %f\n",nbad,gvt);

//...... check links

      len = links->get_length();
      link = (C_LINK *)links->get_top();
      for (i=0; i<len; i++) {
	sid = link->get_sensor_id();
	tid = link->get_track_id();
	object = speedes_state->get_object(sid);
	if (object == NULL) {
	  printf("Error, bad sensor link %d (%d)\n",sid,tid);
	}
	object = speedes_state->get_object(tid);
	if (object == NULL) {
	  printf("Error, bad track link %d (%d)\n",tid,sid);
	}

        link = (C_LINK *)link->get_link();
      }

    }

//...... check that the state is reversible

    for (time = gvt; time > -1000.0; time -= 100.0) {
      speedes_graphics->process(time);
    }
    speedes_graphics->process();

  }

  speedes_graphics->process();
  speedes_graphics->process(-1.0);
  speedes_graphics->process();

//...... user requests

  int itime = 0;
  while (itime != -1) {

    fprintf(stderr,"\nEnter time (-1 to quit) : ");
    fflush(stderr);
    scanf("%d",&itime);

    speedes_graphics->process(double(itime));

    objects = speedes_state->get_objects();
    links = speedes_state->get_links();

    printf("SPEEDES Graphics time = %f, objects = %d, links = %d\n",
	double(itime), objects->get_length(), links->get_length());

  }

  return 1;

}

