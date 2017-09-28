// eoman.C method file

#include <stdio.h>
#include <math.h>
#include "eoman.H"
#include "eom_holder.H"
#include "eom.H"
#include "def.h"

#include "freeobjs.H"
#include "gridid.H"

extern C_FREEOBJS freeobjs;

/************************************************************************
* C_EOMAN : construct a eoman object					*
************************************************************************/
C_EOMAN::C_EOMAN() {

  eoms = new C_XHASH(1000);

}

/************************************************************************
* check_eomtimes : check if the eoms are current			*
************************************************************************/
int C_EOMAN::check_eomtimes(double time) {
  C_XQUEUE *qall;
  int i,j,len;
  int nbad, ndup;
  C_EOM_HOLDER *eom_holder;
  C_EOM_HOLDER *eom_holder1;
  C_EOM *eom;
  int ud;

  nbad = 0;
  ndup = 0;

  eoms->combine();
  qall = eoms->get_all();

  len = qall->get_length();
  eom_holder = (C_EOM_HOLDER *)qall->get_top();
  for (i=0; i<len; i++) {

//...... check if bad endtimes

    eom = eom_holder->get_eom();
    if (eom->get_endtime() < time) {
      nbad++;
    }

//...... check for duplicates

    ud = eom_holder->get_id();
    eom_holder1 = (C_EOM_HOLDER *)eom_holder->get_link();
    for (j=i+1; j<len; j++) {
      if (ud == eom_holder1->get_id()) {
	ndup++;
      }
      eom_holder1 = (C_EOM_HOLDER *)eom_holder1->get_link();
    }

//...... continue loop

    eom_holder = (C_EOM_HOLDER *)eom_holder->get_link();

  }

  if (nbad) {
    fprintf(stderr,"Error (EOMAN) nbad = %d\n",nbad);
  }

  if (ndup) {
    fprintf(stderr,"Error (EOMAN) ndup = %d\n",ndup);
  }

  return (nbad + ndup);

}

