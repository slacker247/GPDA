// missile.C method file

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <memory.h>
#include <time.h>
#include "missile.H"
#include "gbi_item.H"

#ifndef MISSALONE
#include "freeobjs.H"
extern C_FREEOBJS freeobjs;
#endif

#define RE      6378.145  
#define RE2     40680733.64  
#define OMEGAE  7.2921158279144062e-5
//#define OMEGAE  0.0
#ifndef PI
#define PI 3.141592654
#endif
#define RADDEG  0.0174532925199432958
#define VSCALE  0.1

#define PRINTOUT
//#define FASTRANGE

#ifndef MISSALONE
  C_RBQ *C_MISSILE::rbq = NULL;
#else
  void *C_MISSILE::rbq = NULL;
#endif

enum {
  ANALYTIC,
  INTEGRATE
};

/************************************************************************
* C_MISSILE : construct a missile object				*
************************************************************************/
C_MISSILE::C_MISSILE() {

  the1 = -1;
  the2 = -1;
  the3 = -1;

  err1 = -1;
  err2 = -1;
  err3 = -1;

  ntries = 0;
  ECI = 1;

}

/************************************************************************
* create_stages : construct the stages					*
************************************************************************/
void C_MISSILE::create_stages(int n) {
  int i;

  nstages = n;
  stage_type = ANALYTIC;

  eom = new C_EOM *[nstages+1];

  for (i=0; i<nstages; i++) {
    if (rbq == NULL) {

#ifdef MISSALONE
      eom[i] = new C_STAGE();
#else
      eom[i] = (C_STAGE *)freeobjs.new_object(STAGE);
#endif

      if (eom[i] == NULL) {
        fprintf(stderr,"Error (MISSILE), could not create stage\n");
	exit(1);
      }
    }else{

#ifdef MISSALONE
      eom[i] = new C_STAGE();
#else
//      eom[i] = (C_STAGE *)RB_FREE_NEW(STAGE);
      eom[i] = (C_STAGE *)freeobjs.new_object(STAGE);
#endif

    }
    eom[i]->set_ECI(ECI);
  }

  if (rbq == NULL) {

#ifdef MISSALONE
    eom[nstages] = new C_BUS();
#else
    eom[nstages] = (C_BUS *)freeobjs.new_object(BUS);
#endif

    if (eom[nstages] == NULL) {
      fprintf(stderr,"Error (MISSILE), could not create stage\n");
      exit(1);
    }
  }else{

#ifdef MISSALONE
    eom[nstages] = new C_BUS();
#else
//    eom[nstages] = (C_BUS *)RB_FREE_NEW(BUS);
    eom[nstages] = (C_BUS *)freeobjs.new_object(BUS);
#endif

  }

  eom[nstages]->set_ECI(ECI);

}

/************************************************************************
* create_int_stages : construct the integrated stages			*
************************************************************************/
void C_MISSILE::create_int_stages(int n) {
  int i;

  nstages = n;
  stage_type = INTEGRATE;

  eom = new C_EOM *[nstages+1];

  for (i=0; i<nstages; i++) {
    if (rbq == NULL) {

#ifdef MISSALONE
      eom[i] = new C_INT_STAGE();
#else
      eom[i] = (C_INT_STAGE *)freeobjs.new_object(INT_STAGE);
#endif

      if (eom[i] == NULL) {
        fprintf(stderr,"Error (MISSILE), could not create integrated stage\n");
	exit(1);
      }
    }else{

#ifdef MISSALONE
      eom[i] = new C_INT_STAGE();
#else
//      eom[i] = (C_INT_STAGE *)RB_FREE_NEW(INT_STAGE);
      eom[i] = (C_INT_STAGE *)freeobjs.new_object(INT_STAGE);
#endif

    }
    eom[i]->set_ECI(ECI);
  }

  if (rbq == NULL) {

#ifdef MISSALONE
    eom[nstages] = new C_INT_BUS();
#else
    eom[nstages] = (C_INT_BUS *)freeobjs.new_object(INT_BUS);
#endif

    if (eom[nstages] == NULL) {
      fprintf(stderr,"Error (MISSILE), could not create stage\n");
      exit(1);
    }
  }else{

#ifdef MISSALONE
    eom[nstages] = new C_INT_BUS();
#else
//    eom[nstages] = (C_INT_BUS *)RB_FREE_NEW(INT_BUS);
    eom[nstages] = (C_INT_BUS *)freeobjs.new_object(INT_BUS);
#endif

  }

  eom[nstages]->set_ECI(ECI);

}



/************************************************************************
* fill_ECI : fill the stages and the bus with the ECI flag		*
************************************************************************/
void C_MISSILE::fill_ECI() {
  int i;

  for (i=0; i<=nstages; i++) {
    eom[i]->set_ECI(ECI);
  }

}

/************************************************************************
* create_rvs : construct the rvs on the bus				*
************************************************************************/
void C_MISSILE::create_rvs(int n) {
  C_BASE_SPACE *bus;

  bus = get_bus();
  bus->create_rvs(n);

}

/************************************************************************
* aim_latlon : aim the missile in lat lon coordinates			*
************************************************************************/
int C_MISSILE::aim_latlon(double t, double lati, double loni, double latf,
				double lonf) {
  double ri[3];
  double rf[3];
  double re_cos_lat;

  re_cos_lat = RE * cos(lati);
  ri[0] = re_cos_lat * cos(loni);
  ri[1] = re_cos_lat * sin(loni);
  ri[2] = RE * sin(lati);

  re_cos_lat = RE * cos(latf);
  rf[0] = re_cos_lat * cos(lonf);
  rf[1] = re_cos_lat * sin(lonf);
  rf[2] = RE * sin(latf);

  return aim_eci(t, ri, rf);

}


/************************************************************************
* set_aim_angles : based on theta and rimp, set up the missile		*
************************************************************************/
double C_MISSILE::set_aim_angles(double angle, double &phi, double vscale,
				 double r0[3], double rimp[3]) {
  int i;
  double rt[3];
  double vt[3];
  double a0[3];
  double a1[3];
  double a2[3];
  double a1phi[3];
  double nf[3];
  double ab[3];
  double t_tot;
  double a_tot;
  double c,s;
  double cph, sph;
  double /*c0,*/c1,c2;
  double temp;
  double timp_temp;
  double theta;
  double tkep;
  double theta1;
  double tnext;
  double *newrimp;
  C_BASE_STAGE *stage;
  C_BASE_SPACE *bus;

//...... set up the initial vectors

  temp = sqrt(r0[0]*r0[0]+r0[1]*r0[1]+r0[2]*r0[2]);

  a0[0] = r0[0]/temp;
  a0[1] = r0[1]/temp;
  a0[2] = r0[2]/temp;

  temp = sqrt(rimp[0]*rimp[0]+rimp[1]*rimp[1]+rimp[2]*rimp[2]);

  nf[0] = rimp[0]/temp;
  nf[1] = rimp[1]/temp;
  nf[2] = rimp[2]/temp;

  c = a0[0]*nf[0] + a0[1]*nf[1] + a0[2]*nf[2];
  temp = sqrt(1.0-c*c);

  a1[0] = (nf[0] - c*a0[0]) / temp;
  a1[1] = (nf[1] - c*a0[1]) / temp;
  a1[2] = (nf[2] - c*a0[2]) / temp;

  if (fabs(a1[0]*a1[0]+a1[1]*a1[1]+a1[2]*a1[2] - 1.0) > 0.000001) {
    fprintf(stderr,"Error, (MISSILE) bad a1 vector\n");
  }

  if (fabs(a0[0]*a1[0]+a0[1]*a1[1]+a0[2]*a1[2]) > 0.000001) {
    fprintf(stderr,"Error, (MISSILE) bad a1 vector\n");
  }

  a2[0] = a0[1]*a1[2] - a0[2]*a1[1];
  a2[1] = a0[2]*a1[0] - a0[0]*a1[2];
  a2[2] = a0[0]*a1[1] - a0[1]*a1[0];

  if (fabs(a2[0]*a2[0]+a2[1]*a2[1]+a2[2]*a2[2] - 1.0) > 0.000001) {
    fprintf(stderr,"Error, (MISSILE) bad a2 vector\n");
  }

  if (fabs(a0[0]*a2[0]+a0[1]*a2[1]+a0[2]*a2[2]) > 0.000001) {
    fprintf(stderr,"Error, (MISSILE) bad a2 vector\n");
  }

  if (fabs(a1[0]*a2[0]+a1[1]*a2[1]+a1[2]*a2[2]) > 0.000001) {
    fprintf(stderr,"Error, (MISSILE) bad a2 vector\n");
  }

//...... rotate by the amount phi

//  fprintf(stderr,"Phi = %f\n",phi);

  cph = cos(phi);
  sph = sin(phi);

  a1phi[0] = cph*a1[0] + sph*a2[0];
  a1phi[1] = cph*a1[1] + sph*a2[1];
  a1phi[2] = cph*a1[2] + sph*a2[2];

//...... set up the initial position and velocity


  rt[0] = r0[0];
  rt[1] = r0[1];
  rt[2] = r0[2];

  vt[0] = -OMEGAE * r0[1] * vscale;
  vt[1] = OMEGAE * r0[0] * vscale;
  vt[2] = 0.0;

//#define ZV
#ifdef ZV
  vt[0] = 0.0;
  vt[1] = 0.0;
  vt[2] = 0.0;
#endif

  t_tot = 0.0;
  a_tot = 0.0;

  for (i=0; i<nstages; i++) {
    stage = get_stage(i);
    if (stage->get_variable_theta()) {
      t_tot += stage->get_tburn();
    }else{
      a_tot += stage->get_theta();
    }
  }

//...... initialize the first angle

  ab[0] = a0[0];
  ab[1] = a0[1];
  ab[2] = a0[2];

//...... start the loop to find the range and time vs angle

  theta = 0.0;
//  tkep = 0.0;
  tkep = eom[0]->get_start_time();

//...... loop over stages

  for (i=0; i<nstages; i++) {

    stage = get_stage(i);

    stage->init_R0(rt);
    stage->init_V0(vt);
    stage->init_A0(ab);

    if (stage->get_variable_theta()) {
      theta1 = angle * stage->get_tburn() / t_tot;
      stage->set_theta(theta1);
      theta += theta1;
    }else{
      theta += stage->get_theta();
    }

    c = cos(theta);
    s = sin(theta);

    ab[0] = c*a0[0] + s*a1phi[0];
    ab[1] = c*a0[1] + s*a1phi[1];
    ab[2] = c*a0[2] + s*a1phi[2];

    stage->init_AB(ab);

    stage->init_values();
    tnext = stage->get_duration();
    tkep += tnext;
//    stage->get_pos_vel(tnext,rt,vt);
    stage->get_pos_vel(tkep,rt,vt);

  }

  if (fabs(theta-angle-a_tot) > 0.000001) {
    fprintf(stderr,"Error, (MISSILE) didnt do the angle right\n");
  }

//...... check that bus is above the surface of the earth

  temp = sqrt(rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2]);
  if (temp < RE) {
//    fprintf(stderr,"Bad aim parameters, booster crashing\n");
    return -1.0;
  }

//...... now work on the bus

  bus = get_bus();
  bus->init(rt,vt);
  bus->update_impact();
  timp_temp = bus->get_timp();
//  tkep += timp_temp;
  tkep = timp_temp;
  newrimp = bus->get_imp_pos();

//...... work on new phi

  //c0 = newrimp[0]*a0[0] + newrimp[1]*a0[1]+newrimp[2]*a0[2];
  c1 = newrimp[0]*a1phi[0] + newrimp[1]*a1phi[1]+newrimp[2]*a1phi[2];
  c2 = newrimp[0]*a2[0] + newrimp[1]*a2[1]+newrimp[2]*a2[2];
  temp = atan2(c2,c1);
  phi -= temp;

  return tkep - eom[0]->get_start_time();

}

/************************************************************************
* aim_eci : aim the missile in eci coordinates				*
************************************************************************/
int C_MISSILE::aim_eci(double t, double r0[3], double r1[3]) {
  int i;
  int nits;
  //int flag;
  double d0,d1,d2,d;
  double theta, phi;
  double frac;
  double cwt, swt;
  double rimp[3];
  double newrimp[3];
  double *imp;
  double newtimp;
  double newtimp1;
  double error;
  double error_timp;
  double error_timp1;
  double true_dist;
  //double new_dist;
  double vscale;
  C_BASE_SPACE *bus;
  double old_error;
  double dtheta;

//...... set up the start time for the missile

  init_t0(t);
  set_ECI();

//...... find the range

  d0 = r1[0]-r0[0];
  d1 = r1[1]-r0[1];
  d2 = r1[2]-r0[2];
  d = sqrt(d0*d0+d1*d1+d2*d2);
  true_dist = d;

//  fprintf(stderr,"Aiming distance = %f\n",d);

//...... find the initial theta value

  phi = 0.0;
  theta = -1;

  for (i=min_index; i>0; i--) {

    if (ra[i] == -1) break;
    if (ra[i-1] == -1) break;

    if ((ra[i] <= d) && (d <= ra[i-1])) {
      frac = (d - ra[i]) / (ra[i-1] - ra[i]);
      theta = (i - frac) * RADDEG;
      timp = (1.0 - frac) * ta[i] + frac * ta[i-1];
      break;
    }

    if ((ra[i-1] <= d) && (d <= ra[i])) {
      frac = (d - ra[i-1]) / (ra[i] - ra[i-1]);
      theta = (i - 1 + frac) * RADDEG;
      timp = (1.0 - frac) * ta[i-1] + frac * ta[i];
      break;
    }

  }

  if (theta == -1) {
//    fprintf(stderr,"Error, (MISSILE) could not aim: target out of range\n");
    return 0;
  }

//...... get close enough first

  newtimp = timp;
  //flag = -1;
  old_error = -1.0;
  dtheta = -5.0*RADDEG;
  nits = 0;
  vscale = 1.0;

  while (1) {

    nits++;

    error_timp1 = 1.0e20;
    while (fabs(error_timp1) > 10.0) {
      cwt = cos(OMEGAE*newtimp);
      swt = sin(OMEGAE*newtimp);
      rimp[0] = cwt*r1[0] - swt*r1[1];
      rimp[1] = swt*r1[0] + cwt*r1[1];
      rimp[2] = r1[2];
      while (1) {
        phi = 0.0;
        newtimp1 = set_aim_angles(theta, phi, vscale, r0, rimp);
	if (newtimp1 != -1.0) break;
	theta -= RADDEG;
      }
      error_timp1 = newtimp1 - newtimp;
      newtimp = newtimp1;
    }

    bus = get_bus();
    imp = bus->get_imp_pos();
    rimp[0] = imp[0];
    rimp[1] = imp[1];
    rimp[2] = imp[2];

    cwt = cos(OMEGAE*newtimp);
    swt = sin(OMEGAE*newtimp);

    newrimp[0] = cwt*r1[0] - swt*r1[1];
    newrimp[1] = swt*r1[0] + cwt*r1[1];
    newrimp[2] = r1[2];

    d0 = rimp[0] - newrimp[0];
    d1 = rimp[1] - newrimp[1];
    d2 = rimp[2] - newrimp[2];

    error = sqrt(d0*d0+d1*d1+d2*d2);
    if (error < 2000.0) {
      if (vscale == 1.0) break;
      vscale += VSCALE;
      if (vscale > 1.0) vscale = 1.0;
    }

    if (nits > 10) break;

    if (old_error != -1.0) {
      if (error > old_error) {
	dtheta = -dtheta;
	theta += dtheta;
      }
    }
    old_error = error;
    theta += dtheta;
/*
    fprintf(stderr,"First Pass %d, Theta = %f, error = %f, vscale = %f\n",
	nits, theta/RADDEG, error, vscale);
*/
  }  

  timp = newtimp;


//...... calculate the initial ECI impact position (earth rotates);

  error = 1.0e20;
  error_timp = 1.0e20;
  ntries = 0;
  vscale = 1.0;

  while ( ((error > 0.001) || (fabs(error_timp) > 0.001))
	|| (vscale != 1.0) ) {

//...... update vscale

    if (nits > 5) {
      vscale += VSCALE;
      if (vscale > 1.0) vscale = 1.0;
    }

//...... get timp consistent

    error_timp1 = 1.0e20;
    newtimp = timp;
    while (fabs(error_timp1) > 10.0) {
      cwt = cos(OMEGAE*newtimp);
      swt = sin(OMEGAE*newtimp);
      rimp[0] = cwt*r1[0] - swt*r1[1];
      rimp[1] = swt*r1[0] + cwt*r1[1];
      rimp[2] = r1[2];
      newtimp1 = set_aim_angles(theta, phi, vscale, r0, rimp);
      error_timp1 = newtimp1 - newtimp;
      newtimp = newtimp1;
    }

    bus = get_bus();
    imp = bus->get_imp_pos();
    rimp[0] = imp[0];
    rimp[1] = imp[1];
    rimp[2] = imp[2];

    if (fabs(sqrt(rimp[0]*rimp[0]+rimp[1]*rimp[1]+rimp[2]*rimp[2]) - RE)
	> 0.001) {
      fprintf(stderr,
	"Error (MISSILE) impact not on earth (%f) at iteration %d\n",
	fabs(sqrt(rimp[0]*rimp[0]+rimp[1]*rimp[1]+rimp[2]*rimp[2]) - RE),
	nits);
    }

//...... check to see how well we did

    cwt = cos(OMEGAE*newtimp);
    swt = sin(OMEGAE*newtimp);

    newrimp[0] = cwt*r1[0] - swt*r1[1];
    newrimp[1] = swt*r1[0] + cwt*r1[1];
    newrimp[2] = r1[2];

    d0 = rimp[0] - newrimp[0];
    d1 = rimp[1] - newrimp[1];
    d2 = rimp[2] - newrimp[2];
    error = sqrt(d0*d0+d1*d1+d2*d2);
    error_timp = newtimp - timp;

/*
    fprintf(stderr,"Distance = %f, theta = %f, phi = %f, delta timp = %f\n",
	error, theta/RADDEG, phi/RADDEG, error_timp);
*/

//...... set up the next estimate for the parameters

    theta = next_theta(error,theta);

    d0 = rimp[0]-r0[0];
    d1 = rimp[1]-r0[1];
    d2 = rimp[2]-r0[2];
    d = sqrt(d0*d0+d1*d1+d2*d2);
    //new_dist = d;

    timp = newtimp;

    nits++;

    if (nits > 500) {
      fprintf(stderr,"Error (MISSILE) could not aim distance %f (%f - %f)\n",
	true_dist, min_range, max_range);
      return -1;
    }

  }

//...... set up the initial earth position

  set_rotate(t);
//  bus->check_spline6();

//  fprintf(stderr,"Aiming converged in %d iterations\n\n",nits);
  return nits;

}


/************************************************************************
* range : get the range of the missile					*
************************************************************************/
void C_MISSILE::range(double Range_Angle[180], double Time_Angle[180],
			int &done) {
  int i,j;
  double t_tot;
  double a_tot;
  double angle;
  double theta1;
  double tnext;
  double s,c,theta;
  double d0,d1,d2,d,/*dlast,*/tkep;
  double a0[3];
  double a1[3];
  double ab[3];
  double rt[3];
  double vt[3];
  double *pos;
  C_BASE_STAGE *stage;

#ifdef FASTRANGE
  C_BUS bus[1];
#else
  C_BASE_SPACE *bus;
#endif

//...... check if done flag is set

  max_range = 0.0;
  min_range = 1.0e20;
  if (done) {
    ra = Range_Angle;
    ta = Time_Angle;
    for (i=1; i<180; i++) {
      if (Range_Angle[i] != -1.0) {
        if (Range_Angle[i] > max_range) max_range = Range_Angle[i];
        if (Range_Angle[i] < min_range) min_range = Range_Angle[i];
        min_index = i;
      }else{
	break;
      }
    }
    return;
  }

  done = 1;

//...... initialize the range and time arrays

  for (i=0; i<180; i++) {
    Range_Angle[i] = -1.0;
    Time_Angle[i] = -1.0;
  }
  ra = Range_Angle;
  ta = Time_Angle;
  max_range = 0.0;
  min_range = 1.0e20;

//...... find out how much time is spent burning

  t_tot = 0.0;
  a_tot = 0.0;
  //dlast = 0.0;

  tnext = 0.0;
  for (i=0; i<nstages; i++) {
    stage = get_stage(i);
    stage->set_start_time(tnext);
    tnext += stage->get_duration();
    if (stage->get_variable_theta()) {
      t_tot += stage->get_tburn();
    }else{
      a_tot += stage->get_theta();
    }
  }
  eom[nstages]->set_start_time(tnext);

//...... loop over all the angles

  for (i=1; i<180; i++) {

    angle = a_tot / RADDEG + i;
    if (angle > 180) {
//      fprintf(stderr,"MISSILE Range: %f to %f\n",min_range,max_range);
      break;
    }

//...... initialize the position and velocity

    rt[0] = 0.0;
    rt[1] = 0.0;
    rt[2] = RE;

    vt[0] = 0.0;
    vt[1] = 0.0;
    vt[2] = 0.0;


//...... initialize the pointing vectors

    a0[0] = 0.0;
    a0[1] = 0.0;
    a0[2] = 1.0;

    a1[0] = 0.0;
    a1[1] = 1.0;
    a1[2] = 0.0;

//...... initialize the first angle

    ab[0] = a0[0];
    ab[1] = a0[1];
    ab[2] = a0[2];

//...... start the loop to find the range and time vs angle

    theta = 0.0;
    tkep = 0.0;

//...... loop over stages

    for (j=0; j<nstages; j++) {

      stage = get_stage(j);

      stage->init_R0(rt);
      stage->init_V0(vt);
      stage->init_A0(ab);

      if (stage->get_variable_theta()) {
        theta1 = (i * (stage->get_tburn() / t_tot)) * RADDEG;
        stage->set_theta(theta1);
        theta += theta1;
      }else{
        theta += stage->get_theta();
      }

      c = cos(theta);
      s = sin(theta);

      ab[0] = c*a0[0] + s*a1[0];
      ab[1] = c*a0[1] + s*a1[1];
      ab[2] = c*a0[2] + s*a1[2];

      stage->init_AB(ab);

      stage->init_values();
      tnext = stage->get_duration();
      tkep += tnext;
//      stage->get_pos_vel(tnext,rt,vt);
      stage->get_pos_vel(tkep,rt,vt);

    }

//...... now work on the bus

    d = rt[0]*rt[0]+rt[1]*rt[1]+rt[2]*rt[2];
    if (d > RE2) {

#ifndef FASTRANGE
      bus = get_bus();
#endif

      bus->init(rt,vt);
      bus->update_impact();

//      timp = tkep + bus->get_timp();

      timp = bus->get_timp();
      if (timp < 0.0) {
//        fprintf(stderr,"MISSILE Range: %f to %f\n",min_range,max_range);
        break;
      }

      pos = bus->get_imp_pos();
      d0 = pos[0];
      d1 = pos[1];
      d2 = pos[2]-RE;
      d = d0*d0 + d1*d1 + d2*d2;

//      if (d < dlast) break;
      //dlast = d;

      Range_Angle[i] = sqrt(d);
      Time_Angle[i] = timp;

      min_index = i;

      if (max_range < Range_Angle[i]) max_range = Range_Angle[i];
      if (min_range > Range_Angle[i]) min_range = Range_Angle[i];

/*
      fprintf(stderr,"angle %f, Range %f, Time %f\n",
	angle,Range_Angle[i],Time_Angle[i]);
*/

    }else{

//      fprintf(stderr,"MISSILE Range: %f to %f\n",min_range,max_range);
      break;

    }

  }

  fprintf(stderr,"MISSILE Range: %f to %f\n",min_range,max_range);

}

/************************************************************************
* down_range : fill the down range table for the missile		*
************************************************************************/
void C_MISSILE::down_range(C_QUEUE **&Down_Range, int Nr, int Na,
				double Malt, double res) {
  int i;
  int nitems;
  double time;
  double dt;
  double impact_time;
  double theta;
  double phi;
  double r0[3];
  double r1[3];
  double P[3];
  double V[3];
  double Pmag;
  double Vmag;
  double Range;
  double Alt;
  double d0,d1,d2;
  int Rindex, Aindex;
  C_GBI_ITEM *gbi_item;
  C_BASE_SPACE *bus;

//  printf("Down Range: Max Range = %f, Max Alt = %f\n",max_range,Malt);

  init_t0(0.0);
  nitems = 0;

//......create the down range table

  Down_Range = new C_QUEUE *[Nr];
  if (Down_Range == NULL) {
    printf("Error (Missile) could not create Down Range table\n");
  }

  for (i=0; i<Nr; i++) {
    Down_Range[i] = new C_QUEUE[Na];
    if (Down_Range[i] == NULL) {
      printf("Error (Missile) could not create Down Range table\n");
    }
  }

//...... set some variables for the missile object

  nr = Nr;
  na = Na;
  resolution = res;
  max_altitude = Malt;
  dr = Down_Range;

//...... initialize r0 and r1;

  r0[0] = 0.0;
  r0[1] = 0.0;
  r0[2] = RE;

  r1[0] = RE;
  r1[1] = 0.0;
  r1[2] = 0.0;

//...... start main loop over theta

  for (i=1; i<180; i++) {

    theta = i*RADDEG;
    phi = 0.0;

    impact_time = set_aim_angles(theta, phi, 0.0, r0, r1);
    if (impact_time == -1) {
      printf("Max Angle is %d degrees\n",i);
      break;
    }

//...... initialize some things

//    time = 10.0;
    bus = get_bus();
    time = bus->get_start_time() + 120.0;

    while (1) {

      if (time > impact_time) break;

      get_pos_vel(time,P,V);

      Pmag = sqrt(P[0]*P[0]+P[1]*P[1]+P[2]*P[2]);
      Vmag = sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]);

      Alt = Pmag - RE;

      d0 = P[0]-r0[0];
      d1 = P[1]-r0[1];
      d2 = P[2]-r0[2];
      Range = sqrt(d0*d0+d1*d1+d2*d2);

      if (Range > max_range) break;
      if (Alt < 0.0) break;
      if (Alt > Malt) break;
/*
      printf("Down Range: t=%f, theta=%d, Range=%f, Alt=%f\n",
	time,i,Range,Alt);
*/

      Rindex = int(Nr * Range / max_range);
      Aindex = int(Na * Alt / Malt);

      gbi_item = new C_GBI_ITEM();
      if (gbi_item == NULL) {
        fprintf(stderr,"Error, ran out of memory for gbi table\n");
	exit(1);
      }
      gbi_item->set_time(time);
      gbi_item->set_theta(i);
      gbi_item->set_alt(Alt);
      gbi_item->set_range(Range);

      Down_Range[Rindex][Aindex].insert(gbi_item);
      nitems ++;

//...... figure out the next time step

      dt = res / Vmag;
      if (dt > 10.0) dt = 10.0;
      time += dt;

    }

  } // end of loop over theta

  printf("Down Range: Number of items in table = %d\n",nitems);

}

/************************************************************************
* get_theta_gbi : get the theta for aiming a gbi			*
************************************************************************/
double C_MISSILE::get_theta_gbi(double r0[3], double r1[3], double &time) {
  C_GBI_ITEM *gbi_item;
  C_QUEUE q;
  double /*d,*/d0,d1,d2/*,dmin*/;
  double tmin;
  double range;
  double altitude;
  double Rmn, Rmx, Amn, Amx;
  int Rmn_index, Rmx_index, Amn_index, Amx_index;
  int Rindex;
  int Aindex;
  int i,len;
  double theta;
  double return_time;

//...... first get the range and altitude inforation

  d0 = r1[0]-r0[0];
  d1 = r1[1]-r0[1];
  d2 = r1[2]-r0[2];
  range = sqrt(d0*d0+d1*d1+d2*d2);
  altitude = sqrt(r1[0]*r1[0]+r1[1]*r1[1]+r1[2]*r1[2]) - RE;

//  fprintf (stderr,"THETA GBI> Range=%f, Altitude=%f, Time=%f\n",range,altitude,time);

//...... get a list of possible 

  Rmn = range - resolution;
  if (Rmn<0) Rmn = 0;
  Rmx = range + resolution;
  if (Rmx > max_range - 0.001) Rmx = max_range - 0.001;

  Amn = altitude - resolution;
  if (Amn < 0.0) Amn = 0.0;
  Amx = altitude+resolution;
  if (Amx > max_altitude - 0.001) Amx = max_altitude - 0.001;

  Rmn_index = int(nr * Rmn / max_range);
  Rmx_index = int(nr * Rmx / max_range);
  Amn_index = int(na * Amn / max_altitude);
  Amx_index = int(na * Amx / max_altitude);

  for (Rindex=Rmn_index; Rindex<=Rmx_index; Rindex++) {
    for (Aindex=Amn_index; Aindex<=Amx_index; Aindex++) {
      q.join(&dr[Rindex][Aindex]);
    }
  }

  //dmin = 1.0e20;
  tmin = 1.0e20;

  gbi_item = (C_GBI_ITEM *)q.get_top();
  len = q.get_length();
  if (len != 0) {
//    fprintf (stderr,"THETA GBI> length of down range table = %d\n",len);
  }

  for (i=0; i<len; i++) {

    if (gbi_item->get_time() < time) {
      if (time - gbi_item->get_time() < tmin) {
        tmin = time - gbi_item->get_time();
        theta = gbi_item->get_theta();
        return_time = gbi_item->get_time();
      }
    }
    gbi_item = (C_GBI_ITEM *)gbi_item->get_link();

/*
    d0 = gbi_item->get_range() - range;
    d1 = gbi_item->get_alt() - altitude;
    d = sqrt(d0*d0+d1*d1);

    if ((d < dmin) && (gbi_item->get_time() < time)) {
      dmin = d;
      theta = gbi_item->get_theta();
      return_time = gbi_item->get_time();
    }
*/

  }

//  if (dmin < 1.0e20) {
  if (tmin < 1.0e20) {
    time = return_time;
    return theta;
  }else{
    time = 0.0;
    return 0.0;
  }

}

/************************************************************************
* get_theta_gbi : get the theta for aiming a gbi			*
************************************************************************/
double C_MISSILE::get_theta_gbi(double latgbi, double longbi,
		C_EOM *eom_var, double launch_time, double &kill_time) {
  double theta;
  double vmag;
  double r0[3];
  double r1[3];
  double v1[3];
  int eci_flag;

//...... first get the range and altitude inforation

  latlon_to_xyz(launch_time,latgbi,longbi,r0);

//...... use ECI coordinates for eom

  eci_flag = eom_var->get_ECI();
  eom_var->set_ECI(1);
  eom_var->get_pos_vel(kill_time,r1,v1);
  eom_var->set_ECI(eci_flag);

  vmag = sqrt(v1[0]*v1[0]+v1[1]*v1[1]+v1[2]*v1[2]);

  kill_time += 0.5 * (resolution / vmag) - launch_time;
  theta = get_theta_gbi(r0,r1,kill_time);
  kill_time += launch_time;

  return theta;

}

/************************************************************************
* aim_gbi : aim a gbi at an incoming missile described by eom		*
************************************************************************/
double C_MISSILE::aim_gbi(double current_time, double lat, double lon,
			C_EOM *eom_var) {
  double lo_time;
  double hi_time;
  double launch_time;
  double nearest_time;
  double kill_time;
  double time;
  double theta;
  double theta1;
  double dist;
  double timp_ground;
  double r0[3];
  double r1[3];
  double v0[3];
  double v1[3];
  double vscale;
  double phi;
  int eflag;
  double d0,d1,d2,d;
  double dv0,dv1,dv2;
  C_BASE_SPACE *bus;
  double tbus;
  double tbus_start;

//...... first require the rv to be hit at least 50 seconds before impact

  lo_time = current_time + 120.0;
  hi_time = eom_var->get_endtime() - 50.0;

//...... make sure that we have a valid launch window to start with

  if (lo_time > hi_time) {
    fprintf(stderr,"AIM GBI> Warning, could not aim: bad launch window\n");
    return -1.0;
  }

//...... find the minimum time and distance to rv

  nearest_time = mindist(current_time,lat,lon,eom_var,dist);
  if (nearest_time < lo_time) nearest_time = lo_time;
  if (nearest_time > hi_time) nearest_time = hi_time;

//...... go through the launch window starting with the nearest time backwards

  theta = 0.0;

  for (kill_time=nearest_time; kill_time > lo_time; kill_time-=10.0) {
    time = kill_time;
    theta = get_theta_gbi(lat, lon, eom_var, current_time, time);
    if (theta != 0.0) break;
  }

//...... go the other way in time if a launch angle is not found yet

  if (theta == 0) {
    for (kill_time=nearest_time+10.0; kill_time < hi_time; kill_time+=10.0) {
      time = kill_time;
      theta = get_theta_gbi(lat, lon, eom_var, current_time, time);
      if (theta != 0.0) break;
    }
  }

//...... if theta still is zero, then we cant aim this gbi

  if (theta == 0) {
    fprintf(stderr,"AIM GBI> Warning, could not aim: cant reach rv\n");
    fprintf(stderr,"AIM GBI> lo,near,hi = %f %f %f\n",
	lo_time, nearest_time, hi_time);
    return -1.0;
  }

//...... try to get a good launch time

/*
  launch_time = current_time;
  while (1) {
    time = kill_time;
    theta1 = get_theta_gbi(lat, lon, eom_var, launch_time+10.0, time);
    if (theta1 == 0) break;
    theta = theta1;
    launch_time += 10.0;
  }
*/

  launch_time = kill_time - 120.0;

  while (1) {

    time = kill_time;
    theta = get_theta_gbi(lat, lon, eom_var, launch_time, time);
    if ((theta != 0) && (fabs(kill_time-time ) < 10.0)) break;
    launch_time -= 10.0;

    if (launch_time < current_time) {

//      fprintf(stderr,"AIM GBI> Warning, rv too close to hit\n");

      kill_time -= 10.0;
      launch_time = kill_time - 120.0;

      if (kill_time < current_time+120.0) {
        fprintf(stderr,"AIM GBI> Warning, should but could not aim rv\n");
        return -1.0;
      }

    }
  }


//...... print out the value for theta, launch time, and kill time

  fprintf(stderr,"AIM GBI> theta = %f, launch time = %f, kill time = %f\n",
	theta,launch_time,kill_time);

//...... set up the angle parameters for the GBI to hit the target

//  latlon_to_xyz(launch_time,lat,lon,r0);
  latlon_to_xyz(lat,lon,r0);

  eflag = eom_var->get_ECI();
  eom_var->set_ECI(0);
  eom_var->get_pos_vel(kill_time,r1,v1);
  eom_var->set_ECI(eflag);

  init_t0(launch_time);
  set_ECI();

  vscale = 0.0;
  theta *= RADDEG;
  phi = 0.0;

  timp_ground = set_aim_angles(theta, phi, vscale, r0, r1);
  set_rotate(launch_time);

  d0 = r0[0]-r1[0];
  d1 = r0[1]-r1[1];
  d2 = r0[2]-r1[2];
  d = sqrt(d0*d0+d1*d1+d2*d2);

  fprintf(stderr,"AIM GBI> starting position error = %f Km\n",d);

//...... check ECR error

  set_ECR();
  eom_var->set_ECI(0);

  get_pos_vel(kill_time,r0,v0);
  eom_var->get_pos_vel(kill_time,r1,v1);

  d0 = r1[0]-r0[0];
  d1 = r1[1]-r0[1];
  d2 = r1[2]-r0[2];
  d = sqrt(d0*d0+d1*d1+d2*d2);

  fprintf(stderr,"AIM GBI> r0 is %f %f %f\n", r0[0], r0[1], r0[2]);
  fprintf(stderr,"AIM GBI> r1 is %f %f %f\n", r1[0], r1[1], r1[2]);
  fprintf(stderr,"AIM GBI> closest approach = %f Km (ECR)\n",d);

//...... add the first divert correction to the start of the kepler part

  set_ECI();
  eom_var->set_ECI(1);

  bus = get_bus();
  tbus_start = bus->get_start_time();
  tbus = kill_time - tbus_start;
  dv0 = d0 / tbus;
  dv1 = d1 / tbus;
  dv2 = d2 / tbus;
  bus->get_pos_vel(tbus_start,r0,v0);
  unrotate(r0,v0);
  v0[0] += dv0;
  v0[1] += dv1;
  v0[2] += dv2;
  bus->init(r0,v0);

//...... get impact error after final correction

  get_pos_vel(kill_time,r0,v0);
  eom_var->get_pos_vel(kill_time,r1,v1);

  d0 = r1[0]-r0[0];
  d1 = r1[1]-r0[1];
  d2 = r1[2]-r0[2];
  d = sqrt(d0*d0+d1*d1+d2*d2);

  fprintf(stderr,"AIM GBI> initial correction: impact error = %f Km\n",d);

//...... check ECR error

  set_ECR();
  eom_var->set_ECI(0);

  get_pos_vel(kill_time,r0,v0);
  eom_var->get_pos_vel(kill_time,r1,v1);

  d0 = r1[0]-r0[0];
  d1 = r1[1]-r0[1];
  d2 = r1[2]-r0[2];
  d = sqrt(d0*d0+d1*d1+d2*d2);

/*
  fprintf(stderr,"AIM GBI: initial impact error = %f Km (ECR)\n",d);
*/

//...... add the first divert correction to the start of the kepler part

  set_ECI();
  eom_var->set_ECI(1);

  bus = get_bus();
  tbus_start = bus->get_start_time();
  tbus = kill_time - tbus_start;
  dv0 = d0 / tbus;
  dv1 = d1 / tbus;
  dv2 = d2 / tbus;
  bus->get_pos_vel(tbus_start,r0,v0);
  unrotate(r0,v0);
  v0[0] += dv0;
  v0[1] += dv1;
  v0[2] += dv2;
  bus->init(r0,v0);

//...... get impact error after final correction

  get_pos_vel(kill_time,r0,v0);
  eom_var->get_pos_vel(kill_time,r1,v1);

  d0 = r1[0]-r0[0];
  d1 = r1[1]-r0[1];
  d2 = r1[2]-r0[2];
  d = sqrt(d0*d0+d1*d1+d2*d2);

  fprintf(stderr,"AIM GBI> final correction: impact error = %f Km\n",d);

//...... restore the eom to whatever its state is and set the GBI to ECI

  eom_var->set_ECI(eflag);
  set_ECI();

  if (d < 25.0) {
    return kill_time;
  }else{
    return -1.0;
  }

}

/************************************************************************
* mindist : find the minimum distance between gbi site and target	*
************************************************************************/
double C_MISSILE::mindist(double current_time, double lat, double lon,
			C_EOM *eom_var, double &dist) {
  double lo_time, hi_time, mid_time, A_time, B_time;
  double lo_reom[3], hi_reom[3], mid_reom[3];
  double lo_veom[3], hi_veom[3], mid_veom[3];
  double lo_rgbi[3], hi_rgbi[3], mid_rgbi[3];
  double r_lo, r_hi, r_mid, r_A, r_B;
  double rdot_lo, rdot_hi, rdot_mid, rdot_A, rdot_B;
  double vmag;
  double d0,d1,d2;
  double a0,a1,a2,a3;
  double temp, temp1;
  double alpha1;
  double time1;
  int nits;
  int eci_flag;
  double old_time1;

  eci_flag = eom_var->get_ECI();
  eom_var->set_ECI(0);

  lo_time = current_time;  
  hi_time = eom_var->get_endtime(); 
  mid_time = (lo_time + hi_time) / 2.0;

  latlon_to_xyz(lat,lon,lo_rgbi);
  latlon_to_xyz(lat,lon,hi_rgbi);
  
  eom_var->get_pos_vel(lo_time, lo_reom, lo_veom);
  eom_var->get_pos_vel(hi_time, hi_reom, hi_veom);

//...... get the initial lo stuff

  vmag = sqrt(lo_veom[0]*lo_veom[0]+lo_veom[1]*lo_veom[1]
		+lo_veom[2]*lo_veom[2]);
  d0 = lo_reom[0] - lo_rgbi[0];
  d1 = lo_reom[1] - lo_rgbi[1];
  d2 = lo_reom[2] - lo_rgbi[2];
  r_lo = sqrt(d0*d0+d1*d1+d2*d2);
  rdot_lo = (lo_veom[0]*d0+lo_veom[1]*d1+lo_veom[2]*d2) / (RE*vmag);

//...... get the initial hi stuff

  vmag = sqrt(hi_veom[0]*hi_veom[0]+hi_veom[1]*hi_veom[1]
		+hi_veom[2]*hi_veom[2]);
  d0 = hi_reom[0] - hi_rgbi[0];
  d1 = hi_reom[1] - hi_rgbi[1];
  d2 = hi_reom[2] - hi_rgbi[2];
  r_hi = sqrt(d0*d0+d1*d1+d2*d2);
  rdot_hi = (hi_veom[0]*d0+hi_veom[1]*d1+hi_veom[2]*d2) / (RE*vmag);

//...... check if minimum at end points

  if (rdot_lo > 0.0) {
    dist = r_lo;
    return lo_time;
  }

  if (rdot_hi < 0.0) {
    if (r_lo < r_hi) {
      dist = r_lo;
      return lo_time;
    }else{
      dist = r_hi;
      return hi_time;
    }
  }

//...... loop and find the minimum

  nits = 0;
  old_time1 = lo_time;
  A_time = lo_time;
  B_time = hi_time;
  r_A = r_lo;
  r_B = r_hi;
  rdot_A = rdot_lo;
  rdot_B = rdot_hi;

  while (((hi_time-lo_time) > 1.0) || ((B_time - A_time) > 0.001)){
    temp = B_time-A_time;
    a0 = r_A;
    a1 = rdot_A*temp;
    a2 = -(rdot_B+2.0*rdot_A)*temp + 3.0*(r_B-r_A);
    a3 = 2.0*(r_A - r_B) + (rdot_B + rdot_A)*temp;
/*
    fprintf(stderr,"r_A=%f, %f\n",r_A,a0);
    fprintf(stderr,"r_B=%f, %f\n",r_B,a0+a1+a2+a3);
    fprintf(stderr,"v_A=%f, %f\n",rdot_A,a1/temp);
    fprintf(stderr,"v_B=%f, %f\n",rdot_B,(a1+2*a2+3*a3)/temp);
*/
    temp1 = a2*a2-3.0*a3*a1;
    if (temp1 < 0) {
      mid_time = (lo_time+hi_time)/2.0;
    }else{
      temp1 = sqrt(temp1);
      alpha1 = (-a2 + temp1) / (3.0*a3);
      time1 = alpha1*temp + A_time;
      if ((time1 < lo_time) || (time1 > hi_time)) {
        time1 = (lo_time + hi_time) / 2.0;
        mid_time = time1;
/*
        fprintf(stderr,"MINDIST: time1 %f out of range (%f %f)\n",
		time1,lo_time,hi_time);
*/
      }else{
        mid_time = 0.2*(lo_time+hi_time)/2.0 + 0.8*time1;
/*
        fprintf(stderr,"MINDIST: time1 %f in range (%f %f)\n",
		time1,lo_time,hi_time);
*/
      }
    }

//...... get the new point

    latlon_to_xyz(lat,lon,mid_rgbi);
    eom_var->get_pos_vel(mid_time, mid_reom, mid_veom);

    vmag = sqrt(mid_veom[0]*mid_veom[0]+mid_veom[1]*mid_veom[1]
		+mid_veom[2]*mid_veom[2]);
    d0 = mid_reom[0] - mid_rgbi[0];
    d1 = mid_reom[1] - mid_rgbi[1];
    d2 = mid_reom[2] - mid_rgbi[2];
    r_mid = sqrt(d0*d0+d1*d1+d2*d2);
    rdot_mid = (mid_veom[0]*d0+mid_veom[1]*d1+mid_veom[2]*d2) / (RE*vmag);

//...... update the low and high limits

    if (rdot_mid < 0.0) {
      r_lo = r_mid;
      rdot_lo = rdot_mid;
      lo_time = mid_time;
    }else{
      r_hi = r_mid;
      rdot_hi = rdot_mid;
      hi_time = mid_time;
    }

//...... set up the next point for the spline

    if (nits%2) {
      r_A = r_mid;
      rdot_A = rdot_mid;
      A_time = mid_time;
    }else{
      r_B = r_mid;
      rdot_B = rdot_mid;
      B_time = mid_time;
    }

    nits++;

    if (fabs(old_time1-time1) < 0.000001) break;
    old_time1 = time1;

  }

  time1 = mid_time;
  fprintf(stderr,"Minimum distance %f at time %f found in %d iterations\n",
	r_mid, mid_time,nits);
/*

//...... check time1 - 1.0

  latlon_to_xyz(lat,lon,lo_rgbi);
  eom_var->get_pos_vel(time1-1.0, lo_reom, lo_veom);

  vmag = sqrt(lo_veom[0]*lo_veom[0]+lo_veom[1]*lo_veom[1]
		+lo_veom[2]*lo_veom[2]);
  d0 = lo_reom[0] - lo_rgbi[0];
  d1 = lo_reom[1] - lo_rgbi[1];
  d2 = lo_reom[2] - lo_rgbi[2];
  r_lo = sqrt(d0*d0+d1*d1+d2*d2);
  rdot_lo = (lo_veom[0]*d0+lo_veom[1]*d1+lo_veom[2]*d2) / (RE*vmag);

//...... check time1

  latlon_to_xyz(lat,lon,mid_rgbi);
  eom_var->get_pos_vel(time1, mid_reom, mid_veom);

  vmag = sqrt(mid_veom[0]*mid_veom[0]+mid_veom[1]*mid_veom[1]
		+mid_veom[2]*mid_veom[2]);
  d0 = mid_reom[0] - mid_rgbi[0];
  d1 = mid_reom[1] - mid_rgbi[1];
  d2 = mid_reom[2] - mid_rgbi[2];
  r_mid = sqrt(d0*d0+d1*d1+d2*d2);
  rdot_mid = (mid_veom[0]*d0+mid_veom[1]*d1+mid_veom[2]*d2) / (RE*vmag);

//...... check time1 + 1.0

  latlon_to_xyz(lat,lon,hi_rgbi);
  eom_var->get_pos_vel(time1+1.0, hi_reom, hi_veom);

  vmag = sqrt(hi_veom[0]*hi_veom[0]+hi_veom[1]*hi_veom[1]
		+hi_veom[2]*hi_veom[2]);
  d0 = hi_reom[0] - hi_rgbi[0];
  d1 = hi_reom[1] - hi_rgbi[1];
  d2 = hi_reom[2] - hi_rgbi[2];
  r_hi = sqrt(d0*d0+d1*d1+d2*d2);
  rdot_hi = (hi_veom[0]*d0+hi_veom[1]*d1+hi_veom[2]*d2) / (RE*vmag);

//...... print out the check

  fprintf(stderr,"time=%f, dist=%f\n",time1-1.0, r_lo);
  fprintf(stderr,"time=%f, dist=%f\n",time1, r_mid);
  fprintf(stderr,"time=%f, dist=%f\n",time1+1.0, r_hi);

*/

  eom_var->set_ECI(eci_flag);

  dist = r_mid;
  return time1;

}

/************************************************************************
* init_bus : initialize the bus	from the end pos of the last stage	*
************************************************************************/
void C_MISSILE::init_bus() {
  C_BASE_SPACE *bus;
  double tlast;
  double P[3], V[3];

  bus = get_bus();
  tlast = eom[nstages-1]->get_endtime();
  bus->get_pos_vel(tlast,P,V);
  bus->init(P,V);
  bus->update_impact();

}

/************************************************************************
* init_t0 : initialize the launch time from midnight			*
************************************************************************/
void C_MISSILE::init_t0(double t) {
  double time;
  int i;

  t0 = t;

  time = t;
  for (i=0; i<nstages+1; i++) {
    eom[i]->set_start_time(time);
    time += eom[i]->get_duration();
    eom[i]->set_endtime(time);
  }

}

/************************************************************************
* set_rotate : set up the rotation for the stages and the bus		*
************************************************************************/
void C_MISSILE::set_rotate(double t) {
  double a;
  int i;
  C_BASE_STAGE *stage;
  C_BASE_SPACE *bus;

//...... compute the rotation cos and sin

  t0 = t;
  a = t0*OMEGAE;
  coe = cos(a);
  soe = sin(a);

//...... set up the rotations for the initial launch position

  for (i=0; i<nstages; i++) {
    stage = get_stage(i);
    stage->set_rotate(coe,soe);
  }

  bus = get_bus();
  bus->set_rotate(coe,soe);

}

/************************************************************************
* rotate : rotate eci coordinates to the correct launch time		*
************************************************************************/
void C_MISSILE::rotate(double rt[3], double vt[3]) {
  double x,y,vx,vy;

  x = rt[0]*coe - rt[1]*soe;
  y = rt[0]*soe + rt[1]*coe;

  vx = vt[0]*coe - vt[1]*soe;
  vy = vt[0]*soe + vt[1]*coe;

  rt[0] = x;
  rt[1] = y;

  vt[0] = vx;
  vt[1] = vy;

}

/************************************************************************
* unrotate : unrotate eci coordinates  					*
************************************************************************/
void C_MISSILE::unrotate(double rt[3], double vt[3]) {
  double x,y,vx,vy;

  x = rt[0]*coe + rt[1]*soe;
  y = -rt[0]*soe + rt[1]*coe;

  vx = vt[0]*coe + vt[1]*soe;
  vy = -vt[0]*soe + vt[1]*coe;

  rt[0] = x;
  rt[1] = y;

  vt[0] = vx;
  vt[1] = vy;

}

/************************************************************************
* make_buff : make a buffer which can initialize this missile		*
************************************************************************/
char *C_MISSILE::make_buff(int &size) {
  char *buff;
  int i;
  int offset;
  C_STAGE *stage;
  C_BASE_SPACE *bus;

//...... get the size of the buffer

  if (stage_type == ANALYTIC) {
    size = sizeof(C_MISSILE)
	+ nstages*sizeof(C_STAGE)
	+ sizeof(C_BUS);
  }

  if (stage_type == INTEGRATE) {
    size = sizeof(C_MISSILE)
	+ nstages*sizeof(C_INT_STAGE)
	+ sizeof(C_INT_BUS);
  }

  buff = new char[size];

//...... initialize the offset to zero

  offset = 0;

//...... copy the missile data

  memcpy(&buff[offset],(char *)this,sizeof(C_MISSILE));
  offset += sizeof(C_MISSILE);

//...... copy the stage data

  for (i=0; i<nstages; i++) {
    stage = get_stage(i);

    if (stage_type == ANALYTIC) {
      memcpy(&buff[offset],(char *)stage,sizeof(C_STAGE));
      offset += sizeof(C_STAGE);
    }

    if (stage_type == INTEGRATE) {
      memcpy(&buff[offset],(char *)stage,sizeof(C_INT_STAGE));
      offset += sizeof(C_INT_STAGE);
    }

  }

//...... copy the bus data

  bus = get_bus();

  if (stage_type == ANALYTIC) {
    memcpy(&buff[offset],(char *)bus,sizeof(C_BUS));
  }

  if (stage_type == INTEGRATE) {
    memcpy(&buff[offset],(char *)bus,sizeof(C_INT_BUS));
  }

  return buff;

}

/************************************************************************
* init_missile : initialize this missile using a buffer			*
************************************************************************/
void C_MISSILE::init_missile(char *buff) {
  int i;
  int offset;
  C_STAGE *stage;
  C_BASE_SPACE *bus;

//...... initialize the offset to zero

  offset = 0;

//...... copy the missile data

  memcpy((char *)this,&buff[offset],sizeof(C_MISSILE));
  offset += sizeof(C_MISSILE);

//...... copy the stage data

  for (i=0; i<nstages; i++) {
    stage = get_stage(i);

    if (stage_type == ANALYTIC) {
      memcpy((char *)stage,&buff[offset],sizeof(C_STAGE));
      offset += sizeof(C_STAGE);
    }

    if (stage_type == INTEGRATE) {
      memcpy((char *)stage,&buff[offset],sizeof(C_INT_STAGE));
      offset += sizeof(C_INT_STAGE);
    }

  }

//...... copy the bus data

  bus = get_bus();

  if (stage_type == ANALYTIC) {
    memcpy((char *)bus,&buff[offset],sizeof(C_BUS));
  }

  if (stage_type == INTEGRATE) {
    memcpy((char *)bus,&buff[offset],sizeof(C_INT_BUS));
  }


}

/************************************************************************
* next_theta : get the next theta for aiming				*
************************************************************************/
double C_MISSILE::next_theta(double error, double theta) {
  double A[3][3];
  double B[3];
  double the12, the13, the14;
  double the22, the23, the24;
  double the32, the33, the34;
  double det, det1, det2, det3;
  double a0, a1, a2;
  double new_theta;
  double temp;
  double new1, new2;

  err3 = err2;
  err2 = err1;
  err1 = error;

  ntries++;

  if (ntries < 3) {
    the3 = the2;
    the2 = the1;
    the1 = theta;
    return theta + 0.1*RADDEG;
  }
  the3 = the2 - theta;
  the2 = the1 - theta;
  the1 = 0.0;

  the12 = the1*the1;
  the13 = the12*the1;
  the14 = the13*the1;

  the22 = the2*the2;
  the23 = the22*the2;
  the24 = the23*the2;

  the32 = the3*the3;
  the33 = the32*the3;
  the34 = the33*the3;

  A[0][0] = 3.0;
  A[0][1] = the1 + the2 + the3;
  A[0][2] = the12 + the22 + the32;

  A[1][0] = A[0][1];
  A[1][1] = A[0][2];
  A[1][2] = the13 + the23 + the33;

  A[2][0] = A[0][2];
  A[2][1] = A[1][2];
  A[2][2] = the14 + the24 + the34;

  B[0] = err1 + err2 + err3;
  B[1] = err1*the1 + err2*the2 + err3*the3;
  B[2] = err1*the12 + err2*the22 + err3*the32;


  det =   A[0][0]*A[1][1]*A[2][2]
	+ A[0][1]*A[1][2]*A[2][0]
	+ A[0][2]*A[1][0]*A[2][1]
	- A[0][2]*A[1][1]*A[2][0]
	- A[0][0]*A[1][2]*A[2][1]
	- A[0][1]*A[1][0]*A[2][2];

  det1 =  B[0]*A[1][1]*A[2][2]
	+ A[0][1]*A[1][2]*B[2]
	+ A[0][2]*B[1]*A[2][1]
	- A[0][2]*A[1][1]*B[2]
	- B[0]*A[1][2]*A[2][1]
	- A[0][1]*B[1]*A[2][2];

  det2 =  A[0][0]*B[1]*A[2][2]
	+ B[0]*A[1][2]*A[2][0]
	+ A[0][2]*A[1][0]*B[2]
	- A[0][2]*B[1]*A[2][0]
	- A[0][0]*A[1][2]*B[2]
	- B[0]*A[1][0]*A[2][2];

  det3 =  A[0][0]*A[1][1]*B[2]
	+ A[0][1]*B[1]*A[2][0]
	+ B[0]*A[1][0]*A[2][1]
	- B[0]*A[1][1]*A[2][0]
	- A[0][0]*B[1]*A[2][1]
	- A[0][1]*A[1][0]*B[2];

  a0 = det1/det;
  a1 = det2/det;
  a2 = det3/det;

/*
  temp = a0 + a1*the1 + a2*the12 - err1;
  fprintf(stderr,"temp 1 = %f\n",temp);

  temp = a0 + a1*the2 + a2*the22 - err2;
  fprintf(stderr,"temp 2 = %f\n",temp);

  temp = a0 + a1*the3 + a2*the32 - err3;
  fprintf(stderr,"temp 3 = %f\n",temp);
*/

  temp = a1*a1 - 4.0*a0*a2;

  if (temp < 0.0) {

    new_theta = theta - a1 / (2.0*a2);

  }else{

    temp = sqrt(temp);
    new1 = (-a1 + temp) / (2.0*a2);
    new2 = (-a1 - temp) / (2.0*a2);

/*
    temp = a0 + a1*new1 + a2*new1*new1;
    if (fabs(temp) > 0.000001) {
      fprintf(stderr,"Error (MISSILE) bad root\n");
    }
    temp = a0 + a1*new2 + a2*new2*new2;
    if (fabs(temp) > 0.000001) {
      fprintf(stderr,"Error (MISSILE) bad root\n");
    }
*/

    if (fabs(new1) < fabs(new2)) {
      new_theta = theta + new1;
    }else{
      new_theta = theta + new2;
    }

  }

  the1 += theta;
  the2 += theta;
  the3 += theta;

  return new_theta;


}

/************************************************************************
* get_pos_vel : get the position and velocity				*
************************************************************************/
void C_MISSILE::get_pos_vel(double t, double rt[3], double vt[3]) {
  int i,j;
  double dt;
  C_BASE_SPACE *bus;

  dt = t-t0;
  j = nstages+1;
  for (i=0; i<j; i++) {
    if (dt < eom[i]->get_duration()) {
      eom[i]->get_pos_vel(t, rt, vt);
      return;
    }
    dt -= eom[i]->get_duration();
  }

  bus = (C_BASE_SPACE *)eom[nstages];
  bus->get_pos_vel(bus->get_endtime(),rt,vt);
  vt[0] = 0.0;
  vt[1] = 0.0;
  vt[2] = 0.0;

}


