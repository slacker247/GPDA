// kepler.C method file

#include <stdio.h>
#include <math.h>
#include <float.h>
#include "kepler.H"

#define RE   6378.145		/* Radius of the Earth			*/
#define RE3  2.5946761e11	/* Radius of the Earth cubed		*/
#define GM   398601.3		/* Gravitational Constant		*/
#define GM2   (1.5888299e11)	/* Gravitational Constant Squared	*/
#define TWOPI   (6.283185307)	/* two times PI				*/
#define OMEGAE  7.2921158279144062e-5

/************************************************************************
* C_KEPLER : construct a kepler object					*
************************************************************************/
C_KEPLER::C_KEPLER() {

  t0 = 0.0;
  ECI = 1;
  coe = 1.0;
  soe = 0.0;

}

/************************************************************************
* C_KEPLER : init a kepler object					*
************************************************************************/
void C_KEPLER::init(double x[3], double v[3]) {
  double h[3],pv[3],hsq,vsq,r,rsq,r3,cosnu,t;
  double ee,cose,mm,erg,dvx,pp,cf1,temp;
  int j;

//  printf("init kepler...\n");

  R0[0] = x[0];
  R0[1] = x[1];
  R0[2] = x[2];

  rsq = x[0]*x[0] + x[1]*x[1] + x[2]*x[2];
  r = sqrt(rsq);
  r3 = rsq*r;

  for (j=0; j<3; j++) {

    init_pos[j] = x[j];
    init_vel[j] = v[j];
    init_acc[j] = -GM*x[j] / r3;

    cur_pos[j]  = x[j];
    cur_vel[j]  = v[j];

  }

  tcur = 0.0;
  t = 0.0;                              /* Get Request Time             */

  vsq = v[0]*v[0] + v[1]*v[1] + v[2]*v[2];
  h[0] = x[1]*v[2] - x[2]*v[1];         /* Angular Momentum Evaluation  */
  h[1] = x[2]*v[0] - x[0]*v[2];
  h[2] = x[0]*v[1] - x[1]*v[0];
  hsq = h[0]*h[0] + h[1]*h[1] + h[2]*h[2];

  erg = vsq/2.0 - GM/r;              /* Orbital Energy Evaluation    */

  a = -GM/(2.0*erg);                 /* Semi-Major Axis              */
  e = 1.0 + 2.0*erg*hsq/GM2;         /* Orbit Eccentricity           */
  if (e < 0.0) {
    e = 0.0;
  }else{
    e = sqrt(e);
  }
                                        /* Evaluate Perifocal Vector    */
  dvx = v[0]*x[0] + v[1]*x[1] + v[2]*x[2];
  cf1 = vsq - GM/r;
  pv[0] = cf1*x[0] - dvx*v[0];
  pv[1] = cf1*x[1] - dvx*v[1];
  pv[2] = cf1*x[2] - dvx*v[2];
  pp = pv[0]*pv[0] + pv[1]*pv[1] + pv[2]*pv[2]; 

  cosnu = ( hsq/(r*GM) - 1.0 )/e;    /* Evaluate True Anomaly Cosine */

  cose = (e + cosnu)/(1. + e*cosnu);    /* Evaluate Eccentric Anomaly   */

//  printf("arg for acos for e %f\n",cose);
  if (cose > 1.0) cose = 1.0;
  if (cose < -1.0) cose = -1.0;

  ee = acos(cose);
  if( dvx<0.0 ) ee = TWOPI - ee;        

  mm = ee - e*sin(ee);                  /* Evaluate Mean Anomaly At t0  */
  m = mm - t*sqrt( GM/(a*a*a) );

//  printf("arg for acos for i %f\n",h[2]/sqrt(hsq));

  i = acos( h[2]/sqrt(hsq) );        /* Evaluate Orbit Inclination   */
  
  o = atan2( h[0], -h[1] );          /* Get Long. Of Ascending Node  */
    
  temp = ( -h[1]*pv[0] + h[0]*pv[1] )   /* Get Argument Of Periapsis    */
       /sqrt( pp*( h[0]*h[0] + h[1]*h[1] ) );

//  printf("arg for acos for w %f\n",temp);

  w = acos(temp);
  if( pv[2]<0.0 )  w = TWOPI - w;

//...... set some object variables

  av   = sqrt(GM/(a*a*a));
  p = a*(1-e*e);

  si = sin(i);
  ci = cos(i);
  so = sin(o);
  co = cos(o);
  sw = sin(w);
  cw = cos(w);

  soci = so*ci;
  coci = co*ci;

  ean = 0.0;

}

/************************************************************************
* C_KEPLER : init a kepler object					*
************************************************************************/
void C_KEPLER::init_aeiowm(double aa, double ee, double ii, double oo,
			double ww, double mm) {
  int j;
  double r,r2,r3;

  a = aa;
  e = ee;
  i = ii;
  o = oo;
  w = ww;
  m = mm;

//...... set some object variables

  av   = sqrt(GM/(a*a*a));
  p = a*(1-e*e);

  si = sin(i);
  ci = cos(i);
  so = sin(o);
  co = cos(o);
  sw = sin(w);
  cw = cos(w);

  soci = so*ci;
  coci = co*ci;

  ean = 0.0;

//...... set up the initial position and velocity

  tcur = 0.0;
  get_pos_vel(tcur, init_pos, init_vel);
  r2 =  init_pos[0]*init_pos[0]
      + init_pos[1]*init_pos[1]
      + init_pos[2]*init_pos[2];
  r = sqrt(r2);
  r3 = r2*r;

  for (j=0; j<3; j++) {
    init_acc[j] = -GM*init_pos[j] / r3;
    cur_pos[j]  = init_pos[j];
    cur_vel[j]  = init_vel[j];
  }

}

/************************************************************************
* get_pos_vel : get the position and velocity				*
************************************************************************/
void C_KEPLER::get_pos_vel(double tt, double x[3], double v[3]) {
  double cta;
  double cv,sv;
  double u, cu, su;
  double arg1, arg2;
  double r0, v0;
  double t;
  int j;

  t = tt - t0;

//  if (fabs(t-tcur) < 0.001) {
  if (fabs(t-tcur) < 0.0) {

    integrate_pos_vel(t,x,v);	// not quite right so bypass for now

  }else{

    arg1 = (m + av*t)/TWOPI;
    man = TWOPI*modf(arg1, &arg2);
    if (man<0) man+=TWOPI;

    if (!truan()) {

      x[0] = R0[0];
      x[1] = R0[1];
      x[2] = R0[2];

      v[0] = 0.0;
      v[1] = 0.0;
      v[2] = 0.0;

    }else{

      cta = cos(truean);
      u = w + truean;
      su = sin(u);
      cu = cos(u);

      cv = -(su + e*sw);
      sv = cu + e*cw;

      r0 = p/(1.0 + e*cta);

// store the position and velocity vectors in the return argument arrays

      x[0] = r0*(co*cu - soci*su);
      x[1] = r0*(so*cu + coci*su);
      x[2] = r0*su*si;

      v0 = sqrt(GM/p);
      v[0] = v0*(co*cv - soci*sv);
      v[1] = v0*(so*cv + coci*sv);
      v[2] = v0*sv*si;

      for (j=0; j<3; j++) {
        cur_pos[j] = x[j];
        cur_vel[j] = v[j];
      }
      tcur = t;

    }

  }

//...... rotate ECI position and velocity by the launch position

  rotate(x,v);

//...... rotate to ECR coordinates if not ECI

  if (!ECI) {
    eci_to_ecr(tt,x,v);
  }

}

/************************************************************************
* truan : true anomily							*
************************************************************************/
int C_KEPLER::truan() {
  int converged;
  double ean0, eanh, esq;
  double cean0, sean0, ceanh, seanh;
  int nits;

  ean = man;
  converged = 0;
  nits = 0;

  while (!converged) {
    ean0 = ean;
    sean0 = sin(ean0);
    cean0 = cos(ean0);
    ean = ean0 - (ean0 - e*sean0 - man) / (1. - e*cean0);
    if (fabs(ean0-ean) <= 3.e-6*fabs(ean)) converged = 1;
    nits++;
    if (nits > 100) return 0;
  }

  eanh = ean/2.;
  esq = sqrt((1.+e)/(1.-e));

  seanh = sin(eanh);
  ceanh = cos(eanh);
  truean = 2.*atan2( esq*seanh, ceanh );

  return 1;

}

/************************************************************************
* update_impact : compute the time, position (longitude and latitude)
*                 and velocity of the rv impact                         
*                 from latest available orbit parameters                
* outputs (public to C_RV):
*  imp_pos[3]   position of impact in ECI coordinates
*  imp_vel[3]   velocity of impact in ECI coordinates
*  timp                 time of impact after beginning of day
*  impact_error         error (km) of impact point
*  impact_latitude  
*  impact_longitude 
*
************************************************************************/
void C_KEPLER::update_impact(){
  double truean_impact;           // true anomaly at impact
  double cos_truean_impact;       // cosine of true anomaly at impact
  double ean_impact;              // eccentric anomaly at impact
  double man_impact;              // mean anomaly at impact
  double man_tpar;                // mean anomaly at time of parameter update
  double cv,sv;
  double u, cu, su, susi;
  double r0, v0;
 
//  printf("update impact kepler...\n");

  cos_truean_impact = (p/RE - 1.)/e;
  if (cos_truean_impact >= 1.0) {
    integrate_impact();
    return;
  }

  truean_impact     = acos(cos_truean_impact);
  if(truean_impact < M_PI){      // then you have truean of launch, not impact
    truean_impact = TWOPI - truean_impact;      // now you have impact
  }

  ean_impact = acos((e+cos_truean_impact)/(1.+e*cos_truean_impact));
  if(ean_impact < M_PI){         // then you have ean of launch, not impact
    ean_impact = TWOPI - ean_impact;    // now you have impact
  }

// compute impact time and error

  man_impact   = ean_impact - e*sin(ean_impact);
  if(man_impact < 0) man_impact += TWOPI;
  man_tpar     = fmod(m,TWOPI);
  if(man_tpar < 0) man_tpar += TWOPI;
  timp         = (man_impact-man_tpar)/av;

// compute impact position and velocity

  u = w + truean_impact;
  su = sin(u);
  cu = cos(u);
  susi = su*si;

  cv = -(su + e*sw);
  sv = cu + e*cw;

// check for errors...

  r0 = p/(1.0 + e*cos_truean_impact);
  if(fabs(1.0-r0/RE)>1.e-5){
    printf("\n error 1 in C_RV::update impact -- r0 .ne. RE\n");
  }

  imp_pos[0] = RE*(co*cu - soci*su);
  imp_pos[1] = RE*(so*cu + coci*su);
  imp_pos[2] = RE*susi;

  Rend[0] = imp_pos[0];
  Rend[1] = imp_pos[1];
  Rend[2] = imp_pos[2];

  TT = timp;
  Tend = t0 + TT;

  v0 = sqrt(GM/p);
  imp_vel[0] = v0*(co*cv - soci*sv);
  imp_vel[1] = v0*(so*cv + coci*sv);
  imp_vel[2] = v0*sv*si;

  fit_spline6();

}

/************************************************************************
* integrate_impact : integrate the impact position			*
************************************************************************/
void C_KEPLER::integrate_impact() {
  double t,dt;
  double rr[3];
  double vv[3];
  double err;
  double olderr;
  double v2;
  double vmag;
  int nits;

  get_pos_vel(t0,rr,vv);
  err = sqrt(rr[0]*rr[0] + rr[1]*rr[1] + rr[2]*rr[2]) - RE;
  if (err < 0.0) {
    TT = -1.0;
    Tend = -1.0;
    timp = -1.0 - t0;
//  fprintf(stderr,"Warning (KEPLER) can not integrate impact\n");
    return;
  }

  v2 = vv[0]*vv[0] + vv[1]*vv[1] + vv[2]*vv[2];
  vmag = sqrt(v2);
  dt = (vmag/0.0098) * (-1.0 + sqrt(1.0+2.0*err*0.0098/v2));

  t = t0 + dt;
  olderr = 1.0e20;
  err = 1.0e20;
  nits = 0;

  while (fabs(err) > 0.001) {
    get_pos_vel(t,rr,vv);
    err = sqrt(rr[0]*rr[0] + rr[1]*rr[1] + rr[2]*rr[2]) - RE;
    if (err > 0.0) {
      if (err > olderr) {
	TT = -1.0;
	Tend = -1.0;
	timp = -1.0 - t0;
//      fprintf(stderr,"Warning (KEPLER) can not integrate impact\n");
        return;
      }
      v2 = vv[0]*vv[0] + vv[1]*vv[1] + vv[2]*vv[2];
      vmag = sqrt(v2);
      dt = (vmag/0.0098) * (-1.0 + sqrt(1.0+2.0*err*0.0098/v2));
      t += dt;
    }else{
      t -= dt;
      dt /= 2.0;
    }
    nits++;
    olderr = err;
  }

  imp_pos[0] = rr[0];
  imp_pos[1] = rr[1];
  imp_pos[2] = rr[2];

  Rend[0] = imp_pos[0];
  Rend[1] = imp_pos[1];
  Rend[2] = imp_pos[2];

  TT = t-t0;
  Tend = t0 + TT;

  imp_vel[0] = vv[0];
  imp_vel[1] = vv[1];
  imp_vel[2] = vv[2];

  fit_spline6();

}

/************************************************************************
* integrate : integrate the position until impact			*
************************************************************************/
void C_KEPLER::integrate() {
  int j;
  double t,dt;
  double r;
  double int_pos[3];
  double int_vel[3];

  for (j=0; j<3; j++) {
    int_pos[j] = init_pos[j];
    int_vel[j] = init_vel[j];
  }

  dt = 1.0;
  for (t=0.0; t<20000.0; t+=dt) {

    r = sqrt(int_pos[0]*int_pos[0]+int_pos[1]*int_pos[1]+int_pos[2]*int_pos[2]);
    if (r<RE) break;

    printf("t = %7f.1, alt = %7f.1, position = %7f.1 %7f.1 %7f.1\n",
	t, r-RE,int_pos[0], int_pos[1], int_pos[2]);

    for (j=0; j<3; j++) {
      int_pos[j] += int_vel[j]*dt - (dt*dt/2.0)*GM*int_pos[j]/(r*r*r);
      int_vel[j] += - dt*GM*int_pos[j]/(r*r*r);
    }
  }

}

/************************************************************************
* integrate_pos_vel : integrate the position from last time		*
************************************************************************/
void C_KEPLER::integrate_pos_vel(double t, double rt[3], double vt[3]) {
  int j;
  double r;
  double dt;

  dt = t - tcur;
  r = sqrt(cur_pos[0]*cur_pos[0]+cur_pos[1]*cur_pos[1]+cur_pos[2]*cur_pos[2]);

  for (j=0; j<3; j++) {

    rt[j] = cur_pos[j]+ cur_vel[j]*dt - (dt*dt/2.0)*GM*cur_pos[j]/(r*r*r);
    vt[j] = cur_vel[j] - dt*GM*cur_pos[j]/(r*r*r);

    cur_pos[j] = rt[j];
    cur_vel[j] = vt[j];

  }

  tcur = t;

}

/************************************************************************
* fit_spline6 : fit the cubic spline parameters				*
************************************************************************/
void C_KEPLER::fit_spline6() {
  double alpha, beta, gamma;
  double t2,t3;
  int j;

  t2 = TT*TT;
  t3 = TT*t2;

  for (j=0; j<3; j++) {

    alpha = imp_pos[j] - init_pos[j] - init_vel[j]*TT - init_acc[j]*t2/2.0;
    beta  = imp_vel[j] - init_vel[j] - init_acc[j]*TT;
    gamma = (-GM*imp_pos[j] / RE3) - init_acc[j];

    a_spline[j] = (alpha*10.0 - beta*4.0*TT + gamma*t2/2.0) / t3;
    b_spline[j] = (-alpha*15.0/timp + beta*7.0 - gamma*TT) / t3;
    c_spline[j] = (alpha*6.0/t2 - beta*3.0/timp + gamma/2.0) / t3;

  }

}

/************************************************************************
* fast_pos_vel : get the position and velocity using spline		*
************************************************************************/
void C_KEPLER::fast_pos_vel(double t, double rt[3], double vt[3]) {
  int j;
  double t1,t2,t3,t4,t5;

  t1 = t-t0;

//...... compute powers of t

  t2 = t1*t1;
  t3 = t1*t2;
  t4 = t1*t3;
  t5 = t1*t4;

//...... compute the spline

  for (j=0; j<3; j++) {

    rt[j] = init_pos[j] + init_vel[j]*t1 + init_acc[j]*t2/2.0
	+ a_spline[j]*t3 + b_spline[j]*t4 + c_spline[j]*t5;

    vt[j] = init_vel[j] + init_acc[j]*t1
	+ 3.0*a_spline[j]*t2 + 4.0*b_spline[j]*t3 + 5.0*c_spline[j]*t4;

  }

//...... rotate ECI position and velocity by the launch position

  rotate(rt,vt);

//...... rotate to ECR coordinates if not ECI

  if (!ECI) {
    eci_to_ecr(t,rt,vt);
  }

}

/************************************************************************
* check_spline6 : print out the difference using kep, spline		*
************************************************************************/
void C_KEPLER::check_spline6() {
  int j;
  double t,dt;
  double d, err, terr;
  double temp;
  double kpos[3];
  double kvel[3];
  double spos[3];
  double svel[3];
  double pimp[3];
  double vimp[3];
  double mypimp[3];
  double myvimp[3];
  C_KEPLER kep;
  int ECI_flag;
  double dmax;
  //double tmax;
  //double terrmax;
  //double errmax;

  ECI_flag = ECI;
  ECI = 1;
  get_pos_vel(Tend,mypimp,myvimp);

  dt = 1.0;
  dmax = 0.0;
  for (t=t0; t<Tend; t+=dt) {

    get_pos_vel(t,kpos,kvel);
    fast_pos_vel(t,spos,svel);

    kep.init(spos, svel);
    kep.update_impact();
    terr = Tend - kep.get_timp() - t;

    kep.get_pos_vel(kep.get_timp(),pimp,vimp);
    err = sqrt( (pimp[0]-mypimp[0])*(pimp[0]-mypimp[0])
	      + (pimp[1]-mypimp[1])*(pimp[1]-mypimp[1])
	      + (pimp[2]-mypimp[2])*(pimp[2]-mypimp[2]) );

    d = 0.0;
    for (j=0; j<3; j++) {
      temp = (kpos[j]-spos[j]);
      temp *= temp;
      d+=temp;
    }
    d = sqrt(d);

    if (d > dmax) {
      dmax = d;
      //tmax = t;
      //terrmax = terr;
      //errmax = err;
    }

/*
    printf("t = %7f.1, error = %f, impact error (time,dist) = (%f %f)\n",
	tmax, dmax, terrmax, errmax);
*/

  }

  printf("t = %7f.1, error = %f, impact error (time,dist) = (%f %f)\n",
	t, d, terr, err);

  ECI = ECI_flag;

}

/************************************************************************
* rotate : rotate the ECI position and velocity to start position	*
************************************************************************/
void C_KEPLER::rotate(double rt[3], double vt[3]) {
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
* get_duration : get the duration of kepler orbit			*
************************************************************************/
double C_KEPLER::get_duration() {

  return timp;

}



