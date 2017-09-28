// gridman.C method file

#include <stdio.h>
#include <math.h>
#include "gridman.H"
#include "def.h"

#include "freeobjs.H"
#include "gridid.H"

extern C_FREEOBJS freeobjs;

/************************************************************************
* C_GRIDMAN : construct a gridman object				*
************************************************************************/
C_GRIDMAN::C_GRIDMAN() {
  int i;
  double phi;
  int bytot;
  double km;
  C_BASETYPE *basetype;

//...... initialize the free list manager

  RB_DEFINE_FREE(&freeobjs);
  GRID = object_type("GRID");

//...... set up the grid manager

  basetype = parameters_parser->get_basetype("parameters");
  show_mem = basetype->get_logical("show_mem");
  mem_time = basetype->get_float("mem_time");

  basetype = grid_parser->get_basetype("grid");
  km = basetype->get_float("grid_size");
  grid_resolution = basetype->get_float("grid_resolution");
  sensor_resolution = basetype->get_float("sensor_resolution");
  time_resolution = basetype->get_float("time_resolution");

  delay_m2g = LONE * (time_resolution / 3.0);
  delay_g2m = LONE * (time_resolution / 3.0);
  delay_m2e = LONE * (time_resolution / 3.0);
  delay_s2g = LONE * (time_resolution / 3.0);

  N = int(PI*RE/km);
  d = (PI*RE)/N;

//...... initialize book keeping variables

  M = new int[N];
  Mcum = new int[N];
  dphi = new double[N];
  dtheta = PI/N;

//...... construct the hash arrays for gridman lookup and construction

  tot = 0;
  for (i=0; i<N; i++) {
    phi = (i+0.5)*(dtheta);
    M[i] = int(0.5 + 2.0*N*sin(phi));
    tot += M[i];
    dphi[i] = TWOPI/M[i];
  }

  Mcum[0] = 0;
  for (i=1; i<N; i++) {
    Mcum[i] = Mcum[i-1] + M[i-1];
  }

  mytot = tot / N_NODES;
  if ((tot % N_NODES) > NODE) mytot++;
  grid = new C_GRID[mytot];

  for (i=0; i<mytot; i++) {
    grid[i].set_glob_gid(get_glob_gid(NODE,i));
  }

//...... print out the number of bytes used to construct the gridman object

  bytot = mytot * sizeof(C_GRID);
  bytot += N*(2*sizeof(int) + sizeof(double));
  bytot += sizeof(C_GRIDMAN);
  printf("GRIDMAN object created %7d bytes\n",bytot);

//...... tell SPEEDES about the grids

  N_TOT = tot;
  N_LOC = mytot;

  TOTAL_OBJECTS += N_TOT + N_NODES;
  reserve_n_objects(N_LOC);
  for (i=0; i<N_LOC; i++) define_object(&grid[i]);
  reset_interact();

  for (i=0; i<N_LOC; i++) {
    if (get_glob_gid(NODE,i) != grid[i].get_glob_gid()) {
      fprintf(stderr,"Error, (GRIDMAN) bad grid id %d %d\n",
	get_glob_gid(NODE,i), grid[i].get_glob_gid());
    }
  }

}

/************************************************************************
* init_events : initialize events for the grid element objects		*
************************************************************************/
void C_GRIDMAN::init_events() {
  int SHOW_MEM;

  printf("GRIDMAN initializing\n");

  if ((NODE == 0) && (show_mem)) {
    SHOW_MEM = event_type("SHOW_MEM");
    schedule(0.0, SHOW_MEM, GRID, 0, NODE);
  }

}

/************************************************************************
* get_nm : get the n and m indexes for a given theta and phi		*
************************************************************************/
void C_GRIDMAN::get_nm(double theta, double phi, int &n, int &m) {

  n = int(theta / dtheta);
  if (n < 0) {
    fprintf(stderr,"n is %d, theta is %f\n",n,theta);
    n = 0;
  }

  if (n >= N) {

    if (theta > (PI + .001)) {
      fprintf(stderr,"error, theta is out of range %f\n",theta);
    }

    n = N-1;
  }

  m = int(phi / dphi[n]);
  while (m < 0) m += M[n];
  while (m >= M[n]) m -= M[n];

}

/************************************************************************
* get_pt : get phi and theta given lat and lon				*
************************************************************************/
void C_GRIDMAN::get_pt(double lat, double lon, double &theta, double &phi) {

  theta = lat;
  while (theta < -(PI_OVER2)) theta += PI;
  while (theta >= PI_OVER2) theta -= PI;
  theta = PI_OVER2 - theta;

  phi = lon;
  while (phi < 0.0) phi += TWOPI;
  while (phi >= TWOPI) phi -= TWOPI;

}

/************************************************************************
* get_idnd : node and id						*
************************************************************************/
void C_GRIDMAN::get_idnd(double lat, double lon, int &nd, int &gid) {
  int i,n,m;
  double phi,theta;

  get_pt(lat, lon, theta, phi);
  get_nm(theta, phi, n, m);
  i = Mcum[n] + m;

  gid = i / N_NODES;
  nd = i % N_NODES;

  if (i<0) {
    fprintf(stderr,"Error, gridman has negative index %d %d\n",gid,nd);
  }

}

/************************************************************************
* get_gridids : get gridids element nodes and ids			*
************************************************************************/
void C_GRIDMAN::get_gridids(double lat, double lon, double size, C_QUEUE *q) {
  double phi,theta;
  double phi1,phi2;
  double theta1,theta2;
  double dist;
  int i,j,k,l,temp,nd,gid;
  int n,n1,m1,n2,m2;
  C_GRIDID *gridid;

  dist = size;

//...... make sure that the queue is empty

  if (q->length() != 0) fprintf(stderr,"Error, queue not empty for grids\n");

//...... convert lat,lon to theta,phi

  get_pt(lat,lon,theta,phi);

//...... find the max and min values of theta defined by dist

  theta1 = theta - dtheta*dist/d;
  theta2 = theta + dtheta*dist/d;

  n1 = int(theta1/dtheta);
  n2 = int(theta2/dtheta);


  if ((n1 < 0) || (n2 > N-1)) {

    if (n1 < 0) n1 = 0;
    if (n2 > N-1) n2 = N-1;

    for (i=n1; i<=n2; i++) {
      for (j=0; j<M[i]; j++) {
        temp = Mcum[i] + j;

	if ((temp < 0) || (temp >= tot)) {
	  fprintf(stderr,"Error, global id = %d\n",temp);
	}

        gid = temp / N_NODES;
        nd = temp % N_NODES;
        gridid = (C_GRIDID *)RB_FREE_NEW(GRIDID);
//        gridid = (C_GRIDID *)freeobjs.new_object(GRIDID);
        gridid->init(gid,nd);
        q->push_top(gridid);

      }
    }

    return;

  }

//...... loop over the theta indexes covering the phi range for each one

  for (i=n1; i<=n2; i++) {
    theta = (i+0.5)*dtheta;			// choose theta in the middle
    phi1 = phi - dist/(RE*sin(theta));		// determine lo/hi phi values
    phi2 = phi + dist/(RE*sin(theta));

    if (phi2-phi1 >= TWOPI) {
      j = M[i]-1;
      m1 = 0;
    }else{
      get_nm(theta,phi1,n,m1);
      get_nm(theta,phi2,n,m2);

      j = m2-m1;

      if (j<0) j+= M[i];

      if (j >= M[i]) {
        j = M[i]-1;
        m1 = 0;
      }

    }

//...... loop over phi indexes and join target queues together

    for (k=0; k<=j; k++) {
      l = (k+m1) % M[i];
      temp = Mcum[i] + l;

      if ((temp < 0) || (temp >= tot)) {
        fprintf(stderr,"Error, global id = %d\n",temp);
      }

      gid = temp / N_NODES;
      nd = temp % N_NODES;
      gridid = (C_GRIDID *)RB_FREE_NEW(GRIDID);
//      gridid = (C_GRIDID *)freeobjs.new_object(GRIDID);
      gridid->init(gid,nd);
      q->push_top(gridid);

      if (gid < 0) {
	fprintf(stderr,"gid is %d\n",gid);
      }

    }

  }

}

/************************************************************************
* get_gridids : get gridids element nodes and ids			*
************************************************************************/
void C_GRIDMAN::get_gridids(double lat, double lon, double size, C_XQUEUE *q) {
  double phi,theta;
  double phi1,phi2;
  double theta1,theta2;
  double dist;
  int i,j,k,l,temp,nd,gid,glob_gid;
  int n,n1,m1,n2,m2;
  C_GRIDID *gridid;

  dist = size;

//...... set the free list stuff

  RB_DEFINE_FREE(&freeobjs);

//...... make sure that the queue is empty

  if (q->get_length() != 0)
	fprintf(stderr,"Error, queue not empty for grids\n");

//...... convert lat,lon to theta,phi

  get_pt(lat,lon,theta,phi);

//...... find the max and min values of theta defined by dist

  theta1 = theta - dtheta*dist/d;
  theta2 = theta + dtheta*dist/d;

  n1 = int(theta1/dtheta);
  n2 = int(theta2/dtheta);


  if ((n1 < 0) || (n2 > N-1)) {

    if (n1 < 0) n1 = 0;
    if (n2 > N-1) n2 = N-1;

    for (i=n1; i<=n2; i++) {
      for (j=0; j<M[i]; j++) {
        temp = Mcum[i] + j;

	if ((temp < 0) || (temp >= tot)) {
	  fprintf(stderr,"Error, global id = %d\n",temp);
	}

        gid = temp / N_NODES;
        nd = temp % N_NODES;
        gridid = (C_GRIDID *)RB_FREE_NEW(GRIDID);
        gridid->init(gid,nd);
	glob_gid = get_glob_gid(nd,gid);
        gridid->set_id(glob_gid);
        *q += gridid;

      }
    }

    return;

  }

//...... loop over the theta indexes covering the phi range for each one

  for (i=n1; i<=n2; i++) {
    theta = (i+0.5)*dtheta;			// choose theta in the middle
    phi1 = phi - dist/(RE*sin(theta));		// determine lo/hi phi values
    phi2 = phi + dist/(RE*sin(theta));

    if (phi2-phi1 >= TWOPI) {
      j = M[i]-1;
      m1 = 0;
    }else{
      get_nm(theta,phi1,n,m1);
      get_nm(theta,phi2,n,m2);

      j = m2-m1;

      if (j<0) j+= M[i];

      if (j >= M[i]) {
        j = M[i]-1;
        m1 = 0;
      }

    }

//...... loop over phi indexes and join target queues together

    for (k=0; k<=j; k++) {
      l = (k+m1) % M[i];
      temp = Mcum[i] + l;

      if ((temp < 0) || (temp >= tot)) {
        fprintf(stderr,"Error, global id = %d\n",temp);
      }

      gid = temp / N_NODES;
      nd = temp % N_NODES;

      gridid = (C_GRIDID *)RB_FREE_NEW(GRIDID);
      gridid->init(gid,nd);
      glob_gid = get_glob_gid(nd,gid);
      gridid->set_id(glob_gid);
      *q += gridid;

      if (gid < 0) {
	fprintf(stderr,"gid is %d\n",gid);
      }

    }

  }

}

/************************************************************************
* print : print gridman information					*
************************************************************************/
void C_GRIDMAN::print() {
}


