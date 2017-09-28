
/************************************************************************/
/*									*/
/*      File:	m_utilities.C						*/
/*									*/
/*   Purpose:	Various Numerical, Kinematic Utilities For ITW/M	*/
/*									*/
/*      Date:	17 August, 1992						*/
/*									*/
/*     Notes:	This file collects a number of matrix and kinematic	*/
/*		utilities for use throughout the ITW/M tracker.		*/
/*									*/
/*		The general purpose inversion code is stolen from	*/
/*		Numerical Recipies in C					*/
/*									*/
/************************************************************************/

#include "constants.h"			/* ITW/M Tracker Definitions	*/

int ludcmp( double*, int );		/* LU Decomposer		*/
void gimme_ludcmp( double*, double* );	/* LU Decomposition Fetcher	*/
void lubksb( double* );			/* Back-Substitution Driver	*/

/************************************************************************/
/*									*/
/*   Routine:	sinvert_4						*/
/*									*/
/*   Purpose:	Invert A 4x4 Symmetric Matrix				*/
/*									*/
/*     Usage:	int sinvert_4						*/
/*		(							*/
/*		  double *a,	  Input Matrix (Assumed Symmetric)	*/
/*		  double *ain	  Inverse Matrix	       (Output)	*/
/*		)							*/
/*									*/
/************************************************************************/

int sinvert_4(
  double *a,				/* Input Matrix			*/
  double *ain )				/* Inverse Of Input Matrix	*/
{
double m12,m13,m14,m23,m24,m34,det;
int i;
  m12 = a[2]*a[7]-a[3]*a[6];
  m13 = a[2]*a[11]-a[3]*a[10];
  m14 = a[2]*a[15]-a[3]*a[14];
  m23 = a[6]*a[11]-a[7]*a[10];
  m24 = a[6]*a[15]-a[7]*a[14];
  m34 = a[10]*a[15]-a[11]*a[14];
  ain[0] = a[5]*m34-a[9]*m24+a[13]*m23;
  ain[1] = a[9]*m14-a[1]*m34-a[13]*m13;
  ain[2] = a[1]*m24-a[5]*m14+a[13]*m12;
  ain[3] = a[5]*m13-a[1]*m23-a[9]*m12;
  ain[4] = ain[1];
  ain[5] = -a[8]*m14+a[0]*m34+a[12]*m13;
  ain[6] = -a[0]*m24+a[4]*m14-a[12]*m12;
  ain[7] = -a[4]*m13+a[0]*m23+a[8]*m12;
  m12 = a[0]*a[5]-a[1]*a[4];
  m13 = a[0]*a[9]-a[1]*a[8];
  m14 = a[0]*a[13]-a[1]*a[12];
  m23 = a[4]*a[9]-a[5]*a[8];
  m24 = a[4]*a[13]-a[5]*a[12];
  m34 = a[8]*a[13]-a[9]*a[12];
  ain[8] = ain[2];
  ain[9] = ain[6];
  ain[10] = a[3]*m24-a[7]*m14+a[15]*m12;
  ain[11] = a[7]*m13-a[3]*m23-a[11]*m12;
  ain[12] = ain[3];
  ain[13] = ain[7];
  ain[14] = ain[11];
  ain[15] = -a[6]*m13+a[2]*m23+a[10]*m12;

  det = a[0]*ain[0]+a[4]*ain[1]+a[8]*ain[2]+a[12]*ain[3];

  if( abs(det) < ZERO_DET ) return(-1);

  det = 1.0/det;
  for(i=0;i<16;++i) ain[i] *= det;
  return(1);

}

/************************************************************************/
/*									*/
/*   Routine:	sinvert_3						*/
/*  									*/
/*   Purpose:	Dumb 3x3 Symmetric Matrix Inverter			*/
/*									*/
/*     Usage:	int sinvert_3(						*/
/*		  double *matin,	The Matrix			*/
/*		  double *minv )        The Inverse			*/
/*									*/
/*     Notes:	This is not overwhelmingly dumb in that diagonal	*/
/*		entries are scaled out to improve numerical stability.	*/
/*									*/
/************************************************************************/

int sinvert_3(
  double *matin,			/* Initial 3x3 Pos.Def.Symm Mat	*/
  double *minv )			/* The Inverse Matrix		*/
{
double detmn,mat[9],diags[3],term;
int i,j;

  matin[1] = 0.5*( matin[1] + matin[3] );  matin[3] = matin[1];
  matin[2] = 0.5*( matin[2] + matin[6] );  matin[6] = matin[2];
  matin[5] = 0.5*( matin[5] + matin[7] );  matin[7] = matin[5];

  for(i=0;i<3;++i)			/* Get The Scale Factors	*/
  {
    term = matin[4*i];
    if( term <= 0.0 )
    {
      return(-1);
    }
    diags[i] = 1.0/sqrt( term );
  }
  mat[0] = mat[4] = mat[8] = 1.0;	/* Scale The Matrix		*/
  mat[1] = mat[3] = diags[0]*diags[1]*matin[1];
  mat[2] = mat[6] = diags[0]*diags[2]*matin[2];
  mat[5] = mat[7] = diags[1]*diags[2]*matin[5];

  minv[0] = mat[4]*mat[8] - mat[5]*mat[7];
  minv[1] = mat[2]*mat[7] - mat[1]*mat[8];
  minv[2] = mat[1]*mat[5] - mat[2]*mat[4];
  detmn = mat[0]*minv[0] + mat[3]*minv[1] + mat[6]*minv[2];
  if( detmn < ZERO_DET )
  {
    return( -3 );
  }
  minv[4] = mat[0]*mat[8] - mat[2]*mat[6];
  minv[5] = mat[2]*mat[3] - mat[0]*mat[5];
  minv[8] = mat[0]*mat[4] - mat[1]*mat[3];
  minv[3] = minv[1];  minv[6] = minv[2];  minv[7] = minv[5];

  detmn = 1.0/detmn;
  for(i=0;i<9;++i) minv[i] *= detmn;

/* ----- Rescale The Inverse ------------------------------------------	*/

  for(i=0;i<3;++i)
  {
    for(j=0;j<3;++j)
    {
      minv[i+3*j] *= diags[i]*diags[j];
    }
  }
  return(1);

}


#define TINY 1.e-20 			/* Generic Tiny Number		*/

static int BIGGEST_DIM = 0;		/* Largest Prior Matrix		*/
static double *a;			/* Storage For LU Decomposition	*/
static double *vv;			/* Storage For Scalings		*/
static int *indx;			/* Storage For Indices		*/
static int NDIM;			/* Dimension Of Corrent Matrix	*/
static double d;			/* Parity Scale			*/

#define A(I,J) (a[ I + NDIM*(J) ] )	/* Matrix Referencer		*/

/************************************************************************/
/*									*/
/*   Routine:	ludcmp							*/
/*									*/
/*   Purpose:	Construct/Store LU Decomposition Of Matrix		*/
/*									*/
/*     Usage:	int ludcmp						*/
/*		(							*/
/*		  double *amat,	   The Matrix		       (Input)	*/
/*		  int nrows	   Size Of (Square) Matrix     (Input)	*/
/*		)							*/
/*									*/
/*     Notes:	This routine constructs the LU decomposition of the	*/
/*		input matrix into local, static storage for subsequent	*/
/*		use in Matrix inversion, etc.				*/
/*									*/
/*		The routine manages memory allocation for the internal	*/
/*		arrays within this file.				*/
/*									*/
/************************************************************************/

int ludcmp(
  double *amat,				/* Initial Matrix		*/
  int nrows )				/* Number Of Rows		*/
{
int i,imax,j,k;
double big,dum,sum,temp;

  if( nrows<2 )				/* Matrix Too Small !		*/
  {
    fprintf(stderr," LUDCMP: Bad Matrix Size %d\n",nrows);
    return(-1);
  }

  NDIM = nrows;				/* Save Current Dimension	*/
  if( NDIM > BIGGEST_DIM )		/* Need More Memory		*/
  {
    if( BIGGEST_DIM > 0 ) 
    {
      delete indx;  delete vv;  delete a;
    }
    indx = new int[ nrows + 2 ];
    vv = new double[ nrows + 2 ];
    a = new double[ nrows*(nrows+1) +1 ];
    BIGGEST_DIM = NDIM;
  }

  j = NDIM*NDIM;			/* Transfer Input Matrix	*/
  for(i=0;i<j;++i) a[i] = amat[i];

  d = 1.0;				/* Initialize Parity		*/

  for(i=0;i<NDIM;++i)			/* Row Loop : Implicit Scaling	*/
  {
    big = 0.0;
    for(j=0;j<NDIM;++j)			/* Find Biggest Item/Row	*/
    {
      if( (temp = abs(A(i,j))) > big ) big = temp;
    }
    if( big == 0.0 )
    {
      fprintf(stderr," LUDCMP: Singular Matrix Trap 1\n");
      return(-1);
    }
    vv[i] = 1.0/big;			/* Save Scaling Constant	*/
  }					/* End: Initial Row Loop	*/

  for(j=0;j<NDIM;++j)			/* Crout's Column Loop		*/
  {
    for(i=0;i<j;++i)
    {
      sum = A(i,j);  for(k=0;k<i;++k) sum -= A(i,k)*A(k,j);
      A(i,j) = sum;
    }
    big = 0.0;				/* Search For Largest Pivot	*/
    for(i=j;i<NDIM;++i)
    {
      sum = A(i,j);
      for(k=0;k<j;++k) sum -= A(i,k)*A(k,j);
      A(i,j) = sum;
      if( (dum = vv[i]*abs(sum)) >= big ) {big = dum;  imax = i;}
    }
    if( j != imax )			/* Row Interchange Needed	*/
    {
      for(k=0;k<NDIM;++k)
      {
	dum = A(imax,k);  A(imax,k) = A(j,k);  A(j,k) = dum;
      }
      d = -d;				/* Change Parity		*/
      vv[imax] = vv[j];
    }
    indx[j] = imax;
    if( A(j,j) == 0.0 ) A(j,j) = TINY;
    if( j != (NDIM - 1 ) )		/* Divide Through By Pivot	*/
    {
      dum = 1.0/A(j,j);
      for(i=j+1;i<NDIM;++i) A(i,j) *= dum;
    }
  }					/* End Of Crout Column Loop	*/

  return(1);				/* Successful Sign-Off		*/

}

/************************************************************************/
/*									*/
/*   Routine:	gimme_ludcmp						*/
/*									*/
/*   Purpose:	Return L,U Matrices From ludcmp()			*/
/*									*/
/*     Usage:	gimme_ludcmp(lmat,umat)					*/
/*		double *lmat;		L Matrix (alpha)		*/
/*		double *umat;		U Matrix (beta)			*/
/*									*/
/************************************************************************/

void gimme_ludcmp(
  double *lmat,				/* L Matrix			*/
  double *umat )			/* R Matrix			*/
{
int i,j,n2;

  n2 = NDIM*NDIM;
  for(i=0;i<n2;++i) lmat[i] = umat[i] = 0.0;
  for(i=0;i<NDIM;++i)
  {
    for(j=i;j<NDIM;++j) umat[i+NDIM*j] = a[i+NDIM*j];
  }
  for(i=0;i<NDIM;++i)
  { 
    lmat[i*(NDIM+1)] = 1.0;
    for(j=0;j<i;++j) lmat[i+NDIM*j] = a[i+NDIM*j];
  }

}

/************************************************************************/
/*									*/
/*   Routine:	lubksb							*/
/*									*/
/*   Purpose:	Back-Substitution, Using LU Decomposition		*/
/*									*/
/*     Usage:	void lubksb(b)						*/
/*		(							*/
/*		  double *b;	Objective/Solution	       (In/Out)	*/
/*		)							*/
/*									*/
/*     Notes:	This, of course, assumes a prior call to ludcmp()	*/
/*									*/
/************************************************************************/

void lubksb(
double *b )				/* Input/Output Vector		*/
{
int i,ii,ip,j;
double sum;

  ii = -1;
  for(i=0;i<NDIM;++i)
  {
    ip = indx[i];
    sum = b[ip];
    b[ip] = b[i];
    if( ii > -1 )
    {
      for(j=ii;j<i;++j) sum -= A(i,j)*b[j];
    }
    else if( sum ) ii = i;
    b[i] = sum;
  }
  for(i=NDIM-1;i>=0;--i)
  {
    sum = b[i];
    for(j=i+1;j<NDIM;++j) sum -= A(i,j)*b[j];
    b[i] = sum/A(i,i);
  }

}

/************************************************************************/
/*									*/
/*   Routine:	ginv							*/
/*									*/
/*   Purpose:	General Matrix Invertor					*/
/*									*/
/*     Usage:	int ginv						*/
/*		(							*/
/*		  double *amat,		The Matrix	       (Input)	*/
/*		  double *ainv,		The Inverse Matrix     (Output)	*/
/*		  int nrows		Dimension Of Matrix    (Input)	*/
/*		)							*/
/*									*/
/************************************************************************/

ginv(
  double *amat,				/* Input Matrix			*/
  double *ainv,				/* Inverse Matrix		*/
  int nrows )				/* Dimension Of Matrix		*/
{
int i,j,status;
int base;

  status = ludcmp( amat, nrows );
  if( status<0 ) return(status );

  base = 0;
  for(i=0;i<nrows;++i)
  {
    for(j=0;j<nrows;++j) vv[j] = 0.0;
    vv[i] = 1.0;
    lubksb(vv);
    for(j=0;j<nrows;++j) ainv[base+j] = vv[j];
    base += nrows;
  }
  return(1);

}
    
