#include <stdlib.h>
#include <stdio.h>  
#include <time.h>
#include <math.h>

#include "Globals.h"

#define GVERSION        "gforge v1.3a 18MAY96"
#define GAUTHOR         "John Beale <beale@best.com>"
#define V               (void)
#define U               unsigned int

/* Definitions used to address real and imaginary parts in a two-dimensional
   array of complex numbers as stored by fourn(). */

#define Real(v, x, y)  v[1 + (((x) * meshsize) + (y)) * 2]
#define Imag(v, x, y)  v[2 + (((x) * meshsize) + (y)) * 2]

/* Definition for obtaining random numbers. */

#define Nrand 4                       /* Gauss() sample count */
#define Cast(low, high) ((low)+(((high)-(low)) * ((rand() & 0x7FFF) / arand)))


/*  Data types  */

typedef int Boolean;


double sgn1(double x);
float ran1();   /* random number routine */
void seed_ran1(int seed);
/*--------------------------------*-C-*---------------------------------*
 * File:
 *	fftn.h
 * ---------------------------------------------------------------------*
 * Re[]:	real value array
 * Im[]:	imaginary value array
 * nTotal:	total number of complex values
 * nPass:	number of elements involved in this pass of transform
 * nSpan:	nspan/nPass = number of bytes to increment pointer
 *		in Re[] and Im[]
 * isign:	exponent: +1 = forward  -1 = reverse
 * scaling:	normalizing constant by which the final result is *divided*
 *	scaling == -1, normalize by total dimension of the transform
 *	scaling <  -1, normalize by the square-root of the total dimension
 *
 * ----------------------------------------------------------------------
 * See the comments in the code for correct usage!
 */

#ifndef _FFTN_H
#define _FFTN_H

extern void fft_free (void);

/* double precision routine */
extern int fftn (int ndim, const int dims[], double Re[], double Im[],
		 int isign, double scaling);

/* float precision routine */
extern int fftnf (int ndim, const int dims[], float Re[], float Im[],
		  int isign, double scaling);
#endif	/* _FFTN_H */



void f_filter(float *a, int n, double center, double Q, int sign);
void rescale(float **a, int meshsize, float min, float max);
void findpeak(float *a, int meshsize, int *imax, int *jmax);
void fac_warn(int mesh);

/*  Local variables  */

char            output_filename[80];         /* output filename string */
char            input_filename[80];
char            *string;                     /* temp string */
static double   arand, gaussadd, gaussfac;   /* Gaussian random parameters */
static double   fracdim;                     /* Fractal dimension */
static double   powscale;                    /* Power law scaling exponent */
static int      meshsize;                    /* FFT mesh size */
static int      screenxsize, screenysize;    /* output image size in pixels */
static int      xstart, ystart;              /* offset in data array for image */
static float    xfrac, yfrac;                /* peak location in image */
static double   dim[10], dscale[10];         /* 1/f powers and relative scales */
static int      d_factors;                   /* number of 1/f powers defined */
static unsigned int rseed = 3;               /* Current random seed */
static          image_data img;              /* struct for info to file open */
static Boolean  seedspec = FALSE;            /* Did the user specify a seed ? */
static Boolean  peakspec = FALSE;            /* Specified fixed peak location ? */
static Boolean  include_craters = FALSE;     /* add craters to surface */
static Boolean  wrap = TRUE;                 /* wrap craters around at edges */
static double   c_density = 1.0;             /* surface crater density factor */
static double   ch_scale = 1.0;              /* crater amplitude scaling factor */
static Boolean  limitspec = FALSE;           /* set different limits? */
static double   limit_hi = 1.0;
static double   limit_lo = 0.0;              /* limits for scaling before pow() */
static Boolean  bpspec = FALSE;              /* specify bandpass filter ? */
static double   bpcenter, bpQ;               /* bandpass parameters */
static Boolean  brspec = FALSE;              /* specify bandreject fileter ? */
static double   brcenter, brQ;               /* bandreject parameters */
static Boolean  hpspec = FALSE;              /* highpass filter ? */
static double   hpcut, hporder;
static Boolean  lpspec = FALSE;              /* lowpass filter ? */
static double   lpcut, lporder;              /* low-pass cutoff frequency, order */
Boolean         dimspec = FALSE, meshspec = FALSE, powerspec = FALSE,
	        namespec = FALSE, typespec = FALSE, adimspec = FALSE,
	        in_namespec = FALSE;
float           ftmp;                        /* temporary float value */

/*
** Initialise random number generators. As given in Peitgen & Saupe, page 77.
*/
static void initgauss(unsigned int seed)
{
int             stemp;

    /* Range of random generator */
    arand = pow(2.0, 15.0) - 1.0;
    gaussadd = sqrt(3.0 * Nrand);
    gaussfac = 2 * gaussadd / (Nrand * arand);
    /* srand(seed); */
    stemp = seed;                       /* observe that 'seed' is unsigned, ie nonnegative */
    seed_ran1(stemp);                   /* seed the generator */
}

/*
** Return a Gaussian random number. As given in Peitgen & Saupe, page 77.
*/
static double gauss()
{
int             i;
double          sum = 0.0;

    for (i = 1; i <= Nrand; i++) {
	sum += (ran1() * 0x7FFF);
    }
    return gaussfac * sum - gaussadd;
}

/*
** Fill array with 1/f gaussian noise
*/
static void fillarray(float *a, int n, double h, double scale)
{
int             x,y, k, i0, j0, rank, nx, ny, xcent, ycent;
double          rad, phase, rcos, rsin;

    nx = n; ny = n;                     /* horizontal and vertical dimension of array */
    xcent = (int)(nx / 2.0 - 0.5);      /* center dimensions of array */
    ycent = (int)(ny / 2.0 - 0.5);
    printf("filling random array..."); 
    fflush(stdout);
    
    /* fill in mx. in order of radius, so we can generate higher resolutions
       with the same overall aspect (if seed is held fixed) */

    for (rank = 0; rank <= xcent; rank++) {
      /* fill quadrants 2 and 4  */
      for (k=0;k<=rank;k++) {
        x = k; y = rank;
        phase = 2 * M_PI * ((ran1() * 0x7FFF) / arand);
        if ((x == 0) && (y == 0)) rad = 0; 
	  else rad = pow((double) (x*x + y*y), -(h+1) / 2) * gauss();
        rcos = rad * cos(phase)*scale; rsin = rad * sin(phase)*scale;
        Real(a, x, y) += rcos; 
        Imag(a, x, y) += rsin;
        if (!((x == 0) && (y == 0))) { 
	  i0 = nx-x-1; j0 = ny-y-1;
	  Real(a, i0,j0) += rcos;
	  Imag(a, i0,j0) += rsin;
        }

        x = rank; y = k;
        phase = 2 * M_PI * ((ran1() * 0x7FFF) / arand);
        if ((x == 0) && (y == 0)) rad = 0; 
	  else rad = pow((double) (x*x + y*y), -(h+1) / 2) * gauss();
        rcos = rad * cos(phase)*scale; rsin = rad * sin(phase)*scale;
        Real(a, x, y) += rcos;
        Imag(a, x, y) += rsin;
        if (!((x == 0) && (y == 0))) { 
	  i0 = nx-x-1; j0 = ny-y-1;
	  Real(a, i0,j0) += rcos;
	  Imag(a, i0,j0) += rsin;
        }
      } /* end for k */
    
      /* now handle quadrants 1 and 3 */
      for (k=0;k<=rank;k++) {
        x = k; y = rank;
        phase = 2 * M_PI * ((ran1() * 0x7FFF) / arand);
        if ((x == 0) && (y == 0)) rad = 0; 
	  else rad = pow((double) (x*x + y*y), -(h+1) / 2) * gauss();
        rcos = rad * cos(phase)*scale; 
        rsin = rad * sin(phase)*scale;
        Real(a, x, ny-y-1) += rcos; 
        Imag(a, x, ny-y-1) += rsin;
        Real(a, nx-x-1, y) += rcos;
        Imag(a, nx-x-1, y) += rsin;
       
        x = rank; y = k;
        phase = 2 * M_PI * ((ran1() * 0x7FFF) / arand);
        if ((x == 0) && (y == 0)) rad = 0; 
	  else rad = pow((double) (x*x + y*y), -(h+1) / 2) * gauss();
        rcos = rad * cos(phase)*scale; rsin = rad * sin(phase)*scale;
        Real(a, x, ny-y-1) += rcos; 
        Imag(a, x, ny-y-1) += rsin;
        Real(a, nx-x-1, y) += rcos;
        Imag(a, nx-x-1, y) += rsin;
      
      } /* end for k */
    } /* end for rank */
    
    Imag(a, nx / 2, 0) = 0;
    Imag(a, 0, ny / 2) = 0;
    Imag(a, nx / 2, ny / 2) = 0;
} /* end fillarray() */


/*
** Filter array of noise  with peak or notch filter
** since this happens before the inverse FFT, it is
** a frequency-domain filter:
**
**  f_type 1 bpass, -1 breject, 2 lopass, -2 hipass
*/
void f_filter(float *a, int n, double center, double Q, int f_type)
{
int             i, j, i0, j0;
double          rad, fac, p, sfac;
int             xsize, ysize;

    xsize = n;
    ysize = n;
    if (center == 0.0) center = -0.00001;  /* avoid a singularity */

    /* sfac is radius scaling factor */
    sfac = 1.0/sqrt((double)(xsize*xsize/4 + ysize*ysize/4));  
    printf("filtering array..");
    fflush(stdout);
    for (i = 0; i <= n / 2; i++) {          /* do quadrants 2 and 4 */
	for (j = 0; j <= n / 2; j++) {
	    if (i != 0 || j != 0) {
		rad = sqrt((double) (i * i + j * j)) * sfac;
	    } else {
		rad = 0;
	    }
	    p = 1.0 / pow(Q * center, 2);
	    if (abs(f_type)==1)
	      fac = p / (p + pow((1.0-rad/center),2)) ; /* bandpass/rej. */
	    else
	      fac = 1.0 / (1.0 + pow((rad/center),Q) ); /* lo/hi-pass */
	    if (f_type < 0) fac = (1.0 - fac);  /* invert filter */
	    Real(a, i, j) *= fac;
	    Imag(a, i, j) *= fac;
	    i0 = (i == 0) ? 0 : n - i;
	    j0 = (j == 0) ? 0 : n - j;
	    Real(a, i0, j0) *= fac;
	    Imag(a, i0, j0) *= fac;
	}
    }
    printf(".");
    fflush(stdout);
    Imag(a, n / 2, 0) = 0;
    Imag(a, 0, n / 2) = 0;
    Imag(a, n / 2, n / 2) = 0;
    for (i = 1; i <= n / 2 - 1; i++) {     /* do quadrants 1 and 3 */
	for (j = 1; j <= n / 2 - 1; j++) {
	    rad = sfac * sqrt((double) (i * i + j * j));
	    p = 1.0 / pow(Q * center, 2);
	    if (abs(f_type)==1)
	      fac = p / (p + pow((1.0-rad/center),2)) ; /* bandpass/rej. */
	    else
	      fac = 1.0 / (1.0 + pow((rad/center),Q) ); /* lo/hi-pass */
	    if (f_type < 0) fac = (1.0 - fac);  /* invert filter */
	    Real(a, i, n - j) *= fac;
	    Imag(a, i, n - j) *= fac;
	    Real(a, n - i, j) *= fac;
	    Imag(a, n - i, j) *= fac;
	}
    }

} /* end f_filter() */


/*
** Spectrally  synthesised  fractal  motion in two
** dimensions.  This algorithm is given under  the
** name   SpectralSynthesisFM2D  on  page  108  of
** Peitgen & Saupe.
*/
static void spectralsynth(float **x, unsigned int n)
{
int             i,j,nsize[2];
unsigned long   bl;
float           *a;

    bl = ((((unsigned long) n) * n) + 1) * 2 * sizeof(float);

    a = (float *) malloc( (size_t) (n+4)*(n+4)*2 * sizeof(float));
    if (a == (float *) 0) {
	fprintf(stderr,"Cannot allocate %d x %d result array (%lu bytes).\n",
	   n, n, bl);
	exit(1);
	}
    else
       printf("Success allocating %d x %d result array (%lu bytes).\n",
	   n, n, bl);
    *x = a;

    for (j=0;j<n;j++)   {               /* initialize array to zeros */
      for (i=0;i<n;i++)   {
	Real(a,i,j) = 0;
	Imag(a,i,j) = 0;
      }
    }

    /*   printf("factors = %d\n",d_factors); */
    for(i=0; i<d_factors; i++) {
       /*   printf("i:%d pwr:%5.3f scl:%5.3f\n",i,dim[i],dscale[i]); */
       fillarray(a, n, 3.0-dim[i], dscale[i]);   /* put 1/f noise into array */
      }    
      
    if ((i<20) && (j<20)) {
      printf("Real portion of freq. domain array:\n");
      for (j=0;j<n;j++) {
        for (i=0;i<n;i++) {
	  printf("%1.1e ",Real(a,i,j));
        }
        printf("\n");
      }
    }
    if (bpspec) f_filter(a,n,bpcenter,bpQ,1);   /* bandpass filtering */
    if (brspec) f_filter(a,n,brcenter,brQ,-1);  /* bandreject filtering */
    if (lpspec) f_filter(a,n,lpcut,lporder,2);  /* lowpass filter */
    if (hpspec) f_filter(a,n,hpcut,hporder,-2); /* highpass filter */

    nsize[0] = n;
    nsize[1] = n;          /* Dimension of frequency domain array */
    printf("Calculating inverse FFT\n");

			   /* Take inverse 2D Fourier transform */
    fftnf(2, nsize, &Real(a,0,0), &Imag(a,0,0), -2, 1.0);

    
/*    printf("returned from fft\n");
    for (j=0;j<n;j++)   {
      for (i=0;i<n;i++)   {
	printf("%2.3f/%2.3f ", Real(a,i,j),Imag(a,i,j) );
      }
      printf("\n");
    }
  */
}

/*
** Generate initial random seed, if needed.
*/
static void initseed()
{
int             i,stmp;

    stmp = time(NULL) ^ 0xF37C;
			   /* rseed is global var. to store the seed value */
    seed_ran1(stmp);
    for (i=0;i<8;i++) ran1(); 
    rseed = (U)(1000000.0*ran1());
}

/*  PLANET  --  Make the fBm landscape.  */

static Boolean planet(int meshsize)
{
    float *a = (float *) 0;
    int i, j, n;
    double r;
    U num_craters;
    int imax=0, jmax=0;             /* index of maximum picture element */

    if (!seedspec) {
	initseed();     /* stores a seed in global "rseed" */
    }
    initgauss(rseed);

    printf("-seed %d\n",rseed);
    img.rseed = rseed;


		/* generate the height array via IFFT on 1/f scaled noise */
	spectralsynth(&a, meshsize);
	if (a == (float *) 0) {
	    return FALSE;
	}

	n = meshsize;
	printf("rescaling");
	fflush(stdout);

	printf(".");
	fflush(stdout);

	if (limitspec) {        /* rescale to [limit_lo..limit_hi] */
	    rescale(&a, meshsize, limit_lo, limit_hi);
	}  else {
	    rescale(&a, meshsize, 0, 1);
	}
	printf(".");
	fflush(stdout);

	/* Apply power law scaling if non-unity scale is requested. */
	/* also, rescale to[-1..1] for crater program */

/* printf("power = %f\n",powscale); */

	if (powscale != 1.0) {
	    for (i = 0; i < meshsize; i++) {
		for (j = 0; j < meshsize; j++) {
		    r = Real(a, i, j);
		    if (r != 0)
		     Real(a, i, j) = sgn1(r)*pow(fabs(r), powscale);
		}
	    }
	}
	printf(".");
	fflush(stdout);

	  /* rescale all data to lie in the range [-1..1]  */
	rescale(&a, meshsize, -1, 1);
	printf(".");
	fflush(stdout);
    
    if (include_craters) {
	num_craters = (U) (700 * pow(((double)meshsize)/256,0.7)*c_density);
	printf("adding %d craters...", num_craters);
	distribute_craters(a, num_craters, meshsize, wrap, ch_scale);
    }
	rescale(&a, meshsize, 0, 1);            /* rescale to 0..1 */
	findpeak(a, meshsize, &imax, &jmax);     /* find peak location */
	printf("\n");
	
	xstart = imax - (int)((float)screenxsize * xfrac);
	ystart = jmax - (int)((float)screenysize * yfrac);

    genscape(a, meshsize);
    if (a != (float *) 0) {
	free((char *) a);
    }
     return TRUE;
}

void rescale(float **a, int meshsize, float min, float max)
{
int             i,j;
float           r;
float           rmin, rmax, scale, offset;

    rmin = Real(a[0],0,0);
    rmax = rmin;
    for (i = 0; i < meshsize; i++) {     /* find current rmin, rmax */
      for (j = 0; j < meshsize; j++) {
	r = Real(a[0], i, j);
	rmin = min(rmin, r);
        rmax = max(rmax, r);
      }
    }
    scale = (max-min) / (rmax - rmin);
    offset = min;

    /* rescale all data to lie in the range [min..max]  */
    for (i = 0; i < meshsize; i++) {
      for (j = 0; j < meshsize; j++) {
	Real(a[0], i, j) = (scale * (Real(a[0], i, j)-rmin))+ offset;
      }
    }
    /*   printf("\nrescale: min=%f  max = %f\n",rmin,rmax); */
} /* end rescale() */

/*
** Find peak location
*/
void findpeak(float *a, int meshsize, int *imax, int *jmax)
{
int             i,j;
float           r;
float           rmax;

    rmax = Real(a,0,0);
    imax[0] = 0;
    jmax[0] = 0;
    for (i = 0; i < meshsize; i++) {     /* find current rmin, rmax */
      for (j = 0; j < meshsize; j++) {
	r = Real(a, i, j);
	if (r > rmax) {
	  rmax = r;
	  imax[0] = i;
	  jmax[0] = j;
	}
      }
    }
} /* end findpeak() */


/*
** Warn if meshsize is prime or has large prime factor.
*/
void fac_warn(int mesh)
{
int             fac, flimit,num, maxf;

#define div(a,b)  (  (b)*floor((a)/(b)) == (a))

    fac = 2;
    num = mesh;
    maxf = 0;
    flimit = (1+num/2);

    do {
      if (div(num,fac)) {
        num /= fac;
        if (fac > maxf) maxf = fac;
      } else {
        fac++;
      }
    } while (fac < flimit);
    if ((num > 1) && (mesh > 150)) {
	printf("\nAdvisory: your meshsize (%d) is a prime number.\n",mesh);
	printf("If the IFFT is too slow, choose a different meshsize.\n\n");
    } else {
       if (maxf > 150) {
	 printf("\nAdvisory: meshsize %d has a large prime factor (%d).\n",mesh,maxf);
	 printf("If the IFFT is too slow, choose a different meshsize.\n\n");
       }
    } /* end else */
} /* end fac_warn() */


/*  MAIN  --  Main gforge program: read command line, set vars.  */

int main(int argc, char **argv)
{
int i;
char *usage = "  Usage: \n\



-------------------------- */

    fac_warn(meshsize); /* warn if meshsize has large prime factor */

      /* Set defaults when explicit specifications were not given.  */

    if (!dimspec) {
	if (adimspec) pperror("use of -adim requires also the -dim option\n");
	dim[0] = 2.15;
	dscale[0] = 1.0;
	d_factors = 1;      /* number of powers defined */

	fracdim = 2.15;
    }
    if (!powerspec) {
	powscale = 1.2;
    }
    if (!meshspec) {
	meshsize = 128;
      }
    if (!typespec) {
	pp_filetype = TGA;
      }
    if (!namespec) {
	if ((pp_filetype == PGM) || (pp_filetype == PG8)  )
	  strcpy(output_filename,"output.pgm");
	else if (pp_filetype == TGA)
	  strcpy(output_filename,"output.tga");
	else if (pp_filetype == PNG)
	  strcpy(output_filename,"output.png");
	else if (pp_filetype == OCT)
	  strcpy(output_filename,"output.oct");
	else if (pp_filetype == MAT)
	  strcpy(output_filename,"output.mat");
	else strcpy(output_filename,"output.dat");  /* unknown type (!?) */
      }

    if (!wrap && peakspec) {
      pperror("-wrapoff and -peak options are mutually exclusive!\n");
    }
    
    screenxsize = meshsize;
    screenysize = meshsize;

    /* print out parameters so we know how we generated this file */

    printf("Parameters: -dim %.2f -power %.2f -mesh %d -name %s\n",
	    dim[0], powscale, meshsize, output_filename);
    
    if (adimspec) {
      for (i=1;i<d_factors;i++) {
       printf("-ad %0.2f %0.2f ",dim[i],dscale[i]);
       }
      }
    
    if (include_craters)
      printf("-craters %1.1f %1.2f ",c_density,ch_scale);

   if (!wrap)
       printf("-wrapoff ");

   if (!wrap && !include_craters) 
       printf("(-wrapoff option applies only to craters!) ");

   if (peakspec) 
       printf("-peak %0.2f %0.2f \n",xfrac,yfrac);
    
    if (limitspec)
       printf("-limit %1.2f %1.2f ",limit_lo, limit_hi);

    if (bpspec) printf("-bp %1.2f %1.1f ",bpcenter,bpQ);
    if (brspec) printf("-br %1.2f %1.1f ",brcenter,brQ);
    if (lpspec) printf("-lp %1.2f %1.1f ",lpcut,lporder);
    if (hpspec) printf("-hp %1.2f %1.1f ",hpcut,hporder);

    img.dim= (double *)malloc(10*sizeof(double));
    img.dscale = (double *)malloc(10*sizeof(double));
		       
    for (i=0;i<d_factors;i++) {
      img.dim[i] = dim[i];
      img.dscale[i] = dscale[i];
    }
    strcpy(img.fname,output_filename);
    img.type = pp_filetype;
    img.powscale = powscale;
    img.xfrac = xfrac;
    img.yfrac = yfrac;
    img.rseed = rseed;
    img.d_factors = d_factors;
    img.peakspec = peakspec;
    img.adimspec = adimspec;
    img.meshsize = meshsize;
    img.craterspec = include_craters;
    img.craterdens = c_density;
    img.craterscale = ch_scale;
    img.limitspec = limitspec;
    img.limit_hi = limit_hi;
    img.limit_lo = limit_lo;

    return(planet(meshsize) ? 0 : 1);   /* run the algorithm; output results */

} /* end main() */
/*--------------------------------*-C-*---------------------------------*
 * File:
 *	fftn.c
 *
 * Public:
 *	fft_free ();
 *	fftn / fftnf ();
 *
 * Private:
 *	fftradix / fftradixf ();
 *
 * Descript:
 *	multivariate complex Fourier transform, computed in place
 *	using mixed-radix Fast Fourier Transform algorithm.
 *
 *	Fortran code by:
 *	RC Singleton, Stanford Research Institute, Sept. 1968
 *
 *	translated by f2c (version 19950721).
 *
 * Revisions:
 * 26 July 95	John Beale
 *	- added maxf and maxp as parameters to fftradix()
 *
 * 28 July 95	Mark Olesen <olesen@me.queensu.ca>
 *	- cleaned-up the Fortran 66 goto spaghetti, only 3 labels remain.
 *
 *	- added fft_free() to provide some measure of control over
 *	  allocation/deallocation.
 *
 *	- added fftn() wrapper for multidimensional FFTs
 *
 *	- use -DFFT_NOFLOAT or -DFFT_NODOUBLE to avoid compiling that
 *	  precision. Note suffix `f' on the function names indicates
 *	  float precision.
 *
 *	- revised documentation
 *
 * 31 July 95	Mark Olesen <olesen@me.queensu.ca>
 *	- added GNU Public License
 *	- more cleanup
 *	- define SUN_BROKEN_REALLOC to use malloc() instead of realloc()
 *	  on the first pass through, apparently needed for old libc
 *	- removed #error directive in favour of some code that simply
 *	  won't compile (generate an error that way)
 *
 * 1 Aug 95	Mark Olesen <olesen@me.queensu.ca>
 *	- define FFT_RADIX4 to only have radix 2, radix 4 transforms
 *	- made fftradix /fftradixf () static scope, just use fftn()
 *	  instead.  If you have good ideas about fixing the factors
 *	  in fftn() please do so.
 *
 * 8 Jan 95	mj olesen <olesen@me.queensu.ca>
 *	- fixed typo's, including one that broke scaling for scaling by
 *	  total number of matrix elements or the square root of same
 *	- removed unnecessary casts from allocations
 *
 * ======================================================================*
 * NIST Guide to Available Math Software.
 * Source for module FFT from package GO.
 * Retrieved from NETLIB on Wed Jul  5 11:50:07 1995.
 * ======================================================================*
 *
 *-----------------------------------------------------------------------*
 *
 * int fftn (int ndim, const int dims[], REAL Re[], REAL Im[],
 *	    int iSign, double scaling);
 *
 * NDIM = the total number dimensions
 * DIMS = a vector of array sizes
 *	if NDIM is zero then DIMS must be zero-terminated
 *
 * RE and IM hold the real and imaginary components of the data, and return
 * the resulting real and imaginary Fourier coefficients.  Multidimensional
 * data *must* be allocated contiguously.  There is no limit on the number
 * of dimensions.
 *
 * ISIGN = the sign of the complex exponential (ie, forward or inverse FFT)
 *	the magnitude of ISIGN (normally 1) is used to determine the
 *	correct indexing increment (see below).
 *
 * SCALING = normalizing constant by which the final result is *divided*
 *	if SCALING == -1, normalize by total dimension of the transform
 *	if SCALING <  -1, normalize by the square-root of the total dimension
 *
 * example:
 * tri-variate transform with Re[n1][n2][n3], Im[n1][n2][n3]
 *
 *	int dims[3] = {n1,n2,n3}
 *	fftn (3, dims, Re, Im, 1, scaling);
 *
 *-----------------------------------------------------------------------*
 * int fftradix (REAL Re[], REAL Im[], size_t nTotal, size_t nPass,
 *		 size_t nSpan, int iSign, size_t max_factors,
 *		 size_t max_perm);
 *
 * RE, IM - see above documentation
 *
 * Although there is no limit on the number of dimensions, fftradix() must
 * be called once for each dimension, but the calls may be in any order.
 *
 * NTOTAL = the total number of complex data values
 * NPASS  = the dimension of the current variable
 * NSPAN/NPASS = the spacing of consecutive data values while indexing the
 *	current variable
 * ISIGN - see above documentation
 *
 * example:
 * tri-variate transform with Re[n1][n2][n3], Im[n1][n2][n3]
 *
 *	fftradix (Re, Im, n1*n2*n3, n1,       n1, 1, maxf, maxp);
 *	fftradix (Re, Im, n1*n2*n3, n2,    n1*n2, 1, maxf, maxp);
 *	fftradix (Re, Im, n1*n2*n3, n3, n1*n2*n3, 1, maxf, maxp);
 *
 * single-variate transform,
 *    NTOTAL = N = NSPAN = (number of complex data values),
 *
 *	fftradix (Re, Im, n, n, n, 1, maxf, maxp);
 *
 * The data can also be stored in a single array with alternating real and
 * imaginary parts, the magnitude of ISIGN is changed to 2 to give correct
 * indexing increment, and data [0] and data [1] used to pass the initial
 * addresses for the sequences of real and imaginary values,
 *
 * example:
 *	REAL data [2*NTOTAL];
 *	fftradix ( &data[0], &data[1], NTOTAL, nPass, nSpan, 2, maxf, maxp);
 *
 * for temporary allocation:
 *
 * MAX_FACTORS	>= the maximum prime factor of NPASS
 * MAX_PERM	>= the number of prime factors of NPASS.  In addition,
 *	if the square-free portion K of NPASS has two or more prime
 *	factors, then MAX_PERM >= (K-1)
 *
 * storage in FACTOR for a maximum of 15 prime factors of NPASS. if NPASS
 * has more than one square-free factor, the product of the square-free
 * factors must be <= 210 array storage for maximum prime factor of 23 the
 * following two constants should agree with the array dimensions.
 *
 *-----------------------------------------------------------------------*
 *
 * void fft_free (void);
 *
 * free-up allocated temporary storage after finished all the Fourier
 * transforms.
 *
 *----------------------------------------------------------------------*/
#ifndef _FFTN_C
#define _FFTN_C
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "fftn.h"

/* double precision routine */
static int
fftradix (double Re[], double Im[],
	  size_t nTotal, size_t nPass, size_t nSpan, int isign,
	  int max_factors, int max_perm);

/* float precision routine */
static int
fftradixf (float Re[], float Im[],
	   size_t nTotal, size_t nPass, size_t nSpan, int isign,
	   int max_factors, int max_perm);

/* parameters for memory management */

static size_t SpaceAlloced = 0;
static size_t MaxPermAlloced = 0;

/* temp space, (void *) since both float and double routines use it */
static void *Tmp0 = NULL;	/* temp space for real part */
static void *Tmp1 = NULL;	/* temp space for imaginary part */
static void *Tmp2 = NULL;	/* temp space for Cosine values */
static void *Tmp3 = NULL;	/* temp space for Sine values */
static int  *Perm = NULL;	/* Permutation vector */

#define NFACTOR	11
static int factor [NFACTOR];

void
fft_free (void)
{
   SpaceAlloced = MaxPermAlloced = 0;
   if (Tmp0 != NULL) { free (Tmp0); Tmp0 = NULL; }
   if (Tmp1 != NULL) { free (Tmp1); Tmp1 = NULL; }
   if (Tmp2 != NULL) { free (Tmp2); Tmp2 = NULL; }
   if (Tmp3 != NULL) { free (Tmp3); Tmp3 = NULL; }
   if (Perm != NULL) { free (Perm); Perm = NULL; }
}

#if !defined (__FILE__) && !defined (lint)
Error: your compiler is sick!  define __FILE__ yourself (a string)
eg, something like -D__FILE__=\"fftn.c\"
#endif

#ifndef M_PI
# define M_PI	3.14159265358979323846264338327950288
#endif

#ifndef SIN60
# define SIN60	0.86602540378443865	/* sin(60 deg) */
# define COS72	0.30901699437494742	/* cos(72 deg) */
# define SIN72	0.95105651629515357	/* sin(72 deg) */
#endif

/* re-include this source file on the second pass through */
#undef REAL
#undef FFTN
#undef FFTNS
#undef FFTRADIX
#undef FFTRADIXS

#ifndef FFT_NOFLOAT
# define REAL		float
# define FFTN		fftnf		/* trailing 'f' for float */
# define FFTNS		"fftnf"		/* name for error message */
# define FFTRADIX	fftradixf	/* trailing 'f' for float */
# define FFTRADIXS	"fftradixf"	/* name for error message */
# include __FILE__			/* include this file again */
#endif

#undef REAL
#undef FFTN
#undef FFTNS
#undef FFTRADIX
#undef FFTRADIXS

#ifndef FFT_NODOUBLE
# define REAL		double
# define FFTN		fftn
# define FFTNS		"fftn"
# define FFTRADIX	fftradix
# define FFTRADIXS	"fftradix"
# include __FILE__			/* include this file again */
#endif

#if defined (FFT_NOFLOAT) && defined (FFT_NODOUBLE) && !defined (lint)
Error: cannot have both -DFFT_NOFLOAT and -DFFT_NODOUBLE
#endif
#else	/* _FFTN_C */

/*
 *
 */
int
FFTN (int ndim, const int dims[],
      REAL Re [],
      REAL Im [],
      int iSign,
      double scaling)
{
   size_t nSpan, nPass, nTotal;
   int ret, i, max_factors, max_perm;

   /*
    * tally the number of elements in the data array
    * and determine the number of dimensions
    */
   nTotal = 1;
   if (ndim && dims [0])
     {
	for (i = 0; i < ndim; i++)
	  {
	     if (dims [i] <= 0)
	       {
		  fputs ("Error: " FFTNS "() - dimension error\n", stderr);
		  fft_free ();	/* free-up memory */
		  return -1;
	       }
	     nTotal *= dims [i];
	  }
     }
   else
     {
	ndim = 0;
	for (i = 0; dims [i]; i++)
	  {
	     if (dims [i] <= 0)
	       {
		  fputs ("Error: " FFTNS "() - dimension error\n", stderr);
		  fft_free ();	/* free-up memory */
		  return -1;
	       }
	     nTotal *= dims [i];
	     ndim++;
	  }
     }

   /* determine maximum number of factors and permuations */
#if 1
   /*
    * follow John Beale's example, just use the largest dimension and don't
    * worry about excess allocation.  May be someone else will do it?
    */
   max_factors = max_perm = 1;
   for (i = 0; i < ndim; i++)
     {
	nSpan = dims [i];
	if (nSpan > max_factors) max_factors = nSpan;
	if (nSpan > max_perm) max_perm = nSpan;
     }
#else
   /* use the constants used in the original Fortran code */
   max_factors = 23;
   max_perm = 209;
#endif
   /* loop over the dimensions: */
   nSpan = 1;
   for (i = 0; i < ndim; i++)
     {
	nPass = dims [i];
	nSpan *= nPass;
	ret = FFTRADIX (Re, Im, nTotal, nPass, nSpan, iSign,
			max_factors, max_perm);
	/* exit, clean-up already done */
	if (ret)
	  return ret;
     }

   /* Divide through by the normalizing constant: */
   if (scaling && scaling != 1.0)
     {
	if (iSign < 0) iSign = -iSign;
	if (scaling < 0.0)
	  scaling = (scaling < -1.0) ? sqrt (nTotal) : nTotal;
	scaling = 1.0 / scaling;	/* multiply is often faster */
	for (i = 0; i < nTotal; i += iSign)
	  {
	     Re [i] *= scaling;
	     Im [i] *= scaling;
	  }
     }
   return 0;
}

/*----------------------------------------------------------------------*/

/*
 * singleton's mixed radix routine
 *
 * could move allocation out to fftn(), but leave it here so that it's
 * possible to make this a standalone function
 */
static int
FFTRADIX (REAL Re[],
	  REAL Im[],
	  size_t nTotal,
	  size_t nPass,
	  size_t nSpan,
	  int iSign,
	  int max_factors,
	  int max_perm)
{
   int ii, mfactor, kspan, ispan, inc;
   int j, jc, jf, jj, k, k1, k2, k3, k4, kk, kt, nn, ns, nt;

   REAL radf;
   REAL c1, c2, c3, cd, aa, aj, ak, ajm, ajp, akm, akp;
   REAL s1, s2, s3, sd, bb, bj, bk, bjm, bjp, bkm, bkp;

   REAL *Rtmp = NULL;	/* temp space for real part*/
   REAL *Itmp = NULL;	/* temp space for imaginary part */
   REAL *Cos = NULL;	/* Cosine values */
   REAL *Sin = NULL;	/* Sine values */

   REAL s60 = SIN60;		/* sin(60 deg) */
   REAL c72 = COS72;		/* cos(72 deg) */
   REAL s72 = SIN72;		/* sin(72 deg) */
   REAL pi2 = M_PI;		/* use PI first, 2 PI later */

   /* gcc complains about k3 being uninitialized, but I can't find out where
    * or why ... it looks okay to me.
    *
    * initialize to make gcc happy
    */
   k3 = 0;

   /* gcc complains about c2, c3, s2,s3 being uninitialized, but they're
    * only used for the radix 4 case and only AFTER the (s1 == 0.0) pass
    * through the loop at which point they will have been calculated.
    *
    * initialize to make gcc happy
    */
   c2 = c3 = s2 = s3 = 0.0;

   /* Parameter adjustments, was fortran so fix zero-offset */
   Re--;
   Im--;

   if (nPass < 2)
     return 0;

   /*  allocate storage */
   if (SpaceAlloced < max_factors * sizeof (REAL))
     {
#ifdef SUN_BROKEN_REALLOC
	if (!SpaceAlloced)	/* first time */
	  {
	     SpaceAlloced = max_factors * sizeof (REAL);
	     Tmp0 = malloc (SpaceAlloced);
	     Tmp1 = malloc (SpaceAlloced);
	     Tmp2 = malloc (SpaceAlloced);
	     Tmp3 = malloc (SpaceAlloced);
	  }
	else
	  {
#endif
	     SpaceAlloced = max_factors * sizeof (REAL);
	     Tmp0 = realloc (Tmp0, SpaceAlloced);
	     Tmp1 = realloc (Tmp1, SpaceAlloced);
	     Tmp2 = realloc (Tmp2, SpaceAlloced);
	     Tmp3 = realloc (Tmp3, SpaceAlloced);
#ifdef SUN_BROKEN_REALLOC
	  }
#endif
     }
   else
     {
	/* allow full use of alloc'd space */
	max_factors = SpaceAlloced / sizeof (REAL);
     }
   if (MaxPermAlloced < max_perm)
     {
#ifdef SUN_BROKEN_REALLOC
	if (!MaxPermAlloced)	/* first time */
	  Perm = malloc (max_perm * sizeof(int));
	else
#endif
	  Perm = realloc (Perm, max_perm * sizeof(int));
	MaxPermAlloced = max_perm;
     }
   else
     {
	/* allow full use of alloc'd space */
	max_perm = MaxPermAlloced;
     }
   if (Tmp0 == NULL || Tmp1 == NULL || Tmp2 == NULL || Tmp3 == NULL
       || Perm == NULL)
     goto Memory_Error_Label;

   /* assign pointers */
   Rtmp = (REAL *) Tmp0;
   Itmp = (REAL *) Tmp1;
   Cos  = (REAL *) Tmp2;
   Sin  = (REAL *) Tmp3;

   /*
    * Function Body
    */
   inc = iSign;
   if (iSign < 0) {
      s72 = -s72;
      s60 = -s60;
      pi2 = -pi2;
      inc = -inc;		/* absolute value */
   }

   /* adjust for strange increments */
   nt = inc * nTotal;
   ns = inc * nSpan;
   kspan = ns;

   nn = nt - inc;
   jc = ns / nPass;
   radf = pi2 * (double) jc;
   pi2 *= 2.0;			/* use 2 PI from here on */

   ii = 0;
   jf = 0;
   /*  determine the factors of n */
   mfactor = 0;
   k = nPass;
   while (k % 16 == 0) {
      mfactor++;
      factor [mfactor - 1] = 4;
      k /= 16;
   }
   j = 3;
   jj = 9;
   do {
      while (k % jj == 0) {
	 mfactor++;
	 factor [mfactor - 1] = j;
	 k /= jj;
      }
      j += 2;
      jj = j * j;
   } while (jj <= k);
   if (k <= 4) {
      kt = mfactor;
      factor [mfactor] = k;
      if (k != 1)
	mfactor++;
   } else {
      if (k - (k / 4 << 2) == 0) {
	 mfactor++;
	 factor [mfactor - 1] = 2;
	 k /= 4;
      }
      kt = mfactor;
      j = 2;
      do {
	 if (k % j == 0) {
	    mfactor++;
	    factor [mfactor - 1] = j;
	    k /= j;
	 }
	 j = ((j + 1) / 2 << 1) + 1;
      } while (j <= k);
   }
   if (kt) {
      j = kt;
      do {
	 mfactor++;
	 factor [mfactor - 1] = factor [j - 1];
	 j--;
      } while (j);
   }

   /* test that mfactors is in range */
   if (mfactor > NFACTOR)
     {
	fputs ("Error: " FFTRADIXS "() - exceeded number of factors\n", stderr);
	goto Memory_Error_Label;
      }

   /* compute fourier transform */
   for (;;) {
      sd = radf / (double) kspan;
      cd = sin(sd);
      cd = 2.0 * cd * cd;
      sd = sin(sd + sd);
      kk = 1;
      ii++;

      switch (factor [ii - 1]) {
       case 2:
	 /* transform for factor of 2 (including rotation factor) */
	 kspan /= 2;
	 k1 = kspan + 2;
	 do {
	    do {
	       k2 = kk + kspan;
	       ak = Re [k2];
	       bk = Im [k2];
	       Re [k2] = Re [kk] - ak;
	       Im [k2] = Im [kk] - bk;
	       Re [kk] += ak;
	       Im [kk] += bk;
	       kk = k2 + kspan;
	    } while (kk <= nn);
	    kk -= nn;
	 } while (kk <= jc);
	 if (kk > kspan)
	   goto Permute_Results_Label;		/* exit infinite loop */
	 do {
	    c1 = 1.0 - cd;
	    s1 = sd;
	    do {
	       do {
		  do {
		     k2 = kk + kspan;
		     ak = Re [kk] - Re [k2];
		     bk = Im [kk] - Im [k2];
		     Re [kk] += Re [k2];
		     Im [kk] += Im [k2];
		     Re [k2] = c1 * ak - s1 * bk;
		     Im [k2] = s1 * ak + c1 * bk;
		     kk = k2 + kspan;
		  } while (kk < nt);
		  k2 = kk - nt;
		  c1 = -c1;
		  kk = k1 - k2;
	       } while (kk > k2);
	       ak = c1 - (cd * c1 + sd * s1);
	       s1 = sd * c1 - cd * s1 + s1;
	       c1 = 2.0 - (ak * ak + s1 * s1);
	       s1 *= c1;
	       c1 *= ak;
	       kk += jc;
	    } while (kk < k2);
	    k1 += inc + inc;
	    kk = (k1 - kspan) / 2 + jc;
	 } while (kk <= jc + jc);
	 break;

       case 4:			/* transform for factor of 4 */
	 ispan = kspan;
	 kspan /= 4;

	 do {
	    c1 = 1.0;
	    s1 = 0.0;
	    do {
	       do {
		  k1 = kk + kspan;
		  k2 = k1 + kspan;
		  k3 = k2 + kspan;
		  akp = Re [kk] + Re [k2];
		  akm = Re [kk] - Re [k2];
		  ajp = Re [k1] + Re [k3];
		  ajm = Re [k1] - Re [k3];
		  bkp = Im [kk] + Im [k2];
		  bkm = Im [kk] - Im [k2];
		  bjp = Im [k1] + Im [k3];
		  bjm = Im [k1] - Im [k3];
		  Re [kk] = akp + ajp;
		  Im [kk] = bkp + bjp;
		  ajp = akp - ajp;
		  bjp = bkp - bjp;
		  if (iSign < 0) {
		     akp = akm + bjm;
		     bkp = bkm - ajm;
		     akm -= bjm;
		     bkm += ajm;
		  } else {
		     akp = akm - bjm;
		     bkp = bkm + ajm;
		     akm += bjm;
		     bkm -= ajm;
		  }
		  /* avoid useless multiplies */
		  if (s1 == 0.0) {
		     Re [k1] = akp;
		     Re [k2] = ajp;
		     Re [k3] = akm;
		     Im [k1] = bkp;
		     Im [k2] = bjp;
		     Im [k3] = bkm;
		  } else {
		     Re [k1] = akp * c1 - bkp * s1;
		     Re [k2] = ajp * c2 - bjp * s2;
		     Re [k3] = akm * c3 - bkm * s3;
		     Im [k1] = akp * s1 + bkp * c1;
		     Im [k2] = ajp * s2 + bjp * c2;
		     Im [k3] = akm * s3 + bkm * c3;
		  }
		  kk = k3 + kspan;
	       } while (kk <= nt);

	       c2 = c1 - (cd * c1 + sd * s1);
	       s1 = sd * c1 - cd * s1 + s1;
	       c1 = 2.0 - (c2 * c2 + s1 * s1);
	       s1 *= c1;
	       c1 *= c2;
	       /* values of c2, c3, s2, s3 that will get used next time */
	       c2 = c1 * c1 - s1 * s1;
	       s2 = 2.0 * c1 * s1;
	       c3 = c2 * c1 - s2 * s1;
	       s3 = c2 * s1 + s2 * c1;
	       kk = kk - nt + jc;
	    } while (kk <= kspan);
	    kk = kk - kspan + inc;
	 } while (kk <= jc);
	 if (kspan == jc)
	   goto Permute_Results_Label;		/* exit infinite loop */
	 break;

       default:
	 /*  transform for odd factors */
#ifdef FFT_RADIX4
	 fputs ("Error: " FFTRADIXS "(): compiled for radix 2/4 only\n", stderr);
	 fft_free ();		/* free-up memory */
	 return -1;
	 break;
#else	/* FFT_RADIX4 */
	 k = factor [ii - 1];
	 ispan = kspan;
	 kspan /= k;

	 switch (k) {
	  case 3:	/* transform for factor of 3 (optional code) */
	    do {
	       do {
		  k1 = kk + kspan;
		  k2 = k1 + kspan;
		  ak = Re [kk];
		  bk = Im [kk];
		  aj = Re [k1] + Re [k2];
		  bj = Im [k1] + Im [k2];
		  Re [kk] = ak + aj;
		  Im [kk] = bk + bj;
		  ak -= 0.5 * aj;
		  bk -= 0.5 * bj;
		  aj = (Re [k1] - Re [k2]) * s60;
		  bj = (Im [k1] - Im [k2]) * s60;
		  Re [k1] = ak - bj;
		  Re [k2] = ak + bj;
		  Im [k1] = bk + aj;
		  Im [k2] = bk - aj;
		  kk = k2 + kspan;
	       } while (kk < nn);
	       kk -= nn;
	    } while (kk <= kspan);
	    break;

	  case 5:	/*  transform for factor of 5 (optional code) */
	    c2 = c72 * c72 - s72 * s72;
	    s2 = 2.0 * c72 * s72;
	    do {
	       do {
		  k1 = kk + kspan;
		  k2 = k1 + kspan;
		  k3 = k2 + kspan;
		  k4 = k3 + kspan;
		  akp = Re [k1] + Re [k4];
		  akm = Re [k1] - Re [k4];
		  bkp = Im [k1] + Im [k4];
		  bkm = Im [k1] - Im [k4];
		  ajp = Re [k2] + Re [k3];
		  ajm = Re [k2] - Re [k3];
		  bjp = Im [k2] + Im [k3];
		  bjm = Im [k2] - Im [k3];
		  aa = Re [kk];
		  bb = Im [kk];
		  Re [kk] = aa + akp + ajp;
		  Im [kk] = bb + bkp + bjp;
		  ak = akp * c72 + ajp * c2 + aa;
		  bk = bkp * c72 + bjp * c2 + bb;
		  aj = akm * s72 + ajm * s2;
		  bj = bkm * s72 + bjm * s2;
		  Re [k1] = ak - bj;
		  Re [k4] = ak + bj;
		  Im [k1] = bk + aj;
		  Im [k4] = bk - aj;
		  ak = akp * c2 + ajp * c72 + aa;
		  bk = bkp * c2 + bjp * c72 + bb;
		  aj = akm * s2 - ajm * s72;
		  bj = bkm * s2 - bjm * s72;
		  Re [k2] = ak - bj;
		  Re [k3] = ak + bj;
		  Im [k2] = bk + aj;
		  Im [k3] = bk - aj;
		  kk = k4 + kspan;
	       } while (kk < nn);
	       kk -= nn;
	    } while (kk <= kspan);
	    break;

	  default:
	    if (k != jf) {
	       jf = k;
	       s1 = pi2 / (double) k;
	       c1 = cos(s1);
	       s1 = sin(s1);
	       if (jf > max_factors)
		 goto Memory_Error_Label;
	       Cos [jf - 1] = 1.0;
	       Sin [jf - 1] = 0.0;
	       j = 1;
	       do {
		  Cos [j - 1] = Cos [k - 1] * c1 + Sin [k - 1] * s1;
		  Sin [j - 1] = Cos [k - 1] * s1 - Sin [k - 1] * c1;
		  k--;
		  Cos [k - 1] = Cos [j - 1];
		  Sin [k - 1] = -Sin [j - 1];
		  j++;
	       } while (j < k);
	    }
	    do {
	       do {
		  k1 = kk;
		  k2 = kk + ispan;
		  ak = aa = Re [kk];
		  bk = bb = Im [kk];
		  j = 1;
		  k1 += kspan;
		  do {
		     k2 -= kspan;
		     j++;
		     Rtmp [j - 1] = Re [k1] + Re [k2];
		     ak += Rtmp [j - 1];
		     Itmp [j - 1] = Im [k1] + Im [k2];
		     bk += Itmp [j - 1];
		     j++;
		     Rtmp [j - 1] = Re [k1] - Re [k2];
		     Itmp [j - 1] = Im [k1] - Im [k2];
		     k1 += kspan;
		  } while (k1 < k2);
		  Re [kk] = ak;
		  Im [kk] = bk;
		  k1 = kk;
		  k2 = kk + ispan;
		  j = 1;
		  do {
		     k1 += kspan;
		     k2 -= kspan;
		     jj = j;
		     ak = aa;
		     bk = bb;
		     aj = 0.0;
		     bj = 0.0;
		     k = 1;
		     do {
			k++;
			ak += Rtmp [k - 1] * Cos [jj - 1];
			bk += Itmp [k - 1] * Cos [jj - 1];
			k++;
			aj += Rtmp [k - 1] * Sin [jj - 1];
			bj += Itmp [k - 1] * Sin [jj - 1];
			jj += j;
			if (jj > jf) {
			   jj -= jf;
			}
		     } while (k < jf);
		     k = jf - j;
		     Re [k1] = ak - bj;
		     Im [k1] = bk + aj;
		     Re [k2] = ak + bj;
		     Im [k2] = bk - aj;
		     j++;
		  } while (j < k);
		  kk += ispan;
	       } while (kk <= nn);
	       kk -= nn;
	    } while (kk <= kspan);
	    break;
	 }
	 /*  multiply by rotation factor (except for factors of 2 and 4) */
	 if (ii == mfactor)
	   goto Permute_Results_Label;		/* exit infinite loop */
	 kk = jc + 1;
	 do {
	    c2 = 1.0 - cd;
	    s1 = sd;
	    do {
	       c1 = c2;
	       s2 = s1;
	       kk += kspan;
	       do {
		  do {
		     ak = Re [kk];
		     Re [kk] = c2 * ak - s2 * Im [kk];
		     Im [kk] = s2 * ak + c2 * Im [kk];
		     kk += ispan;
		  } while (kk <= nt);
		  ak = s1 * s2;
		  s2 = s1 * c2 + c1 * s2;
		  c2 = c1 * c2 - ak;
		  kk = kk - nt + kspan;
	       } while (kk <= ispan);
	       c2 = c1 - (cd * c1 + sd * s1);
	       s1 += sd * c1 - cd * s1;
	       c1 = 2.0 - (c2 * c2 + s1 * s1);
	       s1 *= c1;
	       c2 *= c1;
	       kk = kk - ispan + jc;
	    } while (kk <= kspan);
	    kk = kk - kspan + jc + inc;
	 } while (kk <= jc + jc);
	 break;
#endif	/* FFT_RADIX4 */
      }
   }

/*  permute the results to normal order---done in two stages */
/*  permutation for square factors of n */
Permute_Results_Label:
   Perm [0] = ns;
   if (kt) {
      k = kt + kt + 1;
      if (mfactor < k)
	k--;
      j = 1;
      Perm [k] = jc;
      do {
	 Perm [j] = Perm [j - 1] / factor [j - 1];
	 Perm [k - 1] = Perm [k] * factor [j - 1];
	 j++;
	 k--;
      } while (j < k);
      k3 = Perm [k];
      kspan = Perm [1];
      kk = jc + 1;
      k2 = kspan + 1;
      j = 1;
      if (nPass != nTotal) {
/*  permutation for multivariate transform */
Permute_Multi_Label:
	 do {
	    do {
	       k = kk + jc;
	       do {
		  /* swap Re [kk] <> Re [k2], Im [kk] <> Im [k2] */
		  ak = Re [kk]; Re [kk] = Re [k2]; Re [k2] = ak;
		  bk = Im [kk]; Im [kk] = Im [k2]; Im [k2] = bk;
		  kk += inc;
		  k2 += inc;
	       } while (kk < k);
	       kk += ns - jc;
	       k2 += ns - jc;
	    } while (kk < nt);
	    k2 = k2 - nt + kspan;
	    kk = kk - nt + jc;
	 } while (k2 < ns);
	 do {
	    do {
	       k2 -= Perm [j - 1];
	       j++;
	       k2 = Perm [j] + k2;
	    } while (k2 > Perm [j - 1]);
	    j = 1;
	    do {
	       if (kk < k2)
		 goto Permute_Multi_Label;
	       kk += jc;
	       k2 += kspan;
	    } while (k2 < ns);
	 } while (kk < ns);
      } else {
/*  permutation for single-variate transform (optional code) */
Permute_Single_Label:
	 do {
	    /* swap Re [kk] <> Re [k2], Im [kk] <> Im [k2] */
	    ak = Re [kk]; Re [kk] = Re [k2]; Re [k2] = ak;
	    bk = Im [kk]; Im [kk] = Im [k2]; Im [k2] = bk;
	    kk += inc;
	    k2 += kspan;
	 } while (k2 < ns);
	 do {
	    do {
	       k2 -= Perm [j - 1];
	       j++;
	       k2 = Perm [j] + k2;
	    } while (k2 > Perm [j - 1]);
	    j = 1;
	    do {
	       if (kk < k2)
		 goto Permute_Single_Label;
	       kk += inc;
	       k2 += kspan;
	    } while (k2 < ns);
	 } while (kk < ns);
      }
      jc = k3;
   }

   if ((kt << 1) + 1 >= mfactor)
     return 0;
   ispan = Perm [kt];
   /* permutation for square-free factors of n */
   j = mfactor - kt;
   factor [j] = 1;
   do {
      factor [j - 1] *= factor [j];
      j--;
   } while (j != kt);
   kt++;
   nn = factor [kt - 1] - 1;
   if (nn > max_perm)
     goto Memory_Error_Label;
   j = jj = 0;
   for (;;) {
      k = kt + 1;
      k2 = factor [kt - 1];
      kk = factor [k - 1];
      j++;
      if (j > nn)
	break;				/* exit infinite loop */
      jj += kk;
      while (jj >= k2) {
	 jj -= k2;
	 k2 = kk;
	 k++;
	 kk = factor [k - 1];
	 jj += kk;
      }
      Perm [j - 1] = jj;
   }
   /*  determine the permutation cycles of length greater than 1 */
   j = 0;
   for (;;) {
      do {
	 j++;
	 kk = Perm [j - 1];
      } while (kk < 0);
      if (kk != j) {
	 do {
	    k = kk;
	    kk = Perm [k - 1];
	    Perm [k - 1] = -kk;
	 } while (kk != j);
	 k3 = kk;
      } else {
	 Perm [j - 1] = -j;
	 if (j == nn)
	   break;		/* exit infinite loop */
      }
   }
   max_factors *= inc;
   /*  reorder a and b, following the permutation cycles */
   for (;;) {
      j = k3 + 1;
      nt -= ispan;
      ii = nt - inc + 1;
      if (nt < 0)
	break;			/* exit infinite loop */
      do {
	 do {
	    j--;
	 } while (Perm [j - 1] < 0);
	 jj = jc;
	 do {
	    kspan = jj;
	    if (jj > max_factors) {
	       kspan = max_factors;
	    }
	    jj -= kspan;
	    k = Perm [j - 1];
	    kk = jc * k + ii + jj;
	    k1 = kk + kspan;
	    k2 = 0;
	    do {
	       k2++;
	       Rtmp [k2 - 1] = Re [k1];
	       Itmp [k2 - 1] = Im [k1];
	       k1 -= inc;
	    } while (k1 != kk);
	    do {
	       k1 = kk + kspan;
	       k2 = k1 - jc * (k + Perm [k - 1]);
	       k = -Perm [k - 1];
	       do {
		  Re [k1] = Re [k2];
		  Im [k1] = Im [k2];
		  k1 -= inc;
		  k2 -= inc;
	       } while (k1 != kk);
	       kk = k2;
	    } while (k != j);
	    k1 = kk + kspan;
	    k2 = 0;
	    do {
	       k2++;
	       Re [k1] = Rtmp [k2 - 1];
	       Im [k1] = Itmp [k2 - 1];
	       k1 -= inc;
	    } while (k1 != kk);
	 } while (jj);
      } while (j != 1);
   }
   return 0;			/* exit point here */

   /* alloc or other problem, do some clean-up */
Memory_Error_Label:
   fputs ("Error: " FFTRADIXS "() - insufficient memory.\n", stderr);
   fft_free ();			/* free-up memory */
   return -1;
}
#endif	/* _FFTN_C */
/* ---------------------- end-of-file (c source) ---------------------- */
/*
C This random number generator originally appeared in "Toward a Universal 
C Random Number Generator" by George Marsaglia and Arif Zaman. 
C Florida State University Report: FSU-SCRI-87-50 (1987)
C 
C It was later modified by F. James and published in "A Review of Pseudo-
C random Number Generators" 
C 
C THIS IS THE BEST KNOWN RANDOM NUMBER GENERATOR AVAILABLE.
C       (However, a newly discovered technique can yield 
C         a period of 10^600. But that is still in the development stage.)
C
C It passes ALL of the tests for random number generators and has a period 
C   of 2^144, is completely portable (gives bit identical results on all 
C   machines with at least 24-bit mantissas in the floating point 
C   representation). 
C 
C The algorithm is a combination of a Fibonacci sequence (with lags of 97
C   and 33, and operation "subtraction plus one, modulo one") and an 
C   "arithmetic sequence" (using subtraction).
C======================================================================== 
This C language version was written by Jim Butler, and was based on a
FORTRAN program posted by David LaSalle of Florida State University.
seed_ran1() and ran1() added by John Beale 7/23/95
*/

#include <stdio.h>
#include <stdlib.h>
#include "gforge.h"

static void rmarin(int ij, int kl);
void ranmar(float rvec[], int len);
float ran1();               /* return a single [0..1] float random # */
void seed_ran1(int seed);   /* use a single integer seed value */


static float u[98], c, cd, cm;
static int i97, j97;
static Boolean test = FALSE;

void seed_ran1(int sval)
{
 int i, j;

  i = abs(97 * sval) % 31329;  /* fixed abs(); overflow cond. 12/16/95 jpb */
  j = abs(33 * sval) % 30082;
  rmarin(i,j);

} /* end seed_ran1 */

static void rmarin(int ij, int kl)
{
/*
C This is the initialization routine for the random number generator RANMAR()
C NOTE: The seed variables can have values between:    0 <= IJ <= 31328
C                                                      0 <= KL <= 30081
C The random number sequences created by these two seeds are of sufficient 
C length to complete an entire calculation with. For example, if several
C different groups are working on different parts of the same calculation,
C each group could be assigned its own IJ seed. This would leave each group
C with 30000 choices for the second seed. That is to say, this random 
C number generator can create 900 million different subsequences -- with 
C each subsequence having a length of approximately 10^30.
C 
C Use IJ = 1802 & KL = 9373 to test the random number generator. The
C subroutine RANMAR should be used to generate 20000 random numbers.
C Then display the next six random numbers generated multiplied by 4096*4096
C If the random number generator is working properly, the random numbers
C should be:
C           6533892.0  14220222.0  7275067.0
C           6172232.0  8354498.0   10633180.0
*/
	int i, j, k, l, ii, jj, m;
	float s, t;
	
	if (ij<0 || ij>31328 || kl<0 || kl>30081) {
		puts("The first random number seed must have a value between 0 and 31328.");
		puts("The second seed must have a value between 0 and 30081.");
		exit(1);
	}
	
	i = (ij/177)%177 + 2;
	j = ij%177 + 2;
	k = (kl/169)%178 + 1;
	l = kl%169;
	
	for (ii=1; ii<=97; ii++) {
		s = 0.0;
		t = 0.5;
		for (jj=1; jj<=24; jj++) {
			m = (((i*j)%179)*k) % 179;
			i = j;
			j = k;
			k = m;
			l = (53*l + 1) % 169;
			if ((l*m)%64 >= 32) s += t;
			t *= 0.5;
		}
		u[ii] = s;
	}
	
	c = 362436.0 / 16777216.0;
	cd = 7654321.0 / 16777216.0;
	cm = 16777213.0 / 16777216.0;
	
	i97 = 97;
	j97 = 33;
	
	test = TRUE;
}

float ran1()
/*  return a single floating-point random number  -mod jpb 7/23/95 */
{
	float uni;  /* the random number itself */

	if (test==FALSE) {
	   fprintf(stderr,
	      "Call the init routine seed_ran1() before calling ran1().");
		exit(1);
	}

	uni = u[i97] - u[j97];    /* difference between two [0..1] #s */
	if (uni < 0.0) uni += 1.0;
	u[i97] = uni;
	i97--;                  /* i97 ptr decrements and wraps around */
	if (i97==0) i97 = 97;
	j97--;                  /* j97 ptr decrements likewise */
	if (j97==0) j97 = 97;
	c -= cd;                /* finally, condition with c-decrement */
	if (c<0.0) c += cm;     /* cm > cd we hope! (only way c always >0) */
	uni -= c;
	if (uni<0.0) uni += 1.0;

	return(uni);            /* return the random # */
}  /* end ran1() */
