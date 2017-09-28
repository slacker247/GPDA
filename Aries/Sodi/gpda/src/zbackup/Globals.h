#ifndef _GLOBALS_H
#define _GLOBALS_H  1


#include <string.h>
#include <math.h>

/*                                                                       */
/*                             D e f i n e s                             */
/* --------------------------------------------------------------------- */
/*                                                                       */
#ifndef FALSE
#define FALSE           0
#endif
#ifndef TRUE
#define TRUE            1
#endif

#ifndef M_PI
#define M_PI            3.14159265358979323846
#endif

#ifndef EPS
#define EPS             0.0000001
#endif

#define DEGRAD          57.29577951308232
#define RADDEG          0.0174532925199432958
#define TORAD(x)        ((M_PI/180.0)*(x))
#define TODEG(x)        ((180.0/M_PI)*(x))

#define odd(n)          ((n) & 1)
#define round(a)        (int)(a+0.5)
#define signum(a)       (int)(a<0 ? -1 : (a>0 ? 1 : 0) )

#ifndef	MIN
#define MIN(a,b)        (((a)<(b)) ? (a) : (b))
#endif
#ifndef	MAX
#define MAX(a,b)        (((a)>(b)) ? (a) : (b))
#endif

#define IRND(x)         ((int)((float)(x) * ((float)random()/(float)0x7fffffff)))
#define drand48()       (((float) rand())/((float) RAND_MAX))
#define srand48(x)      (srand((x)))
#define ranf(x)         drand48(x)

#define cosf(a)         (float)cos((float)a)
#define sinf(a)         (float)sin((float)a)
#define tanf(a)         (float)(cosf(a) == 0.0 ? 1E30*signum(sinf(a)) : sinf(a)/cosf(a))
/*
#define arcsinf(a)      (float)(a>(1.0-EPS) ? (M_PI/2.0) : \
                               (x<(-(1.0-EPS)) ? -M_PI/2.0 : Re(atan(a/sqrtf(1.0-a*a)))))
#define arccosf(a)      (float)( (M_PI/2.0) - arcsinf(a) )
*/
#define sqrf(a)         (float)(a * a)
#define sqrtf(a)        (float)sqrt((float)a)

#define expf(a)         (float)exp((float)a)  
#define logf(x)         (float)log((double)x)

#define absf(a)         ((a) >= 0 ? (a) : -(a))

#define arcf(a)         (a * M_PI/180.0)
#define degf(a)         (a * 180.0/M_PI)
/*                                                                       */
/*                             I n l i n e s                             */
/* --------------------------------------------------------------------- */
/*                                                                       */
/* Utility definition to get an array's element count at compile time or
   the # of bytes in a string constant INCLUDING THE TRAILING NULL.

       int arr[] = {1,2,3,4,5};
       printf("%d", ELEMENTS(arr)); would print a 5.
 
       printf("%d", ELEMENTS("abc")); would print a 4 
*/
#define ELEMENTS(array) (sizeof(array)/sizeof((array)[0]))

inline 
char *strnsub(char *istr, char och, char nch, int maxsize)
{
int             i, size;
 
   for (i=0; i<maxsize; i++) if (istr[i] == och) istr[i] = nch;
   return (istr);
}
inline 
char *strsub(char *istr, char och, char nch)
{
int             i;
 
   for (i=0; i<strlen(istr); i++) if (istr[i] == och) istr[i] = nch;
   return (istr);
}
inline 
char *strtrm(char *strin)
{
int             i,l;
 
   l = strlen(strin) - 1;
   for (i=l; i<1; i--) if (strin[i] != ' ') break;
   strin[i+1] = '\0';
   return (strin);
}
inline
char *strnfill(char *dest, char *src, int n)
{
int             i;

   for (i=0; i<n; i++) dest[i] = ' ';
   for (i=0; i<strlen(src); i++) dest[i] = src[i];
   return(dest);
}

inline
void VecNorm(float v[3])                                /* Normalizes v */
{
  float d = sqrtf(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);

  if (d == 0)
    fprintf(stderr, "Zero length vector in Normalize\n");
  else
    v[0] /= d; v[1] /= d; v[2] /= d;
}

inline  /* calculates a normalized crossproduct to v1, v2 */
void VecCross(float v1[3], float v2[3], float cp[3])
{
  cp[0] = v1[1]*v2[2] - v1[2]*v2[1];
  cp[1] = v1[2]*v2[0] - v1[0]*v2[2];
  cp[2] = v1[0]*v2[1] - v1[1]*v2[0];
  VecNorm(cp);
}

inline
float VecLength(float v[3])
{
  float r = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  return r;
}

inline
double dsign(double x)
{
  if (x > 0.0) return(1.0);
  else if (x < 0.0) return(-1.0);
  else return(0.0);
}

#endif // _GLOBALS_H
