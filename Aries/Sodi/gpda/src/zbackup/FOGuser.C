#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h> 
#include <time.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <errno.h>

#include "FOGtypes.h"

/* --------------------------------------------------------------------- */

#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif

/* --------------------------------------------------------------------- */

typedef struct {
int             opindex[40];
float           fogfactor[40];
} FOGOP;

typedef struct {
int             causeindex;
int             enabled;
FOGOP           effect[3];
} FOG;

/* --------------------------------------------------------------------- */

int             FogEnable;
FOG             FogArray[20];
//
//   Allocate the Fog OPERATORS
//
char            FogOps[12][12] = 
                   { "<No-op>",  "Random",     "Delay",      "Disperse",   "Degrade",
                     "Mistype",  "Conflict",   "Overload",   "Derate",
                     "Transmit", "Disconnect", "Threshold" };
//
//   Allocate the Default Values of Fog
//
float           FogOpsLow[12] = 
                   { 0.0, 0.2, 1.33, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2 };
float           FogOpsMed[12] = 
                   { 0.0, 0.4, 1.66, 1.0, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4 };
float           FogOpsHigh[12] = 
                   { 0.0, 0.6, 2.00, 2.0, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6 };
//
//   Allocate the Causes of Fog
//
char            FogCauses[15][20] = 
                   { "Loss",      "Latency",     "Asynchrony", "Degradation",
                     "Ambiguity", "Conflict",    "Overload",   "Bad_Luck",
                     "Surprises", "Disposition", "Cognition",  "Confusion",
                     "Priorties", "Miscommunication", "Assumptions" };
//
//   Allocate the Effects of Fog
//
char            FogEffects[3][20] = 
                   { "Evidence", "Weights", "Outcomes" };

/* --------------------------------------------------------------------- */

void FOGset(int Icause, int Ieffect, float fog[2]);
int  FOGget(int Icause, int Ieffect, float fog[2]);
void FOGon();
void FOGoff();
int  FOGtest();

/* --------------------------------------------------------------------- */

void
FOGset(int Icause, int Ieffect, float fog[2])
{
   FogArray[Icause].effect[Ieffect].fogfactor[Icause] = fog[0];
   FogArray[Icause].effect[Ieffect].fogfactor[Icause+20] = fog[1];
}

int
FOGget(int Icause, int Ieffect, float fog[2])
{
   fog[0] = FogArray[Icause].effect[Ieffect].fogfactor[Icause];
   fog[1] = FogArray[Icause].effect[Ieffect].fogfactor[Icause+20];

   return (FogArray[Icause].enabled);
}

void
FOGon()
{
   FogEnable = TRUE;
}

void
FOGoff()
{
   FogEnable = FALSE;
}

int
FOGtest()
{
   return (FogEnable);
}

void
FOGload(const char *infilename)
{
FILE       *LOADfp;
int        item = 0;
int        i, j, k;
int        nfogs, nopers, neffects, n, m, op1, op2, enable;
float      x, y, z;
char       chtemp[40];
char       chline[40];
char       bigline[128];

   if (infilename != NULL) {
      LOADfp = fopen(infilename, "r");
//
//   Load the Causes of Fog variables
//
      fscanf(LOADfp, "%d", &nfogs);
      for (i=0; i<nfogs;  i++) {
         fscanf(LOADfp, "%d %s", &n, chline);
         strcpy(FogCauses[i], chline);
      }
//
//   Allocate the OPERATOR variables
//
      fscanf(LOADfp, "%d", &nopers);
      for (i=0; i<nopers;  i++) {
         fscanf(LOADfp, "%d %s %f %f %f", &n, chline, &x, &y, &z);
         strcpy(FogOps[i], chline);
         FogOpsLow[i] = x;
         FogOpsMed[i] = y;
         FogOpsHigh[i] = z;
      }
//
//   Load the Default Effects of Fog variables
//
      fscanf(LOADfp, "%d", &neffects);
      for (k=0; k<neffects;  k++) {
        fscanf(LOADfp, "%d %d %s\n", &n, &m, chline);
        strcpy(FogEffects[k], chline);
        for (j=n; j<=m; j++) {
          fscanf(LOADfp, "%d %s %f %d %s %f\n", &op1, chline, &x, &op2, chtemp, &y);
	  FogArray[j].effect[k].opindex[j]      = op1;
	  FogArray[j].effect[k].fogfactor[j]    = x;
	  FogArray[j].effect[k].opindex[j+20]   = op2;
	  FogArray[j].effect[k].fogfactor[j+20] = y;
        }
      }

      fscanf(LOADfp, "%d\n", &n);
      for (j=0; j<n; j++) {
        fscanf(LOADfp, "%d\n", &m);
        FogArray[j].enabled = m;
      }

      fclose(LOADfp);
   }
}
