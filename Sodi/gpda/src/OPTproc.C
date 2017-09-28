//
//*************************************************************************
//   Things to do:
//     - Eliminate old, unused functions, etc.
//     - Implement Scaling, Elitism, Diversity parameter settings
//     - Add additional capibilities
//
//*************************************************************************
//
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <time.h>
#include <sys/types.h>
#include <iostream.h> 

#include <ga/ga.h>
#include <ga/GARealGenome.h>
#include <ga/GARealGenome.C>

#include "forms.h"
#include "OPTforms.h"

#include "GL/glx.h"
#include "GL/gltk.h"
#include "glfont.h"

/* --------------------------------------------------------------------- */

#ifndef FALSE
#define FALSE   0
#endif
#ifndef TRUE
#define TRUE    1
#endif       

#define APP_CLASS     "GAView"
#define SETTINGS_FILE "OPTinit.dat"
#define MAX_POPS      5 

/* --------------------------------------------------------------------- */

typedef struct {
  float      SVbr;                      // Probability of Pre-launch survivability
  float      ARbr;                      // Probability of Arrival
  float      DMbr;                      // Probability of Damage
  float      SVrb;                      // Probability of Pre-launch survivability
  float      ARrb;                      // Probability of Arrival
  float      DMrb;                      // Probability of Damage
  float      rDE;                       // Estimated rDE 
  char       Weapon[4];                 // (C)onventional, (N)uclear, ...
  char       Arena[16];                 // Tactical | Strategic
  char       Type[16];                  // Preempt, Deter, Deny, Retailiate, ... 
  char       Option[64];                // Response Option name
  char       COA[64];                   // Parent COA id
} RO;
 
typedef struct {
  int        Value;                     // Importance (1 = critical, 4 = low)
  int        Class;                     // Missile, Air base, Sub Base, ...
  int        Mobile;                    // Is target mobile (1 = yes)
  float      Weight;                    // Weighted value
  float      Hard;                      // Target Hardness
  float      Latitude;                  // Target latitude (decimal degrees)
  float      Longitude;                 // Target Longitude (decimal degrees)
  char       Name[32];                  // Site name
  char       Locale[32];                // Site Locale
} TGT;

typedef struct {
  int        Value;                     // Importance (1 = critical, 4 = low) 
  float      SVbr;                      // Probability of Pre-launch survivability
  float      ARbr;                      // Probability of Arrival
  float      DMbr;                      // Probability of Damage
  float      SVrb;                      // Probability of Pre-launch survivability
  float      ARrb;                      // Probability of Arrival
  float      DMrb;                      // Probability of Damage
  float      rDE;                       // Estimated rDE
  float      Weight;                    // Weighted value
  char       Name[32];                  // Site name
} INSTANCES; 

typedef struct _AppData {
  int                 whichGA;          // which genetic algorithm to use
  int                 whichGenome;      // which genome to use
  int                 whichFunction;    // which function to use
  int                 lengthGenome;
  int                 geninc;           // how many generations in each step
  int                 procid;           // idle loop id
  int                 gevals;           // No. of genome evaluations

  long                bestcolor;
  long                popcolor[MAX_POPS]; 
  int                 appc;
  long                canvas, counter;
  float               goal;
  float               tolerance;
  GC                  bestgc, dotgc[MAX_POPS];
 
  GAGenome*           genome;
  GAGeneticAlgorithm* ga;
} AppData, *AppDataPtr, **AppDataHdl;

typedef struct _UserData {
  float               rDE;
} UserData; 

typedef struct _BitmapInfo {
  int width, height;
  unsigned char *bits;
} BitmapInfo;
 
enum {
  bmStop,
  bmRewind,
  bmForwardStop,
  bmFastForwardStop,
  bmFastForward,
  nBitmaps
};

/* --------------------------------------------------------------------- */

int             OPTwinX, OPTwinY;
int             OPTwinW, OPTwinH;
int             OPTinited = 0;
char            OPTlabel[32];
Window          OPTwinid;
int             OPTglwinW, OPTglwinH;
int             OPTglInit = FALSE;
int             OPTloaded = FALSE;

char            opttemp[128];

static AppData  theAppData;             // global data for the program
int             done = 0;               // is the program finished yet?
static char     Text_Which_GA[64];
static char     Text_Which_Genome[64];

RO              *options;
TGT             *alltgts;
int             nRO = 3;
int             n_opts, n_tgts;
int             whichScore = 0;
int             FinalOptions[20], FinalTargets[20], FinalActions;

GARealAlleleSetArray  alleles5;

UserData        userdata;  

int             n_Asts = 8;
int             AstAvail[15] = { 250, 125, 190, 125, 48, 48, 48, 100, 0, 0, 0, 0, 0, 0, 0 };
int             AstComit[15] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
char            AstClass[15][24] = { "ICBM", "SLBM", "Bomber", "NMD", "Patriot",
                                     "Thaad", "Aegis", "Aircraft" };

int             n_Tgts = 11;
int             TgtAvail[15] = { 12, 3, 4, 1, 2, 2, 2, 4, 20, 5, 20, 0, 0, 0, 0 };
int             TgtComit[15] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
char            TgtClass[15][24] = { "Mobile Target",
                                     "Missile Base", "Air Base",  "Sub Base",
                                     "Nuclear Facility", "Chemical Facility",
                                     "Biological Facility", "C&C Facility",
                                     "Population Center", "Time Critical Target",
                                     "Other" };

char            TgtHard[3][8] = { "Soft", "Medium", "Hard" };
 
#include "bitmaps/gaview.xbm" 
#include "bitmaps/rew.xbm"
#include "bitmaps/stop.xbm"
#include "bitmaps/fwds.xbm"
#include "bitmaps/ffst.xbm"
#include "bitmaps/ffwd.xbm"
static BitmapInfo bm[] = {
  {stop_width, stop_height, (unsigned char *)stop_bits},
  {rew_width,  rew_height,  (unsigned char *)rew_bits},
  {fwds_width, fwds_height, (unsigned char *)fwds_bits},
  {ffst_width, ffst_height, (unsigned char *)ffst_bits},
  {ffwd_width, ffwd_height, (unsigned char *)ffwd_bits}
};

FD_gagraph      *fd_gagraph;
FD_gaparms      *fd_gaparms;
FD_gainput      *fd_gainput;
FD_gacommit     *fd_gacommit;

/* --------------------------------------------------------------------- */

void        OPTinit();
void        OPTshow(int xpos, int ypos, int width, int height, Window mainwinID);
int         OPTload();
void        OPTsetupCB(FL_OBJECT *ob, long data);
extern int  EraseActiveEntry(char *text);
extern int  StoreActiveEntry(char *text);
extern int  FinishUp();
extern char *strtrm(char *strin);

void DumpStatsCB(FL_OBJECT *object, long item_no);
void DumpParamsCB(FL_OBJECT *object, long item_no);

void DrawPopulation(const GAPopulation&, GC, GC);
int  OPTidleWP(XEvent *ev, void *data);
void UpdateCounter(GAGeneticAlgorithm*);
int  Evolve(int n);
void QuitCB(FL_OBJECT *object, long item_no);
void InitCB(FL_OBJECT *object, long item_no);
void DrawCB(FL_OBJECT *object, long item_no);
void StepCB(FL_OBJECT *object, long item_no);
void ResetCB(FL_OBJECT *object, long item_no);
void StopCB(FL_OBJECT *object, long item_no);
void EvolveSomeCB(FL_OBJECT *object, long item_no);
void EvolveCB(FL_OBJECT *object, long item_no);

extern "C" GLXContext fl_get_glcanvas_context(FL_OBJECT * ob);

//callback events for opengl canvas
int OPTexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);
int OPTbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud);

// Objective function and initializer declarations.
/*
extern float CalculateDE(DEFACTORS *Roptions, int   Rindex,
                         INSTANCES *Sites,    int   Sindex,
                         float     *DEbr,     float *DErb);
*/
void Initializer(GAGenome &);
GABoolean TerminateUponConvergence(GAGeneticAlgorithm &ga); 
 
float RealObjective(GAGenome&);
float Bin2DecObjective(GAGenome&);
 
typedef float (*Function)(float, float);
float Function1(float x, float y);
float Function2(float x, float y);
float Function3(float x, float y);
float Function4(float x, float y);
float Function5(GAGenome& g);
Function obj[] = { Function1, Function2, Function3, Function4 };
float minx[] = {-6, -60, -500, -10, 0 };
float maxx[] = { 6,  60,  500, 10,  4 };
float ai[25],bi[25];

/* --------------------------------------------------------------------- */

inline float MIN(float val1, float val2)
{
   if (val1 < val2) return (val1);
   else             return (val2);
}

inline float MAX(float val1, float val2)
{
   if (val1 > val2) return (val1);
   else             return (val2);
}
 
double gammaln(double xx)
{
/*
**   This routine returns the value ln(gamma(xx)) for xx > 0.0. Full
**   accuracy is obtained for xx > 1.0. For 0.0 < xx < 1.0, the
**   reflection formula could be used (See Numerical Recipes in C,
**   Chapter 6).
*/
int             j;
double          x, tmp, ser;
static double   gammacof[6] = { 76.18009173, -86.50532033, 24.01409822,
                         -1.23173952,   0.120858003E-2, -0.536382E-5 };

   x = xx - 1.0;
   tmp = x + 5.5;
   tmp -= (x+0.5)*log(tmp);
   ser = 1.0;
   for (j=0; j<=5; j++) {
      x += 1.0;
      ser += gammacof[j]/x;
   }
   printf("GAMMALN returning %lf\n", -tmp + log(2.50662827465*ser)); 
   return (-tmp + log(2.50662827465*ser));
}

double factln(int n)
{
/*
**   This routine returns ln(n!) as a double floating point number
*/
static double   factlna[101];
int             j;
  
   return (0.0);
}
 
double factorial(int n)
{
/*
**   This routine returns n! as a double floating point number
*/
static int      facttop = 4;
static double   facta[33] = { 1.0, 1.0, 2.0, 6.0, 24.0 };
int             j;
 
   if (n < 0) { fprintf(stderr, "Negative # in FACTORIAL call.\n"); n = -n; }
   if (n > 32) return (exp(gammaln((double)(n+1.0))));
 
   while (facttop < n) {
      j = facttop++;
      facta[facttop] = facta[j]*(double)facttop;
   }
 
   return (facta[n]);
}

double bicoeff(int n, int k)
{
/*
**   This routine returns binomial coeff (n/k)  as a double floating point number
*/
double          t;
int             j;

   t = (double)n;
   for( j=1; j<k; j++) t = t*(double)(n-j);
  
   return (t/factorial(k));
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*              M A I N   P R O G R A M   S T A R T S   H E R E          */
/* --------------------------------------------------------------------- */
/*                                                                       */
void OPTinit()
{
FILE            *fp;
int             i;

//for (i=0; i<33; i++) printf("Factorial of %d is %f\n", i, factorial(i));

   if (OPTinited) return;

   fd_gagraph = create_form_gagraph();
   fd_gaparms = create_form_gaparms();
   fd_gainput = create_form_gainput();
   fd_gacommit = create_form_gacommit();

   strcpy(OPTlabel, "Plan-Optimize");

   //fl_set_browser_fontstyle(fd_gagraph->gadata_browser, FL_FIXED_STYLE|FL_BOLD_STYLE);
   fl_set_browser_fontstyle(fd_gagraph->geneview, FL_BOLD_STYLE);
   //fl_set_browser_fontstyle(fd_gagraph->gaparmview, FL_FIXED_STYLE);
   fl_set_browser_fontsize(fd_gainput->gain_solutions, FL_MEDIUM_SIZE);
   fl_set_browser_fontstyle(fd_gainput->gain_solutions, FL_FIXED_STYLE|FL_BOLD_STYLE);
   //fl_set_browser_fontsize(fd_gacommit->opt_tgtlist, FL_MEDIUM_SIZE);
   fl_set_browser_fontstyle(fd_gacommit->opt_tgtlist, FL_FIXED_STYLE);
   //fl_set_browser_fontsize(fd_gacommit->opt_reslist, FL_MEDIUM_SIZE);
   fl_set_browser_fontstyle(fd_gacommit->opt_reslist, FL_FIXED_STYLE);

   fl_hide_object(fd_gainput->gain_go);
   fl_hide_object(fd_gainput->gain_save);

   fl_hide_object(fd_gagraph->ga_start);
   fl_hide_object(fd_gagraph->bitmap_rew);
   fl_hide_object(fd_gagraph->bitmap_stop);
   fl_hide_object(fd_gagraph->bitmap_step);
   fl_hide_object(fd_gagraph->bitmap_some);
   fl_hide_object(fd_gagraph->bitmap_run);

   fl_add_canvas_handler(fd_gagraph->ga_canvas, Expose,      OPTexposeCB, 0);
   fl_add_canvas_handler(fd_gagraph->ga_canvas, ButtonPress, OPTbuttonCB, 0);

   //fl_set_choice(fd_gainput->gamenu_select, 2);

   OPTinited = TRUE;
   fp = fopen("GPDAinfo.log", "a");
   fprintf(fp, "Plan Optimization algorithm initialization complete.\n");
   fclose(fp);
     
   return;
}

int OPTclose(FL_FORM *form, void *data)
{
   OPTexitCB(NULL, 0);

   return(0);
}

void OPTshow(int xpos, int ypos, int width, int height, Window mainwinID)
{

   if(!fl_form_is_visible(fd_gainput->gainput) ) {
      fl_transient();
      fl_winposition(xpos, ypos);
      fl_initial_winsize(width, height);
      OPTwinid = fl_prepare_form_window(fd_gainput->gainput,
                    FL_PLACE_POSITION,FL_TRANSIENT, "Plan Optimization: Operator Screen");
      fl_winreshape(OPTwinid, xpos, ypos, width, height);
      fl_get_wingeometry(OPTwinid, &OPTwinX, &OPTwinY, &OPTwinW, &OPTwinH);
      fl_show_form_window(fd_gainput->gainput);
      fl_set_form_atclose(fd_gainput->gainput, OPTclose, 0);
      StoreActiveEntry(OPTlabel);
   }

   sprintf(opttemp, "%d", 10);
   fl_set_input(fd_gaparms->ga_geninc, opttemp);
   sprintf(opttemp, "%5.2f", 11.0);
   fl_set_input(fd_gainput->ga_goal, opttemp);
   sprintf(opttemp, "%d", 20);
   fl_set_input(fd_gainput->ga_tol, opttemp);
   fl_set_dial_value(fd_gacommit->opt_goodness, 20.0);

   gainbuttonCB(NULL, 0);
   gainbuttonCB(NULL, 1);
   OPTsetupCB(NULL, 1);

   return;
}

int OPTload()
{
FILE            *casefp, *sitefp;
int             i, j, k, Rindex, item;
float           v1, v2, v3, v4, v5, v6, v7;
char            filename[64];

// 
// Load all Response Options
//
   n_opts = 0;
   sprintf(filename, "%s", "ActiveOptions.dat");
   if ((casefp = fopen(filename, "r")) != NULL) {
     do fgets(opttemp, 128, casefp); while (opttemp[0] == '#');
     sscanf(opttemp, "%d", &n_opts);
     options = new RO[n_opts];
     for (i=0; i<n_opts; i++) {
       do fgets(opttemp, 128, casefp); while (opttemp[0] == '#');
       sscanf(opttemp, "%s %s %s %s %f %f %f %f %f %f %f",
              options[i].Option, options[i].Type, options[i].Arena, options[i].Weapon,
              &v1, &v2, &v3, &v4, &v5, &v6, &v7);
       options[i].SVbr  = v1;
       options[i].ARbr  = v2;
       options[i].DMbr  = v3;
       options[i].SVrb  = v4;
       options[i].ARrb  = v5;
       options[i].DMrb  = v6;
       options[i].rDE   = v7;
     }
     fclose(casefp);
   } else {
      fl_show_messages("Error: Unable to open Options file"); 
   }
// 
// Load all Targets
//
   n_tgts = 0;
   sprintf(filename, "%s", "ActiveTargets.dat");
   if ((sitefp = fopen(filename, "r")) != NULL) {
     do fgets(opttemp, 128, sitefp); while (opttemp[0] == '#');
     sscanf(opttemp, "%d", &n_tgts);
     alltgts = new TGT[n_tgts];
     for (i=0; i<n_tgts; i++) {
       do fgets(opttemp, 128, sitefp); while ((opttemp[0] == '#') && !feof(sitefp));
       sscanf(opttemp, "%s %s %f %f %d %d %f %d",
	      alltgts[i].Name, alltgts[i].Locale, &alltgts[i].Latitude, &alltgts[i].Longitude,
              &alltgts[i].Class, &alltgts[i].Value, &alltgts[i].Hard, &alltgts[i].Mobile);
     }
     fclose(sitefp);
   } else {
      fl_show_messages("Error: Unable to open Targets file");
   }

   OPTloaded = TRUE;

   return(0);
}

void SetSort(GAPopulation pop)
{
   GAPopulation::SortOrder sortorder = GAPopulation::LOW_IS_BEST;
   pop.order(sortorder);
}

void OPTsetupCB(FL_OBJECT *ob, long data)
{
FILE            *casefp, *sitefp;
int             i, j, k, Rindex, item;
int             value, tgttype, mobile;
int             ideglat, iminlat, ideglon, iminlon;
int             pi1, pi2;
float           pf1, pf2;
float           v1, v2, v3, v4, v5, v6, v7;
char            filename[64];
int             TgtClass;
int             TgtWeight;
float           rDE, DEbr, DErb;
char            littlestr[40];
char            bigstring[1260];

GARankSelector          RankSelector;
GARouletteWheelSelector WheelSelector;
GATournamentSelector    TourSelector;
GASRSSelector           SRSSelector;
GAUniformSelector       UniSelector;
GADSSelector            DSSelector;
GAParameterList         params;

   if (!OPTloaded) return;
 
   theAppData.whichFunction = 4; //fl_get_choice(fd_gainput->gamenu_function) - 1;
   theAppData.whichGenome = fl_get_choice(fd_gaparms->gamenu_genome) - 1;
   theAppData.whichGA = fl_get_choice(fd_gaparms->gamenu_algorithm) - 1;
   theAppData.geninc = atoi(fl_get_input(fd_gaparms->ga_geninc));
   theAppData.goal = atof(fl_get_input(fd_gainput->ga_goal));
   theAppData.tolerance = atof(fl_get_input(fd_gainput->ga_tol))/100.0 * theAppData.goal;

   theAppData.gevals = 0;
// 
//   Do some setup for one of the functions
// 
   for (int j=0; j<25; j++) {
     ai[j] = 16 * ((j % 5) -2);
     bi[j] = 16 * ((j / 5) -2);
   }
// 
//   Create the appropriate genome
//   -----------------------------
//
   if (theAppData.whichGenome == 2) {
     unsigned int genomeLen = 0;
     theAppData.whichFunction = 4;
     //
     // The genome is created using an array of allele sets.  This means that each
     // element of the genome will assume a value in its corresponding allele set.
     // For example, since the first allele set is [1,15], the first element of the
     // genome will be in [1,15].
     //
     for (int i=0; i<nRO; i++) { 
       alleles5.add(0,n_opts-1,1.0); genomeLen++;
       alleles5.add(0,n_tgts-1,1.0); genomeLen++;
     }
     theAppData.genome = new GARealGenome(alleles5, Function5, (void *)&userdata);
     theAppData.lengthGenome = genomeLen;
     theAppData.genome->initializer(Initializer);
     // 
     // Now set up the crossover scheme
     //
     switch (fl_get_choice(fd_gaparms->gamenu_xover)) {
       case 1:
         theAppData.genome->crossover(GARealPartialMatchCrossover);         
         break;
       case 2:
         theAppData.genome->crossover(GARealOrderCrossover);
         break;
       case 3:
         theAppData.genome->crossover(GARealCycleCrossover);
         break;
       case 4:
         theAppData.genome->crossover(GARealOnePointCrossover);
         break;
       case 5:
         theAppData.genome->crossover(GARealTwoPointCrossover);
         break;
       case 6:
         theAppData.genome->crossover(GARealEvenOddCrossover);
         break;
       case 7:
         theAppData.genome->crossover(GARealUniformCrossover);
         break;
       case 8:
         theAppData.genome->crossover(GARealArithmeticCrossover);
         break;
       case 9:
         theAppData.genome->crossover(GARealBlendCrossover);
         break;
       default:
         break;
     }
     // 
     // Now set up the mutation scheme
     //
     switch (fl_get_choice(fd_gaparms->gamenu_mutation)) {
       case 1:
         theAppData.genome->mutator(GARealUniformMutator);         
         break;
       case 2:
         theAppData.genome->mutator(GARealSwapMutator);
         break;
       case 3:
         theAppData.genome->mutator(GARealGaussianMutator);
         break;
       default:
         break;
     }
     // 
     // Now set up the initialization scheme
     //
     switch (fl_get_choice(fd_gaparms->gamenu_init)) {
       case 1:
         theAppData.genome->initializer(GARealGenome::UniformInitializer);                  
         break;
       case 2:
         theAppData.genome->initializer(GARealGenome::OrderedInitializer);
         break;
       case 3:
         break;
       default:
         break;
     } 
   } else if (theAppData.whichGenome == 1) {
     GABin2DecPhenotype map;
     map.add(31, minx[theAppData.whichFunction],
        maxx[theAppData.whichFunction]);
     map.add(31, minx[theAppData.whichFunction],
        maxx[theAppData.whichFunction]);
     theAppData.genome = new GABin2DecGenome(map, Bin2DecObjective);
   } else { 
     GARealAlleleSet alleleset(minx[theAppData.whichFunction],
                  maxx[theAppData.whichFunction]);
     theAppData.genome = new GARealGenome(2, alleleset, RealObjective);
   }
// 
//   Create the appropriate genetic algorithm
//   ----------------------------------------
// 
   if(theAppData.whichGA == 0) {
     theAppData.ga = new GAIncrementalGA(*theAppData.genome);
     /* 
     // Now set up the replacement scheme
     //
     switch (fl_get_choice(fd_gaparms->gamenu_replace)) {
       case 1:
         //theAppData.ga->replacement(GAPopulation::PARENT, NULL);
         break;
       case 2:
         theAppData.ga->replacement(GAPopulation::RANDOM);
         break;
       case 3:
         theAppData.ga->replacement(GAPopulation::WORST);
         break;
       case 4:
         theAppData.ga->replacement(GAPopulation::BEST);
         break;
       default:
         break;
	 } */  
   } else if(theAppData.whichGA == 1)
     theAppData.ga = new GASimpleGA(*theAppData.genome);
   else if(theAppData.whichGA == 3)
     theAppData.ga = new GADemeGA(*theAppData.genome);
   else
     theAppData.ga = new GASteadyStateGA(*theAppData.genome);
   // 
   //   Now set up the selection scheme
   //
   switch (fl_get_choice(fd_gaparms->gamenu_select)) {
     case 1:
       theAppData.ga->selector(RankSelector);
       break;
     case 2:
       theAppData.ga->selector(WheelSelector);
       break;
     case 3:
       theAppData.ga->selector(TourSelector);
       break;
     case 4:
       theAppData.ga->selector(SRSSelector);
       break;
     case 5:
       theAppData.ga->selector(UniSelector);
       break;
     case 6:
       theAppData.ga->selector(DSSelector);
       break;
     default:
       break;
   }
   // 
   //   Now set up the genetic algorithm parameters
   // 
   theAppData.ga->parameters(SETTINGS_FILE);
   // 
   //   Override the parameters with user specified values
   //
   if (strcmp(fl_get_input(fd_gaparms->gaparm[1]), "\0") != 0)
     theAppData.ga->set(gaNnGenerations,   atoi(fl_get_input(fd_gaparms->gaparm[1])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[2]), "\0") != 0)
     theAppData.ga->set(gaNpConvergence,   atof(fl_get_input(fd_gaparms->gaparm[2])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[3]), "\0") != 0)
     theAppData.ga->set(gaNnConvergence,   atoi(fl_get_input(fd_gaparms->gaparm[3])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[4]), "\0") != 0)
     theAppData.ga->set(gaNpCrossover,     atof(fl_get_input(fd_gaparms->gaparm[4])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[5]), "\0") != 0)
     theAppData.ga->set(gaNpMutation,      atof(fl_get_input(fd_gaparms->gaparm[5])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[6]), "\0") != 0)
     theAppData.ga->set(gaNpopulationSize, atoi(fl_get_input(fd_gaparms->gaparm[6])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[7]), "\0") != 0)
     theAppData.ga->set(gaNnPopulations,   atoi(fl_get_input(fd_gaparms->gaparm[7])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[8]), "\0") != 0)
     theAppData.ga->set(gaNpReplacement,   atof(fl_get_input(fd_gaparms->gaparm[8])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[9]), "\0") != 0)
     theAppData.ga->set(gaNnReplacement,   atoi(fl_get_input(fd_gaparms->gaparm[9])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[10]), "\0") != 0)
     theAppData.ga->set(gaNnBestGenomes,   atoi(fl_get_input(fd_gaparms->gaparm[10])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[11]), "\0") != 0)
     theAppData.ga->set(gaNnOffspring,     atoi(fl_get_input(fd_gaparms->gaparm[11])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[12]), "\0") != 0)
     theAppData.ga->set(gaNpMigration,     atof(fl_get_input(fd_gaparms->gaparm[12])));
   if (strcmp(fl_get_input(fd_gaparms->gaparm[13]), "\0") != 0)
     theAppData.ga->set(gaNnMigration,     atoi(fl_get_input(fd_gaparms->gaparm[13])));
 
   theAppData.ga->initialize();
   theAppData.ga->terminator(TerminateUponConvergence); 
//
//   we don't allow too many populations (due to our color limit)
// 
   if(theAppData.whichGA == 3) {
     int val;
     theAppData.ga->get(gaNnPopulations, &val);
     if(val > MAX_POPS) {
       val = MAX_POPS;
       theAppData.ga->set(gaNnPopulations, val);
       cerr << "this demo limits the number of populations to "<<MAX_POPS<<"\n";
     }
   }

   //fl_set_input(fd_gagraph->ga_genome, fl_get_choice_text(fd_gaparms->gamenu_genome));
   //fl_set_input(fd_gagraph->ga_algor, fl_get_choice_text(fd_gaparms->gamenu_algorithm));
   //sprintf(opttemp, "%s View", fl_get_choice_text(fd_gaparms->gamenu_genome));
   //fl_set_object_label(fd_gagraph->geneview, opttemp);

   fl_show_object(fd_gagraph->bitmap_rew);
   fl_show_object(fd_gagraph->bitmap_stop);
   fl_show_object(fd_gagraph->bitmap_step);
   fl_show_object(fd_gagraph->bitmap_some);
   fl_show_object(fd_gagraph->bitmap_run);

   fl_hide_object(fd_gagraph->ga_start);

   //fl_clear_browser(fd_gagraph->gaparmview);
   params = theAppData.ga->parameters();
   params.get(gaNnGenerations,   &pi1);  params.get(gaNminimaxi,       &pi2);  
   sprintf(bigstring, "Number of Generations  %5d  MiniMaxi                %5d",
           pi1, pi2);
   //fl_addto_browser(fd_gagraph->gaparmview, bigstring);
   params.get(gaNpConvergence,   &pf1);  params.get(gaNnConvergence,   &pi1);
   sprintf(bigstring, "Convergence Percentage %5.2f  Generations to Converge %5d",
           pf1, pi1);
   //fl_addto_browser(fd_gagraph->gaparmview, bigstring);
   params.get(gaNpCrossover,     &pf1);  params.get(gaNpMutation,      &pf2);
   sprintf(bigstring, "Crossover Probability  %5.2f  Mutation Probability    %5.2f",
           pf1, pf2);
   //fl_addto_browser(fd_gagraph->gaparmview, bigstring);
   params.get(gaNpMigration,     &pf1);  params.get(gaNnMigration,     &pi2);
   sprintf(bigstring, "Migration Probability  %5.2f  Migration Number        %5d",
           pf1, pi2);
   //fl_addto_browser(fd_gagraph->gaparmview, bigstring);
   params.get(gaNpopulationSize, &pi1);  params.get(gaNnPopulations,   &pi2);
   sprintf(bigstring, "Population Size        %5d  Number of Populations   %5d",
           pi1, pi2);
   //fl_addto_browser(fd_gagraph->gaparmview, bigstring);
   params.get(gaNpReplacement,   &pf1);  params.get(gaNnReplacement,   &pi2);
   sprintf(bigstring, "Replacement Percentage %5.2f  Generations to Converge %5d",
           pf1, pi2);
   //fl_addto_browser(fd_gagraph->gaparmview, bigstring);
   params.get(gaNscoreFrequency, &pi1);  params.get(gaNflushFrequency, &pi2); 
   sprintf(bigstring, "Score Frequency        %5d  Flush Frequency         %5d",
           pi1, pi2);
   //fl_addto_browser(fd_gagraph->gaparmview, bigstring);
   sprintf(bigstring, "\nDefault Parameter File: %s", SETTINGS_FILE);
   //fl_addto_browser(fd_gagraph->gaparmview, bigstring);
   sprintf(bigstring, "\nSolution Space Size:    %-10.0f\n", bicoeff(n_tgts*n_opts, nRO));
   //fl_addto_browser(fd_gagraph->gaparmview, bigstring);

   UpdateCounter(theAppData.ga);

   //EvolveCB(NULL, 0);
   StepCB(NULL, 1);
}

void Initializer(GAGenome & c)
{
  GARealGenome& genome = (GARealGenome&)c;
  /* 
  genome.insert(3.0);
  genome.insert(21.0);
  genome.insert(7.0);
  genome.insert(14.0);
  */
}

GABoolean TerminateUponConvergence(GAGeneticAlgorithm &ga)
{
int             iopt, itgt, ihrd;
float           score;
char            littlestr[140];
char            bigstring[1260];

   fl_clear_browser(fd_gainput->gain_solutions);

   if (ga.generation() >= ga.nGenerations() ||
       (ga.population().best().score() < theAppData.tolerance)) {
     for(int i=0; i<ga.population().size(); i++) {
       score = ((GARealGenome&)(ga.population().individual(i))).score();
       if (score < theAppData.tolerance) {
         FinalActions = 0;
         strcpy(bigstring, "\n");
         for(int k=0; k<theAppData.lengthGenome; k=k+2) {
	   iopt = (int)((GARealGenome&)(ga.population().individual(i))).gene(k);
           itgt = (int)((GARealGenome&)(ga.population().individual(i))).gene(k+1);
           ihrd = 1;
           if (alltgts[itgt].Hard < 0.35) ihrd = 0;
           if (alltgts[itgt].Hard > 0.65) ihrd = 2;
           sprintf(littlestr, "%-10s %-10s %-4s | %-16s %-20s %-6s | %5.1f\n",
                  options[iopt].Arena, options[iopt].Type, options[iopt].Weapon,
                  alltgts[itgt].Name, TgtClass[alltgts[itgt].Class],
                  TgtHard[ihrd], score);
           strcat (bigstring, littlestr);
	   FinalOptions[FinalActions] = iopt;
	   FinalTargets[FinalActions] = itgt;
	   FinalActions++;
         }
       strcat(bigstring, "\n");
       fl_addto_browser(fd_gainput->gain_solutions, bigstring);
       fl_show_object(fd_gainput->gain_save);   
       } else break;
     }

     StopCB(NULL, 0);
     return (gaTrue);
   } else {
     return (gaFalse);
   }
}
/*                                                                       */
/* --------------------------------------------------------------------- */
/*          S U P P O R T   R O U T I N E S   S T A R T   H E R E        */
/* --------------------------------------------------------------------- */
/*                                                                       */
void UpdateSolution(GAGeneticAlgorithm *ga)
{
float           score;

     for(int i=0; i<ga->population().size(); i++) {
       score = ((GARealGenome&)(ga->population().individual(i))).score();
       if (score < theAppData.tolerance) {
         printf(" Genome %d with score of %f, Genes are: ", i, score);
         for(int k=0; k<theAppData.lengthGenome; k=k+2) {
           printf(" %d %d   ",
	      (int)((GARealGenome&)(ga->population().individual(i))).gene(k),
              (int)((GARealGenome&)(ga->population().individual(i))).gene(k+1));
         }
         printf("\n");
       } else break;
     }

     //TerminateUponConvergence(&ga);
}

void UpdateCounter(GAGeneticAlgorithm* ga)
{
char            littlestr[40];
char            bigstring[1260];
static char     txt[62];
GAGenome        *best;
 
  sprintf(txt, "Generation # %4d,  Best Score is %6.2f,  Population of %4d",
          ga->generation(),
          ga->population().best().score(),
          ga->population().size());
  //fl_set_object_label(fd_gagraph->ga_gen, txt);

  sprintf(bigstring, "Global Statistics:\n");
  sprintf(littlestr, "  Generation # ..... %8d\n", ga->statistics().generation());
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  # Selections ..... %8ld\n", ga->statistics().selections());
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  # Crossovers ..... %8ld\n", ga->statistics().crossovers());
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  # Mutations ...... %8ld\n", ga->statistics().mutations());
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  # Replacements ... %8ld\n", ga->statistics().replacements());
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  # Evaluations .... %8d\n", theAppData.gevals);
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Convergence ...... %8.2f\n", ga->statistics().convergence());
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Maximum score .... %8.2f\n", 0.0); //ga->statistics().worstEver());
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Minimum score .... %8.2f\n", 0.0); //ga->statistics().bestEver());
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Avg of all scores  %8.2f\n", ga->statistics().online());
  strcat (bigstring, littlestr);
  //sprintf(littlestr, "  Avg of max scores ...... %8.2f\n", 0.0);
  //strcat (bigstring, littlestr);
  //sprintf(littlestr, "  Avg of min scores ...... %8.2f\n", 0.0);
  //strcat (bigstring, littlestr);

  sprintf(littlestr, "Initial Population:\n");
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Mean score ....... %8.2f\n",
      ga->statistics().initial(GAStatistics::Mean));
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Max score ........ %8.2f\n",
      ga->statistics().initial(GAStatistics::Maximum));
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Min score ........ %8.2f\n",
      ga->statistics().initial(GAStatistics::Minimum));
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Std. Deviation ... %8.2f\n",
      ga->statistics().initial(GAStatistics::Deviation));
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Diversity ........ %8.2f\n",
      ga->statistics().initial(GAStatistics::Diversity));
  strcat (bigstring, littlestr);

  sprintf(littlestr, "Current Population:\n");
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Mean score ....... %8.2f\n",
      ga->statistics().current(GAStatistics::Mean));
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Max score ........ %8.2f\n",
      ga->statistics().current(GAStatistics::Maximum));
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Min score ........ %8.2f\n",
      ga->statistics().current(GAStatistics::Minimum));
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Std. Deviation ... %8.2f\n",
      ga->statistics().current(GAStatistics::Deviation));
  strcat (bigstring, littlestr);
  sprintf(littlestr, "  Diversity ........ %8.2f\n",
      ga->statistics().current(GAStatistics::Diversity));
  strcat (bigstring, littlestr);

  strcat (bigstring, "\0");

  //fl_clear_browser(fd_gagraph->gadata_browser); 
  //fl_addto_browser(fd_gagraph->gadata_browser, bigstring);

   fl_clear_browser(fd_gagraph->geneview);
   sprintf(opttemp, " No. Target/Actions are %5d\n\n", theAppData.lengthGenome/2);
   fl_addto_browser(fd_gagraph->geneview, opttemp);

   for(int i=0; i<theAppData.lengthGenome; i=i+2) {
     sprintf(opttemp, "Action = %-16s\nTarget = %-16s\n",
             options[(int)(((GARealGenome&)(ga->population().best())).gene(i))].Option,
             alltgts[(int)(((GARealGenome&)(ga->population().best())).gene(i+1))].Name );
     fl_addto_browser(fd_gagraph->geneview, opttemp);
   }

   UserData *data = (UserData *) (((GARealGenome&)(ga->population().best())).userData());

   whichScore = 1;
   sprintf(opttemp, "    -- Score is %5.1f --\n       (Zero (0) is best)",
           ((GARealGenome&)(ga->population().best())).score() );
   //fl_addto_browser(fd_gagraph->geneview, opttemp);

   //UpdateSolution(ga);
}

int Evolve(int n)
{

   if((n < 0 && theAppData.ga->done() == gaFalse) ||
       theAppData.ga->generation() < n) {
     theAppData.ga->step();
     DrawCB(NULL, 0);
     UpdateCounter(theAppData.ga);
     return FALSE;
   }
   return TRUE;
}

int OPTidleWP(XEvent *ev, void *data)
{
int             n;

   n = (int)data;
   Evolve(n);

   return(0);
}

 
void ResetCB(FL_OBJECT *object, long item_no)
{
   //AppDataPtr data = (AppDataPtr)cd;
   if(theAppData.procid) {
     fl_set_idle_callback(0, 0); 
     theAppData.procid = 0; 
   }
   theAppData.ga->initialize();
   //DrawCB(data->canvas, data, 0);
   UpdateCounter(theAppData.ga); 
}
 
void StopCB(FL_OBJECT *object, long item_no)
{
   //AppDataPtr data = (AppDataPtr)cd;
   if(theAppData.procid) { 
     fl_set_idle_callback(0, 0);  
     theAppData.procid = 0;  
   } 
}
 
void StepCB(FL_OBJECT *object, long item_no)
{
   //AppDataPtr data = (AppDataPtr)cd;
   Evolve(theAppData.ga->generation() + 1);
}
 
void EvolveSomeCB(FL_OBJECT *object, long item_no)
{
   //AppDataPtr data = (AppDataPtr)cd;
   fl_set_idle_callback(OPTidleWP,
     (void *)(theAppData.ga->generation() + theAppData.geninc));
   theAppData.procid = theAppData.geninc;
}

void EvolveCB(FL_OBJECT *object, long item_no)
{
   //AppDataPtr data = (AppDataPtr)cd;
   fl_set_idle_callback(OPTidleWP, (void *)-1);
   theAppData.procid = -1;
}
 
void QuitCB(FL_OBJECT *object, long item_no){
   done = 1;
}
/*
void DumpStatsCB(FL_OBJECT *object, long item_no)
{
   cerr << "\nstatistics are:\n" <<
     ((GAGeneticAlgorithm*)cd)->statistics() << "\n";
}
 
void DumpParamsCB(FL_OBJECT *object, long item_no)
{
   cerr << "\nparameters are:\n" <<
     ((GAGeneticAlgorithm*)cd)->parameters() << "\n";
}
*/
/*                                                                       */
/* --------------------------------------------------------------------- */
/*         G R A P H I C S   R O U T I N E S   S T A R T   H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
//
// This routine draws the entire population or a single individual depending
// on the value of the single flag.  It needs to know how much of a
// buffer to use for spacing between individuals.  We assume that each
// individual draws from its centroid.
//
// This is much more nicely done when you derive your own genome that includes
// a draw routine and all the graphics info in it, but for the purpose of this
// example we'll just make a separate function that draws the genome and others
// that return the graphics info about the genomes.
//
#define BUF 10
void DrawCB(FL_OBJECT *ob, long data)
{
int             i;
int             vpwindW, vpwindH;
int             viewport[4];
float           point[3], scale, xtic, fscale;
char            chlabel[16];

   if(!fl_form_is_visible(fd_gagraph->gagraph) ) return;

   glXMakeCurrent(fl_display, fl_get_canvas_id(fd_gagraph->ga_canvas),
                  fl_get_glcanvas_context(fd_gagraph->ga_canvas)); 
   /*
   fl_get_winsize(fl_get_canvas_id(fd_gagraph->ga_canvas), &vpwindW, &vpwindH); 
   glViewport(0, 0, vpwindW, vpwindH);
   */
   if (!OPTglInit) {
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      gluOrtho2D(0.0, vpwindW, -25.0, vpwindH);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();
      glfontMake(GL_ALLFONTS);        // Build the fonts
      glfontSet(GL_STROKE);
      OPTglInit = TRUE;
   }

   glClearColor(0.0, 0.0, 0.0, 0.0);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   if(theAppData.whichGA == 3) {
      GADemeGA* ga = (GADemeGA*)theAppData.ga;
      for(int i=0; i<ga->nPopulations(); i++)
	DrawPopulation(ga->population(i), theAppData.dotgc[i], theAppData.dotgc[i]);
   }
   else {
     DrawPopulation(theAppData.ga->population(), theAppData.dotgc[0], theAppData.bestgc);
   }

   glColor3f(1.0, 1.0, 1.0);
   glBegin(GL_LINES);
     glVertex2f(-10.0,  0.0);
     glVertex2f( 50.0,  0.0);
     glVertex2f(  0.0, -5.0);
     glVertex2f(  0.0, 15.0);
   glEnd();

   for (i=0; i<n_tgts; i++) {
     glPushMatrix();
       glTranslatef(i+1.0, -5.0, 0.0);
       glScalef(0.05, 0.04, 0.05);
       glRotatef(90.0, 0.0, 0.0, 1.0);
       sprintf(opttemp, "%s", alltgts[i].Name);
       glfontPrint(opttemp);
     glPopMatrix();
   }

   for (i=0; i<n_opts; i++) {
     glPushMatrix();
       glTranslatef(-10.0, i+1.0, 0.0);
       glScalef(0.06, 0.04, 0.05);
       strcpy(chlabel, options[i].Arena);
       sprintf(opttemp, "%c-%s%s", chlabel[0], options[i].Type, options[i].Weapon);
       glfontPrint(opttemp);
     glPopMatrix();
   }

   glFlush();
   glXSwapBuffers(fl_display, fl_get_canvas_id(fd_gagraph->ga_canvas));
} 

typedef float Dimension;
typedef struct { float x; float y; } GLPoint;

void DrawPopulation(const GAPopulation& pop, GC dotgc, GC bestgc) {
  static int npts = 0;
  static GLPoint pts[3][300];
 
   if(npts != pop.size()) {
     npts = pop.size();
     //delete [] pts;
     //pts = new GLPoint [nRO][npts];
   }
 
   FL_Coord width = 0, height = 0;
   fl_get_winsize(fl_get_canvas_id(fd_gagraph->ga_canvas), &width, &height); 
   //XtVaGetValues(widget, XtNwidth, &width, XtNheight, &height, NULL);
   Dimension w = width - 2 * BUF;
   Dimension h = height - 2 * BUF;
   Dimension d = (w < h ? w : h);
   w -= d;
   h -= d;
   Dimension originx = BUF + w/2;
   Dimension originy = BUF + h/2;
   float factor = (float)d;
   factor /= (maxx[theAppData.whichFunction] - minx[theAppData.whichFunction]);
   float xbest = 0, ybest = 0;

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(-10.0, 50.0, -5.0, 15.0);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   originx = 1.0, originy = 1.0;
   for(int i=0; i<pop.size(); i++) {
      for(int g=0; g<nRO*2; g=g+2) {
        pts[g/2][i].x = originx + /* d/2 + factor * */
                   ((GARealGenome&)(pop.individual(i))).gene(g+1);
        pts[g/2][i].y = originy + /* d/2 - factor * */
                   ((GARealGenome&)(pop.individual(i))).gene(g);
      }
   }
   xbest = originx + d/2 + factor *
      ((GARealGenome&)(pop.best())).gene(0);
   ybest = originy + d/2 - factor *
      ((GARealGenome&)(pop.best())).gene(1);

   glPointSize(3.0);

   glColor3f(1.0, 1.0, 0.0);
   glBegin(GL_POINTS);
      for (int i=0; i<npts; i++) glVertex2f(pts[0][i].x, pts[0][i].y);  
   glEnd();

   glColor3f(1.0, 1.0, 1.0);
   glBegin(GL_POINTS);
      for (int i=0; i<npts; i++) glVertex2f(pts[1][i].x, pts[1][i].y);  
   glEnd();

   glColor3f(0.5, 0.5, 0.5);
   glBegin(GL_POINTS);
      for (int i=0; i<npts; i++) glVertex2f(pts[2][i].x, pts[2][i].y);  
   glEnd();

   glColor3f(1.0, 1.0, 0.0);
   glBegin(GL_LINE_STRIP);
      glVertex2f(pts[0][0].x, pts[0][0].y);
      glVertex2f(pts[1][0].x, pts[1][0].y);
      glVertex2f(pts[2][0].x, pts[2][0].y);
   glEnd();

   return;
}
#undef BUF
/*                                                                       */
/* --------------------------------------------------------------------- */
/*          G E N O M E    O B J E C T I V E    F U N C T I O N S        */
/* --------------------------------------------------------------------- */
/*                                                                       */
//
// These are the objective functions for the genomes.  They simply call the
// appropriate function.
//
float
Bin2DecObjective(GAGenome& g) {
  GABin2DecGenome& genome = (GABin2DecGenome&)g;
  return (obj[theAppData.whichFunction])(genome.phenotype(0),
                     genome.phenotype(1));
}
 
float
RealObjective(GAGenome& g) {
  GARealGenome& genome = (GARealGenome&)g;

  fl_clear_browser(fd_gagraph->geneview);
  sprintf(opttemp, " -- Genome length is %4d --\n\n", genome.length());
  fl_addto_browser(fd_gagraph->geneview, opttemp);

  for(int i=0; i<genome.length(); i++){
    sprintf(opttemp, "Gene(%2d) = %8.4f\n", i, genome.gene(0));
    fl_addto_browser(fd_gagraph->geneview, opttemp);
  }

  return (obj[theAppData.whichFunction])(genome.gene(0), genome.gene(1));
}
/*****************************************************************************/
/* Type:        2D FUNCTION                                                  */
/* Name:        Objective2D_1                                                */
/* Description: 2D tooth                                                     */
/* Boundaries:  -6 < x < 6                                                   */
/*              -6 < y < 6                                                   */
/* Source:      modified Himmelblau's function from Deb, K.                  */
/*              'GA in multimodal function optimazation' Masters thesis      */
/*      TCGA Rep. 89002 / U. of Alabama                              */
/*****************************************************************************/
float
Function1(float x, float y)
{
  float z = -((x*x+y-11)*(x*x+y-11)+(x+y*y-7)*(x+y*y-7))/200 + 10;
  return z;
}
/*****************************************************************************/
/* Type:        2D FUNCTION                                                  */
/* Name:        Objective2D_2                                                */
/* Description: Foxholes (25)                                                */
/* Boundaries:  -60 < x < 60                                                 */
/*              -60 < y < 60                                                 */
/* Source:      Shekel's Foxholes problem from De Jong's Diss.(1975)         */
/*              'GA in multimodal function optimazation' Masters thesis      */
/*      TCGA Rep. 89002 / U. of Alabama                              */
/*****************************************************************************/
float
Function2(float x, float y)
{
  int i;
  float sum = 0;
 
  for (i=0; i<25; i++) {
    sum += (1 / (1 + i + pow((x-ai[i]),6) + pow((y-bi[i]),6)));
  }
  float z = 500.0 - (1 / (0.002 + sum));
  return z;
} 
/*****************************************************************************/
/* Type:        2D FUNCTION                                                  */
/* Name:        Objective2D_3                                                */
/* Description: Schwefel's nasty (4 glob. Max bei (+-420.96/+-420.96)        */
/* Boundaries:  -500 < x < 500                                               */
/*              -500 < y < 500                                               */
/* Source:      Schwefel's function in Schoeneburg                           */
/*****************************************************************************/
float
Function3(float x, float y)
{
  float z = fabs(x) * sin(sqrt(fabs(x))) + fabs(y) * sin(sqrt(fabs(y)));
  return 500 + z;
}
/*****************************************************************************/
/* Type:        2D FUNCTION                                                  */
/* Name:        Objective2D_4                                                */
/* Description: Mexican Hat                                                  */
/* Boundaries:  -10 < x < 10                                                 */
/*              -10 < y < 10                                                 */
/* Source:                                                                   */
/*****************************************************************************/
float
Function4(float x, float y)
{
  float z = sin(sqrt(x*x + y*y))*sin(sqrt(x*x + y*y)) - 0.5;
  z /= ((1.0 + 0.001*(x*x + y*y))*(1.0 + 0.001*(x*x + y*y)));
  z = (0.5 - z);
  return (z);
}
/*****************************************************************************/
/* Type:        2D FUNCTION                                                  */
/* Name:        Objective2D_4                                                */
/* Description: Mexican Hat                                                  */
/* Boundaries:  -10 < x < 10                                                 */
/*              -10 < y < 10                                                 */
/* Source:                                                                   */
/*****************************************************************************/
float
Function5(GAGenome& g)
{
int             i, ii, found, Sindex;
int             OptnCount, SiteCount;
float           rDE, DEbr, DErb, Tvalue;
float           Tdmbr, Tsvbr;
INSTANCES       tgts[10]; 

   GARealGenome& genome = (GARealGenome&)g;
   float value=0.0;

   theAppData.gevals++;
   /*
   fl_clear_browser(fd_gagraph->geneview);
   sprintf(opttemp, " -- Genome length is %4d --\n\n", genome.length());
   fl_addto_browser(fd_gagraph->geneview, opttemp);

   for(int i=0; i<genome.length(); i=i+2){
     sprintf(opttemp, "Gene(%2d) = %-16s\nGene(%2d) = %-16s\n",
             i,   options[(int)genome.gene(i)].Option,
             i+1, alltgts[(int)genome.gene(i+1)].Name );
     fl_addto_browser(fd_gagraph->geneview, opttemp);
   }
   */
//
//   Combine the factors for like targets and build target table
//
   for (i=0; i<10; i++) strcpy(tgts[i].Name, "--------");
   Sindex = 1;
   for(int g=0; g<genome.length(); g=g+2) {
     //
     //   Add the constraints to the affected factors
     //
     Tdmbr = options[(int)genome.gene(g)].DMbr;
     float Pdh = alltgts[(int)genome.gene(g+1)].Hard;
     if (strcmp(options[(int)genome.gene(g)].Weapon, "(N)") == 0)
         Pdh = Pdh * (1.0-0.7) + 0.7;
     Tdmbr = Tdmbr * (1.0 - Pdh);
     Tsvbr = options[(int)genome.gene(g)].SVbr;
     if (alltgts[(int)genome.gene(g+1)].Mobile) Tsvbr = MIN(1.0, Tsvbr+0.2);
     found = FALSE;
     for (i=0; i<Sindex; i++) {
       if (strcmp(tgts[i].Name, alltgts[(int)genome.gene(g+1)].Name) == 0) {
         tgts[i].SVrb = MAX(tgts[i].SVrb, options[(int)genome.gene(g)].SVrb);
         tgts[i].ARbr = MAX(tgts[i].ARbr, options[(int)genome.gene(g)].ARbr);
         tgts[i].DMbr = tgts[i].DMbr*(1.0-Tdmbr);
         tgts[i].SVbr = MIN(tgts[i].SVbr, Tsvbr);
         tgts[i].ARrb = tgts[i].ARrb*options[(int)genome.gene(g)].ARrb;
         tgts[i].DMrb = MIN(tgts[i].DMrb, options[(int)genome.gene(g)].DMrb);
         found = TRUE;
	 ii = i;
         //printf("Updating Entry %d name is %s with factors of:\n", i, tgts[i].Name);
       }
     }
     if (!found) {
       //
       //   Target not in target table, put it in
       //
       strcpy(tgts[Sindex-1].Name, alltgts[(int)genome.gene(g+1)].Name);
       tgts[Sindex-1].SVbr = options[(int)genome.gene(g)].SVbr;
       tgts[Sindex-1].ARbr = options[(int)genome.gene(g)].ARbr;
       tgts[Sindex-1].DMbr = 1.0 - options[(int)genome.gene(g)].DMbr;
       tgts[Sindex-1].SVrb = options[(int)genome.gene(g)].SVrb;
       tgts[Sindex-1].ARrb = options[(int)genome.gene(g)].ARrb;
       tgts[Sindex-1].DMrb = options[(int)genome.gene(g)].DMrb;
       tgts[Sindex-1].Value = alltgts[(int)genome.gene(g+1)].Value;
       ii = Sindex-1;
       //printf("Adding Entry %d name is %s with factors of:\n", Sindex-1, tgts[Sindex-1].Name);
       Sindex++;
     }
     /*    
     printf("     %f %f %f %f %f %f\n",
                  tgts[ii].SVbr,
                  tgts[ii].ARbr,
                  tgts[ii].DMbr,
                  tgts[ii].SVrb,
                  tgts[ii].ARrb,
                  tgts[ii].DMrb);
     */     
   }
   Sindex--; 
   //printf(" No. of target entries is %d\n", Sindex);
   //
   //   Calculate the combined rDE for each target and total the weights
   //
   Tvalue = 0;
   for (i=0; i<Sindex; i++) {
     tgts[i].rDE = (tgts[i].SVbr * tgts[i].ARbr * (1.0-tgts[i].DMbr)) /
                   (tgts[i].SVrb * tgts[i].ARrb * tgts[i].DMrb);
     Tvalue = Tvalue + tgts[i].Value;
     //printf("   Entry %d name is %s with rDE of %f\n", i, tgts[i].Name, tgts[i].rDE);
   }  
   //
   //   Weight the rDE by each target
   //
   rDE = 0.0;
   for (i=0; i<Sindex; i++) {
     tgts[i].Weight = (1.0 - ((float)tgts[i].Value/(float)Tvalue));
     rDE = rDE + (tgts[i].Weight*tgts[i].rDE);
   }

   rDE = (float)Sindex*rDE;

   UserData *data = (UserData *)genome.userData();
   data->rDE = rDE;
 
   value = theAppData.goal - rDE;
   if (value < 0.0) value = -value;
   if (whichScore) { value = rDE; whichScore = 0; }

   //printf( " -- Genome Score is %5.1f --\n", rDE);
   //fl_addto_browser(fd_gagraph->geneview, opttemp);

   return value;
} 
/*                                                                       */
/* --------------------------------------------------------------------- */
/*      C A L L B A C K    R O U T I N E S    S T A R T    H E R E       */
/* --------------------------------------------------------------------- */
/*                                                                       */
void OPTexitCB(FL_OBJECT *ob, long data)
{
   fl_hide_form(fd_gainput->gainput);
   EraseActiveEntry(OPTlabel);

   FinishUp();

   return;
}

void OPTnoneCB(FL_OBJECT *ob, long data)
{

   return;
}

void gachoiceCB(FL_OBJECT *ob, long data)
{
   switch ((int)data) {
     case 0:
       theAppData.whichGenome = fl_get_choice(fd_gaparms->gamenu_genome)-1;              
       break;
     case 1:
       theAppData.whichGA = fl_get_choice(fd_gaparms->gamenu_algorithm)-1;       
       break;
     case 2:
       //theAppData.whichFunction = fl_get_choice(fd_gainput->gamenu_function)-1;       
       break;
     default:       
       break;
   }
}

void OPTinputCB(FL_OBJECT *ob, long data)
{
float           tol;

   switch ((int)data) {
     case 0:
       theAppData.geninc = atoi(fl_get_input(fd_gaparms->ga_geninc));              
       break;

     case 2:
       theAppData.goal = atof(fl_get_input(fd_gainput->ga_goal));       
       break;

     case 3:
       tol = atof(fl_get_input(fd_gainput->ga_tol));
       theAppData.tolerance = tol/100.0 * theAppData.goal;
       break;

     default:       
       break;
   }
}

void gacanvasCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void gagraphCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void gainmodeCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void gaselectCB(FL_OBJECT *ob, long data)
{
   if (fl_mouse_button() == FL_RIGHTMOUSE) {
     printf("Selection box right clicked\n");
   } 
}

void gaparmsCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

void gasaveCB(FL_OBJECT *ob, long data)
{
FILE            *casefp;
int             i, iopt, itgt;

   if ((casefp = fopen("DetailedPlan.dat", "w")) != NULL) {
     fprintf(casefp, "%5d\n", FinalActions);
     for (i=0; i<FinalActions; i++) {
	iopt = FinalOptions[i];
	itgt = FinalTargets[i];
        fprintf(casefp, "%-32s    %-32s\n", options[iopt].Option, alltgts[itgt].Name);
     }
     fclose(casefp);
   } else {
      fl_show_messages("Error: Unable to open Detailed Plan file"); 
   }
}

void gaexitCB(FL_OBJECT *ob, long data)
{
   switch ((int)data) {
     case 0:          
       break;

     case 1:
       fl_hide_form(fd_gagraph->gagraph);
       OPTexitCB(NULL, 0);
       break;

     case 2:
       fl_hide_form(fd_gaparms->gaparms);
       break;

     case 3:
       fl_hide_form(fd_gacommit->gacommit);
       break;

     default:       
       break;
   }

   return;
}

void gainbuttonCB(FL_OBJECT *ob, long data)
{
int             i;
Window          winid;

   switch ((int)data) {
     case 0:
       sprintf(opttemp, "%d", nRO);
       fl_set_counter_value(fd_gacommit->opt_nactions, (double)nRO);
       fl_set_input(fd_gainput->gain_nRO, opttemp);
       fl_set_input(fd_gainput->gain_coaname, "C2SIM-00-Responses");
       fl_show_object(fd_gainput->gain_go);
       fl_show_object(fd_gagraph->ga_start);
       fl_show_object(fd_gainput->gain_adv);
       OPTload();
       break;

     case 2:
       fl_winposition(OPTwinX, OPTwinY);
       fl_initial_winsize(OPTwinW, OPTwinH);
       winid = fl_prepare_form_window(fd_gaparms->gaparms,
                    FL_PLACE_POSITION,FL_NOBORDER, "Plan Optimization: Parameter Settings");
       fl_winreparent(winid, OPTwinid);
       fl_show_form_window(fd_gaparms->gaparms);              
       break;

     case 1:
       fl_winposition(OPTwinX, OPTwinY);
       fl_initial_winsize(OPTwinW, OPTwinH);
       winid = fl_prepare_form_window(fd_gagraph->gagraph,
                    FL_PLACE_POSITION,FL_NOBORDER, "Plan Optimization: Algorithm Details");
       fl_winreparent(winid, OPTwinid);
       fl_show_form_window(fd_gagraph->gagraph);
       break;

     case 3:
       fl_winposition(OPTwinX, OPTwinY);
       fl_initial_winsize(OPTwinW, OPTwinH);
       winid = fl_prepare_form_window(fd_gacommit->gacommit,
                    FL_PLACE_POSITION,FL_NOBORDER, "Plan Optimization: Explicit Mode");
       fl_winreparent(winid, OPTwinid);
       fl_show_form_window(fd_gacommit->gacommit);

       fl_clear_browser(fd_gacommit->opt_tgtlist);
       for (i=0; i<n_Tgts; i++) {
	 sprintf(opttemp, "%-24s %11d %17d", TgtClass[i], TgtAvail[i], TgtComit[i]);
	 fl_addto_browser(fd_gacommit->opt_tgtlist, opttemp);
       }
       //
       fl_clear_browser(fd_gacommit->opt_reslist);
       for (i=0; i<n_Asts; i++) {
	 sprintf(opttemp, "%-24s %11d %17d", AstClass[i], AstAvail[i], AstComit[i]);
	 fl_addto_browser(fd_gacommit->opt_reslist, opttemp);
       }
       break;

     default:       
       break;
   }
}

/*** callbacks and freeobj handles for form gacommit ***/

void optcommitCB(FL_OBJECT *ob, long data)
{
int             l;

   switch ((int)data) {
     case 2:
       l = fl_get_browser(fd_gacommit->opt_reslist)-1;
       AstComit[l] = (int)fl_get_slider_value(fd_gacommit->opt_rescommit);
       if (AstComit[l] > AstAvail[l]) AstComit[l] = AstAvail[l];
       sprintf(opttemp, "@b%-24s %11d %17d", AstClass[l], AstAvail[l], AstComit[l]);
       fl_replace_browser_line(fd_gacommit->opt_reslist, l+1, opttemp);
       fl_deselect_browser(fd_gacommit->opt_reslist);
       break;

     case 7:
       l = fl_get_browser(fd_gacommit->opt_tgtlist)-1;
       TgtComit[l] = (int)fl_get_slider_value(fd_gacommit->opt_tgtcommit);
       if (TgtComit[l] > TgtAvail[l]) TgtComit[l] = TgtAvail[l];
       sprintf(opttemp, "@b%-24s %11d %17d", TgtClass[l], TgtAvail[l], TgtComit[l]);
       fl_replace_browser_line(fd_gacommit->opt_tgtlist, l+1, opttemp);
       fl_deselect_browser(fd_gacommit->opt_tgtlist);
       break;

     default:       
       break;
   }

   return;
}

void optactionCB(FL_OBJECT *ob, long data)
{
int             tol;

   switch ((int)data) {
     case 0:
       nRO = (int)fl_get_counter_value(fd_gacommit->opt_nactions);
       sprintf(opttemp, "%d", nRO);
       fl_set_input(fd_gainput->gain_nRO, opttemp);          
       break;

     case 1:
       tol = (int)fl_get_dial_value(fd_gacommit->opt_goodness);
       sprintf(opttemp, "%d", tol);
       fl_set_object_label(fd_gacommit->opt_goodness, opttemp);
       fl_set_input(fd_gainput->ga_tol, opttemp);
       theAppData.tolerance = tol/100.0 * theAppData.goal;
       break;

     default:       
       break;
   }

   return;
}

void optconstrainCB(FL_OBJECT *ob, long data)
{
  /* fill-in code for callback */
}

//<------------------------expose-------------------------------------->

//Run when the opengl screen needs to be refreshed 'cause it is "expose"

int OPTexposeCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
   fl_get_winsize(fl_get_canvas_id(fd_gagraph->ga_canvas), &OPTglwinW, &OPTglwinH); 
   glViewport(0, 0, OPTglwinW, OPTglwinH);

   //if (OPTglInit) DrawCB(ob, 0);

   return(0);
}

//<---------------------button press-------------------------------->

//when the user clicks in the opengl canvas window...the program goes here

int OPTbuttonCB(FL_OBJECT *ob, Window win, int w, int h, XEvent *xev, void *ud)
{
FL_Coord        width, height;
float           xpos, ypos, closest, d;
float           slat, slon, salt, shead;
int             i, xval, yval;

   xval = xev->xbutton.x;
   yval = xev->xbutton.y;

   return(0);
}
