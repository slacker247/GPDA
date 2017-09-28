/*
 *   Dempster-Shafer belief network stuff
 */
struct params {
char          chparam[40];
float         latitude;
float         longitude;
float         altitude;
float         deltaStartT;
float         deltaCurrentT;
};

struct assess {
char          chassess[40];
float         belief;
float         plause;
float         Bthreshold;
float         Tthreshold;
int           select;
int           piecolor;
};

struct hypoth {
char          chhypoth[40];
float         belief;
float         plause;
int           select;
float         confidence;
float         deltaT;
};

struct source {
char          chsource[16];
float         belief;
float         plause;
int           evidcolor;
};

struct evid {
char          chsource[16];
char          chdescript[20];
float         confidence;
float         deltaT;
float         latitude;
float         longitude;
float         altitude;
};

struct assesstype {
char          chname[40];
int           n_src;
int           c_src;
struct source sources[10];                 // Sources
int           n_hyp;
int           c_hyp;
struct hypoth hypothesis[10];              // Hypothesis
int           n_ass;
int           c_ass;
struct assess assessment[10];              // Assessment
int           n_par;
int           c_par;
struct params parameters[10];              // Parameters
};
