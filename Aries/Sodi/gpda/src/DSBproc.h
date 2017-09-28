/*
 *   Dempster-Shafer belief network stuff
 */
struct params {
  float         latitude;
  float         longitude;
  float         altitude;
  float         deltaStartT;
  float         deltaCurrentT;
  char          chparam[40];
};

struct assess {
  int           select;
  int           piecolor;
  float         belief;
  float         plause;
  float         Bthreshold;
  float         Tthreshold;
  char          chassess[40];
};

struct hypoth {
  int           select;
  float         belief;
  float         plause;
  float         confidence;
  float         deltaT;
  char          chhypoth[40];
};

struct source {
  int           evidcolor;
  float         belief;
  float         plause;
  char          chsource[16];
};

struct evid {
  int           valid;
  int           source;
  int           level;
  int           type;
  int           subnode;
  int           sublevel;
  float         confidence;
  float         plause;
  float         disbelief;
  float         Tin;
  float         duration;
  char          chsource[16];
  char          chdescript[20];
  char          latitude[16];
  char          longitude[16];
  char          altitude[16];
  char          subfname[64];
};

struct assesstype {
  char          chname[40];
  int           n_src;
  int           c_src;
  struct source sources[10];                 // Sources
  int           n_hyp;
  int           c_hyp;
  int           l_hyp;
  struct hypoth hypothesis[10][4];           // Hypothesis (up to 4 levels)
  int           n_ass;
  int           c_ass;
  struct assess assessment[10];              // Assessment
  int           n_par;
  int           c_par;
  struct params parameters[10];              // Parameters
};
