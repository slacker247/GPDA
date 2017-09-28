#ifndef THREATS_H
#define THREATS_H                            "Threat variables"


struct THREAT {
  float           ltime;                // Launch time
  float           rdist;                // Random distance
  double          llat;                 // Launch latitude
  double          llon;                 // Launch longitude
  double          tlat;                 // Target latitude
  double          tlon;                 // Target longitude
  char	          *names;               // Threat names
  char            *types;               // Threat types (ie: SCUD, SS18)
  char            *lsite;               // Launch site
  char            *tsite;               // Target site
  char            *latNS;
  char            *lonEW;
  char            *tatNS;
  char            *tonEW;
};

extern struct THREAT   *Threats;

extern int   ThreatLoad(char *loadfile);
extern int   ThreatGetLLAH(int index, float *lat, float *lon, float *alt, float *head);
extern int   ThreatGetIcon(int index);
extern float ThreatGetScale(int index);
extern char  *ThreatGetName(int index);
extern void  ThreatPopup(int);
extern void  ThreatUpdate(int);

#endif
