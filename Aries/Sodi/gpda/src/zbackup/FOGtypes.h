
#define FOG_EVIDENCE    0
#define FOG_WEIGHT      1
#define FOG_OUTCOME     2

#define FOG_LOSS        0
#define FOG_LATENCY     1
#define FOG_ASYNCH      2
#define FOG_DEGRADATION 3
#define FOG_AMBIGUITY   4
#define FOG_CONFLICT    5
#define FOG_OVERLOAD    6
#define FOG_BADLUCK     7
#define FOG_SUPPRISE    10
#define FOG_DISPOSITION 11
#define FOG_COGNITION   12
#define FOG_CONFUSION   13
#define FOG_PRIORITIES  14
#define FOG_MISCOMM     15
#define FOG_ASSUMPTION  16

extern void FOGset(int Icause, int Ieffect, float fog[2]);
extern int  FOGget(int Icause, int Ieffect, float fog[2]);
extern void FOGon();
extern void FOGoff();
extern int  FOGtest();

