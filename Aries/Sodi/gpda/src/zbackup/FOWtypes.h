
#define FOW_EVIDENCE    0
#define FOW_WEIGHT      1
#define FOW_OUTCOME     2

#define FOW_LOSS        0
#define FOW_LATENCY     1
#define FOW_ASYNCH      2
#define FOW_DEGRADATION 3
#define FOW_AMBIGUITY   4
#define FOW_CONFLICT    5
#define FOW_OVERLOAD    6
#define FOW_BADLUCK     7
#define FOW_SUPPRISE    10
#define FOW_DISPOSITION 11
#define FOW_COGNITION   12
#define FOW_CONFUSION   13
#define FOW_PRIORITIES  14
#define FOW_MISCOMM     15
#define FOW_ASSUMPTION  16

extern void FOWset(int Icause, int Ieffect, float fog[2]);
extern int  FOWget(int Icause, int Ieffect, float fog[2]);
extern void FOWon();
extern void FOWoff();
extern int  FOWtest();

