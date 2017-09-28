//
//
//   Alarm types (in Timeline file):
//
//     ALARM
//     WAKEUP
//     UPDATE
//     SCHED
//     IGNORE
//
//   Alarm Actions - ALARM
//
#define ALARM_NONE        -1
#define ALARM_INTEL       0
#define ALARM_LIMIT       1
#define ALARM_LAUNCH      5
#define ALARM_TRACK       6
#define ALARM_POTEVENT    7
#define ALARM_JCSWARN     10
#define ALARM_JCSEXEC     11
#define ALARM_JCSTERM     12
//
//   Alarm Actions - WAKEUP
//
#define ALARM_TIMER       100
//
//   Alarm priorities
//
#define PRIORITY_CRIT     4
#define PRIORITY_HI       3
#define PRIORITY_MED      2
#define PRIORITY_LOW      1
#define PRIORITY_OFF      0

extern void Alarm(int action, char *label, int priority, char *arg);
