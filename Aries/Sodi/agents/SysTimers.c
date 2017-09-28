#include "UnixTimers.h"
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/times.h>

void UnixTimers_getTimes(struct HUnixTimers *this) {
    long elapsed;
    struct tms tmptimers;

    elapsed = times(&tmptimers);
/*
    fprintf(stderr, "Elapsed time is %d\n", elapsed);
    fprintf(stderr, "User time is    %d\n", tmptimers.tms_utime);
    fprintf(stderr, "System time is  %d\n", tmptimers.tms_stime);
    fprintf(stderr, "Child user time %d\n", tmptimers.tms_cutime);
    fprintf(stderr, "Child sys time  %d\n", tmptimers.tms_cstime);
*/
    unhand(this)->putime = (double)tmptimers.tms_utime;
    unhand(this)->pstime = (double)tmptimers.tms_stime;
    unhand(this)->cutime = (double)tmptimers.tms_cutime;
    unhand(this)->cstime = (double)tmptimers.tms_cstime;
    unhand(this)->elapsed = (double)elapsed;

    return;
}
