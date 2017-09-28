#include <stdlib.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/sysinfo.h>
#include <stdio.h>


main(int argc, char *argv[])
{
struct sysinfo info;
struct utsname osname;

       sysinfo(&info);

	printf("Seconds since boot %d\n", info.uptime);
	printf("Total usable main memory size %dMb\n", info.totalram/(1024*1000));
	printf("Available main memory %dMb\n", info.freeram/(1024*1000));

//             struct sysinfo {
//                  long uptime;             /* Seconds since boot */
//                  unsigned long loads[3];  /* 1, 5, and 15 minute load averages */
//                  unsigned long totalram;  /* Total usable main memory size */
//                  unsigned long freeram;   /* Available memory size */
//                  unsigned long sharedram; /* Amount of shared memory */
//                  unsigned long bufferram; /* Memory used by buffers */
//                  unsigned long totalswap; /* Total swap space size */
//                  unsigned long freeswap;  /* swap space still available */
//                  unsigned short procs;    /* Number of current processes */
//                  unsigned long totalhigh; /* Total high memory size */
//                  unsigned long freehigh;  /* Available high memory size */
//                  unsigned int mem_unit;   /* Memory unit size in bytes */
//                  char _f[20-2*sizeof(long)-sizeof(int)]; /* Padding for libc5 */
//             };

	uname(&osname);

	printf("Name of the Operating System %s [%s]\n", osname.sysname, osname.release);
	printf("Nmae of Operating System release %s\n", osname.version);
	printf("Name of this node on the network %s\n", osname.nodename);
	printf("Machine hardware architecture is %s\n", osname.machine);

	printf("User's login name %s\n", getlogin() );

}

