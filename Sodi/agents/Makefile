#
all: ParkAgent.class RoamAgent.class HomeAgent.class StatsReader.class Launch.class
# libtimers.so
#
ParkAgent.class: ParkAgent.java
	javac IParkAgent.java
	javac ParkAgent.java
#
RoamAgent.class: RoamAgent.java
	javac IRoamAgent.java
	javac RoamAgent.java
#
HomeAgent.class: HomeAgent.java
	javac IHomeAgent.java
	javac HomeAgent.java
#
StatsReader.class: StatsReader.java
	javac StatsReader.java
#
Launch.class: Launch.java
	javac Launch.java
#
libtimers.so: SysTimers.o UnixTimers.o
	echo Generating dynamic library
	gcc -shared SysTimers.o UnixTimers.o -o libtimers.so
#
SysTimers.o: SysTimers.c
	echo Compiling C-based timer interface routine
	gcc -c -I/usr/local/lib/java/include -I/usr/local/lib/java/include/genunix SysTimers.c
#
UnixTimers.o: UnixTimers.c UnixTimers.java
	echo Compiling Java-based timer routine
	javac UnixTimers.java
	echo Generating include and stub files
	javah -o UnixTimers.h UnixTimers
	javah -stubs UnixTimers
	gcc -c -I/usr/local/lib/java/include -I/usr/local/lib/java/include/genunix UnixTimers.c
#
