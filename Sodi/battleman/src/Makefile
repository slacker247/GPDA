#----------------------------------------------------------------
# Makefile for LANL_BP
#
#----------------------------------------------------------------

#  Define the Architecture here once and for all

#ARCH = SUN4
#ARCH = SGI
#ARCH = KSR1
ARCH  = Linux
#
#----------------------------------------------------------------
# directories
#----------------------------------------------------------------
#
SPEEDESDIR = /usr/local/Speedes
PROXDIR    = $(SPEEDESDIR)/demos/prox
SOURCEDIR  = $(SPEEDESDIR)/demos/prox/Planner
LIBDIR = $(PROXDIR)/lib/$(ARCH)
BINDIR = $(PROXDIR)/bin/$(ARCH)
EXEDIR = $(PROXDIR)/exe/$(ARCH)
#
#----------------------------------------------------------------
# Include directories
#----------------------------------------------------------------
#
INCLUDES = -I$(SPEEDESDIR)/src/speedes -I$(SPEEDESDIR)/src/comm/shmem \
           -I$(SPEEDESDIR)/src/utilities -I$(SPEEDESDIR)/demos/prox/src \
	   -I/usr/local/LessTif/include \
	   -I$(SOURCEDIR) -I/usr/X11/include -I$(HOME)/include -I../simcmdr/include
#
#----------------------------------------------------------------
# Library directories
#----------------------------------------------------------------
#
SPEEDESLIB   = $(SPEEDESDIR)/lib/ArchitectureDirs/$(ARCH)
SHMEMLIB     = $(SPEEDESDIR)/lib/ArchitectureDirs/$(ARCH)
UTILITIESLIB = $(SPEEDESDIR)/lib/ArchitectureDirs/$(ARCH)
#
#----------------------------------------------------------------
#  Fortran, C, and C++ compilers and flags:
#----------------------------------------------------------------
#
C       = gcc
C++     = g++
FOR		= f77
#
FLAGS   = -D$(ARCH) -DCROS -DMERC -DHOST -DFASTCOMB -DCPP20 $(INCLUDES)
CFLAGS  = $(FLAGS) -g -c -lm
CCFLAGS = $(FLAGS) -g -c -lm
FFLAGS  = -g -c -w -dusty $(INCLUDES)
LFLAGS  = -L$(SPEEDESLIB) -l_speedes -l_utilities -l_prox3d \
		-L/usr/X11R6/lib -L/usr/local/LessTif/lib -lXm -lXt -lX11 -lXext -lXmu \
		-L$(HOME)/fzclips/lib -lfclips \
		-L$(HOME)/lib -lXc -lf2c -lm
#
# external libraries (machine constants are in the PORT library)
#
# C++ language source files

CCSRC = planner.C

#
# F language source files

FSRC =	gpals_sim.f \
	fortsetup.f \
	conin1.f \
	distance.f \
	engagement_plan.f \
	expected_damage.f \
	feas.f \
	fly_threat.f \
	get_data.f \
	giinterp.f \
	gisrchar.f \
	gilnchaz.f \
	giprobk.f \
	giquad.f \
	giroots.f \
	gistate.f \
	initial.f \
	matrix.f \
	pre_process.f \
	read_data.f \
	read_threats.f \
	wta.f

#
# Object files:
#OBJECTS = $(CCSRC:.C=.o)  $(CSRC:.c=.o)
OBJECTS = $(CCSRC:.C=.o)  $(CSRC:.c=.o) $(FSRC:.f=.o)

bmc3 : $(OBJECTS)
#	$(C++) -v $(FLAGS) -L/usr/lang/SC1.0 -g -o lanl_bp $(OBJECTS) \
#	-lm -lF77  -lU77 -lI77 -lisam -lmpc
	$(C++) -v -g -o bmc3 $(OBJECTS) $(LFLAGS)

#...............................................................
# keep track of include file dependencies:
.KEEP_STATE:
#
# clear suffix defaults and specify our own:
.SUFORIXES:
.SUFFIXES: .c .C .o .s .f
#
# specify how to make a .o file from a .c file
.c.o:
	$(C) $(CFLAGS) $<
# specify how to make a .o file from a .c file
.C.o:
	$(C++) $(CCFLAGS) $<
#specify how to make a .o file from a .f file
.f.o:
	$(FOR) $(FFLAGS) $<
#
#  end of Makefile
#===============================================================


