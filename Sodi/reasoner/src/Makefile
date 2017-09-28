#----------------------------------------------------------------
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
SPEEDESDIR = /home/dre/speedes0.45
#SPEEDESDIR = /usr/local/Speedes
PROXDIR    = $(SPEEDESDIR)/demos/prox
SOURCEDIR  = $(SPEEDESDIR)/demos/prox/SimCmdr
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
	   -I$(SOURCEDIR) -I/usr/X11/include \
	   -I$(SODIHOME)/Inc \
	   -I$(SODIHOME)/Inc/X11 \
	   -I$(SODIHOME)/Inc/GL \
	   -I$(HOME)/Jtamv/include \
	   -I/usr/include/g++
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
FOR	= $(HOME)/bin/f90
#
FLAGS   = -D$(ARCH) -DCROS -DMERC -DHOST -DFASTCOMB -DCPP20 $(INCLUDES)
CFLAGS  = $(FLAGS) -g -c -lm
CCFLAGS = $(FLAGS) -g -c -lm
FFLAGS  = -g -c -w -dusty $(INCLUDES)
LFLAGS  = -L$(SPEEDESLIB) -l_speedes -l_utilities -l_prox3d \
        -L/usr/X11R6/lib \
		-L$(SODIHOME)/Lib -lforms -lXpm \
		-L$(SODIHOME)/Lib/X11 -lXm -lXt -lX11 -lXext -lXmu \
	    -L$(SODIHOME)/Lib/GL -lGLU -lGL -lGLw -lgltk \
        -lm
#
# external libraries (machine constants are in the PORT library)
#
# C++ language source files

CCSRC = CBRforms.c CBRmain.C CBRcases.C
#CCSRC = CBRforms.c CBRforms_main.c CBRforms_cb.c
#
# Object files:
#OBJECTS = $(CCSRC:.C=.o)  $(CSRC:.c=.o)
OBJECTS = $(CCSRC:.C=.o)  $(CSRC:.c=.o) $(FSRC:.f=.o)

reasoner: $(OBJECTS)
	$(C++) -g -I$(INCLUDES) -o reasoner $(OBJECTS) $(LFLAGS)

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


