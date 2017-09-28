#----------------------------------------------------------------
#         Makefile for General Purpose Decision Aids
#----------------------------------------------------------------
#
#  Define the Architecture here once and for all
#
#ARCH = SUN4
#ARCH = SGI
ARCH  = Linux
#
#----------------------------------------------------------------
# Directories
#----------------------------------------------------------------
#
#SPEEDESDIR = /home/dre/speedes0.45
#PROXLIB    = $(SPEEDESDIR)/lib/ArchitectureDirs/$(ARCH) -l_prox3d
#PROXINC    = $(SPEEDESDIR)/demos/prox/src
SPEEDESDIR = $(HOME)/Speedes
PROXLIB    = $(SODIHOME)/Lib -lprox
PROXINC    = $(SODIHOME)/odisim/include
#
PROXDIR    = $(SPEEDESDIR)/demos/prox
LIBDIR = $(PROXDIR)/lib/$(ARCH)
BINDIR = $(PROXDIR)/bin/$(ARCH)
EXEDIR = $(PROXDIR)/exe/$(ARCH)
#
#----------------------------------------------------------------
# Include directories
#----------------------------------------------------------------
#
INCLUDES = -I$(SPEEDESDIR)/src/speedes \
	-I$(SPEEDESDIR)/src/comm/shmem \
	-I$(SPEEDESDIR)/src/utilities \
	-I$(PROXINC) \
	-I$(SODIHOME)/Inc/X11 -I/usr/X11/include -I$(SODIHOME)/Inc \
	-I$(SODIHOME)/Inc/GL \
	-I$(SODIHOME)/fzclips/include -I$(SODIHOME)/battleman/include -I./case \
	-I$(SODIHOME)/ga -I/usr/include/g++
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
FOR	= g77
LD	= ld
#
FLAGS   = -D$(ARCH) -DLINKLANL -DLINKSC $(INCLUDES)
CFLAGS  = $(FLAGS) -g -c -lm $(INCLUDES)
CCFLAGS = $(FLAGS) -g -c -lm $(INCLUDES)
FFLAGS  = -g -c -w $(INCLUDES)
LFLAGS  = -L$(PROXLIB) \
	-L$(SPEEDESLIB) -l_speedes -l_utilities \
	-L$(SODIHOME)/Lib -lga -lforms -lfclips -llanl -lXc -lXpm \
	-L$(SODIHOME)/Lib/GL -lglut -lGLU -lGL -lGLw -lgltk \
	-L$(SODIHOME)/Lib/X11 -L/usr/X11R6/lib -lXm -lXt -lX11 -lXext -lXmu \
	-lpthread -lf2c -lg2c -lm
#
# C++ language source files
#
CCSRC = BMCmain.C BMCforms.c about.c \
	SitAware.C texture.C gl.c glfont.C \
	DSBforms.c DSBproc.C DSBalgo.C DSBattributes.C ShortPath.f \
	CBPforms.c CBPproc.C \
	REDIforms.c REDIproc.C \
	TGTforms.c TGTproc.C \
	TLEforms.c TLEproc.C \
	ITELforms.c ITELproc.C \
	CLUforms.c CLUproc.C \
	OPTforms.c OPTproc.C \
	PUPforms.c PUPproc.C \
	FOWforms.c FOWproc.C \
	H3Dforms.c H3Dproc.C \
	SIMforms.c SIMproc.C \
	TCPforms.c TCPproc.C
#
SOSRC =	DSBforms.c DSBproc.C DSBalgo.C DSBattributes.C ShortPath.f \
	CBPforms.c CBPproc.C \
	REDIforms.c REDIproc.C \
	TGTforms.c TGTproc.C \
	TLEforms.c TLEproc.C \
	ITELforms.c ITELproc.C \
	CLUforms.c CLUproc.C \
	OPTforms.c OPTproc.C \
	PUPforms.c PUPproc.C \
	FOWforms.c FOWproc.C \
	H3Dforms.c H3Dproc.C \
	TCPforms.c TCPproc.C \
	libprint.c
#
SOOBJ =	DSBforms.o DSBproc.o DSBalgo.o DSBattributes.o ShortPath.o \
	CBPforms.o CBPproc.o \
	REDIforms.o REDIproc.o \
	TGTforms.o TGTproc.o \
	TLEforms.o TLEproc.o \
	ITELforms.o ITELproc.o \
	CLUforms.o CLUproc.o \
	OPTforms.o OPTproc.o \
	PUPforms.o PUPproc.o \
	FOWforms.o FOWproc.o \
	H3Dforms.o H3Dproc.o \
	TCPforms.o TCPproc.o \
	libprint.o
#
#CSRC = decode_body.c decode_header.c \
# 	encode_body.c encode_header.c \
#	translator.c
#
# Object files:
#
OBJECTS = $(CCSRC:.C=.o)  $(CSRC:.c=.o) $(FSRC:.f=.o)
LIBOBJS = $(SOSRC:.C=.o)

all: seded gpda

seded: DSBformsp.h 
	sed -f DSBsed.sed <DSBforms.h >DSBformsp.h
	cp DSBformsp.h DSBforms.h
	sed -f CBPsed.sed <CBPforms.h >CBPformsp.h
	cp CBPformsp.h CBPforms.h

DSB: DSBmain.C DSBproc.C DSBalgo.C DSBattributes.C PUPproc.C
	$(C++) -g -o DSB $(INCLUDES) \
	DSBmain.C DSBproc.C DSBalgo.C DSBattributes.C DSBforms.c \
	PUPproc.C PUPforms.c \
	ShortPath.f FOWproc.C FOWforms.c H3Dproc.C H3Dforms.c $(LFLAGS)

DBM: DBMmain.C ITELproc.C CLUproc.C
	$(C++) -g -o DBM $(INCLUDES) \
	DBMmain.C Common.C ITELproc.C ITELforms.c CLUproc.C CLUforms.c $(LFLAGS)

CLU: CLUmain.C CLUproc.C CLUforms.c
	$(C++) -g -o CLU $(INCLUDES) CLUmain.C CLUproc.C CLUforms.c $(LFLAGS)

TCP: TCPmain.C TCPproc.C TCPforms.c
	$(C++) -g -o TCP $(INCLUDES) TCPmain.C TCPproc.C TCPforms.c $(LFLAGS)

XPS: XPSmain.c XPSproc.c argv.c
	$(C) -g -o XPS $(INCLUDES) XPSmain.c XPSproc.c argv.c XPSforms.c \
	-L./temp -lxfl $(LFLAGS)

OPT: OPTmain.C OPTproc.C OPTforms.c FOGuser.C
	$(C++) -g -o OPT $(INCLUDES) OPTmain.C OPTproc.C OPTforms.c FOGuser.C gl.c glfont.C \
	$(LFLAGS)

SIM: SIMmain.C SIMproc.C SIMforms.c 
	$(C++) -g -o SIM $(INCLUDES) SIMmain.C SIMproc.C SIMforms.c gl.c glfont.C \
	$(LFLAGS)
#
#................. GPDA Testbed ................................
#
gpda: $(OBJECTS)
	$(C++) -g -o gpda $(INCLUDES) $(OBJECTS) $(LFLAGS)

dll: $(LIBOBJS)
	$(C++) -g -c -fPIC $(INCLUDES) $(LIBOBJS) $(LFLAGS)
	$(LD) -shared -soname libGPDA.so.1 -o libGPDA.so.1.0 -lc $(SOOBJ)

test:
	$(C++) -g -o client $(INCLUDES) \
	DBMmain.C Common.C glfont.o \
	-L. -lGPDA $(LFLAGS)
#
#................. GPDA Splash Screen .........................
#
odi: ODImain.C ODIforms.c
	$(C++) -g -o odi $(INCLUDES) ODImain.C ODIforms.c $(LFLAGS)

#
#................ Fused Ops Picture Editor ...................
#
jtamved: jtamved.C VEDforms.c
	$(C++) -g -o jtamved $(INCLUDES) jtamved.C VEDforms.c $(LFLAGS)

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

