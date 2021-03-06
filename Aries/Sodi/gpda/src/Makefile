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
SPEEDESDIR = $(HOME)/Speedes
PROXLIB    = $(SODIHOME)/Lib -lprox
PROXINC    = $(SODIHOME)/odisim/include
#
PROXDIR    = $(SPEEDESDIR)/demos/prox
LIBDIR     = $(PROXDIR)/lib/$(ARCH)
BINDIR     = $(PROXDIR)/bin/$(ARCH)
EXEDIR     = $(PROXDIR)/exe/$(ARCH)
#
#----------------------------------------------------------------
# Include directories
#----------------------------------------------------------------
# 
INCLUDES = -I$(SODIHOME)/Inc/X11 -I/usr/X11/include \
	-I$(SODIHOME)/Inc \
	-I$(SODIHOME)/Inc/GL \
	-I$(SODIHOME)/ga \
	-I$(SODIHOME)/fzclips/include -I./case -I../include \
	-I$(HOME)/unixODBC-2.2.3/include \
	-I/usr/include/g++
#
#   Add the following include directories if SPEEDES and
#   LANL Battle Planner functionality are to be included.
#
#	-I$(SPEEDESDIR)/src/speedes \
#	-I$(SPEEDESDIR)/src/comm/shmem \
#	-I$(SPEEDESDIR)/src/utilities \
#	-I$(PROXINC) \
#	-I$(SODIHOME)/battleman/include
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
PAS     = gpc
LD	= ld
#
#   Define the following for the specified functionality:
# 	LINKLANL			 /* <--- For LANL BP & SPEEDES       */
# 	LINKSC                           /* <--- For Sim. Commander          */
# 	LINKCIC                          /* <--- For Message interface       */
#	DIS				 /* <--- For DIS interface           */
#
FLAGS   = -D$(ARCH) -DLINKCIC -DDIS
CFLAGS  = $(FLAGS) -g -c -lm $(INCLUDES)
CCFLAGS = $(FLAGS) -g -c -lm $(INCLUDES)
FFLAGS  = $(FLAGS) -g -c -w  $(INCLUDES)
#
#   Add the following library directories if SPEEDES and
#   LANL Battle Planner functionality are to be included.
#
#LFLAGS  = -L$(PROXLIB) \
#	-L$(SPEEDESLIB) -l_speedes -l_utilities
# 
LFLAGS = -L$(SODIHOME)/Lib -lga -lforms -lfclips -llanl -lXc -lXpm \
	-L$(SODIHOME)/Lib/GL -lglut -lGLU -lGL -lGLw -lgltk \
	-L$(SODIHOME)/Lib/X11 -L/usr/X11R6/lib -lXm -lXt -lX11 -lXext -lXmu \
	-lpthread -lf2c -lg2c -lm
#
# C++ language source files
#
CCSRC = BMCmain.C BMCforms.c \
	SITproc.C texture.C gl.c glfont.C \
	DSBforms.c DSBproc.C DSBalgo.C DSBattributes.C DSBevidence.C DSBconfig.C \
	savedNet.C DSBpath.f \
	CBPforms.c CBPproc.C \
	REDIforms.c REDIproc.C \
	TGTforms.c TGTproc.C \
	TLEforms.c TLEproc.C \
	CLAforms.c CLAproc.C \
	CLUforms.c CLUproc.C \
	OPTforms.c OPTproc.C \
	PUPforms.c PUPproc.C \
	FOWforms.c FOWproc.C \
	H3Dforms.c H3Dproc.C EXPLforms.c EXPLproc.C \
	SIMforms.c SIMproc.C \
	TCPforms.c TCPproc.C
#
SOSRC =	DSBforms.c DSBproc.C DSBalgo.C DSBattributes.C DSBevidence.C DSBconfig.C \
	savedNet.C DSBpath.f \
	CBPforms.c CBPproc.C \
	REDIforms.c REDIproc.C \
	TGTforms.c TGTproc.C \
	TLEforms.c TLEproc.C \
	CLAforms.c CLAproc.C \
	CLUforms.c CLUproc.C \
	OPTforms.c OPTproc.C \
	PUPforms.c PUPproc.C \
	FOWforms.c FOWproc.C \
	H3Dforms.c H3Dproc.C EXPLforms.c EXPLproc.C \
	TCPforms.c TCPproc.C \
	libprint.c
#
SOOBJ =	DSBforms.o DSBproc.o DSBalgo.o DSBattributes.o DSBevidence.o DSBconfig.o \
	savedNet.o DSBpath.o \
	CBPforms.o CBPproc.o \
	REDIforms.o REDIproc.o \
	TGTforms.o TGTproc.o \
	TLEforms.o TLEproc.o \
	CLAforms.o CLAproc.o \
	CLUforms.o CLUproc.o \
	OPTforms.o OPTproc.o \
	PUPforms.o PUPproc.o \
	FOWforms.o FOWproc.o \
	H3Dforms.o H3Dproc.o EXPLforms.c EXPLproc.C \
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

seded: DSBforms.h CBPforms.h
	sed -f DSBsed.sed <DSBforms.h >DSBformsp.h
	cp DSBformsp.h DSBforms.h
	sed -f CBPsed.sed <CBPforms.h >CBPformsp.h
	cp CBPformsp.h CBPforms.h

DSB: DSBmain.C DSBproc.C DSBalgo.C DSBattributes.C savedNet.C PUPproc.C
	$(C++) -g -o DSB $(INCLUDES) \
	DSBmain.C DSBproc.C DSBalgo.C DSBattributes.C DSBevidence.C DSBconfig.C DSBforms.c \
	PUPproc.C PUPforms.c EXPLproc.C EXPLforms.c \
	savedNet.C DSBpath.f FOWproc.C FOWforms.c H3Dproc.C H3Dforms.c isql.C stacked.c \
	$(LFLAGS) -L.usr/local/lib -lodbc

BOX: BOXmain.C BOXproc.C BOXforms.c
	$(C++) -g -o BOX $(INCLUDES) \
	BOXmain.C BOXproc.C BOXforms.c isql.C \
	$(LFLAGS) -L/usr/local/lib -lodbc

CBP: CBPmain.C CBPproc.C CBPforms.c
	sed -f CBPsed.sed <CBPforms.h >CBPformsp.h
	cp CBPformsp.h CBPforms.h
	$(C++) -g -o CBP $(INCLUDES) \
	CBPmain.C CBPproc.C CBPforms.c \
	$(LFLAGS)

COA: COAmain.C COAproc.C CBPforms.c
	sed -f CBPsed.sed <CBPforms.h >CBPformsp.h
	cp CBPformsp.h CBPforms.h
	$(C++) -g -o COA $(INCLUDES) \
	COAmain.C COAproc.C CBPforms.c \
	$(LFLAGS)

CLA: CLAmain.C CLAproc.C CLAforms.c
	$(C++) -g -o CLA $(INCLUDES) \
	CLAmain.C CLAproc.C CLAforms.c Common.C isql.C \
	$(LFLAGS) -L/usr/local/lib -lodbc

CLU: CLUmain.C CLUproc.C CLUforms.c
	$(C++) -g -o CLU $(INCLUDES) \
	CLUmain.C CLUproc.C CLUforms.c isql.C \
	$(LFLAGS) -L/usr/local/lib -lodbc

ITEL: ITELmain.C ITELproc.C ITELforms.c
	$(C++) -g -o ITEL $(INCLUDES) \
	ITELmain.C ITELproc.C ITELforms.c Common.C isql.C \
	$(LFLAGS) -L/usr/local/lib -lodbc

FOW: FOWmain.C FOWproc.C FOWforms.c
	$(C++) -g -o FOW $(INCLUDES) \
	FOWmain.C FOWproc.C FOWforms.c isql.C \
	$(LFLAGS) -L/usr/local/lib -lodbc

PUP: PUPmain.C PUPproc.C PUPforms.c
	$(C++) -g -o PUP $(INCLUDES) \
	PUPmain.C PUPproc.C PUPforms.c isql.C \
	$(LFLAGS) -L/usr/local/lib -lodbc

TCP: TCPmain.C TCPproc.C TCPforms.c
	$(C++) -g -o TCP $(INCLUDES) \
	TCPmain.C TCPproc.C TCPforms.c isql.C \
	$(LFLAGS) -L/usr/local/lib -lodbc

TGT: TGTmain.C TGTproc.C TGTforms.c EXPLproc.C EXPLforms.c
	$(C++) -g -o TGT $(INCLUDES) \
	TGTmain.C TGTproc.C TGTforms.c EXPLproc.C EXPLforms.c \
	$(LFLAGS)

TLE: TLEmain.C TLEproc.C TLEforms.c
	$(C++) -g -o TLE $(INCLUDES) \
	TLEmain.C TLEproc.C TLEforms.c Common.C gl.c glfont.C \
	$(LFLAGS)

XPS: XPSmain.c XPSproc.c XPSforms.c argv.c
	$(C) -g -o XPS $(INCLUDES) \
	XPSmain.c XPSproc.c argv.c XPSforms.c \
	-L./temp -lxfl $(LFLAGS)

OPT: OPTmain.C OPTproc.C OPTforms.c
	$(C++) -g -o OPT $(INCLUDES) \
	OPTmain.C OPTproc.C OPTforms.c gl.c glfont.C \
	$(LFLAGS)

SIM: SIMmain.C SIMproc.C SIMforms.c 
	$(C++) -g -o SIM $(INCLUDES) \
	SIMmain.C SIMproc.C SIMforms.c gl.c glfont.C \
	$(LFLAGS)

EXPL: EXPLmain.C EXPLproc.C EXPLforms.c
	$(C++) -g -o EXPL $(INCLUDES) \
	EXPLmain.C EXPLproc.C EXPLforms.c \
	$(LFLAGS)

#testform: stacked.c
#	gcc -g -o test $(INCLUDES) stacked.c $(LFLAGS)
#
#
#................. GPDA Testbed ................................
#
gpda: $(OBJECTS)
	$(C++) -g -o gpda $(INCLUDES) \
	$(OBJECTS) stacked.c isql.C \
	$(LFLAGS) -L/usr/local/lib -lodbc

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
VED: VEDmain.C VEDforms.c
	$(C++) -g -o VED $(INCLUDES) VEDmain.C VEDforms.c $(LFLAGS)

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
	echo $(INCLUDES)
	$(C++) $(CCFLAGS) $<
#specify how to make a .o file from a .f file
.f.o:
	$(FOR) $(FFLAGS) $<
#
#  end of Makefile
#===============================================================

