TARGET = jtamv
ARCH   = IRIX
ARCH   = Linux

LIBGROBJS =  GR_Interface.o GR_Widget.o GR_Window.o GR_Shell.o \
             GR_DispList.o GR_DispObj.o
LIBGR2OBJS = GR_2Dlines.o GR_Bearth.o GR_Tearth.o GR_Eearth.o GR_Tarea.o GR_Area.o GR_Region.o \
             GR_Lines.o GR_AirObj.o GR_Trail.o GR_Model.o GR_Sensor.o \
             GR_Links.o GR_Army.o GR_Carib.o GR_Cloud.o GR_Work.o GR_Idlist.o \
             GR_Stars.o GR_Defend.o GR_Impact.o GR_Surface.o GR_Terrain.o dtedutils.o
LIBGR3OBJS = GR_Spslinks.o misc.o GR_Socket.o GR_String.o GR_TrackErr.o
OBJS     = jtamv.o vf.o mv.o ov.o gd.o tv.o tracker.o editor.o texture.o fselector.o winbmp.o
GISPOBJS = GISP_Pers_Obj.o GISP_Obj.o convert.o newsps.o glfont.o assets.o \
			flatmap.o GR_Map2D.o GR_MapPoint.o GR_Image.o 
FOROBJS  = llasec.o
ALLOBJS  = $(OBJS) $(GISPOBJS) $(FOROBJS)

SPEEDESHOME = $(HOME)/Speedes
SPSINCPATH1 = $(SPEEDESHOME)/src/speedes
SPSINCPATH2 = $(SODIHOME)/odisim/include
#SPSINCPATH2 = $(SPEEDESHOME)/demos/prox/src
SPSLIBPATH1 = $(SPEEDESHOME)/demos/prox/lib/$(ARCH)
SPSLIBPATH2 = $(SPEEDESHOME)/lib/ArchitectureDirs/$(ARCH)

SPSLIBS = ../../odisim/src/libprox.a \
	$(SPSLIBPATH2)/lib_speedes.a \
    $(SPSLIBPATH2)/lib_shmem.a \
	$(SPSLIBPATH2)/lib_utilities.a
LIBGR = ../lib/libGR.a
LIBGR2 = ../lib/libGR2.a
LIBGR3 = ../lib/libGR3.a

CC = g++
F77 = f77
CFLAGS = -g -DFUNCPROTO -D$(ARCH) -I../include -I/usr/local/include -I/usr/X11/include \
	-I$(SPEEDESHOME)/src/comm -I$(SPEEDESHOME)/src/utilities \
	-I$(SPEEDESHOME)/src/speedes \
    -I$(SODIHOME)/Inc \
	-I$(SODIHOME)/Inc/X11 \
	-I$(SODIHOME)/Inc/GL -I/usr/include/g++
FFLAGS = -c -g
UFLAGS = -Dc_plusplus -c -D$(ARCH)
CUFLAGS = $(CFLAGS) $(UFLAGS)
SPSFLAGS = -I$(SPSINCPATH1) -I$(SPSINCPATH2) 
ULIBS = $(LIBGR) $(LIBGR2) $(LIBGR3)
###ULIBS = $(LIBGR) $(LIBGR2) $(LIBGR3) ../lib/libMessager.a
#LDFLAGS = -L/usr/local/lib/NAGWare -L$(HOME)/lib -lnag -lnagff9 -lnagfl90 -lncar
LDFLAGS = -L../lib -lGR -lGR2 -lGR3 \
	  -L$(SODIHOME)/Lib/GL -lGLU -lGL \
	  -L$(SODIHOME)/Lib -lXfwf -lXc -lXpm \
	  -L/usr/X11R6/lib -L$(SODIHOME)/Lib/X11 -lXm -lXt -lX11 -lXext -lXmu \
      -L$(SODIHOME)/Lib/GL -lGLw -lgltk \
	  -lf2c -lm
#     -L/lib -L/usr/lib -lf95 -lstdc++ -lm -lgcc -lc -lgcc

all: $(TARGET)
	@echo "--------------- $(TARGET) All Done --------------"
	@echo "---------- For architecture $(ARCH) -----------"

$(TARGET): $(ALLOBJS) $(LIBGR) $(LIBGR2) $(LIBGR3)
	@echo "....... LINKING $@ file ......."
	$(CC) -o $@ $(ALLOBJS) $(SPSLIBS) $(LDFLAGS)
##	f95 -c -w -dusty supmap.for
##	gcc -c -I/usr/X11/include -I$(HOME)/OpenGL/include ncarplot.c
##	$(CC) $(CFLAGS) -o $@ $(ALLOBJS) $(SPSLIBS) $(LDFLAGS)
##	$(CC) $(CFLAGS) -o $@ $(ALLOBJS) $(ULIBS) $(SPSLIBS) $(LDFLAGS)

$(OBJS) $(LIBGROBJS) $(LIBGR2OBJS) $(LIBGR3OBJS):
	@echo "..... COMPILING $@ file ....."
	$(CC) $(CUFLAGS) $(SPSFLAGS) -o $@ $*.C

$(GISPOBJS): $(LIBGR) $(LIBGR2) $(LIBGR3)
	@echo "..... COMPILING $@ file ....."
	$(CC) $(CUFLAGS) $(SPSFLAGS) -o $@ $*.C
##	$(CC) $(CUFLAGS) $(SPSFLAGS) -o $@ $*.C $(ULIBS) $(SPSLIBS)

$(FOROBJS):
	@echo "---> MAKING F77 ROUTINES "
	$(F77) $(FFLAGS) -o $@ $*.f

$(LIBGR): $(LIBGROBJS)
	@echo "---> MAKING LIBRARY $@ "
	ar rv $@ $?

$(LIBGR2): $(LIBGR2OBJS)
	@echo "---> MAKING LIBRARY $@ "
	ar rv $@ $?

$(LIBGR3): $(LIBGR3OBJS)
	@echo "---> MAKING LIBRARY $@ "
	ar rv $@ $?

