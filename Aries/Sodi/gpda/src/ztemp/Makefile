ROOT =		..
PRODUCTS =	xps
INSTALLDIR =	$(BINDIR)
all:		$(PRODUCTS)

PKGINCDIR = -I $(XFORMS)/include
PKGLIBDIR = $(XFORMS)/lib

#-------------------------------------------------------------------------

xps:	xps.o\
	xps_fdui.o\
	argv.o\
	-lxfl\
	-lforms\
	-lX11\
	-lm

xps.o: xps_fdui.h

$(INSTALLDIR)/xps:	xps

#-------------------------------------------------------------------------

include $(ROOT)/Localrules
