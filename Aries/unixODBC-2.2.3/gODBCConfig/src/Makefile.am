## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gODBCConfig
lib_LTLIBRARIES = libgtkodbcconfig.la

include_HEADERS = odbcconfig.h

INCLUDES = \
	-I../../include \
	-I$(top_srcdir)/intl \
	-I../../libltdl \
	$(GNOME_INCLUDEDIR)

gODBCConfig_SOURCES = \
	main.c 

libgtkodbcconfig_la_SOURCES = \
	odbcconfig.c odbcconfig.h \
	support.c support.h \
	interface.h \
	callbacks.c callbacks.h

gODBCConfig_DEPENDENCIES = libgtkodbcconfig.la

gODBCConfig_LDADD = $(GNOME_LIBDIR) \
		$(GNOMEUI_LIBS) \
		$(INTLLIBS) \
		-L$(ODBCLIBS) $(ODBCLIB) \
		libgtkodbcconfig.la 
		

