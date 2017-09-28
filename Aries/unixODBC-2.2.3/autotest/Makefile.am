lib_LTLIBRARIES = libgtrtst.la
noinst_LTLIBRARIES = libgtrtstlc.la

INCLUDES = -I../include -I../odbctest

EXTRA_DIST = \
	gtrtst.exp

libgtrtst_la_LDFLAGS = \
	-version-info @LIB_VERSION@ \
	-export-dynamic \
	-export-symbols gtrtst.exp 

libgtrtst_la_DEPENDENCIES = \
	../DriverManager/libodbc.la 

libgtrtst_la_SOURCES = \
	gtrtst.c

libgtrtstlc_la_SOURCES = \
	gtrtst.c
