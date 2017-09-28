noinst_LTLIBRARIES = libodbcextraslc.la
lib_LTLIBRARIES = libodbcextras.la

INCLUDES = -I../include

EXTRA_DIST = extras.exp \
	vms.c

libodbcextraslc_la_SOURCES = \
    strcasecmp.c

libodbcextras_la_LDFLAGS = \
    -version-info @LIB_VERSION@ \
    -export-dynamic \
	-export-symbols extras.exp 

libodbcextras_la_SOURCES = \
    strcasecmp.c
