bin_PROGRAMS = isql dltest odbcinst iusql

INCLUDES = -I../include $(INCLTDL)

EXTRA_DIST = isql.h isql_axp.opt

if QNX
odbcinst_LDADD = \
	-L$(prefix)/lib -lodbcinst \
	$(LIBLTDL) $(LIBADD_DL) 

odbcinst_DEPENDENCIES = \
    $(LIBLTDL) 
else
odbcinst_LDADD = \
	../odbcinst/libodbcinst.la \
	$(LIBLTDL) $(LIBADD_DL) 

odbcinst_DEPENDENCIES = \
    $(LIBLTDL) \
    ../odbcinst/libodbcinst.la
endif

odbcinst_SOURCES = odbcinst.c 

if QNX
isql_LDADD = \
	-L$(prefix)/lib -lodbc \
	-lodbcinst \
 	$(READLINE)

isql_DEPENDENCIES = \
	$(LIBLTDL)
else
isql_LDADD = \
	../DriverManager/libodbc.la \
	../odbcinst/libodbcinst.la \
 	$(READLINE)

isql_DEPENDENCIES = \
	../odbcinst/libodbcinst.la \
	../DriverManager/libodbc.la \
	$(LIBLTDL)
endif

isql_SOURCES = isql.c

if QNX
iusql_LDADD = \
	-L$(prefix)/lib -lodbc \
	-lodbcinst \
 	$(READLINE)

iusql_DEPENDENCIES = \
	$(LIBLTDL)
else
iusql_LDADD = \
	../DriverManager/libodbc.la \
	../odbcinst/libodbcinst.la \
 	$(READLINE)

iusql_DEPENDENCIES = \
	../odbcinst/libodbcinst.la \
	../DriverManager/libodbc.la \
	$(LIBLTDL)
endif

iusql_SOURCES = iusql.c

dltest_SOURCES = dltest.c

dltest_DEPENDENCIES = $(LIBLTDL)

dltest_LDADD = $(LIBLTDL) $(LIBADD_DL)

