if QT

bin_PROGRAMS = odbctest

INCLUDES = -I../include @QT_INCLUDES@ -DSYSTEM_FILE_PATH=\"@sysconfdir@\" $(INCLTDL)

odbctest_LDADD = @X_LDFLAGS@ \
	@QT_LDFLAGS@ \
	@USER_LDFLAGS@ \
	@LIB_QT@ \
	../odbcinst/libodbcinst.la \
	../DriverManager/libodbc.la \
	$(LIBLTDL) $(LIBADD_DL)

odbctest_DEPENDANCIES = ../odbcinst/libodbcinst.la ../DriverManager/libodbc.la

odbctest_SOURCES = \
	tools.cpp \
	stmt.cpp \
	results.cpp \
	open.cpp \
	odbctest.cpp \
	installer.cpp \
	env.cpp \
	edit.cpp \
	desc.cpp \
	diag.cpp \
	conn.cpp \
	catalog.cpp \
	attr.cpp \
	moc_results.cpp \
	moc_stmt.cpp \
	moc_odbctest.cpp \
	moc_desc.cpp \
	moc_diag.cpp \
	moc_env.cpp \
	moc_catalog.cpp \
	moc_conn.cpp \
	moc_attr.cpp \
	moc_tools.cpp

moc_results.cpp: results.h
	@MOC@ results.h -o moc_results.cpp

moc_stmt.cpp: stmt.h
	@MOC@ stmt.h -o moc_stmt.cpp

moc_odbctest.cpp: odbctest.h
	@MOC@ odbctest.h -o moc_odbctest.cpp

moc_desc.cpp: desc.h
	@MOC@ desc.h -o moc_desc.cpp

moc_diag.cpp: diag.h
	@MOC@ diag.h -o moc_diag.cpp

moc_env.cpp: env.h
	@MOC@ env.h -o moc_env.cpp

moc_catalog.cpp: catalog.h
	@MOC@ catalog.h -o moc_catalog.cpp

moc_conn.cpp: conn.h
	@MOC@ conn.h -o moc_conn.cpp

moc_attr.cpp: attr.h
	@MOC@ attr.h -o moc_attr.cpp

moc_tools.cpp: tools.h
	@MOC@ tools.h -o moc_tools.cpp

endif

EXTRA_DIST = \
	results.h \
	stmt.h \
	odbctest.h \
	desc.h \
	diag.h \
	env.h \
	catalog.h \
	conn.h \
	attr.h \
	tools.h 

