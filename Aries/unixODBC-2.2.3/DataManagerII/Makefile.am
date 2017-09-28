if QT

bin_PROGRAMS = DataManagerII

INCLUDES = -I../include @QT_INCLUDES@

DataManagerII_LDADD = @X_LDFLAGS@ \
	@QT_LDFLAGS@ \
	@USER_LDFLAGS@ \
	@LIB_QT@ \
	../odbcinst/libodbcinst.la \
	../DriverManager/libodbc.la 

DataManagerII_DEPENDANCIES = \
    ../odbcinst/libodbcinst.la \
	../DriverManager/libodbc.la 

DataManagerII_SOURCES = \
	classBrowse.cpp \
	classBrowseFrame.cpp \
	classCanvas.cpp \
	classColumn.cpp \
	classConnectionFrame.cpp \
	classDataManager.cpp \
	classDataSource.cpp \
	classDataSources.cpp \
	classDriver.cpp \
	classDrivers.cpp \
	classISQL.cpp \
	classIndexs.cpp \
	classLogin.cpp \
	classNode.cpp \
	classODBC.cpp \
	classPrimaryKeys.cpp \
	classSpecialColumns.cpp \
	classTable.cpp \
	classTables.cpp \
	main.cpp \
	mclassDataManager.cpp \
	mclassCanvas.cpp \
	mclassLogin.cpp \
	mclassISQL.cpp \
	mclassConnectionFrame.cpp \
	mclassBrowseFrame.cpp \
	mclassBrowse.cpp 

mclassDataManager.cpp: classDataManager.h
	@MOC@ classDataManager.h -o mclassDataManager.cpp

mclassCanvas.cpp: classCanvas.h
	@MOC@ classCanvas.h -o mclassCanvas.cpp

mclassLogin.cpp: classLogin.h
	@MOC@ classLogin.h -o mclassLogin.cpp

mclassISQL.cpp: classISQL.h
	@MOC@ classISQL.h -o mclassISQL.cpp

mclassConnectionFrame.cpp: classConnectionFrame.h
	@MOC@ classConnectionFrame.h -o mclassConnectionFrame.cpp

mclassBrowseFrame.cpp: classBrowseFrame.h
	@MOC@ classBrowseFrame.h -o mclassBrowseFrame.cpp

mclassBrowse.cpp: classBrowse.h
	@MOC@ classBrowse.h -o mclassBrowse.cpp

endif

EXTRA_DIST = \
	classCanvas.h \
	classColumn.h \
	classDataManager.h \
	classDataSource.h \
	classDataSources.h \
	classDriver.h \
	classDrivers.h \
	classConnectionFrame.h \
	classBrowseFrame.h \
	classBrowse.h \
	classISQL.h \
	classIndex.h \
	classIndexs.h \
	classLogin.h \
	classNode.h \
	classODBC.h \
	classPrimaryKeys.h \
	classSpecialColumns.h \
	classTable.h \
	classTables.h \
	LinuxODBC.xpm \
	column.xpm \
	computergreen.xpm \
	computerred.xpm \
	datasourceoff.xpm \
	datasourceon.xpm \
	datasourcessystem.xpm \
	datasourcesuser.xpm \
	driver.xpm \
	drivers.xpm \
	exec.xpm \
	id.xpm \
	keygold.xpm \
	keyred.xpm \
	keysilver.xpm \
	keysilver2.xpm \
	new.xpm \
	open.xpm \
	run.xpm \
	save.xpm \
	table.xpm \
	tables.xpm \
	ODBC.xpm
