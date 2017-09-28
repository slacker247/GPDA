libodbcinstQ_la_LDFLAGS = \
        -version-info 1:0:0 \
		-export-dynamic \
		-export-symbols odbcinstQ.exp \
	    -module

if QT

lib_LTLIBRARIES = libodbcinstQ.la

INCLUDES = -I../include @QT_INCLUDES@ \
    -DSYSTEM_FILE_PATH=\"@sysconfdir@\" -DDEFLIB_PATH=\"@libdir@\" $(INCLTDL)

libodbcinstQ_la_LIBADD = @X_LDFLAGS@ \
	@QT_LDFLAGS@ \
	@USER_LDFLAGS@ \
	@LIB_QT@ \
	../odbcinst/libodbcinst.la \
	../extras/libodbcextraslc.la \
	../DriverManager/libodbc.la 

libodbcinstQ_la_DEPENDANCIES = \
	../odbcinst/libodbcinst.la \
	../extras/libodbcextraslc.la \
	../DriverManager/libodbc.la

libodbcinstQ_la_SOURCES = \
	CAbout.cpp \
	CCredits.cpp \
	CDLL.cpp \
	CDSNList.cpp \
	CDriverPrompt.cpp \
	CDrivers.cpp \
	CFileDSN.cpp \
	CFileList.cpp \
	CFileSelector.cpp \
	CODBCConfig.cpp \
	CProperties.cpp \
	CPropertiesFrame.cpp \
	CSplashDialog.cpp \
	CStatDetails.cpp \
	CStatSummary.cpp \
	CStats.cpp \
	CSystemDSN.cpp \
	CTracing.cpp \
	CUserDSN.cpp \
	mCAbout.cpp \
	mCCredits.cpp \
	mCDSNList.cpp \
	mCDriverPrompt.cpp \
	mCDrivers.cpp \
	mCFileDSN.cpp \
	mCFileList.cpp \
	mCFileSelector.cpp \
	mCODBCConfig.cpp \
	mCProperties.cpp \
	mCPropertiesFrame.cpp \
	mCStatDetails.cpp \
	mCStatSummary.cpp \
	mCStats.cpp \
	mCSystemDSN.cpp \
	mCTracing.cpp \
	mCUserDSN.cpp \
	SQLManageDataSources.cpp

mCDrivers.cpp: CDrivers.h
	@MOC@ CDrivers.h -o mCDrivers.cpp

mCODBCConfig.cpp: CODBCConfig.h
	@MOC@ CODBCConfig.h -o mCODBCConfig.cpp

mCSystemDSN.cpp: CSystemDSN.h
	@MOC@ CSystemDSN.h -o mCSystemDSN.cpp

mCFileDSN.cpp: CFileDSN.h
	@MOC@ CFileDSN.h -o mCFileDSN.cpp

mCUserDSN.cpp: CUserDSN.h
	@MOC@ CUserDSN.h -o mCUserDSN.cpp

mCTracing.cpp: CTracing.h
	@MOC@ CTracing.h -o mCTracing.cpp

mCStats.cpp: CStats.h
	@MOC@ CStats.h -o mCStats.cpp

mCStatSummary.cpp: CStatSummary.h
	@MOC@ CStatSummary.h -o mCStatSummary.cpp

mCStatDetails.cpp: CStatDetails.h
	@MOC@ CStatDetails.h -o mCStatDetails.cpp

mCAbout.cpp: CAbout.h
	@MOC@ CAbout.h -o mCAbout.cpp

mCDriverPrompt.cpp: CDriverPrompt.h
	@MOC@ CDriverPrompt.h -o mCDriverPrompt.cpp

mCPropertiesFrame.cpp: CPropertiesFrame.h
	@MOC@ CPropertiesFrame.h -o mCPropertiesFrame.cpp

mCProperties.cpp: CProperties.h
	@MOC@ CProperties.h -o mCProperties.cpp

mCFileSelector.cpp: CFileSelector.h
	@MOC@ CFileSelector.h -o mCFileSelector.cpp

mCDSNList.cpp: CDSNList.h
	@MOC@ CDSNList.h -o mCDSNList.cpp

mCFileList.cpp: CFileList.h
	@MOC@ CFileList.h -o mCFileList.cpp

mCCredits.cpp: CCredits.h
	@MOC@ CCredits.h -o mCCredits.cpp

endif

EXTRA_DIST = \
	CAbout.h \
	CTracing.h \
	CStats.h \
	CStatSummary.h \
	CSplashDialog.h \
	CStatDetails.h \
	CDLL.h \
	CDSNList.h \
	CFileList.h \
	CDriverPrompt.h \
	CDrivers.h \
	CFileSelector.h \
	CODBCConfig.h \
	CPropertiesFrame.h \
	CProperties.h \
	CSystemDSN.h \
	CFileDSN.h \
	CUserDSN.h \
	CCredits.h \
	Splash.xpm \
	ODBC.xpm \
	computer.xpm \
	driver.xpm \
	info.xpm \
	checkOk.xpm \
	checkCancel.xpm \
	person.xpm \
	server.xpm \
	smallLinuxODBC.xpm \
	ODBC.xpm \
	Canada.xpm \
	GreatBritain.xpm \
	trace.xpm \
	NoFlag.xpm \
	odbcinstQ.exp


