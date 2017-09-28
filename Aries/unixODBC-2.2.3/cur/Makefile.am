lib_LTLIBRARIES = libodbccr.la

INCLUDES = -I../include \
            -I../DriverManager $(INCLTDL)

EXTRA_DIST = \
	cursorlibrary.h \
	cur.exp

if QNX
libodbccr_la_LIBADD = \
	-L$(prefix)/lib -lodbc \
	$(LIBLTDL) $(LIBADD_DL)
else
libodbccr_la_LIBADD = \
	../DriverManager/libodbc.la \
	$(LIBLTDL) $(LIBADD_DL)
endif

libodbccr_la_LDFLAGS = \
	-version-info @LIB_VERSION@ \
	-export-symbols cur.exp -export-dynamic \
	-module

# PAH - Redundant?
#libodbccr_la_DEPENDENCIES = \
#    $(LIBLTDL)

libodbccr_la_SOURCES = \
    SQLAllocHandle.c \
    SQLAllocHandleStd.c \
    SQLAllocStmt.c \
    SQLBindCol.c \
    SQLBindParam.c \
    SQLBindParameter.c \
    SQLCancel.c \
    SQLCloseCursor.c \
    SQLColAttribute.c \
    SQLColAttributes.c \
    SQLColumnPrivileges.c \
    SQLColumns.c \
    SQLConnect.c \
    SQLCopyDesc.c \
    SQLDescribeCol.c \
    SQLDescribeParam.c \
    SQLEndTran.c \
    SQLError.c \
    SQLExecDirect.c \
    SQLExecute.c \
    SQLExtendedFetch.c \
    SQLFetch.c \
    SQLFetchScroll.c \
    SQLForeignKeys.c \
    SQLFreeHandle.c \
    SQLFreeStmt.c \
    SQLGetConnectAttr.c \
    SQLGetConnectOption.c \
    SQLGetCursorName.c \
    SQLGetData.c \
    SQLGetDescField.c \
    SQLGetDescRec.c \
    SQLGetDiagRec.c \
    SQLGetDiagField.c \
    SQLGetInfo.c \
    SQLGetStmtAttr.c \
    SQLGetStmtOption.c \
    SQLGetTypeInfo.c \
    SQLMoreResults.c \
    SQLNativeSql.c \
    SQLNumParams.c \
    SQLNumResultCols.c \
    SQLParamData.c \
    SQLParamOptions.c \
    SQLPrepare.c \
    SQLPrimaryKeys.c \
    SQLProcedureColumns.c \
    SQLProcedures.c \
    SQLPutData.c \
    SQLRowCount.c \
    SQLSetConnectAttr.c \
    SQLSetConnectOption.c \
    SQLSetCursorName.c \
    SQLSetDescRec.c \
    SQLSetDescField.c \
    SQLSetParam.c \
    SQLSetPos.c \
    SQLSetScrollOptions.c \
    SQLSetStmtAttr.c \
    SQLSetStmtOption.c \
    SQLSpecialColumns.c \
    SQLStatistics.c \
    SQLTablePrivileges.c \
    SQLTables.c \
    SQLTransact.c
