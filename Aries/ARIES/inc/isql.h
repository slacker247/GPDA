#include <sqlext.h>
#include "ini.h"

#define MAX_DATA_WIDTH 300

int OpenDatabase( SQLHENV *phEnv, SQLHDBC *phDbc, char *szDSN, char *szUID, char *szPWD );
int ExecuteSQL( SQLHDBC hDbc, char *szSQL, char cDelimiter, int bColumnNames, int bHTMLTable, int &nColumns );
int CloseDatabase( SQLHENV hEnv, SQLHDBC hDbc );
int DumpODBCLog( SQLHENV hEnv, SQLHDBC hDbc, SQLHSTMT hStmt );
SQLUINTEGER OptimalDisplayWidth( SQLHSTMT hStmt, SQLINTEGER nCol, int nUserWidth );
