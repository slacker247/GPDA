#include <stdio.h>
#include <stdlib.h>
#include <sqlext.h>
#include <ini.h>

#define MAX_DATA_WIDTH 300

#ifndef max
#define max( a, b ) (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
#define min( a, b ) (((a) < (b)) ? (a) : (b))
#endif

int OpenDatabase( SQLHENV *phEnv, SQLHDBC *phDbc, char *szDSN, char *szUID, char *szPWD );
int ExecuteSQL( SQLHDBC hDbc, char *szSQL, char cDelimiter, int bColumnNames, int bHTMLTable );
int CloseDatabase( SQLHENV hEnv, SQLHDBC hDbc );
int DumpODBCLog( SQLHENV hEnv, SQLHDBC hDbc, SQLHSTMT hStmt );
SQLUINTEGER OptimalDisplayWidth( SQLHSTMT hStmt, SQLINTEGER nCol, int nUserWidth );

