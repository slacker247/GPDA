//
// To Compile: g++ -g -I../../unixODBC-2.2.3/include sqltest.C -L/usr/local/lib -lodbc
//

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

int             bHTMLTable    = 0;
int             bBatch        = 1;
int             cDelimiter    = 0;
int             bColumnNames  = 0;
int 	        bVerbose      = 0;
int             nUserWidth    = 0;
SQLHSTMT        hStmt;
SQLHENV         hEnv          = 0;
SQLHDBC         hDbc          = 0;

int OpenDatabase(SQLHENV *phEnv, SQLHDBC *phDbc, char *szDSN, char *szUID, char *szPWD);
int ExecuteSQL(SQLHDBC hDbc, char *szSQL, char cDelimiter, int bColumnNames, int bHTMLTable);
int CloseDatabase(SQLHENV hEnv, SQLHDBC hDbc);
int DumpODBCLog(SQLHENV hEnv, SQLHDBC hDbc, SQLHSTMT hStmt);

int TestSQL()
{
int 	        nArg, count;
int 	        bHTMLTable	        = 0;
int	        bBatch			= 1;
int 	        cDelimiter		= 0;
int             bColumnNames            = 0;
char            *line;
int             malloced;
char	        szDSN[MAX_DATA_WIDTH+1]	= "MySQL";
char	        szUID[MAX_DATA_WIDTH+1]	= "root";
char	        szPWD[MAX_DATA_WIDTH+1]	= "gpda";
char	        szSQL[9001]		= "show databases;";
char            *pEscapeChar;

   /****************************
    * CONNECT
    ***************************/	
   if ( !OpenDatabase( &hEnv, &hDbc, szDSN, szUID, szPWD ))
     return(-1);
   if ( bVerbose ) {
     printf( "+---------------------------------------+\n" );
     printf( "| Connected!                            |\n" );
     printf( "+---------------------------------------+\n" );
   }

   /****************************
    * EXECUTE
    ***************************/
   strcpy(szSQL, "show databases;");
   ExecuteSQL( hDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );

   strcpy(szSQL, "use IW_DCI;");
   ExecuteSQL( hDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );

   strcpy(szSQL, "show tables;");
   ExecuteSQL( hDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );

   strcpy(szSQL, "SELECT Hypothesis, Belief FROM Evidence;");
   ExecuteSQL( hDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );
   /*
   //   Strip away escape chars
   //
   while ( (pEscapeChar=(char*)strchr(szSQL, '\n')) != NULL ||
	   (pEscapeChar=(char*)strchr(szSQL, '\r')) != NULL ) *pEscapeChar = ' ';
   //
   if ( szSQL[1] != '\0' ) {
     ExecuteSQL( hDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );
   }
   */
   /****************************
    * DISCONNECT
    ***************************/
   CloseDatabase( hEnv, hDbc );

   return( 0 );
}

/****************************
 * OptimalDisplayWidth
 ***************************/
SQLUINTEGER OptimalDisplayWidth( SQLHSTMT hStmt, SQLINTEGER nCol, int nUserWidth )
{
SQLUINTEGER	nLabelWidth                     = 10;
SQLUINTEGER	nDataWidth                      = 10;
SQLUINTEGER	nOptimalDisplayWidth            = 10;
SQLCHAR         szColumnName[MAX_DATA_WIDTH+1]	= "";	

   SQLColAttribute( hStmt, nCol, SQL_DESC_DISPLAY_SIZE, NULL, 0, NULL, &nDataWidth );
   SQLColAttribute( hStmt, nCol, SQL_DESC_LABEL, szColumnName,
		    sizeof(szColumnName), NULL, NULL );
   nLabelWidth = strlen((char*) szColumnName );

   nOptimalDisplayWidth = max( nLabelWidth, nDataWidth );

   if ( nUserWidth > 0 )
     nOptimalDisplayWidth = min( nOptimalDisplayWidth, nUserWidth );

   if ( nOptimalDisplayWidth > MAX_DATA_WIDTH ) 
     nOptimalDisplayWidth = MAX_DATA_WIDTH;

   return nOptimalDisplayWidth;
}

/****************************
 * OpenDatabase - do everything we have to do to get a viable connection to szDSN
 ***************************/
int OpenDatabase( SQLHENV *phEnv, SQLHDBC *phDbc, char *szDSN, char *szUID, char *szPWD )
{
   if ( SQLAllocEnv( phEnv ) != SQL_SUCCESS ) {
     fprintf( stderr, "ERROR: Could not Allocate SQL Env\n" );
     return 0;
   }

   if ( SQLAllocConnect( *phEnv, phDbc ) != SQL_SUCCESS ) {
     if ( bVerbose ) DumpODBCLog( hEnv, 0, 0 );
     fprintf( stderr, "ERROR: Could not Connect to SQL Server\n" );
     SQLFreeEnv( *phEnv );
     return 0;
   }

   if ( !SQL_SUCCEEDED( SQLConnect( *phDbc, (SQLCHAR*)szDSN, SQL_NTS,
				    (SQLCHAR*)szUID, SQL_NTS,
				    (SQLCHAR*)szPWD, SQL_NTS ))) {
     if ( bVerbose ) DumpODBCLog( hEnv, hDbc, 0 );
     fprintf( stderr, "ERROR: Could not Connect to SQL Server\n" );
     SQLFreeConnect( *phDbc );
     SQLFreeEnv( *phEnv );
     return 0;
   }

   return 1;
}

/****************************
 * CloseDatabase - cleanup in prep for exiting the program
 ***************************/
int CloseDatabase( SQLHENV hEnv, SQLHDBC hDbc )
{
   SQLDisconnect( hDbc );
   SQLFreeConnect( hDbc );
   SQLFreeEnv( hEnv );

   return 1;
}

/****************************
 * ExecuteSQL - create a statement, execute the SQL, and get rid of the statement
 *            - show results as per request; bHTMLTable has precedence over other options
 ***************************/
int ExecuteSQL(SQLHDBC hDbc, char *szSQL, char cDelimiter, int bColumnNames, int bHTMLTable)
{
SQLCHAR		szSepLine[32001] = "";	
SQLSMALLINT     cols;
SQLLEN          nRows = 0;
SQLINTEGER      ret;

SQLINTEGER    	nCol            		= 0;
SQLSMALLINT	nColumns			= 0;
SQLCHAR		szColumn[MAX_DATA_WIDTH+20]	= "";	
SQLCHAR		szColumnName[MAX_DATA_WIDTH+1]	= "";	
SQLCHAR		szHdrLine[32001]		= "";	
SQLUINTEGER	nOptimalDisplayWidth            = 10;

SQLLEN		nIndicator			= 0;
SQLCHAR		szColumnValue[MAX_DATA_WIDTH+1]	= "";
SQLRETURN	nReturn				= 0;

   /****************************
    * EXECUTE SQL
    ***************************/
   if ( SQLAllocStmt( hDbc, &hStmt ) != SQL_SUCCESS ) {
     if ( bVerbose ) DumpODBCLog( hEnv, hDbc, 0 );
     fprintf( stderr, "ERROR: Could not Allocate SQL Stmt\n" );
     return 0;
   }

   if ( SQLPrepare( hStmt, (SQLCHAR*)szSQL, SQL_NTS ) != SQL_SUCCESS ) {
     if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
     fprintf( stderr, "ERROR: Could not SQLPrepare\n" );
     SQLFreeStmt( hStmt, SQL_DROP );
     return 0;
   }

   ret =  SQLExecute( hStmt );

   if ( ret == SQL_NO_DATA) {
     fprintf( stderr, "INFO: SQLExecute returned SQL_NO_DATA\n" );
   } else if ( ret == SQL_SUCCESS_WITH_INFO ) {
     if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
     fprintf( stderr, "INFO: SQLExecute returned SQL_SUCCESS_WITH_INFO\n" );
   } else if ( ret != SQL_SUCCESS ) {
     if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
     fprintf( stderr, "ERROR: Could not SQLExecute [%s]\n", szSQL);
     SQLFreeStmt( hStmt, SQL_DROP );
     return 0;
   }

   /*
    * check to see if it has generated a result set
   */

   if ( SQLNumResultCols( hStmt, &nColumns ) != SQL_SUCCESS ) {
     if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
     fprintf( stderr, "ERROR: Could not SQLNunResultCols\n" );
     SQLFreeStmt( hStmt, SQL_DROP );
     return 0;
   }

   /****************************
    * CLEANUP
    ***************************/
   //SQLFreeStmt( hStmt, SQL_DROP );

   return nColumns;
}

int DumpODBCLog( SQLHENV hEnv, SQLHDBC hDbc, SQLHSTMT hStmt )
{
SQLCHAR		szError[501];
SQLCHAR		szSqlState[10];
SQLINTEGER      nNativeError;
SQLSMALLINT	nErrorMsg;

   if ( hStmt ) {
     while ( SQLError( hEnv, hDbc, hStmt, szSqlState,
		       &nNativeError, szError, 500, &nErrorMsg ) == SQL_SUCCESS ) {
       printf( "%s\n", szError );
     }
   }

   if ( hDbc ) {
     while ( SQLError( hEnv, hDbc, 0, szSqlState,
		       &nNativeError, szError, 500, &nErrorMsg ) == SQL_SUCCESS ) {
       printf( "%s\n", szError );
     }
   }

   if ( hEnv ) {
     while ( SQLError( hEnv, 0, 0, szSqlState,
		       &nNativeError, szError, 500, &nErrorMsg ) == SQL_SUCCESS ) {
       printf( "%s\n", szError );
     }
   }

   return 1;
}

