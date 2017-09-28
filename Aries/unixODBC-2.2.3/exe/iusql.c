
/**************************************************
 * isql
 *
 **************************************************
 * This code was created by Peter Harvey @ CodeByDesign.
 * Released under GPL 18.FEB.99
 *
 * Contributions from...
 * -----------------------------------------------
 * Peter Harvey		- pharvey@codebydesign.com
 **************************************************/

#define UNICODE
#include "isql.h"
#include "sqlucode.h"
#ifdef HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

int 	bVerbose					= 0;
SQLHENV	hEnv						= 0;
SQLHDBC	hDbc						= 0;

void UWriteHeaderNormal( SQLHSTMT hStmt, SQLTCHAR *szSepLine );
void UWriteFooterNormal( SQLHSTMT hStmt, SQLTCHAR *szSepLine );

static char * uc_to_ascii( SQLWCHAR *uc )
{
    char *ascii = (char *)uc;
    int i;

    for ( i = 0; uc[ i ]; i ++ )
    {
        ascii[ i ] = uc[ i ] & 0x00ff;
    }

    ascii[ i ] = 0;

    return ascii;
}

static void ansi_to_unicode( char *szSQL, SQLWCHAR *szUcSQL )
{
    int i;

    for ( i = 0; szSQL[ i ]; i ++ )
    {
        szUcSQL[ i ] = szSQL[ i ];
    }
    szUcSQL[ i ] = 0;
}

int main( int argc, char *argv[] )
{
	int 	nArg, count;
	int 	bHTMLTable					= 0;
	int		bBatch						= 0;
	int 	cDelimiter					= 0;
    int     bColumnNames                = 0;
	char	szDSN[MAX_DATA_WIDTH+1]		= "";
	char	szUID[MAX_DATA_WIDTH+1]		= "";
	char	szPWD[MAX_DATA_WIDTH+1]		= "";
	char	szSQL[9001]					= "";
	char    *pEscapeChar;

	if ( argc < 2 )
	{
		fprintf( stderr, szSyntax );
		exit( 1 );
	}

	/****************************
	 * PARSE ARGS
	 ***************************/
	for ( nArg = 1, count = 1 ; nArg < argc; nArg++ )
	{
		if ( argv[nArg][0] == '-' )
		{
			/* Options */
			switch ( argv[nArg][1] )
			{
			case 'd':
				cDelimiter = argv[nArg][2];
				break;
			case 'w':
				bHTMLTable = 1;
				break;
			case 'b':
				bBatch = 1;
				break;
			case 'c':
				bColumnNames = 1;
				break;
			case 'v':
				bVerbose = 1;
				break;
			case '-':
				printf( "unixODBC " VERSION "\n" );
				exit(0);
#ifdef HAVE_STRTOL
            case 'x':
				cDelimiter = strtol( argv[nArg]+2, NULL, 0 );
                break;
#endif
			default:
				fprintf( stderr, szSyntax );
				exit( 1 );
			}
			continue;
		}
		else if ( count == 1 )
			strncpy( szDSN, argv[nArg], MAX_DATA_WIDTH );
		else if ( count == 2 )
			strncpy( szUID, argv[nArg], MAX_DATA_WIDTH );
		else if ( count == 3 )
			strncpy( szPWD, argv[nArg], MAX_DATA_WIDTH );
		count++;
	}

	/****************************
	 * CONNECT
	 ***************************/
	if ( !OpenDatabase( &hEnv, &hDbc, szDSN, szUID, szPWD ) )
		exit( 1 );

	/****************************
	 * EXECUTE
	 ***************************/
	if ( !bBatch )
	{
		printf( "+---------------------------------------+\n" );
		printf( "| Connected!                            |\n" );
		printf( "|                                       |\n" );
		printf( "| sql-statement                         |\n" );
		printf( "| help [tablename]                      |\n" );
		printf( "| quit                                  |\n" );
		printf( "|                                       |\n" );
		printf( "+---------------------------------------+\n" );
	}
	do
	{
		if ( !bBatch )
#ifndef HAVE_READLINE
			printf( "SQL> " );
#else
		{
			char *line;
            int malloced;

			line=readline("SQL> ");
            if ( !line )        /* EOF - ctrl D */
            {
                malloced = 1;
                line = strdup( "quit" );
            }
            else
            {
                malloced = 0;
            }
			strncpy(szSQL, line, sizeof(szSQL));
			add_history(line);
            if ( malloced )
            {
                free(line);
            }
		} else 
#endif
        {
			char *line;
            int malloced;

		    line = fgets( szSQL, sizeof(szSQL), stdin );
            if ( !line )        /* EOF - ctrl D */
            {
                malloced = 1;
                line = strdup( "quit" );
            }
            else
            {
                malloced = 0;
            }
			strncpy(szSQL, line, sizeof(szSQL));
            if ( malloced )
            {
                free(line);
            }
        }

		/* strip away escape chars */
		while ( (pEscapeChar=(char*)strchr(szSQL, '\n')) != NULL || (pEscapeChar=(char*)strchr(szSQL, '\r')) != NULL )
			*pEscapeChar = ' ';

		if ( szSQL[1] != '\0' )
		{
			if ( strncmp( szSQL, "quit", 4 ) == 0 )
				szSQL[1] = '\0';
			else if ( strncmp( szSQL, "help", 4 ) == 0 )
				ExecuteHelp( hDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );
			else if (memcmp(szSQL, "--", 2) != 0) 
				ExecuteSQL( hDbc, szSQL, cDelimiter, bColumnNames, bHTMLTable );
		}

	} while ( szSQL[1] != '\0' );

	/****************************
	 * DISCONNECT
	 ***************************/
	CloseDatabase( hEnv, hDbc );

	exit( 0 );
}

/****************************
 * OpenDatabase - do everything we have to do to get a viable connection to szDSN
 ***************************/
int OpenDatabase( SQLHENV *phEnv, SQLHDBC *phDbc, char *szDSN, char *szUID, char *szPWD )
{
    SQLTCHAR dsn[ 1024 ], uid[ 1024 ], pwd[ 1024 ];
    SQLTCHAR cstr[ 1024 ];
    char zcstr[ 1024 ];
    int i;

	if ( SQLAllocEnv( phEnv ) != SQL_SUCCESS )
	{
		fprintf( stderr, "[ISQL]ERROR: Could not SQLAllocEnv\n" );
		return 0;
	}

	if ( SQLAllocConnect( *phEnv, phDbc ) != SQL_SUCCESS )
	{
        if ( bVerbose ) DumpODBCLog( hEnv, 0, 0 );
		fprintf( stderr, "[ISQL]ERROR: Could not SQLAllocConnect\n" );
		SQLFreeEnv( *phEnv );
		return 0;
	}

    for ( i = 0; i < strlen( szDSN ); i ++ )
    {
        dsn[ i ] = szDSN[ i ];
    }

    for ( i = 0; i < strlen( szUID ); i ++ )
    {
        uid[ i ] = szUID[ i ];
    }

    for ( i = 0; i < strlen( szPWD ); i ++ )
    {
        pwd[ i ] = szPWD[ i ];
    }

    sprintf( zcstr, "DSN=%s;UID=%s;PWD=%s", szDSN, szUID, szPWD );
    for ( i = 0; i < strlen( zcstr ); i ++ )
    {
        cstr[ i ] = zcstr[ i ];
    }

    if ( !SQL_SUCCEEDED( SQLDriverConnect( *phDbc, NULL, cstr, SQL_NTS, NULL, 0, NULL, SQL_DRIVER_NOPROMPT  )))
	{
        if ( bVerbose ) DumpODBCLog( hEnv, hDbc, 0 );
		fprintf( stderr, "[ISQL]ERROR: Could not SQLDriverConnect\n" );
		SQLFreeConnect( *phDbc );
		SQLFreeEnv( *phEnv );
		return 0;
	}
    if ( bVerbose ) DumpODBCLog( hEnv, hDbc, 0 );

	return 1;
}

/****************************
 * ExecuteSQL - create a statement, execute the SQL, and get rid of the statement
 *            - show results as per request; bHTMLTable has precedence over other options
 ***************************/
int ExecuteSQL( SQLHDBC hDbc, char *szSQL, char cDelimiter, int bColumnNames, int bHTMLTable )
{
	SQLHSTMT		hStmt;
	SQLTCHAR		szSepLine[32001];	
	SQLTCHAR		szUcSQL[32001];	
    SQLSMALLINT     cols;
    SQLINTEGER      ret;

    szSepLine[ 0 ] = 0;

    ansi_to_unicode( szSQL, szUcSQL );

	/****************************
	 * EXECUTE SQL
	 ***************************/
	if ( SQLAllocStmt( hDbc, &hStmt ) != SQL_SUCCESS )
	{
        if ( bVerbose ) DumpODBCLog( hEnv, hDbc, 0 );
		fprintf( stderr, "[ISQL]ERROR: Could not SQLAllocStmt\n" );
		return 0;
	}

	if ( SQLPrepare( hStmt, szUcSQL, SQL_NTS ) != SQL_SUCCESS )
	{
        if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
		fprintf( stderr, "[ISQL]ERROR: Could not SQLPrepare\n" );
		SQLFreeStmt( hStmt, SQL_DROP );
		return 0;
	}

    ret =  SQLExecute( hStmt );

    if ( ret == SQL_NO_DATA )
    {
		fprintf( stderr, "[ISQL]INFO: SQLExecute returned SQL_NO_DATA\n" );
    }
    else if ( ret == SQL_SUCCESS_WITH_INFO )
    {
        if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
		fprintf( stderr, "[ISQL]INFO: SQLExecute returned SQL_SUCCESS_WITH_INFO\n" );
    }
    else if ( ret != SQL_SUCCESS )
	{
        if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
		fprintf( stderr, "[ISQL]ERROR: Could not SQLExecute\n" );
		SQLFreeStmt( hStmt, SQL_DROP );
		return 0;
	}

    /*
     * check to see if it has generated a result set
     */

    if ( SQLNumResultCols( hStmt, &cols ) != SQL_SUCCESS )
    {
        if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
		fprintf( stderr, "[ISQL]ERROR: Could not SQLNunResultCols\n" );
		SQLFreeStmt( hStmt, SQL_DROP );
		return 0;
    }

    if ( cols > 0 )
    {
        /****************************
         * WRITE HEADER
         ***************************/
        if ( bHTMLTable )
            WriteHeaderHTMLTable( hStmt );
        else if ( cDelimiter == 0 )
            UWriteHeaderNormal( hStmt, szSepLine );
        else if ( cDelimiter && bColumnNames )
            WriteHeaderDelimited( hStmt, cDelimiter );

        /****************************
         * WRITE BODY
         ***************************/
        if ( bHTMLTable )
            WriteBodyHTMLTable( hStmt );
        else if ( cDelimiter == 0 )
            WriteBodyNormal( hStmt );
        else
            WriteBodyDelimited( hStmt, cDelimiter );
    }

    /****************************
     * WRITE FOOTER
     ***************************/
    if ( bHTMLTable )
        WriteFooterHTMLTable( hStmt );
    else if ( cDelimiter == 0 )
        UWriteFooterNormal( hStmt, szSepLine );

	/****************************
	 * CLEANUP
	 ***************************/
	SQLFreeStmt( hStmt, SQL_DROP );

	return 1;
}

/****************************
 * ExecuteHelp - create a statement, execute the SQL, and get rid of the statement
 *             - show results as per request; bHTMLTable has precedence over other options
 ***************************/
int ExecuteHelp( SQLHDBC hDbc, char *szSQL, char cDelimiter, int bColumnNames, int bHTMLTable )
{
	char 			szTable[250]						= "";
	SQLHSTMT		hStmt;
	SQLTCHAR		szSepLine[32001];	

    szSepLine[ 0 ] = 0;

	/****************************
	 * EXECUTE SQL
	 ***************************/
	if ( SQLAllocStmt( hDbc, &hStmt ) != SQL_SUCCESS )
	{
        if ( bVerbose ) DumpODBCLog( hEnv, hDbc, 0 );
		fprintf( stderr, "[ISQL]ERROR: Could not SQLAllocStmt\n" );
		return 0;
	}

	if ( iniElement( szSQL, ' ', '\0', 1, szTable, sizeof(szTable) ) == INI_SUCCESS )
	{
        SQLWCHAR tname[ 1024 ];

        ansi_to_unicode( szTable, tname );
		/* COLUMNS */
		if ( SQLColumns( hStmt, NULL, 0, NULL, 0, tname, SQL_NTS, NULL, 0 ) != SQL_SUCCESS )
		{
			if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
			fprintf( stderr, "[ISQL]ERROR: Could not SQLColumns\n" );
			SQLFreeStmt( hStmt, SQL_DROP );
			return 0;
		}
	}
	else
	{
		/* TABLES */
		if ( SQLTables( hStmt, NULL, 0, NULL, 0, NULL, 0, NULL, 0 ) != SQL_SUCCESS )
		{
			if ( bVerbose ) DumpODBCLog( hEnv, hDbc, hStmt );
			fprintf( stderr, "[ISQL]ERROR: Could not SQLTables\n" );
			SQLFreeStmt( hStmt, SQL_DROP );
			return 0;
		}
	}

	/****************************
	 * WRITE HEADER
	 ***************************/
	if ( bHTMLTable )
		WriteHeaderHTMLTable( hStmt );
	else if ( cDelimiter == 0 )
		UWriteHeaderNormal( hStmt, szSepLine );
    else if ( cDelimiter && bColumnNames )
        WriteHeaderDelimited( hStmt, cDelimiter );

	/****************************
	 * WRITE BODY
	 ***************************/
	if ( bHTMLTable )
		WriteBodyHTMLTable( hStmt );
	else if ( cDelimiter == 0 )
		WriteBodyNormal( hStmt );
	else
		WriteBodyDelimited( hStmt, cDelimiter );

	/****************************
	 * WRITE FOOTER
	 ***************************/
	if ( bHTMLTable )
		WriteFooterHTMLTable( hStmt );
	else if ( cDelimiter == 0 )
		UWriteFooterNormal( hStmt, szSepLine );

	/****************************
	 * CLEANUP
	 ***************************/
	SQLFreeStmt( hStmt, SQL_DROP );

	return 1;
}


/****************************
 * CloseDatabase - cleanup in prep for exiting the program
 ***************************/
int	CloseDatabase( SQLHENV hEnv, SQLHDBC hDbc )
{
	SQLDisconnect( hDbc );
	SQLFreeConnect( hDbc );
	SQLFreeEnv( hEnv );

	return 1;
}


/****************************
 * WRITE HTML
 ***************************/
void WriteHeaderHTMLTable( SQLHSTMT hStmt )
{
    SQLINTEGER    	nCol            				= 0;
	SQLSMALLINT		nColumns						= 0;
	SQLTCHAR		szColumnName[MAX_DATA_WIDTH+1];	

    szColumnName[ 0 ] = 0;

	printf( "<table BORDER>\n" );
	printf( "<tr BGCOLOR=#000099>\n" );

    if ( SQLNumResultCols( hStmt, &nColumns ) != SQL_SUCCESS )
        nColumns = -1;

	for( nCol = 1; nCol <= nColumns; nCol++ )
	{
		SQLColAttribute( hStmt, nCol, SQL_DESC_LABEL, szColumnName, sizeof(szColumnName), NULL, NULL );

		printf( "<td>\n" );
		printf( "<font face=Arial,Helvetica><font color=#FFFFFF>\n" );
		printf( "%s\n", uc_to_ascii( szColumnName ));
		printf( "</font></font>\n" );
		printf( "</td>\n" );
	}
	printf( "</tr>\n" );
}

void WriteBodyHTMLTable( SQLHSTMT hStmt )
{
    SQLINTEGER    	nCol            				= 0;
	SQLSMALLINT		nColumns						= 0;
	SQLLEN		    nIndicator						= 0;
	SQLTCHAR		szColumnValue[MAX_DATA_WIDTH+1];
	SQLRETURN		nReturn							= 0;
    SQLRETURN       ret;

	szColumnValue[ 0 ]	= 0;

    if ( SQLNumResultCols( hStmt, &nColumns ) != SQL_SUCCESS )
        nColumns = -1;

    while( (ret = SQLFetch( hStmt )) == SQL_SUCCESS ) /* ROWS */
    {
		printf( "<tr>\n" );
		
        for( nCol = 1; nCol <= nColumns; nCol++ ) /* COLS */
        {
			printf( "<td>\n" );
			printf( "<font face=Arial,Helvetica>\n" );

            nReturn = SQLGetData( hStmt, nCol, SQL_C_WCHAR, (SQLPOINTER)szColumnValue, sizeof(szColumnValue), &nIndicator );
            if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA )
            {
                uc_to_ascii( szColumnValue );
                fputs((char*) szColumnValue, stdout );
            }
	    else if ( nReturn == SQL_ERROR ) {
		ret = SQL_ERROR;
		break;
	    }
            else
                printf( "%s\n", "" );

			printf( "</font>\n" );
			printf( "</td>\n" );
        }
	if (ret != SQL_SUCCESS)
	    break;
		printf( "</tr>\n" );
    }
}

void WriteFooterHTMLTable( SQLHSTMT hStmt )
{
	printf( "</table>\n" );
}

/****************************
 * WRITE DELIMITED
 * - this output can be used by the ODBC Text File driver
 * - last column no longer has a delimit char (it is implicit)...
 *   this is consistent with odbctxt
 ***************************/
void WriteHeaderDelimited( SQLHSTMT hStmt, char cDelimiter )
{
    SQLINTEGER    	nCol            				= 0;
	SQLSMALLINT		nColumns						= 0;
	SQLTCHAR			szColumnName[MAX_DATA_WIDTH+1];	

	szColumnName[ 0 ]	= 0;	

    if ( SQLNumResultCols( hStmt, &nColumns ) != SQL_SUCCESS )
        nColumns = -1;

	for( nCol = 1; nCol <= nColumns; nCol++ )
	{
		SQLColAttribute( hStmt, nCol, SQL_DESC_LABEL, szColumnName, sizeof(szColumnName), NULL, NULL );
        fputs((char*) uc_to_ascii( szColumnName ), stdout );
        if ( nCol < nColumns )
            putchar( cDelimiter );
	}
    putchar( '\n' );
}

void WriteBodyDelimited( SQLHSTMT hStmt, char cDelimiter )
{
    SQLINTEGER    	nCol            				= 0;
	SQLSMALLINT		nColumns						= 0;
	SQLLEN		    nIndicator						= 0;
	SQLTCHAR			szColumnValue[MAX_DATA_WIDTH+1];
	SQLRETURN		nReturn							= 0;
    SQLRETURN       ret;

	szColumnValue[ 0 ]	= 0;

    if ( SQLNumResultCols( hStmt, &nColumns ) != SQL_SUCCESS )
        nColumns = -1;

	/* ROWS */
    while(( ret = SQLFetch( hStmt )) == SQL_SUCCESS )
    {
		/* COLS */
        for( nCol = 1; nCol <= nColumns; nCol++ )
        {
            nReturn = SQLGetData( hStmt, nCol, SQL_C_WCHAR, (SQLPOINTER)szColumnValue, sizeof(szColumnValue), &nIndicator );
            if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA )
            {
                uc_to_ascii( szColumnValue );
                fputs((char*) szColumnValue, stdout );
                if ( nCol < nColumns )
                    putchar( cDelimiter );
            }
	        else if ( nReturn == SQL_ERROR ) 
            {
		        ret = SQL_ERROR;
		        break;
	        }
            else
            {
                if ( nCol < nColumns )
                    putchar( cDelimiter );
            }
        }
	    if (ret != SQL_SUCCESS)
	        break;
        printf( "\n" );
    }
    if ( ret == SQL_ERROR )
    {  
        if ( bVerbose ) DumpODBCLog( 0, 0, hStmt );
    }
}

/****************************
 * WRITE NORMAL
 ***************************/
void UWriteHeaderNormal( SQLHSTMT hStmt, SQLTCHAR *szSepLine )
{
    SQLINTEGER    	nCol            				= 0;
	SQLSMALLINT		nColumns						= 0;
	SQLULEN		    nMaxLength						= 10;
	SQLTCHAR			szColumn[MAX_DATA_WIDTH+20];	
	SQLTCHAR			szColumnName[MAX_DATA_WIDTH+1];	
	SQLTCHAR			szHdrLine[32001];	

	szColumn[ 0 ]		= 0;	
	szColumnName[ 0 ]	= 0;	
	szHdrLine[ 0 ]		= 0;	

    if ( SQLNumResultCols( hStmt, &nColumns ) != SQL_SUCCESS )
        nColumns = -1;

	for( nCol = 1; nCol <= nColumns; nCol++ )
	{
		SQLColAttribute( hStmt, nCol, SQL_DESC_DISPLAY_SIZE, NULL, 0, NULL, &nMaxLength );
		SQLColAttribute( hStmt, nCol, SQL_DESC_LABEL, szColumnName, sizeof(szColumnName), NULL, NULL );
		if ( nMaxLength > MAX_DATA_WIDTH ) nMaxLength = MAX_DATA_WIDTH;

        uc_to_ascii( szColumnName );

		/* SEP */
        memset( szColumn, '\0', sizeof(szColumn) );
		memset( szColumn, '-', max( nMaxLength, strlen((char*)szColumnName) ) + 1 );
		strcat((char*) szSepLine, "+" );
		strcat((char*) szSepLine,(char*) szColumn );

		/* HDR */
		sprintf((char*) szColumn, "| %-*s", max( nMaxLength, strlen((char*)szColumnName) ), szColumnName );
        strcat((char*) szHdrLine,(char*) szColumn );
	}
	strcat((char*) szSepLine, "+\n" );
	strcat((char*) szHdrLine, "|\n" );
	
	printf((char*) szSepLine );
	printf((char*) szHdrLine );
	printf((char*) szSepLine );

}

SQLLEN WriteBodyNormal( SQLHSTMT hStmt )
{
    SQLINTEGER    	nCol            				= 0;
	SQLSMALLINT		nColumns						= 0;
	SQLLEN		    nIndicator						= 0;
	SQLTCHAR			szColumn[MAX_DATA_WIDTH+20];
	SQLTCHAR			szColumnValue[MAX_DATA_WIDTH+1];
	SQLTCHAR			szColumnName[MAX_DATA_WIDTH+1];	
	SQLUINTEGER		nMaxLength						= 10;
	SQLRETURN		nReturn							= 0;
    SQLRETURN       ret;
    SQLLEN          nRows                           = 0;

	szColumn[ 0 ]		= 0;
	szColumnValue[ 0 ]	= 0;
	szColumnName[ 0 ]	= 0;	

    if ( SQLNumResultCols( hStmt, &nColumns ) != SQL_SUCCESS )
        nColumns = -1;

	/* ROWS */
    while(( ret = SQLFetch( hStmt )) == SQL_SUCCESS )
    {
		/* COLS */
        for( nCol = 1; nCol <= nColumns; nCol++ )
        {
			SQLColAttribute( hStmt, nCol, SQL_DESC_LABEL, szColumnName, sizeof(szColumnName), NULL, NULL );
			SQLColAttribute( hStmt, nCol, SQL_DESC_DISPLAY_SIZE, NULL, 0, NULL, &nMaxLength );

            uc_to_ascii( szColumnName );

			if ( nMaxLength > MAX_DATA_WIDTH ) nMaxLength = MAX_DATA_WIDTH;
            nReturn = SQLGetData( hStmt, nCol, SQL_C_WCHAR, (SQLPOINTER)szColumnValue, sizeof(szColumnValue), &nIndicator );
            szColumnValue[MAX_DATA_WIDTH] = '\0';
            uc_to_ascii( szColumnValue );

            if ( nReturn == SQL_SUCCESS && nIndicator != SQL_NULL_DATA )
            {
                if ( strlen((char*)szColumnValue) < max( nMaxLength, strlen((char*)szColumnName )))
                {
                    int i;
                    strcpy((char*) szColumn, "| " );
                    strcat((char*) szColumn, (char*) szColumnValue );

                    for ( i = strlen((char*) szColumnValue ); 
                        i < max( nMaxLength, strlen((char*)szColumnName )); i++ )
                    {
                        strcat((char*) szColumn, " " );
                    }
                }
                else
                {
                    strcpy((char*) szColumn, "| " );
                    strcat((char*) szColumn, (char*) szColumnValue );
                }
            }
	        else if ( nReturn == SQL_ERROR ) 
            {
		        ret = SQL_ERROR;
		        break;
	        }
            else
            {
                sprintf((char*)  szColumn, "| %-*s", max( nMaxLength, strlen((char*) szColumnName) ), "" );
            }
			fputs((char*)  szColumn, stdout );
        }
	    if (ret != SQL_SUCCESS)
	        break;
        printf( "|\n" );
        nRows++;
    } 
    if ( ret == SQL_ERROR )
    {
        if ( bVerbose ) DumpODBCLog( 0, 0, hStmt );
    }

    return nRows;
}

void UWriteFooterNormal( SQLHSTMT hStmt, SQLTCHAR	*szSepLine )
{
    SQLLEN  nRowsAffected	= -1;

	printf((char*) szSepLine );

    SQLRowCount( hStmt, &nRowsAffected );

	printf( "%d rows affected\n", nRowsAffected );
}



int DumpODBCLog( SQLHENV hEnv, SQLHDBC hDbc, SQLHSTMT hStmt )
{
	SQLTCHAR		szError[501];
	SQLTCHAR		szSqlState[10];
    SQLINTEGER  nNativeError;
    SQLSMALLINT	nErrorMsg;

	if ( hStmt )
	{
		while ( SQLError( hEnv, hDbc, hStmt, szSqlState, &nNativeError, szError, 500, &nErrorMsg ) == SQL_SUCCESS )
		{
			printf( "%s\n", uc_to_ascii( szError ));
		}
	}

	if ( hDbc )
	{
		while ( SQLError( hEnv, hDbc, 0, szSqlState, &nNativeError, szError, 500, &nErrorMsg ) == SQL_SUCCESS )
		{
			printf( "%s\n", uc_to_ascii( szError ));
		}
	}

	if ( hEnv )
	{
		while ( SQLError( hEnv, 0, 0, szSqlState, &nNativeError, szError, 500, &nErrorMsg ) == SQL_SUCCESS )
		{
			printf( "%s\n", uc_to_ascii( szError ));
		}
	}

	return 1;
}

