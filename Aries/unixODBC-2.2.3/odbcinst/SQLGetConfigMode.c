/**************************************************
 * SQLGetConfigMode
 *
 **************************************************
 * This code was created by Peter Harvey @ CodeByDesign.
 * Released under LGPL 28.JAN.99
 *
 * Contributions from...
 * -----------------------------------------------
 * Peter Harvey		- pharvey@codebydesign.com
 * Nick Gorham      - nick@easysoft.com
 **************************************************/
#include <stdlib.h>
#include <odbcinstext.h>

BOOL SQLGetConfigMode(          UWORD	*pnConfigMode )
{
	char *p;

	p  = getenv( "ODBCSEARCH" );

	if ( p )
	{
		if ( strcmp( p, "ODBC_SYSTEM_DSN" ) == 0 )
		{
	        *pnConfigMode = ODBC_SYSTEM_DSN;
			return TRUE;
		}
		else if ( strcmp( p, "ODBC_USER_DSN" ) == 0 )
		{
	        *pnConfigMode = ODBC_USER_DSN;
			return TRUE;
		}
		else if ( strcmp( p, "ODBC_BOTH_DSN" ) == 0 )
		{
	        *pnConfigMode = ODBC_BOTH_DSN;
			return TRUE;
		}
	}

#ifdef HAVE_SETENV
	if ( setenv( "ODBCSEARCH", "ODBC_BOTH_DSN", 1 ) == 0 )
#else
	if ( putenv( "ODBCSEARCH=ODBC_BOTH_DSN" ) == 0 )
#endif
		*pnConfigMode = ODBC_BOTH_DSN;
	else
	{
		inst_logPushMsg( __FILE__, __FILE__, __LINE__, 
                LOG_CRITICAL, ODBC_ERROR_OUT_OF_MEM, "" );
		return FALSE;
	}

	return TRUE;
}


