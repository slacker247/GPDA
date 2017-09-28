/**************************************************
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

BOOL SQLSetConfigMode(			UWORD	nConfigMode )
{
	switch ( nConfigMode )
	{
	case ODBC_USER_DSN:
#ifdef HAVE_SETENV
		if ( setenv( "ODBCSEARCH", "ODBC_USER_DSN", 1 ) == 0 )
#else
		if ( putenv( "ODBCSEARCH=ODBC_USER_DSN" ) == 0 )
#endif
		{
			return TRUE;
		}
		break;
	case ODBC_SYSTEM_DSN:
#ifdef HAVE_SETENV
		if ( setenv( "ODBCSEARCH", "ODBC_SYSTEM_DSN", 1 ) == 0 )
#else
		if ( putenv( "ODBCSEARCH=ODBC_SYSTEM_DSN" ) == 0 )
#endif
		{
			return TRUE;
		}
		break;
    default:
#ifdef HAVE_SETENV
		if ( setenv( "ODBCSEARCH", "ODBC_BOTH_DSN", 1 ) == 0 )
#else
		if ( putenv( "ODBCSEARCH=ODBC_BOTH_DSN" ) == 0 )
#endif
		{
			return TRUE;
		}
		break;
	}

	inst_logPushMsg( __FILE__, 
            __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_OUT_OF_MEM, "" );
	
	return FALSE;
}





