/**************************************************
 * SQLConfigDataSource
 *
 * Determine the DriverSetup file name and then try to pass
 * the work along to its ConfigDSN().
 *
 **************************************************
 * This code was created by Peter Harvey @ CodeByDesign.
 * Released under LGPL 28.JAN.99
 *
 * Contributions from...
 * -----------------------------------------------
 * Peter Harvey		- pharvey@codebydesign.com
 **************************************************/
#include <odbcinstext.h>

BOOL SQLConfigDataSource(		HWND	hWnd,
								WORD	nRequest,
								LPCSTR	pszDriver,				/* USER FRIENDLY NAME (not file name) */
								LPCSTR	pszAttributes )
{
	BOOL	(*pFunc)( HWND, WORD, LPCSTR, LPCSTR	);
	BOOL	nReturn;
	void 	*hDLL	= FALSE;
	HINI	hIni;
	char	szDriverSetup[ODBC_FILENAME_MAX+1];
    char    szIniName[ INI_MAX_OBJECT_NAME + 1 ];
    char    szDontDLClose[ INI_MAX_OBJECT_NAME + 1 ];


	/* SANITY CHECKS */
	if ( pszDriver == NULL )
	{
		inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );
		return FALSE;
	}
	if ( pszDriver[0] == '\0' )
	{
		inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );
		return FALSE;
	}
	switch ( nRequest )
	{
	case ODBC_ADD_DSN:
	case ODBC_CONFIG_DSN:
	case ODBC_REMOVE_DSN:
	case ODBC_ADD_SYS_DSN:
	case ODBC_CONFIG_SYS_DSN:
	case ODBC_REMOVE_SYS_DSN:
	case ODBC_REMOVE_DEFAULT_DSN:
		break;
	default:
		inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_INVALID_REQUEST_TYPE, "" );
		return FALSE;
	}

#ifdef VMS
    sprintf( szIniName, "%sODBCINST.INI", odbcinst_system_file_path() );
#else
    sprintf( szIniName, "%s/odbcinst.ini", odbcinst_system_file_path() );
#endif

	/* OK */
	if ( iniOpen( &hIni, szIniName, '#', '[', ']', '=', TRUE ) != INI_SUCCESS )
	{
		inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );
		return FALSE;
	}

    /*
     * initialize libtool
     */

    lt_dlinit();

	if ( iniPropertySeek( hIni, (char *)pszDriver, "Setup", "" ) == INI_SUCCESS )
	{
   		iniValue( hIni, szDriverSetup );

        strcpy( szDontDLClose, "" );

    	if( iniPropertySeek( hIni, (char *)pszDriver, "DontDLClose", "" ) == INI_SUCCESS )
        {
            iniValue( hIni, szDontDLClose );
        }

		iniClose( hIni );

		nReturn = FALSE;
		if ( (hDLL = lt_dlopen( szDriverSetup ))  )
		{
			pFunc = (BOOL (*)(HWND, WORD, LPCSTR, LPCSTR )) lt_dlsym( hDLL, "ConfigDSN" );
/*            if ( lt_dlerror() == NULL ) */
			if ( pFunc )
            {
                /*
                 * set the mode
                 */
	            switch ( nRequest )
                {
                    case ODBC_ADD_DSN:
                    case ODBC_CONFIG_DSN:
                    case ODBC_REMOVE_DSN:
                    case ODBC_REMOVE_DEFAULT_DSN:
                      SQLSetConfigMode( ODBC_USER_DSN );
                      break;

                    case ODBC_ADD_SYS_DSN:
                      SQLSetConfigMode( ODBC_SYSTEM_DSN );
                      nRequest = ODBC_ADD_DSN;
                      break;

                    case ODBC_CONFIG_SYS_DSN:
                      SQLSetConfigMode( ODBC_SYSTEM_DSN );
                      nRequest = ODBC_CONFIG_DSN;
                      break;

                    case ODBC_REMOVE_SYS_DSN:
                      SQLSetConfigMode( ODBC_SYSTEM_DSN );
                      nRequest = ODBC_REMOVE_DSN;
                      break;
                }
				nReturn = pFunc( hWnd, nRequest, pszDriver, pszAttributes );
            }
			else
				inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );

            /*
             * check if this is safe
             */

            if ( strlen( szDontDLClose ) < 1 || 
                    szDontDLClose[ 1 ] == '0' )
            {
                lt_dlclose( hDLL );
            }
		}
		else
			inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );

        SQLSetConfigMode( ODBC_BOTH_DSN );
		return nReturn;

	}

	inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );
	iniClose( hIni );

    SQLSetConfigMode( ODBC_BOTH_DSN );

	return FALSE;
}


