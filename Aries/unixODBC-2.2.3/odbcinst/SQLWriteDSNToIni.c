/**************************************************
 * SQLWriteDSNToIni
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

BOOL SQLWriteDSNToIni(			LPCSTR	pszDSN,
								LPCSTR	pszDriver )
{
	HINI	hIni;
	char	szFileName[ODBC_FILENAME_MAX+1];

	/* SANITY CHECKS */
	if ( pszDSN == NULL )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );
		return FALSE;
	}
	if ( pszDSN[0] == '\0' )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );
		return FALSE;
	}
	if ( (strcasecmp( pszDSN, "DEFAULT" ) != 0 ) && (pszDriver == NULL ) )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_INVALID_NAME, "" );
		return FALSE;
	}
	if ( (strcasecmp( pszDSN, "DEFAULT" ) != 0 ) && (pszDriver[0] == '\0') )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_INVALID_NAME, "" );
		return FALSE;
	}
	if ( SQLValidDSN( pszDSN ) == FALSE )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_INVALID_DSN, "" );
		return FALSE;
	}

	/* OK */
	if ( _odbcinst_ConfigModeINI( szFileName ) == FALSE )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_REQUEST_FAILED, "" );
		return FALSE;
	}
	if ( iniOpen( &hIni, szFileName, '#', '[', ']', '=', TRUE  ) != INI_SUCCESS )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_REQUEST_FAILED, "" );
		return FALSE;
	}
	iniObjectInsert( hIni, (char *)pszDSN );
	if ( pszDriver != NULL )
	{
		iniPropertyInsert( hIni, "Driver", (char *)pszDriver );
	}
	if ( iniCommit( hIni ) != INI_SUCCESS )
	{
		iniClose( hIni );
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_REQUEST_FAILED, "" );
		return FALSE;
	}

	iniClose( hIni );

	return TRUE;
}



