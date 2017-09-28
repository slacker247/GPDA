/**************************************************
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

/*
 * Add the historic ODBCINI value, mainly for applix.
 */

#ifdef VMS

char *odbcinst_system_file_path( void )
{
    char *path;

    if (( path = getvmsenv( "ODBCSYSINI" )))
        return path;
#ifdef SYSTEM_FILE_PATH
    else 
        return SYSTEM_FILE_PATH;
#else
    else 
        return "SYS$SHARE:";
#endif
}

BOOL _odbcinst_SystemINI( char *pszFileName, BOOL bVerify )
{
	FILE			*hFile;

    sprintf( pszFileName, "%s:odbc.ini", odbcinst_system_file_path());
	
	if ( bVerify )
	{
		hFile = fopen( pszFileName, "r" );
		if ( hFile )
			fclose( hFile );
		else
			return FALSE;
	}

	return TRUE;
}

#else

char *odbcinst_system_file_path( void )
{
    char *path;

    if (( path = getenv( "ODBCSYSINI" )))
        return path;
#ifdef SYSTEM_FILE_PATH
    else 
        return SYSTEM_FILE_PATH;
#else
    else 
        return "/etc";
#endif
}

BOOL _odbcinst_SystemINI( char *pszFileName, BOOL bVerify )
{
	FILE			*hFile;

    sprintf( pszFileName, "%s/odbc.ini", odbcinst_system_file_path());
	
	if ( bVerify )
	{
        /* try opening for read */
		hFile = fopen( pszFileName, "r" );
		if ( hFile )
			fclose( hFile );
		else
        {
            /* does not exist so try creating it */
            hFile = fopen( pszFileName, "w" );
            if ( hFile )
                fclose( hFile );
            else
                return FALSE;
        }
	}

	return TRUE;
}

#endif

