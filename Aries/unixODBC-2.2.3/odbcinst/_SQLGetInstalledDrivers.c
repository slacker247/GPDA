/**************************************************
 * _SQLGetInstalledDrivers
 *
 * Added to allow ODBC Config programs and the ODBC
 * driver manager to access system information without
 * having to worry about where it is... just like accessing
 * Data Source information. So no surprise... its just
 * like SQLGetPrivateProfileString()!
 *
 * see SQLGetPrivateProfileString to see how this is called.
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

int _SQLGetInstalledDrivers(    LPCSTR  pszSection,
                                LPCSTR  pszEntry,
                                LPCSTR  pszDefault,
                                LPCSTR  pRetBuffer,
                                int     nRetBuffer )
{
    HINI    hIni;
    int     nBufPos         = 0;
    int     nStrToCopy;
    char    szObjectName[INI_MAX_OBJECT_NAME+1];
    char    szPropertyName[INI_MAX_PROPERTY_NAME+1];
    char    szValue[INI_MAX_PROPERTY_VALUE+1];
    char    szIniName[ INI_MAX_OBJECT_NAME + 1 ];

    /* SANITY CHECKS */
    if ( pRetBuffer == NULL || nRetBuffer < 2 )
    {
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );
        return -1;
    }

#ifdef VMS
    sprintf( szIniName, "%sODBCINST.INI", odbcinst_system_file_path() );
#else
    sprintf( szIniName, "%s/odbcinst.ini", odbcinst_system_file_path() );
#endif

    /* PROCESS ODBC INI FILE */
    if ( iniOpen( &hIni, szIniName, '#', '[', ']', '=', TRUE ) != INI_SUCCESS )
    {
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_COMPONENT_NOT_FOUND, "" );
        return -1;
    }

    nBufPos = 0;
    if ( pszSection == NULL )
    {
        /* JUST COLLECT SECTION NAMES */
        iniObjectFirst( hIni );
        while ( iniObjectEOL( hIni ) != TRUE )
        {
            iniObject( hIni, szObjectName );
            if ( strcmp( szObjectName, "ODBC" ) == 0 )
            {
                iniObjectNext( hIni );
                continue;
            }

            nStrToCopy = strlen( szObjectName ) + 1;            /* factor NULL terminator for string */
            if ( nBufPos + nStrToCopy + 1 > nRetBuffer )        /* factor NULL terminator for buffer */
                nStrToCopy = nRetBuffer - nBufPos - 2;
            strncpy( (char *)&(pRetBuffer[nBufPos]), szObjectName, nStrToCopy );
            nBufPos += nStrToCopy;
            iniObjectNext( hIni );
        }
    }
    else if ( pszEntry == NULL )
    {
        /* COLLECT ALL ENTRIES FOR THE GIVEN SECTION */
        iniObjectSeek( hIni, (char *)pszSection );
        iniPropertyFirst( hIni );
        while ( iniPropertyEOL( hIni ) != TRUE )
        {
            iniProperty( hIni, szPropertyName );
            nStrToCopy = strlen( szPropertyName ) + 1;          /* factor NULL terminator for string */
            if ( nBufPos + nStrToCopy + 1 > nRetBuffer )        /* factor NULL terminator for buffer */
                nStrToCopy = nRetBuffer - nBufPos - 2;
            strncpy( (char *)&(pRetBuffer[nBufPos]), szPropertyName, nStrToCopy );
            nBufPos += nStrToCopy;
            iniPropertyNext( hIni );
        }
    }
    else
    {
        /* TRY TO GET THE ONE ITEM MATCHING Section & Entry */
        if ( iniPropertySeek( hIni, (char *)pszSection, (char *)pszEntry, "" ) != INI_SUCCESS )
        {
            strncpy( (char *)pRetBuffer, pszDefault, nRetBuffer );
        }
        else
        {
            iniValue( hIni, szValue );
            nStrToCopy = strlen( szValue ) + 1;                 /* factor NULL terminator for string */
            if ( nBufPos + nStrToCopy + 1 > nRetBuffer )        /* factor NULL terminator for buffer */
                nStrToCopy = nRetBuffer - nBufPos - 2;
            strncpy( (char *)&(pRetBuffer[nBufPos]), szValue, nStrToCopy );
            nBufPos += nStrToCopy;
        }
    }

    /* CLOSE */
    iniClose( hIni );

    return nBufPos;
}


