/**********************************************************************************
 * iniCommit
 *
 *
 **************************************************
 * This code was created by Peter Harvey @ CodeByDesign.
 * Released under LGPL 28.JAN.99
 *
 * Contributions from...
 * -----------------------------------------------
 * Peter Harvey		- pharvey@codebydesign.com
 **************************************************/

#include "ini.h"

int iniCommit( HINI hIni )
{
    FILE    *hFile;

    /* SANITY CHECK */
    if ( hIni == NULL )
        return INI_ERROR;

	if ( hIni->bReadOnly )
        return INI_ERROR;

    /* OPEN FILE */
    hFile = uo_fopen( hIni->szFileName, "w" );
    if ( !hFile )
        return INI_ERROR;

	_iniDump( hIni, hFile );

    /* CLEANUP */
    if ( hFile != NULL )
		uo_fclose( hFile );

    return INI_SUCCESS;
}


