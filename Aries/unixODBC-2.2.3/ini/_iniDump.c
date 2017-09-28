/**********************************************************************************
 * _iniDump
 *
 * Dump contents to hStream.
 *
 * - iniCommit calls this. You can bypass iniCommit restrictions to get some debugging information by calling directly.
 * - Make sure the stream is open before calling.
 * - leaves list position at iniObjectFirst()
 * - always returns true
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

int _iniDump( HINI hIni, FILE *hStream )
{

    /* SANITY CHECK */
    if ( hIni == NULL )
        return INI_ERROR;

	if ( !hStream )
        return INI_ERROR;

	/* SCAN OBJECTS */
	iniObjectFirst( hIni );
	while ( iniObjectEOL( hIni ) == FALSE )
	{
		uo_fprintf( hStream, "%c%s%c\n", hIni->cLeftBracket, hIni->hCurObject->szName, hIni->cRightBracket );
		iniPropertyFirst( hIni );
		while ( iniPropertyEOL( hIni ) == FALSE )
		{
			uo_fprintf( hStream, "%s\t\t%c %s\n", hIni->hCurProperty->szName, hIni->cEqual, hIni->hCurProperty->szValue );
			iniPropertyNext( hIni );
		}
		uo_fprintf( hStream, "\n" );
		iniPropertyFirst( hIni );
		iniObjectNext( hIni );
	}
	iniObjectFirst( hIni );

	return INI_SUCCESS;
}


