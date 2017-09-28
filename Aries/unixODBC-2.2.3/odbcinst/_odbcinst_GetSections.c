/****************************************************
 * _odbcinst_GetSections
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

int _odbcinst_GetSections(	HINI	hIni,
							LPSTR	pRetBuffer,
                            int		nRetBuffer,
							int		*pnBufPos 		/* SET TO 0 IF RESULT DATA IS EMPTY */
						)
{
	int		nStrToCopy;
	char	szObjectName[INI_MAX_OBJECT_NAME+1];

	/* JUST COLLECT SECTION NAMES */
	iniObjectFirst( hIni );
	while ( iniObjectEOL( hIni ) != TRUE )
	{
		iniObject( hIni, szObjectName );
		if ( strcasecmp( szObjectName, "ODBC Data Sources" ) != 0 )
		{
			nStrToCopy = strlen( szObjectName ) + 1;			/* factor NULL terminator for string */
			if ( (*pnBufPos) + nStrToCopy + 1 > nRetBuffer )		/* factor NULL terminator for buffer */
				nStrToCopy = nRetBuffer - (*pnBufPos) - 2;
			strncpy( &(pRetBuffer[(*pnBufPos)]), szObjectName, nStrToCopy );
			(*pnBufPos) += nStrToCopy;
		}
		iniObjectNext( hIni );
	}

	return (*pnBufPos);
}
														

