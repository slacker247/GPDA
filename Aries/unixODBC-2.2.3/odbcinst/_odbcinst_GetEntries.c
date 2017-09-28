/****************************************************
 * _odbcinst_GetEntries
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

int _odbcinst_GetEntries(	HINI	hIni,
							LPCSTR	pszSection,
							LPSTR	pRetBuffer,
							int		nRetBuffer,
							int		*pnBufPos
						)
{
	int		nStrToCopy;
	char	szPropertyName[INI_MAX_PROPERTY_NAME+1];
	
	/* COLLECT ALL ENTRIES FOR THE GIVEN SECTION */
	iniObjectSeek( hIni, (char *)pszSection );
	iniPropertyFirst( hIni );
	while ( iniPropertyEOL( hIni ) != TRUE )
	{
		iniProperty( hIni, szPropertyName );
		nStrToCopy = strlen( szPropertyName ) + 1;			/* factor NULL terminator for string */
		if ( (*pnBufPos) + nStrToCopy + 1 > nRetBuffer )		/* factor NULL terminator for buffer */
			nStrToCopy = nRetBuffer - (*pnBufPos) - 2;
		strncpy( &(pRetBuffer[(*pnBufPos)]), szPropertyName, nStrToCopy );
		(*pnBufPos) += nStrToCopy;
		iniPropertyNext( hIni );
	}

	return (*pnBufPos);
}
														

