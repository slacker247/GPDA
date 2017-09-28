#include "sqp.h"

void sqpStoreDataType( char *pszType, int nPrecision, int nScale )
{
    HSQPDATATYPE    hDataType;
	
#ifdef SQPDEBUG
	printf( "[SQP][%s][%d] BEGIN: %s ( %d, %d )\n", __FILE__, __LINE__, pszType, nPrecision, nScale );
#endif

    if ( g_hDataType )
    {
        printf( "[SQP][%s][%d] END: Warning; Possible memory leak.\n", __FILE__, __LINE__ );
        return;
    }

	hDataType = (HSQPDATATYPE)malloc( sizeof(SQPDATATYPE) );
	hDataType->pszType	    = (char*)strdup( pszType );
    hDataType->nPrecision   = nPrecision;
	hDataType->nScale       = nScale;

    g_hDataType = hDataType;

#ifdef SQPDEBUG
	printf( "[SQP][%s][%d] END:\n", __FILE__, __LINE__ );
#endif

}


