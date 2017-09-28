/********************************************************************
*
* SUBDUE
*
* FILE NAME: maths.c
*
********************************************************************/



#include "subdue.h"


/*******************************************************************************
FUNCTION NAME: Base2Log
INPUTS:		ULONG number
RETURNS:	DOUBLE 
PURPOSE:	Return the base 2 logarithm of number.
CALLED BY:	dl.c: SubGraphDL()
			dl.c: GraphDL()
*******************************************************************************/

DOUBLE Base2Log( ULONG number )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: Base2Log()\n", __FILE__ );
#endif
	
	if ( number <= 1 )
		return 0.0;
	
	return (DOUBLE) ( log( (DOUBLE)number ) / LOG_2 );
}


/*******************************************************************************
FUNCTION NAME: Base2LogOfFact		-- Not used. We use exact values now.
INPUTS:		ULONG number
RETURNS:	DOUBLE
PURPOSE:	Approximate the base 2 logarithm of the factorial of number. 
CALLED BY:	maths.c: Base2LogOfComb()
*******************************************************************************/

DOUBLE Base2LogOfFact( ULONG number )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: Base2LogOfFact()\n", __FILE__ );
#endif
	
	if ( number < 2)
		return 0.0;

	if ( number == 2 )
		return 1.0;
	else
		return (DOUBLE) ( ( 0.5 * ( log( (DOUBLE) ( TWO_PI * number ) ) / LOG_2 ) ) 
							+ ( ( number + ( 1.0 / ( 12.0 * number ) ) ) 
							* log( (DOUBLE) ( number / e ) ) / LOG_2 ) );
}


/*******************************************************************************
FUNCTION NAME: Base2LogOfComb		--  Deprecated! Use Base2LogOfCombination().
INPUTS:		ULONG number1, 
			ULONG number2
RETURNS:	DOUBLE
PURPOSE:	Approximate the base 2 logarithm of the combination of number1, choose
			number 2.
CALLED BY:	dl.c: SubGraphDL()
			dl.c: GraphDL()
*******************************************************************************/

DOUBLE Base2LogOfComb( ULONG number1, ULONG number2 )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: Base2LogOfComb()\n", __FILE__ );
#endif

	if ( number2 == 0)
		return 0.0;
	else
		return (DOUBLE) ( Base2LogOfFact( number1 ) - Base2LogOfFact( number2 ) - 
			Base2LogOfFact( number1 - number2 ) );
}

/*******************************************************************************
FUNCTION NAME: Base2LogOfCombination
INPUTS:		PGRAPH_VARIABLES GV
			ULONG number1
			ULONG number2
RETURNS:	DOUBLE
PURPOSE:	Calculate the base 2 logarithm of the combination of number1, choose
			number 2.  Use lookup table for exact values.
CALLED BY:	dl.c: SubGraphDL()
			dl.c: GraphDL()
*******************************************************************************/

DOUBLE Base2LogOfCombination(PGRAPH_VARIABLES GV, ULONG number1, ULONG number2 )
{
	
#ifdef DEBUG_TRACE
	printf( "%s: Base2LogOfComb()\n", __FILE__ );
#endif

	if ( number2 == 0)
		return 0.0;

	if (number1 >= GV->lgOfFactTableSize || number2 >= GV->lgOfFactTableSize) {
		CalculateLgFactN(GV, number1 > number2 ? number1 : number2 );
	}
		
	return GV->lgOfFact[ number1 ] - GV->lgOfFact[ number2 ] - 
			GV->lgOfFact[ number1 - number2 ];
}

/*******************************************************************************
FUNCTION NAME: CalculateLgFactN
INPUTS:		PGRAPH_VARIABLES GV
			NULONG numberOfVertices
RETURNS:	none
PURPOSE:	Generate lookup table for: lg(n!)
CALLED BY:	readgraph.c: ReadGraph()
*******************************************************************************/
	
void CalculateLgFactN( PGRAPH_VARIABLES GV, ULONG numberOfVertices )
{
	ULONG startN;
	ULONG n;
	
#ifdef DEBUG_TRACE
	printf( "%s: CalculateLgFactN()\n", __FILE__ );
#endif

	startN = GV->lgOfFactTableSize;
	if (numberOfVertices < startN)
		return;

	GV->lgOfFact = Realloc(GV->lgOfFact, (numberOfVertices + 10) * sizeof(DOUBLE));
	GV->lgOfFactTableSize = numberOfVertices + 10;

	if (startN == 0) {										// if starting at 0
		GV->lgOfFact[0] = 0;								// lg 0! = 0.0
		startN++;
	}

	for (n=startN; n < GV->lgOfFactTableSize; n++)					// fill in table
		GV->lgOfFact[n] = GV->lgOfFact[n - 1] + Base2Log(n);
}


