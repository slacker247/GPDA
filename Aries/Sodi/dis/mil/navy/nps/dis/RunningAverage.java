/*
 File:		RunningAverage.java
 CVS Info:	
 Compiler:	jdk 1.3 
 */
package mil.navy.nps.dis;                       // package we belong to

import java.util.*;

/**
 * This Java class allows for an object that will keep N items 
 * and return the average of those items when queried.
 *
 *@version 1.0
 *@author <a href="mailto:sdheller@cs.nps.navy.mil">Scott D. Heller</a>
 * (<a href="http://web.nps.navy.mil/~brutzman">http://web.nps.navy.mil/~brutzman</a>)
 * This code includes parts of Kent Watsen's EAI-based World.java/Ownship.java
 * and Don McGregors's testing/BehaviorStreamBufferTest.java
 *
 *<dt><b>Location:</b>
 *<dd>
 *<a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/RunningAverage.java">
 *             
 *http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/RunningAverage.java</a>
 *
 *<dt><b>History:</b>
 *<TABLE>
 *<tr>
 *	<td>	21 Nov 98
 *	<td>	Scott D. Heller
 *	<td>	New
 *</TABLE>
 *<P>
 *
 *@see EspduTransform
 */

public class RunningAverage
{

/**
 * When DEBUG is true, System.out.println text messages trace the internals of script operation.
 * Text output appears in the Java Console (CosmoPlayer browser) or in the VRML console (WorldView browser)
 */
 
	private static boolean DEBUG		= false;  // diagnostic messages on/off

/**
 * Vector to keep all data points. Note: Vector requires objects.
 */
	private Vector 	dataVector;
	private int		numberOfDataPoints = 0;
	
/**
 * Constructor requires the number of data points to be retained
 * to be specified.
 */

	public RunningAverage( int requestedNumberOfDataPoints )
	{
		numberOfDataPoints = requestedNumberOfDataPoints;
		dataVector = new Vector( numberOfDataPoints );
	}
	
/**
 * Add one data point to the vector, and return the new average.
 */

	public float addDataPoint( float dataPoint )
	{
		Float floatData = new Float( dataPoint );
		
		debug(" dataVector.size() = " + dataVector.size() );
		// remove the oldest data point and add at the rear.
		if ( dataVector.size() >= numberOfDataPoints )
		{
			dataVector.removeElementAt( 0 );
		}
			
		dataVector.addElement( floatData );
		
		debug( "ave is " + average() );
		return( average() );
		
	}
	
/**
 * Function to return the calculated average.
 */
	public float average()
	{
		int currentSize = dataVector.size();
		float result = (float)0.0;
		Float tempFloatObject;
		
		for( int index = 0; index < currentSize; index++ )
		{
			tempFloatObject = (Float) dataVector.elementAt( index );
			result += tempFloatObject.floatValue();
		}
		result /= (currentSize + 1);
		
		return result;
		
	}
	
/**
 * Returns the last update made to the circular queue. 
 * Useful when calculating deltas between updates.
 */
	public float getPrevUpdate()
	{
		return ( (Float)dataVector.lastElement() ).floatValue();
	}
	

public boolean getDEBUG ()
{
	debug ("getDEBUG " + DEBUG);

	return DEBUG;
}

public void setDEBUG (boolean pDEBUG)
{
	DEBUG = pDEBUG;

//	traceJava.setValue (pDEBUG);
 
	// VRML console trace messages
	trace ("RunningAverage.java: setDEBUG " + pDEBUG);

	System.out.println ("  RunningAverage.java: setDEBUG " + pDEBUG);
}

/**
  Debugging output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected static void debug (String pDiagnostic)
{
  if (DEBUG) System.out.println("  RunningAverage: " + pDiagnostic);
}

/**
  Debugging output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected static void trace (String pDiagnostic)
{
  System.out.println("  EspduTransform: " + pDiagnostic);
}

}
