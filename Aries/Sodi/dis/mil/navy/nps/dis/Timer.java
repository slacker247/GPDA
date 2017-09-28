/*
 File:		Timer.java
 CVS Info:	
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;
import java.lang.System.*;

/**
 * This Java class provides a simple timer using the system clock converted to seconds.
 *
 *@version 1.0
 *@author <a href="mailto:brutzman@nps.navy.mil">Scott Heller</a> 
 *(<a href="http://web.nps.navy.mil/~brutzman">http://web.nps.navy.mil/~brutzman</a>)
 * This code includes parts of Kent Watsen's EAI-based World.java/Ownship.java
 * and Don McGregors's testing/BehaviorStreamBufferTest.java
 *
 *<dt><b>Location:</b>
 *<dd><a href=
 *"http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/Timer.java">
 * http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/Timer.java</a>
 *
 *<dt><b>History:</b>
 *<TABLE>
 *<tr>
 *	<td>	13 December 1998
 *	<td>	Scott Heller
 *	<td>	New
 *</TABLE>
 */


public class Timer
{
	private long startTime = 0;

	/** 
	 * Constuctor: Sets the current timer to zero.
	 */
	public Timer()
	{
		startTime = System.currentTimeMillis();
	}
	
	/** 
	 * Sets the current timer to zero.
	 */
	public void reset()
	{
		startTime = System.currentTimeMillis();
	}

	/** 
	 * Returns the number of seconds since the timer was reset.
	 */
	public float getDuration()
	{
		return toSec( System.currentTimeMillis() - startTime );
	}

	public String toString()
	{
		return ("" + getDuration() );
	}

	private float toSec( long t )
	{
		return (float)t / 1000;
	}



}// end class timer
