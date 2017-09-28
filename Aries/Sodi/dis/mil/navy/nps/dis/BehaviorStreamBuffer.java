
package mil.navy.nps.dis;		              // the package we belong to

import java.util.*;
import java.io.*;

import org.web3d.vrtp.security.*;

/**
 * BehaviorStreamBuffer is an abstract class that defines functionality for
 * other subclasses, including BehaviorStreamBufferNetwork and BehaviorStreamBufferFile.
 * The idea is that we have a single interface for getting things from the external
 * world, be it from a network or a file.<p>
 *
 * This includes a BehaviorStreamBufferInfo object, which includes info about
 * the stream such as URLs where more info can be found, rtp status, version 
 * number, etc.<p>
 *
 * @author DMcG
 */

public abstract class BehaviorStreamBuffer extends Object implements Runnable, AllPermissionsBadge
{

  protected boolean      inputThreadStarted = false;  // Already running the input thread? Prevents starting it twice
  protected boolean      runContinue = true;          // continue looping in run() method?
  protected boolean      rtpEnabled = false;          // are there RTP headers for this behavior stream?
  protected BehaviorStreamBufferInfo info;            // Info about this stream 
  protected boolean      readThreadRunning = false;   // Set to true once the read thread actually executes

  /**
   * SecurityStrategy is used as a way to get around the java sandbox. A security 
   * scheme is picked at runtime for whatever type of box we are running on.
   */

  protected SecurityStrategy strategy = SecurityStrategy.getSecurityStrategy();


/**
 * Simple method to launch thread. This kicks off the subclass of BSB
 * and starts it reading in its own thread. This is required for
 * some subclasses, such as sockets; for files, this is optional--
 * we can read one PDU at a time from them without blocking.
 */

protected abstract void  startInputThread();

/**
 * Starts the input thread, but with security hoop-jumping enabled.
 */

public void startInputThreadWithSecurity()
{
  	try
    {
			debug ("initialize:  strategy.invokePrivilege(this, \"startInputThread using security bypassing\");");
			strategy.invokePrivilege(this, "startInputThread");
		}
		catch (Exception catchAllException)
		{
			trace ("initialize:   exception from strategy.invokePrivilege " + catchAllException);
		 	catchAllException.printStackTrace();
 		}
		debug ("datagramStreamBufferThreadStarted = " + inputThreadStarted);
}



/**
 *  Terminate the run loop and shutdown the thread. This should
 *  be used only for threaded readers.
 */
public abstract void shutdown ();

/**
 *   suspend reading in the DatagramStreamBuffer
 */
public abstract void suspendReading();

/**
 * Start reading packets from the datagramStreamBuffer again.
 */

public abstract void resumeReading();


/**
 *  Threading method to read/write until shutdown.
 */
public abstract void run();

/**
 * setRtpEnabled turns on or off the RTP headers.
 */

public void setRtpEnabled(boolean pEnabled)
{
  rtpEnabled = pEnabled;
}

/**
 * Returns true if this BSB is using RTP.
 */

public boolean getRtpEnabled()
{ return rtpEnabled;
}

public BehaviorStreamBufferInfo getInfo()
{ return info;
}

public void setInfo(BehaviorStreamBufferInfo pInfo)
{ info = pInfo;
}

/**
 * Returns a vector of all the PDUs received since the last time we
 * asked. Queries the underlying input buffer for this information.\
 * this is generally used with threaded readers.
 */

public abstract Vector receivedPdus();

/**
 * Get the next PDU from the input stream. This is generally used
 * with unthreaded readers.
 */

public abstract ProtocolDataUnit getNextPdu();

/**
 * Sends a PDU. If the underlying destination address has already
 * been set, for example in a multicast or file, we don't need
 * to supply an address.
 */

public abstract void sendPdu(ProtocolDataUnit pPdu);

/**
 * Send a PDU to an address. Since the address can be in many forms,
 * for example an InetAddress and a port number, we cheat here. The
 * destination address is passed in as generic objects; a concrete
 * subclass, such as a Unicast UDP object, will cast the generic
 * objects here to what it expects, such as an InetAddress. (After
 * checking for the right type with instanceof, of course!)<p>
 *
 * @param pPdu protocol data unit being sent
 * @param pAddress1 the first part of the address, typically the inet address
 * @param pAddress2 the second part of the address, typically null or the destination port
 */

public abstract void sendPdu(ProtocolDataUnit pPdu,
                             Object pAddress1,
                             Object pAddress2);

/**
 * Utility method accepts int ports
 */

public void sendPdu(ProtocolDataUnit pPdu,
                             Object pAddress1,
                             int    pAddress2)
{
	sendPdu(pPdu, pAddress1, new Integer(pAddress2));
}

/**
 * Closes down input buffers, sockets, or open files nicely
 */

public abstract void cleanup();

/**
 *  Finalize method--used to clean up any sockets that are still open
 */

protected abstract void finalize() throws Throwable;


/**
 * When DEBUG is true, System.out.println text messages trace the internals of script operation.
 * Text output appears in the Java Console (CosmoPlayer browser) or in the VRML console (WorldView browser)
 */
 
	protected boolean DEBUG = false;  // diagnostic messages on/off

/**
 * Retrieve value of DEBUG.  When DEBUG is true, System.out.println text messages trace the internals of script operation.
 * Text output appears in the Java Console (CosmoPlayer browser) or in the VRML console (WorldView browser)
 */
 
public boolean getDEBUG ()
{
	debug ("getDEBUG " + DEBUG);

	return DEBUG;
}

/**
 * Set value of DEBUG.  When DEBUG is true, System.out.println text messages trace the internals of script operation.
 * Text output appears in the Java Console (CosmoPlayer browser) or in the VRML console (WorldView browser)
 */
 
public void setDEBUG (boolean pDEBUG)
{
	DEBUG = pDEBUG;

	trace ("setDEBUG " + pDEBUG);
}

/**
  Debugging output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected void debug (String pDiagnostic)
{
  if (DEBUG)
  {
  	System.out.println("BehaviorStreamBuffer: " + pDiagnostic);
  	System.out.flush();
  }
}

/**
 *   Guaranteed debugging output. Pass in a string, and it gets printed out on the console.
 *   You can pass in strings such as "foo " + bar.getName().
 */

protected void trace (String pDiagnostic)
{
  System.out.println("BehaviorStreamBuffer: " + pDiagnostic);
  System.out.flush();
}

/**
 * This is some thread synchronization code. When we start a bsb, the thread
 * may take some amount of time to actually begin reading. This uses a boolean
 * that gets flipped to true when the run() method actually begins. If the
 * boolean isn't flipped, we wait for 5 sec, which should be plenty of time
 * for the thread to finish starting and start reading.
 */

protected void checkForThreadStart()
{
  if(readThreadRunning = false)
  {
    try
    {
      Thread.sleep(5000);   // 5000 is an arbitrary large time
    }
    catch(Exception e)
    {
      System.out.println(e);
    }
  }
}


} // end of class BehaviorStreamBuffer

