/*
 File:		EntityDispatcher.java
 CVS Info:	$Id: EntityType.java,v 1.2 1998/01/27 18:44:11 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;

import java.io.*;
import java.net.*;
import java.util.*;

// Java JSAI classes for VRML

import vrml.*;
import vrml.field.*;
import vrml.node.*;

import mil.navy.nps.util.*;
import mil.navy.nps.disEnumerations.*;

import org.web3d.vrtp.security.*;   // Security stuff (dangerous, not very secure)

/**
 * EntityDispatcher is an independent thread that connects all EsdpuTransforms
 * to the BehaviorStreamBuffer.
 * 
 * A single EntityDispatcher object handles all routing of PDUs to & from the
 * EspduTransform scripts which connect to individual entities in the VRML scene.
 * The EntityDispatcher is static, meaning that only one copy can exist.
 * EntityDispatcher is instantiated and threaded by the first EspduTransform
 * encountered.  In turn, the EntityDispatcher similarly instantiates and
 * threads the static BehaviorStreamBuffer, which independently reads all
 * network PDUs and then hands them over in a list.
 *<P>
 * 
 * The EntityDispatcher contains
 *<ul>
 * 	<li>	A BehaviorStreamBuffer object which reads PDUs from the network
 * 	<li>	A Hashtable of all the active entities (i.e. EspduTransform nodes)
 *</ul>
 *<P>
 * 
 * EntityDispatcher responsibilities include
 *<ul>
 * 	<li>	Receiving all PDUs in any order from the behaviorStreamBuffer
 * 	<li>	Sending all PDUs originating in the VRML scene to the behaviorStreamBuffer
 *			(and thus to the network)
 * 	<li>	Relaying ESPDUs and any other entity-specific information to & from the 
 * 			corresponding entity's EspduTransform
 * 	<li>	Handling Simulation Management PDUs (future work...)
 *</ul>
 *<P>
 *
 * The VRML scene interrogates each entity at periodicities approximately equal
 * to the author-provided values readInterval (or writeInterval).  Defaults
 * are set in EspduTransformPROTO.wrl
 *<P>
 *
 *<PRE>
 *                 *--------------------------------------------------*
 *                /                                                  /|
 *               *--------------------------------------------------* |
 *     Browser   |            VRML Scene                            | |
 *    [threaded] |                                                  | |
 *               |                                                  | |
 *               |                                                  | |
 *               |  geometry              geometry                  | |
 *               |     :                     :            Radio     | |
 *               |EspduTransform     EspduTransform   Communications| |
 *               |   PROTO                 PROTO          PROTOs    | |
 *               |     :                     :              :       | *
 *               |(Script node)  ...  (Script node)  (Script node)  |/ 
 *               *-----+----------+----------+----------------------*
 *                  Entity 1     ...      Entity n    Radios 1..n
 *                     |          |          |             |
 *                     |          |          |             +----------------------+
 *                     |          |   *------+--------*                           |
 *                     |         ...  |EspduTransform | dead reckoning            |
 *                     |    *-----+---------------*   |  updates here             |
 * multiple copies,    |    |EspduTransform       |   | (PDU posture              |
 *  one per entity  *--+----------------------*   |   |  extrapolation)  *-------+--------*
 *                  |EspduTransform           |   |---*                  | Radio          |
 *                  |                         |   |                      | Communications |
 *                  |                         |---*                      | PduScriptNode  |
 *                  *--+----------------------*                          *-------+--------*
 *                     ^          |          ^                                    ^
 *                     |          |          |                                    | Signal,
 *                     |          v          |                                    | Receiver,
 *                  *--+----------+----------+--*                                 | Transmitter
 *     singleton    |                           |    (separate subscribe loop)    | PDUs
 *    [threaded]    |     EntityDispatcher      |---------------------------------+
 *  if thread fails |                           |
 *  to start, runs  |         EntityHashTable   |
 *  single read per |            - Entity 1     |
 *  each draw loop  |            - Entity 2     |
 *                  |            - [...]        |
 *   single copy    |            - Entity n     |     (reads)
 *                  *-------------+-------------*         ^
 *                                |                       |
 *                                |               Entity State, Radio, other PDUs
 *                                |                             |
 *                  *-------------+-------------*               V
 *                  |                           |           (writes)
 *      static      |                           |
 *    [threaded]    |   BehaviorStreamBuffer    | 
 *    single copy   |                           |
 *                  |                           |
 *                  *-------------+-------------* 
 *                                |
 *                  *-------------+-------------*
 *       static     |                           | 
 *    [threadable]  |    org.web3d.vrtp.net.    |    browser-specific
 * but not threaded |   DatagramStreamBuffer    |        security
 *    single copy   |                           |      permissions
 *                  |                           |
 *                  *----------+-----+----------* 
 *                             |     |
 *                             | ... |  multicast address(es)/port(s)
 *                             |     |  (area of interest management)
 *                             |     |
 *              Network 0======+=====+=====0
 *</PRE>
 * 
 * The behaviorStreamBuffer, an instance of a BehaviorStreamBuffer, reads PDUs
 * from the wire. The EntityDispatcher occasionally pulls
 * PDUs from the behaviorStreamBuffer. For each PDU, the simulation manager
 * looks at the type. 
 *<P>
 * 
 * If it's an ESPDU, the EntityDispatcher code looks at the EntityID 
 * triplet (site, application, ID) that uniquely identifies each 
 * entity, uses that aggregated triplet as a hash key to the Hashtable
 * to find the appropriate EspduTransform reference, and then passes
 * the PDU data via that EspduTransform to the correct entity. If the
 * PDU has an unrecognized EntityID, we may optionally instantiate a
 * local entity to reflect it, and insert it into the scene. The
 * EspduTransform object is responsible for communications with the
 * VRML scene. ESPDU culling may be performed either here or
 * at the entity level. Since arrivals of PDUs for a given entity may
 * occur out of order, it seems more logical to put PDU interpretation
 * at the EspduTransform level, where there is a bit more information
 * about update frequency and the like. In some special cases, it might
 * make more sense to cull special PDUs early in to cut down on PDU 
 * handling overhead. That's an implementation and optimization detail.
 *<P>
 * 
 * If it's a SimulationManagement PDU, it will be handled by
 * a different block of code. SimulationManagement PDUs include
 * Start Simulation PDU, Stop Simulation PDU, Create Entity
 * PDU, EventReport PDU, Query PDU and Data PDU. Each of
 * these will probably require some special handling, and
 * perhaps some state changes in the EntityDispatcher object.
 * Some of them will require that PDUs be sent in response.
 *<P>
 * 
 * Cardinality: there is one and only one instance of a EntityDispatcher
 * per DIS application. The EntityDispatcher is instantiated and operated
 * through class (static) methods.
 *<P>
 * 
 * More functionality might be integrated into this object as well.
 * Deciding which functionality goes where is the objective of the vrtp
 * streaming stack.
 *<P>
 *   
 *<dt><b>History:</b>
 *<TABLE>
 *<tr>
 *	<td>	29 Oct 97
 *	<td>	Don McGregor
 *	<td>	New
 *<tr>
 *	<td>	19 Oct 98
 *	<td>	Don Brutzman & Don McGregor
 *	<td>	integrate revised EspduTransform and Javadoc updates for jdk1.2b4,
 *		fix threading, remove references to SimulationManager and
 *		EntityState classes.
 *<tr>
 *	<td>	12 Nov 98
 *	<td>	Don McGregor
 *	<td>	Added PduPublisher and PduSubscriber interfaces.
 *<tr>
 *	<td>	14 Feb 99
 *	<td>	Don Brutzman
 *	<td>	Added Fire and Collision PDU dispatching to EspduTransform.
 *<tr>
 *	<td>	20 Feb 99
 *	<td>	Don McGregor
 *	<td>	Added MulticastTunnelServer connectivity.
 *<tr>
 *	<td>	4 Apr 99
 *	<td>	Don Brutzman
 *	<td>	Thread shutdown and various trace fixes.
 *<tr>
 *	<td>	10 Sep 99
 *	<td>	Don McGregor & Don Brutzman
 *	<td>	Security strategy
 *<tr>
 *	<td>	28 Sep 99
 *	<td>	Don Brutzman
 *	<td>	Added Comment and CreateEntity PDUs
 *<tr>
 *	<td>	3 September 2000
 *	<td>	Don Brutzman
 *	<td>	Changed dis.PduTypeEnum to disEnumerations.PduTypeField
 *<tr>
 *	<td>	18 September 2000
 *	<td>	Don Brutzman and Dave Laflam
 *	<td>	Added, tested RadioCommunicationFamily PDUs
 *</TABLE>
 *<P>
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/dis/EntityDispatcher.java">
 *                          ~/mil/navy/nps/dis/EntityDispatcher.java</a>
 *
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EntityDispatcher.java">
 *                   http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EntityDispatcher.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 * @author Don McGregor (mcgredo@nps.navy.mil)
 * @author Don Brutzman (brutzman@nps.navy.mil)
 * 
 * @see EspduTransform
 * @see BehaviorStreamBuffer
 * @see org.web3d.vrtp.net.DatagramStreamBuffer
 * @see org.web3d.vrtp.security.SecurityStrategy
 **/
 

public class EntityDispatcher extends Object implements PduPublisher, Runnable, NetworkCommBadge
{

/**
 *  used to kill thread
 */
  private static boolean keepRunThreadAlive = true;

/**
 *  Handles communication with the wire, collecting PDUs from the packets received by a
 *  DatagramStreamBuffer.
 */
  protected static BehaviorStreamBuffer	behaviorStreamBuffer = null;

/**
 *  Thread for BehaviorStreamBuffer
 */
  protected Thread behaviorStreamBufferThread;

/**
 *  Internal threadable class that handles tunnel communications with a MulticastRelayServer
 *  if no native multicast traffic heard on local LAN.
 */
  protected static EntityDispatcher.TunnelManager tunnelManager = null;

/**
 *  Contains references to all EspduTransforms, RadioCommunicationsFamily nodes and other Script nodes in the VRML world,
 *  using the entity-unique site-ID/applicationID/entityID triplet
 *  as the hash key.
 */
  protected static Hashtable	pduSubscriberScriptNodesHashtable = new Hashtable();

 /**
 *  maxSleepInterval (milliseconds) helps to provide proper balance of resources
 *  among threads, where milliseconds of delay are set based on readInterval/writeInterval
 *  periodicities provided in the VRML scene; also provided to behaviorStreamBuffer.
 */
  private final	long		maxSleepInterval = 10000;

  /**
 *  minSleepInterval (milliseconds) prevents race conditions (lack of sleep).
 */
  private final	long		minSleepInterval = 50;

/**
 *  sleepInterval (milliseconds) can be set by VRML scene, to best match entity read/write periodicity.
 */
  private	long		sleepInterval = maxSleepInterval;

/** platform-specific security strategy
 */
   protected SecurityStrategy strategy;               // Security model for this browser

/**
 * Singleton copy of EntityDispatcher. See the comments for getEntityDispatcher for
 * details.
 */

  private static EntityDispatcher entityDispatcher = null;

/**
 *  Flag for debugging/diagnostic output enabled
 */
  protected static boolean DEBUG = false;

/**
 *  Get debugging flag
 */
public boolean getDEBUG ()
{
	debug ("getDEBUG " + DEBUG);

	return DEBUG;
}

/**
 *  Set debugging flag
 */
public void setDEBUG (boolean pDEBUG)
{
	DEBUG = pDEBUG;

	behaviorStreamBuffer.setDEBUG (pDEBUG);  // pass it along
 
	trace ("setDEBUG " + pDEBUG);
}

  private boolean runStarted = false;

  private int traceCounter = 0;

  /**
   * These variables are used as part of the singleton pattern.
   * We remember the original parameters to the ED created; if
   * we later ask for an ED with other than these parameters, we
   * know there's been an error.
   */

  private static String   originalMcastAddress = null;
  private static int      originalPort = -1;

/**
 * getEntityDispatcher returns the single, shared instance of the EntityDispatcher
 * object. This is known as the Singleton pattern; see Mark Grand, "Patterns
 * in Java", p. 127. Rather than creating an entityDispatcher by calling new,
 * you should call getEntityDispatcher. This handles creation of a single, shared
 * instance for all callers.<p>
 *
 * Note that using null for the multicast address will, the first time, return
 * a unicast EntityDispatcher. If, later, the getEntityDispatcher method is
 * called with anything other than the original parameters, the program is
 * stopped and an error message printed. 
 *
 * @param pAddress string format multicast address, usually in dotted decimal format
 *        (eg, 225.7.8.10).
 * @param pPort port in integer format, eg 6025.
 */

  public static synchronized EntityDispatcher getEntityDispatcher(String pAddress, int pPort)
  {
   /* Implementation details: we have to worry about multiple threads calling
    * the same method at the same time. We could make the entire method
    * synchronized, but this would mean a fairly big performance hit--acquiring
    * a lock takes a lot of time. Also, there is a single monitor/lock per
    * object, and locking the EntityDispatcher class object might slow down
    * access elsewhere. So I took this approach instead. What happens is we
    * check to see if the object is null; if not, we return it, since it has
    * already been created. If it is null, we lock on an arbitrary object
    * and create the EntityDispatcher singleton instance. Then we return
    * that. 
    *
    * Note that the ED constructor takes arguments for the port and multicast
    * group. It's possible for someone to pass in a multicast group and port
    * for other than the currently existing group and port. This is treated as
    * an error, and causes a program stop. This means that ED is currently
    * currently limited to a single multicast group (address + port combination).
    * There are better ways to handle multiple groups and/or multiple data
    * sources, but such an implementation is deferred to future work.
    */

    boolean error = false;   // Matches older creation

    // NB: we must synchronize/lock BEFORE testing for ED being null. The
    // entire section is the critical area.

    try
    {
		if (DEBUG) System.out.println ("entering getEntityDispatcher");

		// No ed exists; create a new one
		if (entityDispatcher == null)
		{
 			originalPort = pPort;
    			originalMcastAddress = pAddress;
        
    			if(pAddress == null)
			{
				entityDispatcher = new EntityDispatcher(pPort);
			}
			else
			{
				entityDispatcher = new EntityDispatcher(pAddress, pPort);
	        	}
      		} // end ed == null, getting ed for first time.
		else
		{
			// ed was not null, there was an existing one. Check to make sure the
			// newly requested one has the same parameters as the existing one.

			// System.out.println ("getEntityDispatcher checking port...");

			// Ports don't match?
			if(EntityDispatcher.originalPort != pPort)
			  throw new RuntimeException("Requested an EntityDispatcher with other than the original parameters.");

			// System.out.println ("getEntityDispatcher checking address...");

			// Originally a unicast, now trying to get a multicast?
			if((EntityDispatcher.originalMcastAddress == null) && (pAddress != null))
			  throw new RuntimeException("Requested an EntityDispatcher with other than the original parameters.");

			// System.out.println ("getEntityDispatcher checking address being changed to unicast...");

			// Originally a multicast, now trying to get a unicast?
			if((EntityDispatcher.originalMcastAddress != null) && (pAddress == null))
			  throw new RuntimeException("Requested an EntityDispatcher with other than the original parameters.");

		//	System.out.println ("getEntityDispatcher checking address being changed to another multicast...");

			// Both multicast, but trying to get different mcast addresses. this is true if we
			// have string addresses for both, but they don't match. Use compareToIgnoreCase
			// on the off chance that someone is using strings rather than dotted decimals
			// for the mcast address.

		// causes crash :(
		//	if(((EntityDispatcher.originalMcastAddress != null) && (pAddress != null)) && 
		//	   (EntityDispatcher.originalMcastAddress.compareToIgnoreCase(pAddress) != 0))
		//	  throw new RuntimeException("Requested an EntityDispatcher with other than the original parameters.");

		//	System.out.println ("getEntityDispatcher done checking...");

		} // end of getting existing ed case

		if (DEBUG) System.out.println ("exiting getEntityDispatcher");

    }
    catch (Exception catchAllException)
    {
		System.out.println ("startBehaviorStreamBufferThread() exception: " + catchAllException);
	catchAllException.printStackTrace();
    }
    
   return entityDispatcher;
  } // end of getEntityDispatcher

        
/**
 * Example of delegation of writing a PDU to the single static behaviorStreamBuffer.
 */
 
public void sendPdu (ProtocolDataUnit pPdu)
{
	behaviorStreamBuffer.sendPdu (pPdu);
}

/**
 * Example of delegation of writing a PDU to the single static behaviorStreamBuffer.
 */
  
public synchronized void sendPdu(ProtocolDataUnit pdu,
                                 String pDestinationHost,
                                 int    pDestinationPort)
{
	behaviorStreamBuffer.sendPdu (pdu, pDestinationHost, new Integer (pDestinationPort));
}


/**
  Debugging output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected void debug (String pDiagnostic)
{
	if (DEBUG)
	{
  		System.out.println("EntityDispatcher: " + pDiagnostic);
  		System.out.flush();
	}
}

/**
  Guaranteed trace output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected void trace (String pDiagnostic)
{
	System.out.println("EntityDispatcher: " + pDiagnostic);
	System.out.flush();
}

/**
    Unicast constructor; listens on the given unicast port. Note that this is private;
    you should use getEntityDispatcher to get a reference to the singleton.
*/
private EntityDispatcher(int pPort)
{
	trace ("start unicast constructor...");

	trace ("Get platform-specific security strategy");
	strategy = SecurityStrategy.getSecurityStrategy();

        if(behaviorStreamBuffer != null)
        {
		trace ("**** behaviorStreamBuffer not null during EntityDispatcher constructor!!  unsynchronized method on entityDispatcher?? returning with no action");
		return;
	//	trace ("behaviorStreamBuffer.cleanup(); // Close down sockets....");
	//	behaviorStreamBuffer.cleanup();
        }

//	createUnicastBehaviorStreamBuffer (pPort);

	Object args[] = new Object[1];
	args[0] = new Integer(pPort);

	// Enable security, then call back to the method 'createMulticastSocket', which actually opens the sockets.
	trace ("strategy.invokePrivilege(this, \"createUnicastBehaviorStreamBuffer\", [" +
		+ pPort + "])");
	strategy.invokePrivilege(this, "createUnicastBehaviorStreamBuffer", args);

        trace ("created unicast behaviorStreamBuffer (" + pPort + ") (this should appear only once)");
	trace ("constructor complete");
        return;
}

/**
 * Multicast constructor; listens on the given port and group. Note that this is private;
 * you should use getEntityDispatcher to retrieve the singleton entity dispatcher.
 */

private EntityDispatcher(String pMulticastAddress, int pPort)
{
	trace ("start multicast constructor...");

	trace ("Get platform-specific security strategy");
	strategy = SecurityStrategy.getSecurityStrategy();

	if(behaviorStreamBuffer != null)
	{
		trace ("**** behaviorStreamBuffer not null during EntityDispatcher constructor!!  unsynchronized method on entityDispatcher??? returning with no action");
		return;
	//	trace ("behaviorStreamBuffer.cleanup(); // Close down sockets....");
	//	behaviorStreamBuffer.cleanup();
	}

//	behaviorStreamBuffer = new BehaviorStreamBuffer(pMulticastAddress, pPort);

	Object args[] = new Object[2];
	args[0] = pMulticastAddress;
	args[1] = new Integer(pPort);

	// Enable security, then call back to the method 'createMulticastSocket', which actually opens the sockets.
	trace ("strategy.invokePrivilege(this, \"createMulticastBehaviorStreamBuffer\", [" +
		pMulticastAddress + " " + pPort + "])");
	strategy.invokePrivilege(this, "createMulticastBehaviorStreamBuffer", args);

	trace ("created multicast behaviorStreamBuffer (this should appear only once), " +
			pMulticastAddress + "/" + pPort);

//	Thread behaviorStreamBufferThread = new Thread(behaviorStreamBuffer, "BSBTrafficCheck-" + ClassUtilities.nextSerialNum());
//	behaviorStreamBufferThread.start(); 
//	startBehaviorStreamBufferThread();

	trace ("constructor:  strategy.invokePrivilege(this, \"startBehaviorStreamBufferThread\")");
	strategy.invokePrivilege(this, "startBehaviorStreamBufferThread");

	trace ("constructor complete");
	return;
}

/**
 * Creates unicast UDP BehaviorStreamBuffer. This should be called by the appropriate security 
 * strategy first; eg, the constructor calls the security strategy, which, after the
 * appropriate security calls have been made, calls this. Passes in datagramPort
 * indirectly.
 */
public synchronized void createUdpUnicastBehaviorStreamBuffer (Integer pPortNumber)
{
	debug ("commence createUdpUnicastBehaviorStreamBuffer (" + pPortNumber + ")");
	behaviorStreamBuffer = new BehaviorStreamBufferUDP(pPortNumber.intValue() ); // unicast since no multicast address provided
	debug ("complete createUdpUnicastBehaviorStreamBuffer (" + pPortNumber + ")");
}


/**
 * Creates multicast BehaviorStreamBuffer. This should be called by the appropriate security 
 * strategy first; eg, the constructor calls the security strategy, which, after the
 * appropriate security calls have been made, calls this. Passes in datagramPort
 * indirectly.
 */
public synchronized void createMulticastBehaviorStreamBuffer (String pMcastAddress, Integer pPortNumber)
{
	debug ("commence createMulticastBehaviorStreamBuffer (" + pMcastAddress + ", " + pPortNumber + ")");
	behaviorStreamBuffer = new BehaviorStreamBufferUDP(pMcastAddress, pPortNumber.intValue() ); // multicast since address provided
	debug ("complete createMulticastBehaviorStreamBuffer (" + pMcastAddress + ", " + pPortNumber + ")");
}


/**
 *  startBehaviorStreamBufferThread starts the behaviorStreamBuffer thread.
 *  <p>
 *  Called by run(); this is the essential setup portion of the run loop.
 *  run() has enabled platform-specific security at this point,
 *  so startBehaviorStreamBufferThread doesn't have to worry about network sandboxes.
 */
 
public void startBehaviorStreamBufferThread () 
{
   try { //  catchAllException to diagnose run-time errors while VRML browser continues

	trace ("started startBehaviorStreamBufferThread ()");
	
	behaviorStreamBufferThread = new Thread(behaviorStreamBuffer, "BSB-" + ClassUtilities.nextSerialNum());

	trace ("startBehaviorStreamBufferThread:  thread starting...");
	behaviorStreamBufferThread.start(); 

	// trace ("behaviorStreamBufferThread ThreadGroup = " + behaviorStreamBufferThread.getThreadGroup()); // crashes Netscape

	trace ("startBehaviorStreamBufferThread:  " + behaviorStreamBufferThread.getName () + " thread started");

	trace ("startBehaviorStreamBufferThread complete");
    }
    catch (Exception catchAllException)
    {
		trace ("startBehaviorStreamBufferThread() exception: " + catchAllException);
	catchAllException.printStackTrace();
	setDEBUG (true);
    }
} // end of method


/**
 * Adds a subscribing Scipt node to the pduSubscriberScriptNodesHashtable
 * being maintained. Once an EspduTransform (or other Script node) has been added to
 * the list, it can receive PDUs addressed to that EntityID triplet.
 * This method is invoked by the Script node to record its reference
 * when it is constructed.  Availability of all the Script node references
 * in the Hashtable means that PDUs can be delivered when ready by this
 * EntityDispatcher thread.
 *<P>
 * This is synchronized to ensure serial access. (This might not actually
 * be neccesary; I think hashtable is synchronized internally, but can't
 * confirm that.)
 *
 * addListener and removeListener are the two methods that implement the
 * PduPublisher interface. Clients subscribe via this mechanism.
 * Note that more than one object may subscribe to an entityID.
 */

public synchronized void addListener (PduSubscriber pSubscriber, EntityID pEntityID)
{
	Vector hashtableList;

	try { //  catchAllException to diagnose run-time errors while VRML browser continues

		debug ("start addListener()");
	
		// Get the list of recipients associated with each entityID from the hashtable
		hashtableList = (Vector) pduSubscriberScriptNodesHashtable.get(pEntityID);

		// Null? must be a new list. Create a new one and add it to the hash table
		// It would be better to use a Set here instead of a Vector; adding multiple
		// instances of an object would result in only one instance in the Set.
		// But that's a JDK 1.2 feature, which puts us out of Netscape territory for now.

		if(hashtableList == null)
		{
		  hashtableList = new Vector();
		  pduSubscriberScriptNodesHashtable.put(pEntityID, hashtableList);
		}

		// Add the node to the list associated with this entityID
		hashtableList.addElement(pSubscriber);

		trace ("Hashtable entry [" + pEntityID.getSiteID() + ", " + pEntityID.getApplicationID() + ", " +
				pEntityID.getEntityID() + "]");  
		return;
	    }
	    catch (Exception catchAllException)
	    {
			trace ("addListener() exception: " + catchAllException);
		catchAllException.printStackTrace();
		setDEBUG (true);
	    }
}

/**
 *  addListener and removeListener are the two methods that implement the
 *  PduPublisher interface.  Clients unsubscribe from EntityDispatcher via this mechanism.
 *  Note that more than one object (i.e. more than one Script node in the VRML scene) may subscribe using the same entityID.
 *<P>
  *This process is repeated for each subscribing Scipt node in the VRML scene.
 *<P>
 *
 *  removeListener is synchronized to ensure serial access.
 *
 *  If an object has subscribed multiple times, this will remove only
 *  one instance of the subscription. This is probably bad.
 */
public synchronized void removeListener(PduSubscriber listenerToRemove, EntityID pEntityID)
{
  Vector hashtableList;

   hashtableList = (Vector) pduSubscriberScriptNodesHashtable.get(pEntityID);

   // Don't have anything with that key? Punt and return
   if(hashtableList == null)
      return;

   // remove that element from the list. NOTE: this removes ONLY THE FIRST
   // ELEMENT from the list. If the object is present multiple times,
   // the object will still be on the recieving list.

   hashtableList.removeElement(listenerToRemove);

   return;
}

/**
 *  Test for the existence of multicast (or unicast) and, if not
 *  found, open up a tunnel to the tunnel server, and replace the
 *  original BSB with this new one.
 *
 *  The algorithm here is to listen on the default BSB port for
 *  some amount of time; if nothing is received, create a new
 *  BSB that points to the tunnel server, and get packets from
 *  that, instead.
 *
 *  If we do fall back to the tunnel, we need to initiate some
 *  housekeeping with the tunnel server, mostly sending keepalive
 *  packets to it on a regular basis.
 */

public synchronized void checkForTrafficAndFallBackToTunnel(String pTunnelAddress,  // tunnel to connect to
                                          int pTunnelPort,        // port on that machine
                                          int pLocalPort)         // unicast port

{
   try { //  catchAllException to diagnose run-time errors while VRML browser continues

    long	timeoutPeriod = 5000; // Timeout period listening for multicast
    Vector	newPdus;
    String	mcastAddress;
    int		mcastPort;

  // read on the bsb and listen for a while.
  // If we don't hear any traffic, assume that we can't hear multicast

	trace ("commenced checkForTrafficAndFallBackToTunnel...");

	trace ("  now wait " + (float) timeoutPeriod / 1000.0f + " seconds to see if BSB gets any multicast packets...");

  int sleepCount = 0;
  for (int i = 0; i < 5; i++)
  {
    sleepCount ++;
    try
    {
      Thread.sleep(timeoutPeriod / 5);
    }
    catch (InterruptedException ie)
    {
      trace ("fitful sleep: " + ie);
    }
    newPdus = behaviorStreamBuffer.receivedPdus();

    if (newPdus.size() > 0)
    {
      // We got one or more PDUs while listening; apparently we're connected.
      trace ("received " + newPdus.size() + " PDUs on multicast interface");
      trace ("completed checkForTrafficAndFallBackToTunnel ()");
      return;
    }
  }

    trace ("timeoutPeriod " + (float) (sleepCount / 5.0f) * timeoutPeriod / 1000.0f + " seconds completed, nothing heard");
//  behaviorStreamBuffer.shutdown();  // shut down thread listening

    trace("*** tunnelThread option not operational, continuing...");
    return;
/*
    // yes, we have no connection. Create a new BSB that points to the
    // tunnel server, and start listening to it, instead.

    trace ("*** No traffic on local net, attempt connecting to tunnel...");

    InetAddress tunnelAddress;

    try
    {
      tunnelAddress = InetAddress.getByName(pTunnelAddress);
    }
    catch(UnknownHostException unhe)
    {
      trace("*** tunnelThread startup exception:");
      trace("can't find tunnel host: " + unhe);
	if (DEBUG) unhe.printStackTrace();
      trace("this could be a problem connecting to network,");
      trace("  Domain Naming System (DNS) problem,");
      trace("  or unknown host...  returning/continuing");
      return;
    }
    catch (Exception catchAllException)
    {
		trace ("*** tunnelThread startup exception: " + catchAllException);
	if (DEBUG) catchAllException.printStackTrace();
      trace("this could be a problem connecting to network,");
      trace("  Domain Naming System (DNS) problem or unknown host.");
      trace("  Continuing in standalone mode...");
      return;
    }

    mcastAddress = behaviorStreamBuffer.getAddress().getHostAddress();
    mcastPort = behaviorStreamBuffer.getPort();
    
    behaviorStreamBuffer = new BehaviorStreamBuffer();
    int emphemeralPort = behaviorStreamBuffer.getPort();

    // Set up a housekeeping thread that sends periodic keepalive
    // messages to the tunnel server, subscribes us,  etc.
 
		tunnelManager = new EntityDispatcher.TunnelManager(pTunnelAddress, 
                                                        pTunnelPort,
                                                        emphemeralPort,
                                                        mcastAddress, mcastPort);
    	Thread  tunnelThread = new Thread(tunnelManager, "UnicastTunnel-" + ClassUtilities.nextSerialNum());
    	trace ("running tunnelManager tunnelThread");
	tunnelThread.start();
	trace (tunnelThread.getName() + " tunnelThread.start() complete");

        trace ("checkForTrafficAndFallBackToTunnel (...) complete");
*/
    }
    catch (Exception catchAllException)
    {
    	trace ("checkForTrafficAndFallBackToTunnel exception: " + catchAllException);
	catchAllException.printStackTrace();
    }
    return;
}

/**
 * Accessor method.
 */

public synchronized float getSleepInterval ()
{
  return sleepInterval;
}

/**
 * Somewhat careful accessor method that decides
 *  how long to let the thread sleep, so that it doesn't steal
 * excessive cycles from the VRML 3D graphics rendering process(es).
 * This is synchronized to ensure serial access, and make sure multiple
 * instances of clients don't step on themselves.
 */

public synchronized void adviseSleepInterval (float anotherReadWriteInterval)
{
  if ((long) anotherReadWriteInterval < minSleepInterval)
  {
        debug ("new sleepInterval of " + anotherReadWriteInterval +
  		   " is less than minSleepInterval " + minSleepInterval + " (msec), ignored, using "
  		   + sleepInterval);
  	return;
  }

  if (sleepInterval > (long) anotherReadWriteInterval)
      sleepInterval = (long) anotherReadWriteInterval;

  if ((long) anotherReadWriteInterval > maxSleepInterval)
  {
        sleepInterval = maxSleepInterval;
  	debug ("new sleepInterval of " + anotherReadWriteInterval +
  		   " is more than maxSleepInterval " + maxSleepInterval + ", used " + maxSleepInterval);
  	return;
  }

  sleepInterval = (long) anotherReadWriteInterval;

  debug ("sleepInterval = " + sleepInterval);

  return;
}

/**
 * run() switches on platform-secific security, gets the thread running using doThread (),
 * then calls doRun () which does the
 * actual work of reading from the network.
 * <P>
 * This is the implementation of the
 * runnable interface. Each method invocation is two-step process --
 * run calls SecurityStrategy's invokePrivilege(),
 * which in turn calls back to this object -- which is required because some platforms
 * require the security calls to be "above" in the stack. Just switching on
 * the security parameters in strategy, and then returning and attempting to read
 * from a socket, is not enough.
 */

public void run()
{

    try { //  catchAllException to diagnose run-time errors while VRML browser continues

	debug ("run:  strategy.invokePrivilege(this, \"doRun\")");
	strategy.invokePrivilege(this, "doRun");

//	debug ("run:  directly invoke doRun () (no strategy.invokePrivelege)");
//	doRun ();

	debug ("run () done with doRun reading/dispatching loop, all finished");
    }
    catch (Exception catchAllException)
    {
    	trace ("run() exception: " + catchAllException);
	catchAllException.printStackTrace();
	setDEBUG (true);
    }
}

/**
 *  After the behaviorStreamBuffer thread is started, doRun loops endlessly to pull unordered lists
 *  of PDUs from the behaviorStreamBuffer, sending the PDUs one-by-one to the
 *  appropriate EspduTransform matching the entity.
 *  <p>
 *  Called by run(); this is the essential portion of the run loop.
 *  run() has enabled platform-specific security at this point,
 *  so doRun doesn't have to worry about network sandboxes.
 */
 
public void doRun () 
{
   try { //  catchAllException to diagnose run-time errors while VRML browser continues

	if (runStarted)
	{
		debug ("*** doRun() invoked when runStarted is already true, ignored");
		return;
	}

	debug ("started doRun (), entering doRun loop");

	runStarted = true;

	while (keepRunThreadAlive)
	{
		singleReadLoop ();

		if (traceCounter % 20 == 0) debug ("[20th loop] sleep " + sleepInterval);
			
		try
		{
			Thread.sleep(sleepInterval);  // go to sleep to avoid using too many draw cycles
		}
		catch(InterruptedException interruptedException)
		{
			throw new RuntimeException(" exceptional sleep: " + interruptedException);
		}
  	}
    }
    catch (Exception catchAllException)
    {
    	trace ("doRun() exception: " + catchAllException);
	catchAllException.printStackTrace();
	setDEBUG (true);
    }
} // end of method


/**
 * singleRunLoop() is called via doRun when threading, or via the invoking application if not threading
 * (e.g. due to IE security restrictions).
 */

public void singleReadLoop ()
{
	Vector			newPdus;
	Enumeration		enumeration;
	ProtocolDataUnit	pdu;
 	Vector			subscribers = null;
 	EntityID		eid_1 = new EntityID ();
	EntityID		eid_2 = new EntityID ();
	boolean			subscriberFound = false;

	debug ("started singleReadLoop");

	try { //  catchAllException to diagnose run-time errors while VRML browser continues

		newPdus = behaviorStreamBuffer.receivedPdus();

		if (traceCounter % 20 == 0)
			debug ("[20th cycle] got " + newPdus.size() + " PDUs this cycle");
		else if (newPdus.size() > 0)
			debug ("got " + newPdus.size() + " PDUs this cycle");
		traceCounter++;

		enumeration = newPdus.elements();

//	debug ("while (enumeration.hasMoreElements()=" + enumeration.hasMoreElements() + ")");

		while (enumeration.hasMoreElements() )
		{
			pdu = (ProtocolDataUnit) enumeration.nextElement();

			debug (pdu.pduName () + " received (type " + (pdu.getPduType()).intValue() + ")");
				
			switch ((pdu.getPduType()).intValue())
			{
			  case  PduTypeField.ENTITYSTATE:
				eid_1 = ((EntityStatePdu)pdu).getEntityID();
				eid_2 = null;
	       			debug ("entityStatePdu.getEntityID: " + eid_1);
	       			break;

			  case  PduTypeField.FIRE:
				eid_1 = ((FirePdu)pdu).getFiringEntityID ();
				eid_2 = ((FirePdu)pdu).getTargetEntityID ();  // perhaps unknown to shooter
	       			debug (	"firePdu.getFiringEntityID: " + eid_1 + ", " +
	       				"firePdu.getTargetEntityID: " + eid_2);
	       			break;

			  case  PduTypeField.DETONATION:
				eid_1 = ((DetonationPdu)pdu).getFiringEntityID();
				eid_2 = ((DetonationPdu)pdu).getTargetEntityID();
	       			trace ("detonationPdu.getFiringEntityID: " + eid_1);
	       			trace ("detonationPdu.getTargetEntityID: " + eid_2);
	       			break;

			  case  PduTypeField.COLLISION:
				eid_1 = ((CollisionPdu)pdu).getIssuingEntityID ();
	       			eid_2 = ((CollisionPdu)pdu).getCollidingEntityID (); // perhaps unknown to sender
				debug (	"collisionPdu.getIssuingEntityID:   " + eid_1 + ", " +
	       				"collisionPdu.getCollidingEntityID: " + eid_2);
	       			break;
        			
			  case  PduTypeField.COMMENT:
				eid_1 = ((CommentPdu)pdu).getOriginatingEntityID ();
				eid_2 = null;
	       			trace ("commentPdu.getEntityID: " + eid_1);
	       			break;

			  case  PduTypeField.CREATEENTITY:
				eid_1 = ((CreateEntityPdu)pdu).getOriginatingEntityID ();
				eid_2 = null;
	       			trace ("createEntityPdu.getEntityID: " + eid_1);
	       			break;

			  case  PduTypeField.REMOVEENTITY:
				eid_1 = ((RemoveEntityPdu)pdu).getOriginatingEntityID ();
				eid_2 = null;
	       			trace ("removeEntityPdu.getEntityID: " + eid_1);
	       			break;

			  // RadioCommunicationsFamily (change these trace calls to debug once tested...)

	       		  case  PduTypeField.SIGNAL:
				eid_1 = ((SignalPdu)pdu).getEntityID ();
				debug ("signalPdu.getEntityID: " + eid_1 +   ", .getRadioID: " +
					((SignalPdu)pdu).getRadioID ());
	       			break;
				
	       		  case  PduTypeField.RECEIVER:
				eid_1 = ((ReceiverPdu)pdu).getEntityID ();
				debug ("receiverPdu.getEntityID: " + eid_1 + ", .getRadioID: " +
				                              ((ReceiverPdu)pdu).getRadioID ());
	       			break;
				
	       		  case  PduTypeField.TRANSMITTER:
				eid_1 = ((TransmitterPdu)pdu).getEntityID ();
				debug ("transmitterPdu.getEntityID: " + eid_1 + ", .getRadioID: " +
					                      ((TransmitterPdu)pdu).getRadioID ());
	       			break;
				
			  // Some other type of PDU.

			  default:
			  	trace ("don't know how to handle PDU of type " + pdu.getPduType().intValue()
				         + " = " + PduTypeField.toString (pdu.getPduType().intValue()));
	       			break;

	       		} // End of switch by PDU type

			// Find the list of subscribed objects for this entityID. We get back a Vector
			// from this operation, and forward the PDU to all of the objects in the list.
			
			// To prevent duplicate rendering, and because secondary entities might be unknown:
			//    Collision PDUs only go to the entity detecting and announcing the collision
			//	with itself, not to the entity being collided with.
			//    Fire PDUs only go to the firing entities.

// combine ReceiverPduScriptNode.java, SignalPduScriptNode.java, TransmitterPduScriptNode.java (similar to EspduTransform.java)
// then use those DIS -> script node values to draw BeamCones etc. in the correspondingly named PROTOs:
// Transmitting entities will be the scene graph nodes that do the actual drawing, to avoid double-drawing of visualization geometry
// don't forget to update javadoc words and diagram at the top of this class

			subscribers = (Vector) (pduSubscriberScriptNodesHashtable.get(eid_1));

			if (subscribers == null)
			{
				trace ("didn't find any PDU subscribers in VRML world matching received " +
					pdu.pduName () + " with EntityID" +
					" site=" + eid_1.getSiteID().intValue() +
					" application=" + eid_1.getApplicationID().intValue() +
					" entityID=" + eid_1.getEntityID().intValue());
				// eventually add code here to add newly discovered entity to the VRML scene
			}
			else  // dispatch pdu to the entity/entities subscribed
			{
				debug ("found " + subscribers.size() + " subscriber node(s) for EntityID" +
					" site=" + eid_1.getSiteID().intValue() +
				 	" application=" + eid_1.getApplicationID().intValue() +
			 		" entityID=" + eid_1.getEntityID().intValue() + ", dispatching");
			 	
				// loop through list, sending pdu to subscribers, by invoking receivePDU method
			 	// of subscribed Script node (for example EspduTransform)
			 		
	        		for (int idx = 0; idx < subscribers.size(); idx++)
				{
				   ((PduSubscriber)(subscribers.elementAt(idx))).receivePDU(pdu);
				   subscriberFound = true;
				}
			}
		}   // end of while enumeration, all PDUs in this bunch have been handled
	}
	catch (Exception catchAllException)
	{
		trace ("singleReadLoop() exception: " + catchAllException);
		catchAllException.printStackTrace();
		setDEBUG (true);
	}
	debug ("finished singleReadLoop");
}

/**
 * shutdown() is called when VRML scene exits.
 */

public void shutdown () {

	debug ("commencing shutdown ()...");
	try { //  catchAllException to diagnose run-time errors while browser continues
		if (behaviorStreamBuffer != null)	behaviorStreamBuffer.cleanup ();
		if (tunnelManager != null)		tunnelManager.stopSendingServerKeepAlives ();
	}
	catch (Exception catchAllException)
	{
		trace ("entityDispatcher.shutdown () exception: " + catchAllException);
	}
	keepRunThreadAlive = false;  // shut down self last since BSB pushes up PDUs

	trace ("completed shutdown (), set keepRunThreadAlive = false");
}

/////////////////////////////////////////////////////////////////////////////////////////////////

protected class TunnelManager implements Runnable
{
  DatagramStreamBuffer  tunnelComm = null;
  String                tunnelAddressString = null;
  int                   tunnelPort;
  boolean               stillSending = true;
  String                mcastAddress;
  int                   mcastPort;
  int                   localPort;

/** Constructor creates a new DatagramStreamBuffer.  Currently disabled, awaiting repairs.
 */
 
  TunnelManager(String pTunnelAddress, int pTunnelPort, int pLocalPort,
                String pMcastAddress, int pMcastPort)
  {
    trace ("TunnelManager constructor starting");
    tunnelAddressString = pTunnelAddress;
    tunnelPort = pTunnelPort;
    localPort = pLocalPort;
    mcastAddress = pMcastAddress;
    mcastPort = pMcastPort;

    tunnelComm = new DatagramStreamBufferNetscape(8080);
    trace ("TunnelManager constructor complete");
  }

/**
  Debugging output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected void debug (String pDiagnostic)
{
	if (DEBUG)
	{
  		System.out.println("EntityDispatcher.TunnelManager: " + pDiagnostic);
  		System.out.flush (); // ensure threaded output goes to console promptly
	}
}

/**
  Guaranteed trace output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected void trace (String pDiagnostic)
{
	System.out.println("EntityDispatcher.TunnelManager: " + pDiagnostic);
  	System.out.flush (); // ensure threaded output goes to console promptly
}

/** Run as a thread to relay PDUs via a unicast-multicast tunnel.
 */
public void run()
{

    InetAddress       tunnelAddress;
    DatagramPacket    packet;
    byte              buff[] = new byte[1500];
    String            connect = "connect";          // String for initial connection
    String            keepAlive = "keepAlive ";      // command for keepalive packets
    String            disconnect = "disconnect ";    // disconnect command  

    // Send a connect command, then every once in a while send a keepalive
    // packet.
    
    trace ("run () starting");
    try
    {
      String fullConnectCommand;
      InetAddress localAddress;

      trace ("getting address for tunnel, " + tunnelAddressString);
      tunnelAddress = InetAddress.getByName(tunnelAddressString);
      trace ("got address for tunnel");

      localAddress = InetAddress.getLocalHost();
      fullConnectCommand = connect.concat(" ");
      fullConnectCommand = fullConnectCommand.concat(localAddress.getHostAddress());
      fullConnectCommand = fullConnectCommand.concat(" ");
      fullConnectCommand = fullConnectCommand.concat(String.valueOf(localPort));
      fullConnectCommand = fullConnectCommand.concat(" ");
      fullConnectCommand = fullConnectCommand.concat(mcastAddress);
      fullConnectCommand = fullConnectCommand.concat(" ");
      fullConnectCommand = fullConnectCommand.concat(String.valueOf(mcastPort));
      // This last bit is REQUIRED; if left out, the parse on the server side
      // can mistakenly read binary data carried along with the data packet,
      // which will result in a bad parse.
      fullConnectCommand = fullConnectCommand.concat(" ");

      trace ("connect command = " + fullConnectCommand);
      buff = fullConnectCommand.getBytes();
      trace ("length = " + buff.length);
      packet = new DatagramPacket(buff, buff.length);
      packet.setPort(tunnelPort);
      packet.setAddress(tunnelAddress);
      //trace ("packet = " + packet);

      // Anything I say three times is true; send some connect packets at
      // 1 second intervals. One of 'em ought to get through.

      trace ("Sending connect packets");
      tunnelComm.sendDatagram(packet); 
      trace ("that's one"); Thread.sleep(1000);
      tunnelComm.sendDatagram(packet); Thread.sleep(1000);
      tunnelComm.sendDatagram(packet);
      trace ("sent connect packets");

      // we are, presumably, connected; should probably do some handshaking
      // here. Anyway, send keepalive packets every now and then.

      buff = keepAlive.getBytes();
      packet = new DatagramPacket(buff, buff.length);
      packet.setPort(tunnelPort);
      packet.setAddress(tunnelAddress);

      while(stillSending)
      {
         tunnelComm.sendDatagram(packet);
         trace ("sent keep-alive packet");

         // sleep 60 sec, do it again

         Thread.sleep(60000);
      }

      // send a disconnect packet

      buff = disconnect.getBytes();
      packet = new DatagramPacket(buff, buff.length);
      packet.setPort(tunnelPort);
      packet.setAddress(tunnelAddress);
      
      trace ("Sending tunnel disconnect packets");
      tunnelComm.sendDatagram(packet); Thread.sleep(1000);
      tunnelComm.sendDatagram(packet); Thread.sleep(1000);
      tunnelComm.sendDatagram(packet);
      trace ("tunnel disconnect packets sent");
      
    } // end of try
    catch(InterruptedException ie)
    {
      trace ("Troubled Sleep, and no, not the novel by Sartre");
    }
    catch(UnknownHostException unkhe)
    {
      trace ("Unknown host exception " + unkhe);
    }
    catch(IOException ioe)
    {
      trace ("IO exception " + ioe);
    }
    catch(Exception catchAllException)
    {
	trace ("catchAllException " + catchAllException);
	catchAllException.printStackTrace();
    }
    
} // end of run

/**
 * gracefully terminate tunnelThread
 */
  protected void stopSendingServerKeepAlives ()
  {
  	stillSending = false;
  }
  
} // end of inner class

} // end of class
      
