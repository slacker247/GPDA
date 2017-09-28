
package mil.navy.nps.dis;		// the package we belong to

import mil.navy.nps.disEnumerations.*;	// Enumerations for DIS

import org.web3d.vrtp.net.*;		  // network utilities, especially privileges & DatagramStreamBuffer
import org.web3d.vrtp.security.*; // x-platform security

import java.net.*;
import java.util.*;
import java.io.*;

/**
 * BehaviorStreamBufferUDP is responsible for reading from the network, specifically
 * from packet-oriented (UDP and multicast) sockets. it is a subclass of the
 * abstract class BehaviorStreamBuffer. <p>
 * 
 * In general, this should be run via a thread--create a thread with an instance of
 * this class in it, and run the thread. This will prevent blocking IO on the socket
 * from stopping the application.
 *
 * @author DMcG
 */

public class BehaviorStreamBufferUDP extends BehaviorStreamBuffer implements Runnable, AllPermissionsBadge
{
  protected org.web3d.vrtp.net.DatagramStreamBuffer datagramStreamBuffer;      // holds networking code

  /**
   * this is used in conjuction with getNextPdu
   */

  private Vector cachedDatagrams = null;


/**
 * A constructor that creates a new unicast socket. this creates an internal
 * object responsible for reading from the socket, in this case a UDP unicast socket.
 * It also starts a thread that reads from the socket.
 *
 * @param pDatagramPort the port we read from on this side
 */

public BehaviorStreamBufferUDP(int pDatagramPort)            // INPUT: port we read from
{	
	datagramStreamBuffer = new org.web3d.vrtp.net.DatagramStreamBuffer(pDatagramPort);
	datagramStreamBuffer.setDEBUG (DEBUG);

	// Create a new info object
	info = new BehaviorStreamBufferInfo();

	this.setRtpEnabled(info.getRtpEnabled());

	if(inputThreadStarted == false)
  {
    this.startInputThreadWithSecurity();
  }

	debug ("finished constructor unicast port BehaviorStreamBufferUDP");

	return;
}

/**
 * A constructor that creates a new unicast socket datagramStreamBuffer on an
 * ephemeral (system-chosen) port.<p>
 *
 * If the Netscape security classes are visible, this instantiates itself
 * with an underlying Netscape-specific DatagramStreamBuffer. Otherwise,
 * it uses an generic, security-free datagramStreamBuffer. This also starts
 * a thread responsible for reading from the socket.
 */

public BehaviorStreamBufferUDP()
{
	datagramStreamBuffer = new org.web3d.vrtp.net.DatagramStreamBuffer();
	datagramStreamBuffer.setDEBUG (DEBUG);

	// Create a new info object
	info = new BehaviorStreamBufferInfo();

	this.setRtpEnabled(info.getRtpEnabled());

	if(inputThreadStarted == false)
  {
    this.startInputThreadWithSecurity();
  }

	debug ("finished constructor unicast ephemeral BehaviorStreamBufferUDP");

	return;
 }
/**
 *   A constructor that creates a multicast socket datagramStreamBuffer
 * 
 *   If the netscape security classes are visible, this instantiates
 *   itself as a netscape-specific datagramStreamBuffer, able to
 *   bypass the Java sandbox. Otherwise, a generic DSB is used
 *   that has no security calls.<p>
 *
 *   Note that this creates a thread to read from the socket.
 */

public BehaviorStreamBufferUDP(String pMulticastAddress, int pDatagramPort)            // INPUT: multicast Address, port we read from
{
	// NOTE: only use with browsers that support java 1.1. Multicast

	debug ("starting constructor multicast BehaviorStreamBufferNetwork");

	datagramStreamBuffer = new org.web3d.vrtp.net.DatagramStreamBuffer(pMulticastAddress, pDatagramPort);
	datagramStreamBuffer.setDEBUG (DEBUG);

	// Create a new info object
	info = new BehaviorStreamBufferInfo();

	this.setRtpEnabled(info.getRtpEnabled());

	if(inputThreadStarted == false)
  {
    this.startInputThreadWithSecurity();
  }

	debug ("finished multicast BehaviorStreamBufferUDP constructor");

	return;
}

/**
 * Simple method to launch thread because Microsoft puts security restraints on everything.
 * <P>
 * EntityDispatcher is static; only one copy is shared between all instances
 * of the EspduTransform and other Script nodes.  If it's null the first time through here, create
 * and thread it.
 * <P>
 * This is synchronized, since we may have N EspduTransforms attempting
 * to instantiate themselves at once, all accessing this shared class
 * variable. The synchronization prevents multiple access.
 * <P>
 * This is actually some magic code. Java keeps several mutex locks around,
 * one for every instance of a class and one for the class itself. We
 * can't sychronize on entityDispatcher because that's null the first
 * time through, which will fail silently. We also can't use a java primitive
 * type. So I created a completely arbitrary instance of an object as
 * a class variable, and synchronize on that. It's a standin for the
 * EntityDispatcher in a way.  When done, it starts reading from the input socket.
 */

public synchronized void startInputThread()
{
   if (inputThreadStarted)
   {
   		trace ("startInputThread() invoked again, ignored...");
   		return;
   }
   
	trace ("startInputThread() method started...");

 	try
	{
		// Thread used to run the entityDispatcher

		Thread datagramStreamBufferThread = new Thread(datagramStreamBuffer);//, "DebuggingDatagramStreamBuffer-" + ClassUtilities.nextSerialNum()
		datagramStreamBufferThread.start();
		inputThreadStarted = true;
	}
	catch (Exception catchAllException)
	{
		trace ("startInputThread: exception\n" + catchAllException);
		trace ("startInputThread: attempting to continue without datagramStreamBuffer threaded...");
	}
	return;
}

/**
 * Creates a BehaviorStreamBufferNetwork with the specified DatagramStream. Does not start
 * running a thread. 
 */
public BehaviorStreamBufferUDP(org.web3d.vrtp.net.DatagramStreamBuffer pDatagramStreamBuffer)
{
  datagramStreamBuffer = pDatagramStreamBuffer;
}

/**
 *  Terminate the run loop and shutdown the thread.
 */
public void shutdown ()
{
  trace ("shutdown ();");
  runContinue = false;
  this.suspendReading();
}

/**
 *  Accessor method to provide current multicast address.
 */
public InetAddress getAddress()
{
  return datagramStreamBuffer.getMulticastAddress();
}

/**
 *  Accessor method to provide current multicast port.
 */
public int getPort()
{
  return datagramStreamBuffer.getDatagramPort();
}

/**
 *  Threading method to read/write via datagramStreamBuffer until shutdown.
 */
public void run()
{
  trace ("commencing datagramStreamBuffer.run ()...");

  readThreadRunning = true;

  datagramStreamBuffer.run();

  trace ("datagramStreamBuffer.run() invocation returned");

  while (runContinue == true) /* spin until told to stop running */
  {
/*
*** commented out because JIT errors result!  apparently not needed anyway.

  	try
  	{
  		while(datagramStreamBuffer.getReadingActive() == true) wait();
  	}
  	catch(InterruptedException ie)
  	{
		trace ("Interrupted exception on wait; problems with suspendReading and co");
	throw new RuntimeException("Problems with suspend/read on DatagramStreamBuffer");
	}
*/
  }
  trace ("completed run () wait loop");
}

/**
Returns a vector of all the PDUs received since the last time we
asked. Queries the underlying DatagramStreamBuffer for this information.
*/

public Vector receivedPdus()
{  
  Vector      newDatagrams,     // raw datagrams we get from the datagramStreamBuffer
              pduVector;        // the above, "promoted" to PDU objects
  Enumeration datagramEnumeration;

   this.checkForThreadStart();

  // get datagrams from the underlying datagramStreamBuffer, which is synchronized with itself
  
  //  newDatagrams = datagramStreamBuffer.receivedDatagrams(); // renamed
  newDatagrams = datagramStreamBuffer.getPackets();
  pduVector = new Vector();

  // Step through the vector, translating each datagram into a PDU of whatever type.

  datagramEnumeration = newDatagrams.elements();
  while(datagramEnumeration.hasMoreElements())
  {
    byte                datagramData[] = (byte[])datagramEnumeration.nextElement();
    ProtocolDataUnit    aPdu;

    aPdu = ProtocolDataUnit.byteArrayToPdu(datagramData);
    if(aPdu != null)
    {
        // Save in list
    	pduVector.addElement(aPdu);
    	debug ("pduVector.addElement(aPdu)");
    }
  }

  return pduVector;
} // end of receivedPdus

public ProtocolDataUnit getNextPdu()
{
  byte              byteData[];
  ProtocolDataUnit  aPdu;

   this.checkForThreadStart();

  // the first time, we ask for the full list of PDUs we've received. We
  // take the first pdu from the list until the list is exhausted; then
  // we get another list.

  if(cachedDatagrams == null)
    cachedDatagrams = this.receivedPdus();

  // No packets received from net
  if(cachedDatagrams.size() == 0)
  {
    cachedDatagrams = null;
    return null;
  }

  // first element in list
  byteData = (byte[])cachedDatagrams.remove(0);

  // List now empty? null out cachedDatagrams so we'll get a new list next time
  if(cachedDatagrams.size() == 0)
    cachedDatagrams = null;

  // Convert the datagram to a PDU object
  aPdu = ProtocolDataUnit.byteArrayToPdu(byteData);

  return aPdu;
}

/**
 * JDK 1.2 version:  set the multicast socket time-to-live, with 15=> local distribution, 63=>regional
 * distribution, 127=>global distribution.
 * <P>
 * Note that browsers are still using JDK 1.1 while command-line applications can use JDK 1.2.
 * <P>
 * see java.net.MulticastSocket.setTTL()
 */

public void setTimeToLive(int pTTL)
{
  datagramStreamBuffer.setTimeToLive(pTTL);
}


/**
 * (Deprecated) JDK 1.1 version:  set the multicast socket time-to-live, with 15=> local distribution, 63=>regional
 * distribution, 127=>global distribution. It seems that there is a bug under NS
 * 4.04 (surprise, surprise) so that setTTL on the mcast socket throws an exception,
 * so you probably shouldn't use this yet.
 * 
 *   JDK 1.2 deprecated the setTTL method on the MulticastSocket class
 *   and replaced it with the setTimeToLive method, which takes an int
 *   instead of a byte. If you have problems compiling under 1.1
 *   this is probably the problem.
 * <P>
 * see java.net.MulticastSocket.setTimeToLive()
 */
  
public void setTTL(byte pTTL)
{
  datagramStreamBuffer.setTTL(pTTL);
}


/**
 * Sends a PDU. If the underlying destination address has already
 * been set, for example in a multicast or file, we don't need
 * to supply an address.
 */

public void sendPdu(ProtocolDataUnit pPdu)
{
  // Check to make sure we're sending to a multicast stream, or
  // that we already have an address to send to

  if(datagramStreamBuffer.getUsingMulticast())
    this.sendPduMulticast(pPdu);

  return;
}


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
 * @param pAddress2 the second part of the address, typically null or the destiation port
 */

public void sendPdu(ProtocolDataUnit pPdu,
                             Object pAddress1,
                             Object pAddress2)
{
  String      netAddress;
  int         portNumber;

  // Make sure our arguments are i the right format
  // pAddress1 should be an inetAddress in string format, pAddress2 an integer object

  if(pAddress1 instanceof String)
  {
    netAddress = (String)pAddress1;
  }
  else
  {
    trace("address to send PDU to is not an InetAddress");
    return;
  }

  // Check port number
  if(pAddress2 instanceof Integer)
  {
    portNumber = ((Integer)pAddress2).intValue();
  }
  else
  {
    trace("port number to sendPDU to is not an Integer object");
    return;
  }

  // let an existing method handle sending it
  this.sendPdu(pPdu, netAddress, portNumber);

}


public synchronized void sendPduMulticast (ProtocolDataUnit pPdu)
{
  if (datagramStreamBuffer.getUsingMulticast())
  {
    // We assume the destination port on the machine we're sending to is the same
    // as our sending port.
    this.sendPdu (pPdu,                                                           // pdu
                  datagramStreamBuffer.getMulticastAddress().getHostAddress(),    // destination mcast addr
                  datagramStreamBuffer.getDatagramPort());                        // destination port 
  }
}

public synchronized void sendPdu(ProtocolDataUnit pdu,
                                 String pDestinationHost,
                                 int    pDestinationPort)
{
	ByteArrayOutputStream oStream = null;
	DataOutputStream      dataOutputStream = null;
	DatagramPacket        dgramPacket = null;
	InetAddress           destHostAddress = null;

	debug ("in sendPdu, address " + pDestinationHost + " port " + pDestinationPort);

	try
	 {
	 	destHostAddress = InetAddress.getByName(pDestinationHost);
	 }
	catch(UnknownHostException e)
	 {
	 	System.out.println("cannot resolve address " + pDestinationHost + ", packet not sent");
	 }

	oStream = new ByteArrayOutputStream();
	dataOutputStream = new DataOutputStream(oStream);

	debug ("Preparing to serialize a PDU");

	pdu.serialize(dataOutputStream);

	if(datagramStreamBuffer.getUsingMulticast())
	  dgramPacket = new DatagramPacket(oStream.toByteArray(),
	                                 oStream.size(),
	                                 destHostAddress,
	                                 pDestinationPort);
	else
	  dgramPacket = new DatagramPacket(oStream.toByteArray(),
	                                 oStream.size(),
	                                 destHostAddress,
	                                 pDestinationPort);

	debug ("created datagram packet for sending");

	datagramStreamBuffer.sendDatagram(dgramPacket);

	return;
}

public synchronized void sendDatagram(InetAddress pDestHost, int pPort, String pCommand)
{

  DatagramPacket dgramPacket;
  byte           buf[];

  System.out.println("Sending packet unicast");

  buf = pCommand.getBytes();
  dgramPacket = new DatagramPacket(buf, buf.length, pDestHost, pPort);
  
  datagramStreamBuffer.sendDatagram(dgramPacket);
}

/**
 *   suspend reading in the DatagramStreamBuffer
 */
public synchronized void suspendReading()
{
  datagramStreamBuffer.setReadingActive(false);
}

/**
 * Start reading packets from the datagramStreamBuffer again.
 */

public void resumeReading()
{
  synchronized(this)        // Lets us take ownership of the object monitor
  {
    if(datagramStreamBuffer.getReadingActive() == false)
    {
      try
      {
        notify();       // Wakes up thread in run(), which is executing a wait()
      }
      catch(IllegalMonitorStateException badMonitorNoDonut)
      {
        System.out.println("This thread is not the owner of the monitor; can't resume");
        throw new 
          RuntimeException("Exception in DatagramStreamBuffer; don't own monitor");
    } // catch
    } // if thread suspended
  }   // synchronized

  datagramStreamBuffer.run();
}


/**
 * Closes down sockets nicely
 */

public synchronized void cleanup()
{
  datagramStreamBuffer.cleanup();
}

/**
 *  Finalize method--used to clean up any sockets that are still open
 */

protected synchronized void finalize() throws Throwable
{
  cleanup();
}

} // end of class BehaviorStreamBufferNetwork

