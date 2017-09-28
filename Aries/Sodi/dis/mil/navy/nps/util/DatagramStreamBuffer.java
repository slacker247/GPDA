/**
  
    DatagramStreamBuffer

  This is a reusable class for reading datagrams from a socket, with
  subclass(es) for each unique java security model.

  The DatagramStreamBuffer reads and stores datagrams (NOT PDUs).
  Since this is a fairly generic operation, this should be a very
  reusable class. It implements the runnable interface, and generally
  runs in its own thread, reading data from the socket as rapidly as
  datagrams come in. When no datagrams are arriving, the thread
  blocks on the receive() method of the socket class, consuming
  no CPU resources.

  BehaviorStreamBuffer and others can act as an adaptor to DSB,
  utilizing this object's generic capability to provide PDUs
  to higher level objects.

  The top level class DatagramStreamBuffer, implements a generic,
  security-model-free datagram reading routine. Subclasses can
  implement a security-model-specific class, such as DatagramStreamBufferNetscape.
  These subclasses generally only need to make specific browser security
  calls, then call the DatagramStreamBuffer to do the actual work.

  AUTHOR: DMcG

  HISTORY: 11/20/98 New
            8/16/99 setTTL exception handling

  */

package mil.navy.nps.util;

import java.net.*;
import java.util.*;
import java.io.*;

public class DatagramStreamBuffer extends Object implements Runnable
{
    // constants

    private static final int	MAX_DATAGRAM_SIZE = 1500;   // MTU

    // ivars relating to the socket we're reading from

    boolean          usingMulticast;         // YES=> using the multicast socket NO=> using unicast socket

   int              datagramPort;           // what port we read from (used for both unicast & multicast)
   DatagramSocket   datagramSocket  = null; // the socket we read from (configured w/ datagram port)
   MulticastSocket  multicastSocket = null; // socket we read from, if we're using multicast (configured w/ datagram port & multicast address)

   InetAddress      multicastAddress;         // multicast address, doesn't work on all versions of java
   int              ttl = 15;                 // multicast time-to-live; 15=site, 63=regional, 127=global

   // buffer we hold datagrams in until the DIS application asks for them

   Vector           datagramBuffer;         // holding pen for incoming datagrams

   boolean          readingActive = true;   // exit read loop in run


/**
 * When DEBUG is true, System.out.println text messages trace the internals of script operation.
 * Text output appears in the Java Console (CosmoPlayer browser) or in the VRML console (WorldView browser)
 */
 
	private static boolean DEBUG		= false;  // diagnostic messages on/off

public static boolean getDEBUG ()
{
	debug ("getDEBUG " + DEBUG);

	return DEBUG;
}

public static void setDEBUG (boolean pDEBUG)
{
	DEBUG = pDEBUG;

	trace ("setDEBUG " + pDEBUG);
}

/**
  Debugging output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected static void debug (String pDiagnostic)
{
  if (DEBUG) System.out.println("DatagramStreamBuffer: " + pDiagnostic);
}

/**
  Guaranteed debugging output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected static void trace (String pDiagnostic)
{
  System.out.println("mil.navy.nps.util.DatagramStreamBuffer: " + pDiagnostic);
}

public InetAddress getMulticastAddress()
{return multicastAddress;
}

public int getDatagramPort()
{ 
  if(usingMulticast)
    return multicastSocket.getLocalPort();
  else
    return datagramSocket.getLocalPort();
}

public boolean getUsingMulticast()
{ return usingMulticast;
}

public DatagramStreamBuffer(int pDatagramPort)            // INPUT: port we read from
 {

 	debug ("*** creating new instance (this should only appear once..)");
 	  
    // Create the unicast datagram socket we read from

    usingMulticast = false;

    try
    {
    	debug ("try unicast new DatagramSocket(pDatagramPort)...");
      datagramSocket = new DatagramSocket(pDatagramPort);
    }
    catch (SocketException socketException)
    {
    	trace ("caught unicast socketException: " + socketException + "/" + socketException.getMessage ());
      throw new 
          RuntimeException("Exception in DatagramStreamBuffer; unable to create socket in constructor");
    }
    
    datagramPort = pDatagramPort;           // not really needed, but useful info to have around, I suspect
    datagramBuffer = new Vector();

    // The thread is created and run in the startReading method

	debug ("finished unicast DatagramStreamBuffer constructor");
	  
    return;
 }

/**
Create a new unicast DSB on an ephemeral port, one picked by the system.
*/
public DatagramStreamBuffer() 
 {

 	debug ("*** creating new instance (this should only appear once..)");
 	  
    // Create the unicast datagram socket we read from

    usingMulticast = false;

    try
    {
      debug ("try unicast new DatagramSocket()...");
      datagramSocket = new DatagramSocket();
    }
    catch (SocketException socketException)
    {
    	trace ("caught unicast socketException: " + socketException + "/" + socketException.getMessage ());
      throw new 
          RuntimeException("Exception in DatagramStreamBuffer; unable to create socket in constructor");
    }
    
    datagramPort = 0;           // not really needed, but useful info to have around, I suspect
    datagramBuffer = new Vector();

    // The thread is created and run in the startReading method

    debug ("finished unicast DatagramStreamBuffer constructor");
	  
    return;
 }

/**
multicast constructor.

  This has one ugly wart. We attempt a join in this class on the specified
  multicast group. But this requires a way to bypass the sandbox, if a 
  sandbox is active.

  The subclass should handle the security calls. But one of the requirements
  of Java constructors is that the superclass constructor has to be called
  before any code in the subclass constructor--which doesn't give us a
  chance to do any security calls before the join() operation. We get
  around this by catching the failed join exception and ignoring it.
  Then, in the subclass, we attempt another join after we've done the
  security calls. That should work, but it leaves some ugly debugging 
  messages on the console.
  */

public DatagramStreamBuffer(String pMulticastAddress, int pDatagramPort)            // INPUT: multicast Address, port we read from
{
	// NOTE: this should NEVER be called until browsers support java 1.1. Multicast
	// support just isn't there.

    debug ("in multicast constructor, address " + pMulticastAddress + " port " + pDatagramPort);

    usingMulticast = true;

    try
    {
    	debug ("try new MulticastSocket(pDatagramPort, )...");
    	multicastSocket = new MulticastSocket(pDatagramPort);
    }
    catch (Exception socketException)
     {
       trace ("caught exception/unable to create multicastSocket "
           + socketException + socketException.getMessage ());

      throw new 
          RuntimeException("Exception in DatagramStreamBuffer; unable to create multicastsocket in constructor");
     }
    debug ("created multicast socket");
     
    try
    {
        debug ("trying to join Address " + pMulticastAddress);
        multicastAddress = InetAddress.getByName(pMulticastAddress);
        multicastSocket.joinGroup(multicastAddress);
        trace ("joined multicast address " + pMulticastAddress);
    }
    catch (Exception socketException)
     {
        trace ("unable to join multicast address " + pMulticastAddress +
           ", likely due to security exception");
        trace("this may be picked up by a join attempt in a subclass...");
     }
 
    datagramPort = pDatagramPort;           // not really needed, but useful info to have around, I suspect


    datagramBuffer = new Vector();
    ttl = 15;

	debug ("finished multicast DatagramStreamBuffer constructor");
	  
    return;
}

/**
When this is set to false, the run() loop will terminate.
*/
public void setReadingActive(boolean pReadingActive)
{readingActive = pReadingActive;
}

/**
Returns the run status
*/
public boolean getReadingActive()
{ return readingActive;
}

/**
Implements the runnable interface, so this class can optionally be used
in threads.

  This loops, reading packets and adding them to the list of recieved
  packets, until readingActive is set to false. In the course of events
  the receivedDatagrams() method will usually be called, which grabs
  the vector of recieved datagrams and replaces it with a new, empty
  vector.
  */

public void run()
{
    // loops . 

    DatagramPacket  datagramPacket;
    byte            dataBuffer[]; 
   
    while(readingActive == true)
     {
       // create a new datagram object, then read it from the wire.

      dataBuffer      = new byte[MAX_DATAGRAM_SIZE];
      datagramPacket  = new DatagramPacket(dataBuffer, dataBuffer.length);

      // read from the correct socket, either unicast or multicast.
      // Note that receive() is a blocking call; if there are no packets
      // present, the thread will stop and consume no cycles. Once a
      // packet arrives on the socket, the thread starts up again and
      // processes the packet. If NO packets arrive, the code will
      // simply sit at the receive() call. There is no timeout set.
            
      // UGLY NETSCAPE BUG ALERT: In netscape 4.02-4.04, at least, there is
      // an error when reading from datagram (UDP) sockets. An exception will
      // be thrown when a packet actually arrives. the workaround is to read
      // from the socket _again_, in the catch statement.

      try
       {
        if(usingMulticast)
          multicastSocket.receive(datagramPacket);
        else
          datagramSocket.receive(datagramPacket); 


       } // end of try
      catch (IOException ioError)
      {
        try
        {
          if(usingMulticast)
            multicastSocket.receive(datagramPacket);
          else
            datagramSocket.receive(datagramPacket);
        }  // end of second-level try
       catch(IOException io2error)
        {
          throw new 
            RuntimeException("Exception in DatagramStreamBuffer. Error reading from datagram socket.");
        } // end of second-level catch
          // A null pointer exception is thrown after we do a close on the sockets, then
          // start reading again. I think this is caused by the sockets being closed down
          // while waiting for a receive(). In any event it seems to be harmless.
        catch(java.lang.NullPointerException npe)
        {
          debug("null pointer, probably caused by a thread executing after cleanup has been performed");
        }
      } // end of first-level catch

     // we have to be careful about access to the datagram holding pen
     synchronized(datagramBuffer)
      {
       datagramBuffer.addElement(datagramPacket);
       debug("Got datagram, total size in buffer now " + datagramBuffer.size());
      }

    }   // end of while

}

/**
Returns a vector of datagrams that have been received by this object
since the last time this method was called.
*/
public Vector receivedDatagrams()
{
    // Returns a Vector of all the Datagrams received since we last asked.
    // Creates a new datagram buffer for the read thread, then takes the
    // old datagram buffer for its own nefarious purposes.
    
    Vector  localDatagramBuffer;                // just a ptr to keep the old buffer from being GC'd
    int     idx = 0;
    Vector  newDatagramBuffer;
    Enumeration datagramEnumeration;

    // Try to make the changeover fast.

    newDatagramBuffer = new Vector(); 

    // don't want to stomp on this while another thread is operating on it.

    //debug ("receivedPdus (): ready for synchronized (datagramBuffer)...");
    
    synchronized(datagramBuffer)
    {
    	//debug ("receivedPdus (): datagramBuffer size is " + datagramBuffer.size());
      //debug ("receivedPdus (): starting swap");
         
    	localDatagramBuffer = datagramBuffer;
      datagramBuffer = newDatagramBuffer;   // a fast swap, with minimal locked time

      //debug ("receivedPdus (): datagramBuffer size is " + datagramBuffer.size());
    }

   // debug ("receivedPdus (): synchronized () complete");

    //debug ("receivedPdus (): got buffer of " + localDatagramBuffer.size() + " datagrams");

    return localDatagramBuffer;
}

/**
This is JDK 1.2 specific code; if you have problems compiling
under 1.1, this is probably the cause. JDK 1.2 introduced an
int rather than a byte when setting the time-to-live on a
socket; the setTTL(byte) method was deprecated in preference
to the setTimeToLive(int) method. This just brings the code
in line with the current practice. To make this work under
1.1, just comment out all of this method, recompile, and use the
setTTL method instead.

Set the multicast socket time-to-live. 15=> local distribution, 63=>regional
distribution, 127=>global distribution. 
*/

public void setTimeToLive(int pTTL)
{
  if(usingMulticast)  // only makes sense on mcast sockets
  {
    ttl = pTTL;
    try
    {
    /* If you have problems compiling, you'll probably need to
       upgrade to JDK 1.2.2 or else eliminate this method.
    */

       multicastSocket.setTimeToLive(ttl); // JDK 1.2.2
    }
    catch(Exception e)  // likely java.lang.NoSuchMethodError if JDK 1.1
    {
    	System.out.println ("Exception " + e);
    	System.out.println ("  try deprecated JDK 1.1.8 setTTL method instead");
    	try
    	{
    		multicastSocket.setTTL((byte) ttl); // JDK 1.1.8, deprecated
    	} 
    	catch(Exception ee) 
    	{
    		trace ("Exception " + ee);
    		trace ("  no joy, ttl is unchanged...");
    	}
    }
  }   // end usingMulticast
}


/**
Set the multicast socket time-to-live. 15=> local distribution, 63=>regional
distribution, 127=>global distribution. It seems that there is a bug under NS
4.04 (surprise, surprise) so that setTTL on the mcast socket throws an exception,
so you probably shouldn't use this yet.

  JDK 1.2 deprecated the setTTL method on the MulticastSocket class
  and replaced it with the setTimeToLive method, which takes an int
  instead of a byte. If you have problems compiling under 1.1
  this is probably the problem.
*/
  
public void setTTL(byte pTTL)
{
  if(usingMulticast)  // only makes sense on mcast sockets
  {
    // Some folderol to handle unsigned bytes, which should never come
    // up in the first place, since 127 is the effective max TTL anyway.

    if(pTTL > 0)
      ttl = pTTL;
    else 
      ttl = 126 - pTTL;

    try
    {
       /*
       If you get deprecation warnings you can just comment out this
       whole method and use setTimeToLive instead. At least if your
       browser can also handle this.
       */
       multicastSocket.setTTL(pTTL); // deprecated JDK 1.1.8 method for Netscape, provided as a backup
    }
    catch(IOException ioe)
    {
      throw new RuntimeException("cannot change TTL on multicast socket");
    } // end catch
  }   // end usingMulticast
}

/**
Sends out a datagram to the designated destination and desitnation port.
*/

public synchronized void sendDatagram(DatagramPacket pDatagram,
                                   String pDestinationHost,
                                   int    pDestinationSocket)
{
  InetAddress  destAddress;

  try
  {
    destAddress = InetAddress.getByName(pDestinationHost);
    pDatagram.setAddress(destAddress); // works for multicast or unicast
  }
  catch(UnknownHostException unhe)
  {
    trace ("Unknown host");
  }

  if(!usingMulticast)
    pDatagram.setPort(pDestinationSocket);


  // send out the datagram over the socket
	sendDatagram(pDatagram);

  return;
}

/**
Sends out a datagram. The destination should already have been
set by the time it gets to this point.
*/

public synchronized void sendDatagram(DatagramPacket pDatagram)
{

  debug ("sending datagram");
  // send out the datagram over the socket
	try
	{
    if(usingMulticast)
    {
      // debug ("sending multicast");
      multicastSocket.send(pDatagram);
      // debug ("sent multicast");
    }
     else
     {
       // debug ("sending unicast");
	  datagramSocket.send(pDatagram);
       // debug ("sent unicast");
     }
	}
	catch(IOException e)
	{
		trace ("failed to send packet");
	}
  // debug ("sent datagram");


}




/**
Closes down sockets nicely
*/

public void cleanup()
{
  readingActive = false;

  if(datagramSocket != null)
    datagramSocket.close();

  if(multicastSocket != null)
    multicastSocket.close();
}

/**
  Finalize method--used to clean up any sockets that are still open
*/

protected void finalize() throws Throwable
{
  cleanup();

}

/**
For testing purposes. Opens up a socket on the designated address and port,
and listens for datagrams, storing them up. if you get datagrams, things
are working. This also serves as an example of how to use the class.
*/  
public static void main(String args[])
{
  DatagramStreamBuffer datasource = new DatagramStreamBuffer("224.2.181.145", 62040);
  Thread runThread;

  runThread = new Thread(datasource);
  runThread.start();

}

} // end of class DatagramStreamBuffer

