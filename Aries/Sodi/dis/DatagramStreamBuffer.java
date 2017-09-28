
// standard java packages

import java.net.*;
import java.util.*;
import java.io.*;

/**
 *     DatagramStreamBuffer
 * 
 *   This is a reusable class for reading datagrams from a socket, in a
 *   platform-independent way.<p>
 * 
 *   The DatagramStreamBuffer reads and stores datagrams (NOT PDUs).
 *   Since this is a fairly generic operation, this should be a very
 *   reusable class. It implements the runnable interface, and generally
 *   runs in its own thread, reading data from the socket as rapidly as
 *   datagrams come in. When no datagrams are arriving, the thread
 *   blocks on the receive() method of the socket class, consuming
 *   no CPU resources.<p>
 * 
 *   BehaviorStreamBuffer and others can act as an adaptor to DSB,
 *   utilizing this object's generic capability to provide PDUs
 *   to higher level objects.<p>
 * 
 *   This uses the security classes, which provide transparent browser-independent
 * access to things outside the sandbox. Note that use of these security classes
 * isn't particularly secure. Since they are declared as public interfaces and
 * classes, a rougue class can also seize them and bypass security. We live with
 * this, since our primary objective right now is to make security go away and
 * stop hurting us, rather than having it protect us.<p>
 * 
 *
 *<dt><b>Location:</b>
 *<a href="../../../../../../org/web3d/vrtp/net/DatagramStreamBuffer.java">
 *                         ~/org/web3d/vrtp/net/DatagramStreamBuffer.java</a>
 *
 * Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/org/web3d/vrtp/net/DatagramStreamBuffer.java">
 *                http://www.web3d.org/WorkingGroups/vrtp/org/web3d/vrtp/net/DatagramStreamBuffer.java</a>
 *
 * @author Don McGregor (<a href="mailto:mcgredo@nps.navy.mil">mcgredo@nps.navy.mil</a>)
 * @author Don Brutzman (<a href="mailto:brutzman@nps.navy.mil">brutzman@nps.navy.mil</a>)
 * 
 * @see mil.navy.nps.dis.BehaviorStreamBuffer
 * @see org.web3d.vrtp.security.SecurityStrategy
 */

public class DatagramStreamBuffer extends Object implements Runnable
{

   private PduDispatcher pduDis;
   private static final int  MAX_DATAGRAM_SIZE = 1500;   // MTU   
   private DatagramSocket datagramSocket  = null; // the socket we read from (configured w/ datagram port)
   int              datagramPort;             // what port we read from (used for both unicast & multicast)
   InetAddress      multicastAddress;         // multicast address, doesn't work on all versions of java
   int              ttl = 15;                 // multicast time-to-live; 15=site, 63=regional, 127=global
   boolean          usingMulticast;           // YES=> using the multicast socket NO=> using unicast socket
/*
 * Holds datagrams received recently
 * Buffer we hold datagrams in until the DIS application asks for them
 */
   Vector           datagramBuffer;           // holding pen for incoming datagram data
   boolean          readingActive = true;     // exit read loop in run
/*
 * temporary holding area for datagram being sent; ugly hack that prevents need
 * for parameters in security scheme
 */
   private DatagramPacket _dgram = null;      // Temporary holding area for datagram being sent     
/*
 * When DEBUG is true, System.out.println text messages trace the internals of script operation.
 * Text output appears in the Java Console (CosmoPlayer browser) or in the VRML console (WorldView browser)
 */
   private static boolean DEBUG = true;       // diagnostic messages on/off

public void setParentClass(PduDispatcher in)
{
    pduDis = in;
}

/*
 * Returns the status of debugging output, true = on
 */
public static boolean getDEBUG ()
{
	debug ("getDEBUG " + DEBUG);

	return DEBUG;
}
/*
 * Turns on or off debugging output
 */
public static void setDEBUG (boolean pDEBUG)
{
	DEBUG = pDEBUG;

	trace ("setDEBUG " + pDEBUG);
}
private boolean FIRSTCATCH, SECONDCATCH = false;
/*
 *  Debugging output. Pass in a string, and it gets printed out on the console.
 *  You can pass in strings such as "foo " + bar.getName().
 */
protected static void debug (String pDiagnostic)
{
  if (DEBUG)
  {
      System.out.println("DatagramStreamBuffer(debug): " + pDiagnostic);
      System.out.flush ();
  }
}
/*
 *   Guaranteed debugging output. Pass in a string, and it gets printed out on the console.
 *   You can pass in strings such as "foo " + bar.getName().
 */
protected static void trace (String pDiagnostic)
{
    System.out.println("DatagramStreamBuffer(trace): " + pDiagnostic);
    System.out.flush ();
}
/*
 * Returns the multicast address used by this socket
 */
public InetAddress getMulticastAddress()
{
    return multicastAddress;
}
/*
 * returns the port used by this socket
 */
public int getDatagramPort()
{
    return datagramSocket.getLocalPort();
}
/*
 * returns true of this is using multicast to communicate
 */
public boolean getUsingMulticast()
{
    return usingMulticast;
}

/*
 * Unicast constructor:  construct a new unicast datagram socket on the given port.
 */
public DatagramStreamBuffer(int pDatagramPort)    // INPUT: port we read from
{
    debug ("*** constructor new instance unicast org.web3d.net.DatagramStreamBuffer\n"
       + "    (" + pDatagramPort + ")");
    debug ("    (this should only appear once..)");
 	  
    usingMulticast = false;                       // Save various parameters
    datagramPort = pDatagramPort; 
  
    datagramBuffer = new Vector();                // Buffer for incoming datagrams

    //Object arg[] = new Object[1];
    //arg[0] = new Integer(datagramPort);

    debug (" openUnicastSocket" + datagramPort + ");");
    openUnicastSocket(datagramPort);

    debug ("finished unicast DatagramStreamBuffer constructor");
	  
    return;
}

/*
 * Unicast constructor:  create a new unicast DSB on an ephemeral port (a port picked by the system).
 */
public DatagramStreamBuffer() 
{

    debug ("*** constructor new instance unicast org.web3d.net.DatagramStreamBuffer ()\n"
       + "    (this should only appear once..)");
 	  
    debug ("strategy.invokePrivilege(this, \"openEphemeralUnicastSocket\");");
    openEphemeralUnicastSocket();
    
    usingMulticast = false;                         // Save various parameters
    datagramBuffer = new Vector();

    datagramPort = this.getDatagramPort();

    debug ("finished unicast DatagramStreamBuffer constructor");
	  
    return;
}

/*
 * Multicast constructor:  
 * given a multicast address and a port number, create a socket that is
 * joined to that group on that port number.
 */
public DatagramStreamBuffer(String pMulticastAddress, int pDatagramPort) // INPUT: multicast Address, port we read from
{

    debug ("*** constructor new instance multicast org.web3d.net.DatagramStreamBuffer\n"
         + "    (" + pMulticastAddress + ", " + pDatagramPort + ")\n"
         + "    (this should only appear once..)");

    datagramPort = pDatagramPort; 
    usingMulticast = true;
    datagramBuffer = new Vector();
    ttl = 15;
    
    try
    {
      // trace("Before getByName");
      multicastAddress = InetAddress.getByName(pMulticastAddress);
      // trace("After  getByName");
    }
    catch(UnknownHostException hnfe)
    {
      trace (hnfe.getMessage());
    }
    catch(Exception catchAllException)
    {
      trace (catchAllException.getMessage());
    }

    //Object args[] = new Object[2];
    //args[0] = multicastAddress;
    //args[1] = new Integer(pDatagramPort);

    debug ("strategy.invokePrivilege(this, \"createMulticastSocket\", [" +
    	multicastAddress + " " + pDatagramPort + "]);");

    createMulticastSocket(multicastAddress, datagramPort);

    debug ("finished constructor multicast DatagramStreamBuffer");
	  
    return;
}

/**
 * Creates unicast datagram socket. This should be called by the appropriate security 
 * strategy first; eg, the constructor calls the security strategy, which, after the
 * appropriate security calls have been made, calls this. Passes in datagramPort
 * indirectly.
 */
public synchronized void openUnicastSocket(int pPortNumber)
{
    try 
	{
	    debug ("try unicast new DatagramSocket(datagramPort)...");
	    datagramSocket = new DatagramSocket(pPortNumber);
	}
    catch (SocketException socketException)
	{
	    trace ("caught unicast socketException: " + socketException + "/" + socketException.getMessage ());
	    throw new 
		RuntimeException("Exception in DatagramStreamBuffer; unable to create socket in constructor");
	}
}

/**
 * Creates ephemeral unicast datagram socket, on a port picked by the system.  This should be 
 * called by the appropriate security strategy, eg the constructor calls the security strategy,
 * which in turn calls this after turning on approprate security calls.
 */
public synchronized void openEphemeralUnicastSocket()
{
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
}

/**
 * Create a multicast socket on a given port. The port is passed in via the instance
 * variable "datagramPort". The multicast address to be joined is also passed in as an
 * instance variable "parameter".
F */
public synchronized void createMulticastSocket(InetAddress pMcastAddress, int pPort)
{
    if (datagramSocket != null)
    {
    	trace ("repeat invocation of createMulticastSocket !!! datagramSocket already exists, ignored");
    	return;
    }

    // Step 1: Create the socket on the given port.
    try
    {
    	debug ("createMulticastSocket:  try new MulticastSocket(" + pPort + ")...");
    	System.out.flush();  // possible problem with IE5 exception preempting
    	datagramSocket = new MulticastSocket(pPort);
    }
    catch (Exception socketException)
    {
      System.out.println(socketException.toString());
      trace ("caught exception,\n unable to create MulticastSocket on port " + pPort);

      trace ("attempting openUnicastSocket (" + pPort + ");");
      openUnicastSocket(pPort);
      trace ("completed  openUnicastSocket (" + pPort + ");");
      debug("finished createMulticastSocket");
      return;
    }
     
    // Step 2: Join the multicast address.
    try
    {
        ((MulticastSocket)datagramSocket).joinGroup(pMcastAddress);
        debug ("joined multicast address " + multicastAddress.getHostAddress()  + ", port " + pPort);
    }
    catch (Exception socketException)
    {
        System.out.println(socketException.toString());
        datagramSocket.disconnect();
        datagramSocket.close();
        trace ("caught exception,\n unable to join multicast address " + pMcastAddress +
           ", likely due to security exception.");
        trace ("attempting openUnicastSocket (" + pPort + ");");
        openUnicastSocket (pPort);
        trace ("completed  openUnicastSocket (" + pPort + ");");
        debug("finished createMulticastSocket with UnicastSocket");
        return;
    }
    debug("finished createMulticastSocket");
}

/**
 * When this is set to false, the run() loop will terminate.
 */
public synchronized void stopReading()
{
    debug ("stopReading:  readingActive = false");
    readingActive = false;
}

public synchronized void resumeReading()
{
    debug ("resumeReading:  readingActive = true");
    readingActive = true;
}

/**
 * Returns the run status
 */

public synchronized boolean getReadingActive()
{
    return readingActive;
}

public synchronized void setReadingActive(boolean pState)
{
    debug ("setReadingActive:  " + pState);
    readingActive = pState;
}

/**
 * run() switches on platform-secific security, then calls doRun, which does the
 * actual work of reading from the network.
 * <P>
 * This is the implementation of the
 * runnable interface. The two-step process--run calls SecurityStrategy's invokePrivilege(),
 * which in turn calls back to this object--is required because some platforms
 * require the security calls to be "above" in the stack. Just switching on
 * the security parameters in strategy, and then returning and attempting to read
 * from a socket, is not enough.
 */

public void run()
{
    // now must do things this way, even under netscape security
    debug ("run:  strategy.invokePrivilege(this, \"doRun\");");
    doRun();

    debug ("run () execution of doRun while loop complete, all finished");

    return;
}

/**
 *  doRun loops, reading packets and adding them to the list of recieved
 *  packets, until readingActive is set to false. In the course of events
 *  the receivedDatagrams() method will usually be called, which grabs
 *  the vector of recieved datagrams and replaces it with a new, empty
 *  vector.
 *  <p>
 *  Called by run(); this is the essential portion of the run loop.
 *  run() has enabled platform-specific security at this point,
 *  so doRun doesn't have to worry about network sandboxes.
 */

public void doRun()
{
    DatagramPacket  datagramPacket; // Received datagram
    byte            dataBuffer[];   // data from received datagram
    boolean         readOK = false;
   
    debug ("commence doRun ();");

    while (readingActive == true)
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
        readOK = false;
        datagramSocket.receive(datagramPacket);
        readOK = true;
      } // end of try
      catch (IOException ioError)
      {
        if (!FIRSTCATCH) debug ("1st-level exception reading from datagram socket:\n  " + ioError);
        try
        {
          datagramSocket.receive(datagramPacket);
          readOK = true;
          if (!FIRSTCATCH) trace ("succeeded in 2nd-level read from datagram socket");
          FIRSTCATCH = true;
        }  // end of second-level try
        catch(IOException ioError2)
        {
            // ouch!! rogue thread??
            debug ("2nd-level exception reading from datagram socket:\n  " + ioError2);
            debug ("  ** (likely causes: security permissions/strategy problem, or thread shutdown) **");
            debug (this.toString());
            //System.exit (-1);
        } // end of second-level catch
          // A null pointer exception is thrown after we do a close on the sockets, then
          // start reading again. I think this is caused by the sockets being closed down
          // while waiting for a receive(). 
        catch(java.lang.NullPointerException npe)
        {
          if (!SECONDCATCH)
          {
          	debug ("second-level catch: datagramSocket null pointer, bad initialization/restart??");
          	debug (npe.getMessage());
                System.exit (-1);
          }
          SECONDCATCH = true;
          // may be caused by a thread executing after cleanup has been performed
        }
      } // end of first-level catch
      catch(java.lang.NullPointerException npe)
      {
          if (!FIRSTCATCH)
          {
          	trace ("first-level catch: datagramSocket null pointer, bad initialization/restart?");
         	trace (npe.getMessage());
          }
          FIRSTCATCH = true;
          // may be caused by a thread executing after cleanup has been performed
      }

     // we have to be careful about access to the datagram holding pen
     synchronized(datagramBuffer)
     {
       if(readOK)
       {
        datagramBuffer.addElement(datagramPacket.getData());
        debug ("Got datagram, total size in buffer now " + datagramBuffer.size());
        pduDis.singleReadLoop();
       }
     }

	try
	{
		Thread.sleep(1);  // go to sleep (msec) to avoid using too many draw cycles
	}
	catch(InterruptedException interruptedException)
	{
		throw new RuntimeException(" exceptional sleep: " + interruptedException);
	}

    }   // end of while

    debug ("complete doRun ();");

}

/**
 * Returns a vector of datagrams that have been received by this object
 * since the last time this method was called.
 */

public Vector getPackets()
{
    // Returns a Vector of all the Datagrams received since we last asked.
    // Creates a new datagram buffer for the read thread, then takes the
    // old datagram buffer for its own nefarious purposes.
    
    Vector  localDatagramBuffer;                // just a ptr to keep the old buffer from being GC'd
    int     idx = 0;
    Vector  newDatagramBuffer;
    Enumeration datagramEnumeration;

    // Try to make the changeover fast by creating new buffer before the switch.

    newDatagramBuffer = new Vector(); 

    // Synchronize (ensure exclusive access) on datagramBuffer. We lock the object,
    // then grab the old buffer and substitute a new one in its place.
    
    synchronized(datagramBuffer)
    {
    	localDatagramBuffer = datagramBuffer;
      datagramBuffer = newDatagramBuffer;   // a fast swap, with minimal locked time
    }

    return localDatagramBuffer;
}

/**
 * Returns either the next datagram packet, or the null if we
 * have no datagram packets.
 */

public DatagramPacket getNextPacket()
{
  return null;
}



/**
 * This is JDK 1.2 specific code; if you have problems compiling
 * under 1.1, this is probably the cause. JDK 1.2 introduced an
 * int rather than a byte when setting the time-to-live on a
 * socket; the setTTL(byte) method was deprecated in preference
 * to the setTimeToLive(int) method. This just brings the code
 * in line with the current practice. To make this work under
 * 1.1, just comment out all of this method, recompile, and use the
 * setTTL method instead.
 * 
 * Set the multicast socket time-to-live. 15=> local distribution, 63=>regional
 * distribution, 127=>global distribution. 
 */

public void setTimeToLive(int pTTL)
{
  if(usingMulticast)  // only makes sense on mcast sockets
  {
    ttl = pTTL;
    try
    {
    /* If you have problems compiling, you'll probably need
       to upgrade to JDK 1.2.2 or else eliminate this method.     */
      
       ((MulticastSocket)datagramSocket).setTimeToLive(ttl); // JDK 1.2.2
    }
    catch(Exception e)  // likely java.lang.NoSuchMethodError if JDK 1.1
    {
        trace ("DatagramStreamBuffer: " + e);
        trace ("...try deprecated JDK 1.1 setTTL method instead");
        try
        {
                ((MulticastSocket)datagramSocket).setTTL((byte) ttl); // JDK 1.1.8, deprecated
        }
        catch(Exception ee)
        {
                trace ("DatagramStreamBuffer: " + ee);
                trace ("  no joy, ttl is unchanged...");
                ee.printStackTrace();
        }
    }
    catch(java.lang.NoSuchMethodError nsme)  // likely java.lang.NoSuchMethodError if JDK 1.1
    {
        trace (nsme.toString());
        trace ("...try deprecated JDK 1.1 setTTL method instead");
        try
        {
                ((MulticastSocket)datagramSocket).setTTL((byte) ttl); // JDK 1.1.8, deprecated
                trace ("((MulticastSocket)datagramSocket).setTTL((byte)" + ttl + ") worked");
        }
        catch(Exception ee)
        {
                trace ("Exception " + ee);
                trace ("  no joy, ttl remains unchanged.");
                ee.printStackTrace();
        }
    }
  }   // end usingMulticast
}


/**
 * Set the multicast socket time-to-live. 15=> local distribution, 63=>regional
 * distribution, 127=>global distribution. It seems that there is a bug under NS
 * 4.04 (surprise, surprise) so that setTTL on the mcast socket throws an exception,
 * so you probably shouldn't use this yet.
 * 
 * JDK 1.2 deprecated the setTTL method on the MulticastSocket class
 * and replaced it with the setTimeToLive method, which takes an int
 * instead of a byte. If you have problems compiling under 1.1
 * this is probably the problem.
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
       ((MulticastSocket)datagramSocket).setTTL(pTTL);
    }
    catch(IOException ioe)
    {
      throw new RuntimeException("cannot change TTL on multicast socket");
    } // end catch
  }   // end usingMulticast
}

/**
 * Sends out a datagram to the designated destination and desitnation port. This is
 * a unicast send, which requires a machine name and port name to send the datagram to.<p>
 *
 * @param pDatagram datagram being sent out
 * @param pDestinatinHost place being sent to, in string format, either name or dotted decimal
 * @param pDestinationPort the port to be sent to on the remote machine
 */

public synchronized void sendDatagram(DatagramPacket pDatagram,
                                   String pDestinationHost,
                                   int    pDestinationPort)
{
  InetAddress  destAddress;

  try
  {
    destAddress = InetAddress.getByName(pDestinationHost);
    pDatagram.setAddress(destAddress); // works for multicast or unicast
    pDatagram.setPort(pDestinationPort);
  }
  catch(UnknownHostException unhe)
  {
    trace ("Unknown host");
  }

  // send out the datagram over the socket
	sendDatagram(pDatagram);

  return;
}

public synchronized void sendDatagram(DatagramPacket pDatagram,
                                   InetAddress pDestinationHost,
                                   int    pDestinationPort)
{
  pDatagram.setAddress(pDestinationHost);
  pDatagram.setPort(pDestinationPort);
  sendDatagram(pDatagram);
}


/**
 * A hack. This is only for internal used but must be declared public so it
 * can be accessed from the security packages.
 */
public synchronized void sendStoredDatagram()
{
 debug ("sending datagram");
  // send out the datagram over the socket
	try
	{
	  datagramSocket.send(_dgram);
	}
	catch(IOException e)
	{
		trace ("failed to send packet, sending to loopback.");

    // We failed to send for some reason--probably we have no interface
    // up or the interface is not multicast-capable. To maintain the 
    // illusion of being able to work stand-alone, we just turn around
    // and add the datagram to our own buffer.
    datagramBuffer.add(_dgram.getData());
	}
   debug ("sent datagram");

}

/**
 * Sends out a datagram. The destination should already have been
 * set by the time it gets to this point.
 */

public synchronized void sendDatagram(DatagramPacket pDatagram)
{

  _dgram = pDatagram;

  debug ("sendDatagram:  strategy.invokePrivilege(this, \"sendStoredDatagram\");");
  sendStoredDatagram();

}

/**
 * Closes down sockets nicely
 */

public synchronized void cleanup()
{
  readingActive = false;


  Exception e = new Exception("Trace exception");

  System.out.println("shutdown traceback");
  e.printStackTrace();

  if(datagramSocket != null)
  {
  	if (usingMulticast)
  	{
  	   try
  	   {
  		debug ("leaving multicast group");
  		((MulticastSocket)datagramSocket).leaveGroup (multicastAddress);
  	   }
  	   catch (IOException eleave)
  	   {
  	   	trace ("couldn't leave multicast group during cleanup, ignored...\n " + eleave);
  	   }
  	}
  	datagramSocket.close();
  	debug ("cleanup:  datagramSocket.close, thread shutting down ==================");
  }
}


public String toString()
{
  String response = "DatagramStreamBuffer info:";
  Integer iPort = new Integer(datagramPort);

  response = response + iPort + " ";
  response = response + multicastAddress;

  return response;
}

/**
  Finalize method--used to clean up any sockets that are still open
*/

protected synchronized void finalize() throws Throwable
{
  cleanup();

}

/**
 * For testing purposes. Opens up a socket on the designated address and port,
 * and listens for datagrams, storing them up. if you get datagrams, things
 * are working. This also serves as an example of how to use the class.
 */  
public static void main(String args[])
{
  DatagramStreamBuffer datasource = new DatagramStreamBuffer("224.2.181.145", 62040);
  Thread runThread;

  runThread = new Thread(datasource);
  runThread.start();

}

} // end of class DatagramStreamBuffer

