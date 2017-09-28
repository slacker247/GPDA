
package mil.navy.nps.dis;		              // the package we belong to

import org.web3d.vrtp.security.*;         // x-platform security

import java.io.*;
import java.net.*;
import java.util.*;

/**
 * BehaviorStreamBufferTCP is a class responsible for interpreting data received
 * via a TCP socket and interpreting it as DIS PDUs.<p>
 * 
 * DIS is by nature packet-oriented, while TCP is stream-oriented. this means
 * that we must parse the data as it comes in, to determine how many bytes
 * to read. This can be done via the PDU length field in the PDU header. This
 * is always at the same point in the packet, and describes how many bytes to
 * read. By nature, if this count is off, the stream will get scrambled.
 *
 * @author DMcG
 */

public abstract class BehaviorStreamBufferTCP extends BehaviorStreamBuffer implements Runnable, AllPermissionsBadge
{
  public static final int MAX_DATAGRAM_SIZE = 1500;
  public static boolean DEBUG = true;

  /**
   * SecurityStrategy is used as a way to get around the java sandbox. A security 
   * scheme is picked at runtime for whatever type of box we are running on.
   */

  protected SecurityStrategy strategy = SecurityStrategy.getSecurityStrategy();



  // Are we a server or a client? We actually punt on the choice. All we
  // care about is the fully connected duplex socket. If we act as the 
  // server, we need some serverSocket external to this listening for
  // connections. When we get one, we pass off the fully connected socket
  // to us in a constructor call.

  /**
   * Full duplex communications channel with another host
   */

  private Socket       socket;

  /**
   * Saved up PDUs. These are fully promoted PDU objects.
   */

  private Vector cachedPdus = null;

  /**
   * Before we write the first PDU, we should write configuration information about
   * the stream. This is boolean keeps track of whether we've written a PDU; if not,
   * we write the config info first, then the PDU.
   */
  private boolean firstPduWritten = false;

  /**
   * The inverse of the above. Before we read the first PDU, we should read configuration
   * data. If the first PDU hasn't been read, read the config data; otherwise, read the
   * pdu.
   */

  private boolean firstPduRead = false;

/**
 * Constructor, takes a connected socket. Also starts up a listening
 * thread on this socket.
 */

public BehaviorStreamBufferTCP(Socket pSocket)
{
  cachedPdus = new Vector();
  socket = pSocket;

  if (inputThreadStarted == false)
	{
		try
    {
			trace ("initialize:  strategy.invokePrivilege(this, \"startInputThread for unicast TCP socket\");");
			strategy.invokePrivilege(this, "startInputThread");
		}
		catch (Exception catchAllException)
		{
			trace ("initialize:   exception from strategy.invokePrivilege " + catchAllException);
		// 	catchAllException.printStackTrace();
 			}
		debug ("inputStreamThread = " + inputThreadStarted);
	}
	debug ("finished constructor TCP socket BehaviorStreamBufferTCP");

	return;
}

/**
 * Constructor; takes a string inet address and a port number. Establishes
 * a connection to this server and uses that connection for communications.
 * This also starts up a thread for communications with that server.
 */

public BehaviorStreamBufferTCP(String pAddress, int port)
{
  InetAddress addr;

  // Convert the string into an inet address, and establish a connection
  // with the given address.

  try
  {
    addr = InetAddress.getByName(pAddress);

    socket = new Socket(addr, port);
  }
  catch(Exception e)
  {
    System.out.println("network problem, " + e);
  }

  // Start a thread for reading from the socket

  if (inputThreadStarted == false)
	{
		try
    {
			trace ("initialize:  strategy.invokePrivilege(this, \"startInputThread for unicast TCP socket\");");
			strategy.invokePrivilege(this, "startInputThread");
		}
		catch (Exception catchAllException)
		{
			trace ("initialize:   exception from strategy.invokePrivilege " + catchAllException);
		// 	catchAllException.printStackTrace();
 			}
		debug ("inputStreamThread = " + inputThreadStarted);
	}
	debug ("finished constructor TCP socket BehaviorStreamBufferTCP");

	return;
}

/**
 * Simple method to launch thread. This kicks of the BSBTCP
 * and starts it reading in its own thread. This is required for
 * some subclasses of BSB, such as sockets; for files, this is optional--
 * we can read one PDU at a time from them without blocking.
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
		// Thread used to run the bsb

		Thread tcpThread = new Thread(this);//, "DebuggingDatagramStreamBuffer-" + ClassUtilities.nextSerialNum()
		tcpThread.start();
		inputThreadStarted = true;
	}
	catch (Exception catchAllException)
	{
		trace ("startInputThread: exception\n" + catchAllException);
		trace ("startInputThread: attempting to continue without tcp input thread threaded...");
	}
	return;
}


/**
 *  Terminate the run loop and shutdown the thread. This should
 *  be used only for threaded readers.
 */
public void shutdown ()
{
}

/**
 *   suspend reading in the DatagramStreamBuffer
 */
public void suspendReading()
{
}

/**
 * Start reading packets from the datagramStreamBuffer again.
 */

public void resumeReading()
{
}

/**
 * Whenever we start reading from a socket, it is assumed that the first few
 * bytes will be configuration data about the PDU stream--informaton such as
 * URLs where we can find the data, whether the information has RTP headers,
 * and so on. This method reads that data.
 */

private void openForReading()
{
  DataInputStream dis;
  int             headerVersion;
  int             configLength;
  byte            configBytes[];

  try
  {
    dis = new DataInputStream(socket.getInputStream());
    
    // Read the initial header information, so we can figure out what version
    // of the config data we've got.

    headerVersion = dis.readInt();

    // First version of the config data
    if(headerVersion == 1)  // Initial version
    {
      int bytesRead = 0;
      StringTokenizer tokenizer;
      String          configData;

      // How long the configuraton data is
      configLength = dis.readInt();

      // Sanity clause. 
      // ("don't be silly--there ain't no sanity claus" --A Night at the Opera, the Marx Brothers)
      // If the length of the config data is way big, it's likely we're off on our pointers
      // somewhere. But continue anyway after duly warning the luser.

      if(configLength > 2048)
      {
        System.out.println("When reading from PDU file read an implausibly long piece of data");
        System.out.println("from the header that describes how long the initial configuration");
        System.out.println("data for the file is: " + configLength + ". This is probably caused");
        System.out.println("by reading from an unexpected place in the file. See BehaviorStreamBufferTCP.");
      }

      // Allocate an array to hold the config data, and read into it.
      configBytes = new byte[configLength];
      bytesRead = dis.read(configBytes, 0, configLength);

      // Couldn't read enough bytes from the file to fill out the advertised length of the config data
      if(bytesRead != configLength)
      {
        System.out.println("Had problems reading header");
        return;
      }

      // configData should contain all the configuration information in
      // space delimited attribute-value pairs, such as "rtpEnabled=yes blahEnabled=no".

      configData = new String(configBytes, 0, configBytes.length);

      // Create a new info object      
      info = new BehaviorStreamBufferInfo(configData);

      this.setRtpEnabled(info.getRtpEnabled());
    } // End of version 1 header data
  }
  catch(IOException ioe)
  {
    System.out.println(ioe);
  }

  return;
} // End of openForReading

/**
 * open a socket for writing. This involves writing initial configuration
 * data from the socket that describes the PDU stream--is it RTP enabled,
 * URLs for more info about the stream, etc. This should be executed only
 * before the first write of a PDU.
 */

private void openForWriting(Socket pSock)
{
  // We have to open the file, and write some initial 
  // configuration data to it.

  try
  {
    DataOutputStream dos = new DataOutputStream(pSock.getOutputStream());

    // Write header data to the file that tells us about the stream.
    // Start off with the version number that specifies the layout
    // of the header.
    dos.writeInt(info.getVersion());

    // Write config string. 

    String pairs = info.toString();
    
    int configLength;         // How long the string is, in bytes
    byte configDataBytes[];   // String converted to byte array

    configDataBytes = pairs.getBytes();
    configLength = configDataBytes.length;

    // Write how long the string is, then the string
    dos.writeInt(configLength);
    dos.write(configDataBytes);
  }
  catch(IOException ioe)
  {
    System.out.println("Problem getting file to write to" + ioe);
    return;
  }
}


/**
 *  Threading method to read/write until shutdown.
 */
public void run()
{
    byte              dataBuffer[];   // data from received datagram
    DataInputStream   dis = null;
    int               packetLength = 0;
    ProtocolDataUnit  aPdu = null;

    readThreadRunning = true;

    // Create the data input stream. This input stream should exist for the
    // same length of time as the socket itself.
    try
    {
      dis = new DataInputStream(socket.getInputStream());
    }
    catch(IOException ioe)
    {
      System.out.println("Problem getting input stream from TCP socket" + ioe);
    }

     // If we haven't read any PDUs, we need to read the initial configuration
     // data off the stream.

    if(!firstPduRead)
    {
      this.openForReading();
      firstPduRead = true;
    }

    // Ick. Lots of this code is shared between here and BehaviorStreamBufferFile.
    // there should be some way to factor this code out and share it between the
    // two files.

    while ((runContinue == true) && (dis != null))
    {
          // create a new data array, then read it from the wire.
      int        position = 0;
      byte       pduHeader[] = new byte[ProtocolDataUnit.sizeOf];    // holds PDU header
      byte       rtpHeader[] = new byte[RtpHeader.sizeOf];           // holds rtp header

      try
      {
        // Step 1: We need to determine if the stream is using RTP headers
        // or not. This _should_ be set in the info field, which tells us
        // beforhand whether this is RTP-enabled or not.

        // Read in the RTP headers. The RTP header is 12 bytes long.
        if(info.getRtpEnabled() == true)
        {
          for(int idx = 0; idx < RtpHeader.sizeOf; idx++)
            rtpHeader[idx] = dis.readByte();
        }

        // At this point we should be pointing to the start of the pdu data.

        // Initial header stuff from the PDU, plus the length field.

        for(int idx = 0; idx < ProtocolDataUnit.sizeOf; idx++)
          pduHeader[idx] = dis.readByte();

        // Figure out how many bytes are in this packet. We can do this by
        // bit-masking the bytes at pduHeader[8] and pduHeader[9], which
        // hold the 16 bit integer that describes the length. This number
        // must be in network (big-endian) byte order.

        packetLength = 0;   // Be paranoid. Be very paranoid.

        // First byte bit-masked in, shifted left, then the second byte bit-masked in.
        packetLength = ((int)pduHeader[8]) & 0xff;
        packetLength = packetLength << 8;
        packetLength = ((int)pduHeader[9]) & 0xff;

        // Read in the rest of the packet, now that we know the length
        dataBuffer = new byte[rtpHeader.length + packetLength];

        // copy the existing stuff we've read into the main array. Rtp header, if any
        if(info.getRtpEnabled() == true)
        {
          System.arraycopy(rtpHeader, 0, dataBuffer, 0, rtpHeader.length);
          position = position + rtpHeader.length;
        }

        // Pdu header, starting at the correct place
        System.arraycopy(pduHeader, 0, dataBuffer, position, pduHeader.length);
        position = position + pduHeader.length;

        // Copy in the remainder of the non-header data in the pdu
        for(int idx = 0; idx < packetLength-pduHeader.length; idx++)
          dataBuffer[idx + position] = dis.readByte();

        // Create a new PDU
        aPdu = ProtocolDataUnit.byteArrayToPdu(dataBuffer);
      }
      catch(EOFException eofe)
      {
        debug("End of file");
      }
      catch(IOException ioe)
      {
        System.out.println("Exception reading from file, other than EOF; this is unusual " + ioe);
      }
     
     // we have to be careful about access to the datagram holding pen
     synchronized(cachedPdus)
     {
       cachedPdus.addElement(aPdu);
       debug ("Got datagram, total size in buffer now " + cachedPdus.size());
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

  trace ("complete doRun ();");

}


/**
 * Returns a vector of all the PDUs received since the last time we
 * asked. Queries the underlying input buffer for this information.\
 * this is generally used with threaded readers.
 */

public Vector receivedPdus()
{
  Vector tempList;

   this.checkForThreadStart();

   synchronized(cachedPdus)
     {
       tempList = cachedPdus;
       cachedPdus = new Vector();
     }
   return tempList;
}

/**
 * Get the next PDU from the input stream. This is generally used
 * with unthreaded readers.
 */

public ProtocolDataUnit getNextPdu()
{
  this.checkForThreadStart();

  synchronized(cachedPdus)
  {
    // No pdus to return?
    if(cachedPdus.size() == 0)
      return null;

     ProtocolDataUnit aPdu;
     aPdu = (ProtocolDataUnit)(cachedPdus.remove(0));

     return aPdu;
  }
}

/**
 * Sends a PDU. If the underlying destination address has already
 * been set, for example in a multicast or file, we don't need
 * to supply an address.
 */

public void sendPdu(ProtocolDataUnit pPdu)
{
  byte buffer[];
  ByteArrayOutputStream bos = new ByteArrayOutputStream();
  DataOutputStream dos = new DataOutputStream(bos);
  OutputStream     os;

  pPdu.serialize(dos);

  buffer = bos.toByteArray();

  // If this is our very first PDU, we need to prepend this with some
  // configuration data, such as the RTP status, URLs with more information,
  // etc.
  if(firstPduWritten == false)
  {
    this.openForWriting(socket);
    firstPduWritten = true;
  }

  try
  {
    os = socket.getOutputStream();
    os.write(buffer);
  }
  catch(IOException ioe)
  {
    System.out.println(ioe);
  }
}


/**
 * Send a PDU to an address. Since the address can be in many forms,
 * for example an InetAddress and a port number, we cheat here. The
 * destination address is passed in as generic objects; a concrete
 * subclass, such as a Unicast UDP object, will cast the generic
 * objects here to what it expects, such as an InetAddress. (After
 * checking for the right type with instanceof, of course!)<p>
 *
 * Note that if you're sending to the "standard" tcp destination 
 * address, you should use sendPdu().
 *
 * @param pPdu protocol data unit being sent
 * @param pAddress1 TCP address of destination
 * @param pAddress2 port of destination
 */

public void sendPdu(ProtocolDataUnit pPdu,
                             Object pAddress1,
                             Object pAddress2)
{
  int                   port;
  String                address;
  InetAddress           ipAddr = null;
  Socket                sock;
  byte                  buffer[];
  ByteArrayOutputStream baos;
  DataOutputStream      dos;
  OutputStream          os;

  // Check to make sure the args are the type we expect for this concrete subclass.
  // punt if they're not the correct types.

  if(!(pAddress1 instanceof String))
    return;

  if(!(pAddress2 instanceof Integer))
    return;

  // Get the address and port in a standard way
  address = (String)pAddress1;
  port    = ((Integer)pAddress2).intValue();

  // Open up a connection to the server that is presumed to be runnning
  // on the other machine.
  try
  {
    ipAddr = InetAddress.getByName(address);

    sock = new Socket(ipAddr, port);

    // Open up the socket for writing by prepending configuration data, such
    // as RTP status.

    this.openForWriting(sock);

    // Serialize the PDU we're sending into a byte array.
    baos = new ByteArrayOutputStream();
    dos = new DataOutputStream(baos);
    pPdu.serialize(dos);
    buffer = baos.toByteArray();

    // Write the buffer to the socket
    os = sock.getOutputStream();
    os.write(buffer);

    // Close down the socket. It's presumed that our sending this PDU to
    // a machine other than our "standard" destination address is a rare
    // one, so we can afford to open and close a socket for every connection
    // we make.

    sock.close();
  }
  catch(Exception e)
  {
    System.out.println(e);
  }

}

/**
 * Closes down input buffers, sockets, or open files nicely
 */

public void cleanup()
{
}

/**
 *  Finalize method--used to clean up any sockets that are still open
 */

protected void finalize() throws Throwable
{
}

/**
 * tracing output
 */

public void trace(String pMessage)
{
  System.out.println("BehaviorStreamBufferTCP: " + pMessage);
}

/**
 * Debugging output
 */

public void debug(String pMessage)
{
  if(DEBUG)
    System.out.println("BehaviorStreamBufferTCP: " + pMessage);
}


} // end of class BehaviorStreamBuffer

