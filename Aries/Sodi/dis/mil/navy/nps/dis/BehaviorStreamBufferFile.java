
package mil.navy.nps.dis;		              // the package we belong to

import org.web3d.vrtp.security.*;         // x-platform security

import java.io.*;
import java.util.*;

/**
 * BehaviorStreamBufferFile is a class responsible for interpreting data received
 * via a file and interpreting it as DIS PDUs.<p>
 * 
 * DIS is by nature packet-oriented, while files are stream-oriented. this means
 * that we must parse the data as it comes in, to determine how many bytes
 * to read. This can be done via the PDU length field in the PDU header. This
 * is always at the same point in the packet, and describes how many bytes to
 * read. By nature, if this count is off, the stream will get scrambled.<p>
 *
 * Note that if we're reading from a file, we don't want to get TOO carried 
 * away with this whole reading thing. This might read the entire contents of
 * a file in before anyone consumes any PDUs. This would be a disaster for
 * big files. We do some thread synchronization with wait() and notify() in
 * the methods that read data and consume PDUs. This regulates the number of
 * pdus read in beforehand to DEFAULT_READAHEAD_SIZE.<p>
 *
 * The bsbf has prepended data that says something about the stream saved
 * inside. This consists of a version number, a byte count, and a string
 * that describes the data. this string right now contains a URL and a 
 * flag that tells whether the saved PDUs are using RTP headers.<p>
 *
 * @author DMcG
 */

public class BehaviorStreamBufferFile extends BehaviorStreamBuffer implements Runnable, AllPermissionsBadge
{
  public static final int MAX_DATAGRAM_SIZE = 1500;
  public static final int DEFAULT_READAHEAD_SIZE = 50;
  public static final int HEADER_VERSION = 1;           // Version of the header info prepended to file data

  public static boolean DEBUG = true;

  /**
   * File for writing or reading. We can only do one or the other at once.
   */

  private File file = null;

  /**
   * defines whether we're readig or writing from our file. We can't
   * be doing both.
   */

  private boolean isReader = true;

  /**
   * fos is used for writing data to a file. 
   */

  private FileOutputStream fos = null; 

  /**
   * fis is used as a reference to a file input stream we're reading from
   */

  private FileInputStream fis = null;

  /**
   * Saved up PDUs. These are fully promoted PDU objects.
   */

  private Vector cachedPdus = null;

  /**
   * Read-ahead size--how many PDUs to read from the file before pausing
   * and waiting for the client to request some.
   */

  private int maxReadAheadSize = DEFAULT_READAHEAD_SIZE;

  /**
   * Set to true if the file is at EOF.
   */

  public boolean atEOF = false;

  /**
   * Set the max readahead size. This is the number of PDUs the class will
   * Read from the file before stopping. This prevents big gobs of PDUs from
   * being read and blowing out VM.
   */

public void setReadAheadSize(int pNewReadAheadSize)
{
  // just for good measure, if the read thread is waiting, tell it to read more.

  if(maxReadAheadSize < pNewReadAheadSize)
  {
    synchronized(cachedPdus)
    {
      cachedPdus.notify();
    }
  }

  maxReadAheadSize = pNewReadAheadSize;
}

/**
 * sets whether we're a reader or writer.
 */

public void setIsReader(boolean pIsReader)
{
  isReader = pIsReader;
}


/**
 * Constructor, takes a file. Also starts a thread running to read from the file.
 * Can take a string if this is a writer, which does double duty. Passing in 
 * the string says both that this is a writer, and prepends the data to the 
 * output stream. If the string is null, it's assumed that this is a reader.
 * Kinda questionable to kick of the thread in the constuructor.
 *
 * @param pFile file object to read or write from
 * @param pIsReader are we a reader or writer?
 */

public BehaviorStreamBufferFile(File pFile, boolean pIsReader)
{
  this(pFile, new BehaviorStreamBufferInfo(false), pIsReader);
}

/**
 * Constructor; takes a string filename. we open the file ourselves. Also starts
 * an input thread.
 */

public BehaviorStreamBufferFile(String pFileName, boolean pIsReader)
{
  this(new File(pFileName), new BehaviorStreamBufferInfo(false), pIsReader);
}

/**
 * yet another constructor
 */

public BehaviorStreamBufferFile(String pFileName, BehaviorStreamBufferInfo pInfo, boolean pIsReader)
{
  this(new File(pFileName), pInfo, pIsReader);
}

/**
 * All-signing, all-dancing constructor
 */

public BehaviorStreamBufferFile(File pFile, BehaviorStreamBufferInfo pInfo, boolean pIsReader)
{
  file = pFile;
  cachedPdus = new Vector();
  isReader = pIsReader;

  info = pInfo;

  if(isReader)
    this.openForReading();
  else
    this.openForWriting();
  
  // Start a thread for reading from the socket

   if (inputThreadStarted == false)
	{
	  this.startInputThreadWithSecurity();
	}

	debug ("finished constructor file BehaviorStreamBufferFile");

	return;
}

/**
 * Constructor, takes a fileName and a reader/writer configuration string ("reader" or "writer").
 */

public BehaviorStreamBufferFile(String pFileName, String readerOrWriter)
{
	file = new File(pFileName);
	cachedPdus = new Vector();
  
  info = new BehaviorStreamBufferInfo(false);  // no RTP by default

	if (readerOrWriter.equalsIgnoreCase ("reader"))
	{
		isReader = true;
	}
	else if (readerOrWriter.equalsIgnoreCase ("writer"))
	{
		isReader = false;
	}
	else
	{
		System.out.println ("Illegal BehaviorStreamBufferFile constructor readerOrWriter=" +
			readerOrWriter + ", exiting.");
		System.exit (-1);
	}

	// Start a thread for reading from the socket
	if (inputThreadStarted == false)
	{
	  this.startInputThreadWithSecurity();
	}
	debug ("finished constructor file BehaviorStreamBufferFile");

	return;
}

/**
 * Simple method to launch thread. This kicks of the subclass of BSB
 * and starts it reading in its own thread. This is required for
 * some subclasses, such as sockets; for files, this is optional--
 * we can read one PDU at a time from them without blocking.
 */

public void  startInputThread()
{
  
   if (inputThreadStarted)
   {
   		trace ("startInputthread() invoked again, ignored...");
   		return;
   }
   
	trace ("startInputthread() method started...");

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
 * we open a file for reading. This involves opening the file,
 * reading any initial config data from the start. This uses
 * the file specified in the constructor.
 */


private void openForReading()
{
  DataInputStream dis;
  int             headerVersion;
  int             configLength;
  byte            configBytes[];

  try
  {
    fis = new FileInputStream(file);
    dis = new DataInputStream(fis);
    
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
      // If the length of the config data is way off, it's likely we're off on our pointers
      // somewhere. But continue anyway after duly warning the luser.

      if(configLength > 2048)
      {
        System.out.println("When reading from PDU file read an implausibly long piece of data");
        System.out.println("from the header that describes how long the initial configuration");
        System.out.println("data for the file is: " + configLength + ". This is probably caused");
        System.out.println("by reading from an unexpected place in the file. See BehaviorStreamBufferFile.");
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

      System.out.println("got info data of " + info);

      this.setRtpEnabled(info.getRtpEnabled());
    } // End of version 1 header data
  }
  catch(IOException ioe)
  {
    System.out.println(ioe);
  }

  return;
}
/**
 * open a file for writing. This involves opening the file,
 * and writing some initial configuration data to the file
 * that contains information about the pdu stream. This uses
 * the file specified in the constructor.
 */

private void openForWriting()
{
  // We have to open the file, and write some initial 
  // configuration data to it.

  try
  {
    fos = new FileOutputStream(file);
    DataOutputStream dos = new DataOutputStream(fos);

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
    byte              dataBuffer[];   // data from file
    int               packetLength = 0;
    ProtocolDataUnit  aPdu = null;
    DataInputStream   dis = null;

    atEOF = false;
    readThreadRunning = true;

    // We're either a reader or a writer. If we're a reader we need to open
    // up the file, and start reading from it. If we're a writer we can 
    // just open the file for writing and leave--all the sending will write
    // to the file, and we don't have to read anything.

    //System.out.println("Running, isReader = " + isReader);

    if(isReader)
    {
        dis = new DataInputStream(fis); 
    }
    else   // We're a writer. Exit immeidately 
    {
      return;
    }


    while ((runContinue == true) && (fis != null) && (!atEOF))
    {
       // create a new data array, then read it from the wire.
      int        position    = 0;
      byte       rtpMarker   = 0;
      boolean    isRtp       = false;
      byte       pduHeader[] = new byte[ProtocolDataUnit.sizeOf];    // holds PDU header
      byte       rtpHeader[] = new byte[RtpHeader.sizeOf];           // holds rtp header

      try
      {
        // Step 1: We need to determine if the stream is using RTP headers
        // or not. This _should_ be set in the info field, which tells us
        // beforhand whether this is RTP-enabled or not. But since I'm paranoid,
        // I'll check the first byte of the data. If this matches the magic value
        // that specifies an RTP header, assume we have an rtp header.

        rtpMarker = dis.readByte();

        //System.out.println("rtpMarker = " + rtpMarker + " magic number: " + RtpHeader.RTP_VERSION * 64);

        if(-rtpMarker == RtpHeader.RTP_VERSION * 64)
        {
          isRtp = true;
          rtpHeader[0] = rtpMarker;
          for(int idx = 1; idx < RtpHeader.sizeOf; idx++)
            rtpHeader[idx] = dis.readByte();
        }

        // At this point we should be pointing to the start of the pdu data.

        // Initial header stuff from the PDU, plus the length field.

        // We did the initial peek at the first byte. If that turned out to 
        // be a dry hole--in fact we're not RTP--we're one byte ahead of ourselves
        // when reading from the main PDU data.

        int startPoint;
        if(isRtp)
          startPoint = 0;
        else
          startPoint = 1;

        for(int idx = startPoint; idx < ProtocolDataUnit.sizeOf; idx++)
          pduHeader[idx] = dis.readByte();

        // Figure out how many bytes are in this packet. We can do this by
        // bit-masking the bytes at dataBuffer[8] and dataBuffer[9], which
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
        if(isRtp)
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

        /*
        for(int idx = 0; idx < dataBuffer.length; idx++)
          System.out.print(dataBuffer[idx] + " ");
        System.out.println();
        */

        // Create a new PDU
        aPdu = ProtocolDataUnit.byteArrayToPdu(dataBuffer);

      }
      catch(EOFException eofe)
      {
        debug("End of file");
        atEOF = true;
      }
      catch(IOException ioe)
      {
        System.out.println("Exception reading from file, other than EOF; this is unusual " + ioe);
        atEOF = true;
      }

     // want to skip overthis if we hit EOF; we didn't get enough data
      if(!atEOF)
      {
         // we have to be careful about access to the datagram holding pen
         synchronized(cachedPdus)
         {
           cachedPdus.addElement(aPdu);
           //debug ("Got datagram, total size in buffer now " + cachedPdus.size());
         }
      }

      // Way tricky stuff. We don't want to read every single PDU in a huge 
      // file of 50 MB. That's effectively memory mapping the whole file.
      // So instead we have a limit of MaxPdus pdus read from the file. Once
      // that limit is hit we do a wait() on cachedPdus, which stops us
      // from reading. Elsewhere, when we do a getPDU of some type, we do
      // a notifyAll() on the cachedPdus object. This wakes up any threads
      // waiting on the object, namely us. Now we can read more without
      // wiping out VM. Note that we don't bother locking the cachedPdus 
      // object; no biggie if we're off. 

	    if(cachedPdus.size() >= maxReadAheadSize)
      {
        try
        {
          synchronized(cachedPdus)
          {
            cachedPdus.wait();
          }
        }
        catch(InterruptedException ie)
        {
          System.out.println(ie);
        }
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

   // The run() loop may be waiting on us, because it's max read ahead
   // has been hit. This wakes up the thread waiting on us to get a PDU,
   // so the read-ahead can return to reading.

   synchronized(cachedPdus)
   {
     cachedPdus.notify();
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
  ProtocolDataUnit aPdu = null;

  this.checkForThreadStart();

  // No pdus to return? We could have buffer underflow, or
  // we could really be at the end of the file. In the first
  // case, return null. In the latter, give the reader thread
  // a chance to catch up.

  try
  {
    while(cachedPdus.size() == 0)
    {
      if(atEOF)
        return null;
      else
      {
        Thread.sleep(100);    // give reader thread a chance to keep up
      }
    }
  }
  catch(Exception e)
  {
    System.out.println("Problem with sleep");
  }

  // Our read-ahead buffer may be full, and removing a PDU from the
  // read-ahead buffer frees up room for more. Notify anyone waiting
  // on it.

  synchronized(cachedPdus)
  {
     cachedPdus.notifyAll();

     aPdu = (ProtocolDataUnit)(cachedPdus.remove(0));
  }

  return aPdu;

}

/**
 * Sends a PDU. If the underlying destination address has already
 * been set, for example in a multicast or file, we don't need
 * to supply an address.
 */

public void sendPdu(ProtocolDataUnit pPdu)
{
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    byte             data[];

    // Null pdu? Punt.
    if(pPdu == null)
      return;

    // Write the PDU to a byte array, and get a reference to 
    // the byte array. We can only write if we're a writer.

    if(!isReader)
    {
      pPdu.serialize(dos);
      data = baos.toByteArray();

      // Write the data to the PDU stream.

      try
      {
        fos.write(data);
      }
      catch(IOException ioe)
      {
        System.out.println(ioe);
      }
    }
}

/**
 * Send a PDU to an address. Since the address can be in many forms,
 * for example an InetAddress and a port number, we cheat here. The
 * destination address is passed in as generic objects; a concrete
 * subclass, such as a Unicast UDP object, will cast the generic
 * objects here to what it expects, such as an InetAddress. (After
 * checking for the right type with instanceof, of course!) In this
 * case we simply write to the default file. In the future this might
 * be changed to some other file.<p>
 *
 * @param pPdu protocol data unit being sent
 * @param pAddress1 ignored
 * @param pAddress2 ignored
 */

public void sendPdu(ProtocolDataUnit pPdu,
                             Object pAddress1,
                             Object pAddress2)
{
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    byte             data[];

    // No pdu? Punt.
    if(pPdu == null)
      return;

    // We can only write if we're a writer.
    if(isReader)
      return;

    pPdu.serialize(dos);
    data = baos.toByteArray();

    try
    {
      fos.write(data);
    }
    catch(IOException ioe)
    {
      System.out.println(ioe);
    }
}

/**
 * Closes down input buffers, sockets, or open files nicely
 */

public void cleanup()
{
  try
  {
    if(fos != null)
      fos.close();

    if(fis != null)
      fis.close();
  }
  catch(IOException ioe)
  {
    System.out.println(ioe);
  }

  fos = null;
  fis = null;
}

/**
 *  Finalize method--used to clean up any files that are still open
 */

protected void finalize() throws Throwable
{
  this.cleanup();
}

/**
 * tracing output
 */

public void trace(String pMessage)
{
  System.out.println("BehaviorStreamBufferFile: " + pMessage);
}

/**
 * Debugging output
 */

public void debug(String pMessage)
{
  if(DEBUG)
    System.out.println("BehaviorStreamBufferFile: " + pMessage);
}

/**
 * Used to debug/test the class
 */

public static void main(String args[])
{
  FileInputStream fis = null;
  BehaviorStreamBufferFile bsbf;    // What we read from
  BehaviorStreamBufferFile sink;    // what we write to
  BehaviorStreamBufferInfo info;    // info about destination sink
  String                   urls[] = new String[2];

  urls[0] = "http://www.drudgereport.com";
  urls[1] = "http://www.ar15.com";

  
  bsbf = new BehaviorStreamBufferFile("TestData.pdu", true);
  System.out.println("Created first bsbf");

  info = new BehaviorStreamBufferInfo(true, urls);

  sink = new BehaviorStreamBufferFile("Sink.pdu", false);
  sink.setInfo(info);

  while(true)
  {
    ProtocolDataUnit pdu = bsbf.getNextPdu();
    sink.sendPdu(pdu);

    try
    {
      Thread.sleep(1000);
    }
    catch(Exception e)
    {
      System.out.println(e);
    }
  }

}

  



} // end of class BehaviorStreamBuffer

