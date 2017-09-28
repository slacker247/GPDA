package mil.navy.nps.dis;

import mil.navy.nps.util.*;         
import java.io.*;

/**
 * This class encapsulates the header of the Real-time Transport Protocol (RTP)
 * when used to transfer DIS packets as a payload.
 *
 *@version 1.0
 *@author Francisco Afonso (afonso@cs.nps.navy.mil)
 *
 *<b>Location:</b>
 *Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/RtpHeader.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/RtpHeader.java</a>
 *or locally:  <a href="../../../../../../mil/navy/nps/dis/RtpHeader.java">
 *  ~/mil/navy/nps/dis/RtpHeader.java</a>
 *
 *<P>
 *<b>References:</b>
 *RTP: (RFC1889) <a href="http://www.ietf.org/internet-drafts/draft-ietf-avt-rtp-new-04.txt">
 *http://www.ietf.org/internet-drafts/draft-ietf-avt-rtp-new-04.txt</a>
 *
 */

public class RtpHeader extends PduElement
{
   /**
     * this SSRC will be used for all transmitted packets
     */
   private static long mySSRC;  
   /**
     * contains the next sequence number of a transmitted packet
     */
   private static int  nextSequenceNumber;

   static // static initialization block
   {
      // assigns a random integer to the SSRC
      mySSRC        = (long)( Math.random() * UnsignedInt.MAX_INT_VALUE );

      // assign a random integer to the first sequence number
      nextSequenceNumber = (int) ( Math.random() * UnsignedShort.MAX_SHORT_VALUE );
   }


   /**
    * Identifies the version of RTP (2 bits). RFC1889 defines the actual version as two (2).
    *
    */
   public static final int RTP_VERSION = 2;
   
   
   /**
    * Padding is being performed at the DIS protocol level. 
    * Therefore the padding bit is set to zero.
    *
    */
   public static final int RTP_PADDING = 0;
   
   
   /**
    * The extension bit defines if the normal header will be followed by an extension header.
    * Not needed in this application, and so set to zero.
    *
    */
   public static final int RTP_EXTENSION = 0;
   
   
   /**
    * Contains the number of contributing source identifiers in this header. 
    * This is used only by mixers. Set to zero.
    *
    */
   public static final int RTP_CSRC_COUNT = 0;


   /**
    * This bit is used as a marker by a specific profile or application.
    * Not used so far. Set to zero.
    *
    */
   public static final int RTP_MARKER = 0;


   /**
    * We arbitrarily set the payload type number for experimental DIS work to 111.
    * It must be n the dynamic assignment range [96..127].
    * Numbers in this range do not need to be registered. <p>
    * See Section 3 of
    * <a href="http://www.ietf.org/internet-drafts/draft-ietf-avt-profile-new-06.txt">RTP Profile for Audio and Video Conferences with Minimal Control</a>
    * (<i><a href="http://www.ietf.org/internet-drafts/draft-ietf-avt-profile-new-06.txt">http://www.ietf.org/internet-drafts/draft-ietf-avt-profile-new-06.txt</i></a>)
    * and
    * <a href="http://www.isi.edu/in-notes/iana/assignments/rtp-parameters">RTP Payload types (PT) for standard audio and video encodings</a>
    * (<a href="http://www.isi.edu/in-notes/iana/assignments/rtp-parameters"><i>www.isi.edu/in-notes/iana/assignments/rtp-parameters</i></a>).
    * <P>
    * E-mail discussion on this topic is archived at
    * <a href="http://www.web3D.org/WorkingGroups/vrtp/dis-java-vrml/hypermail/1999/9902/0083.html">http://www.web3D.org/WorkingGroups/vrtp/dis-java-vrml/hypermail/1999/9902/0083.html</a>
    */
   public static final int RTP_PAYLOAD_TYPE_FOR_DIS = 111;


   /**
    * Contains the size of the header in bytes (= 12). 
    */
   public static final int sizeOf = 12;  
   
   // the packet sequence number
   private UnsignedShort sequenceNumber;

   // the packet timestamp
   private UnsignedInt   timestamp;

   // the packet Synchronization Source Identifier (SSRC)
   private UnsignedInt   SSRC;
   



   /**
    * Constructor. An empty header is created.
    */
   public RtpHeader()
   {
      sequenceNumber = new UnsignedShort();
      timestamp = new UnsignedInt();
      SSRC      = new UnsignedInt();
     
      return;
   }


   /**
   *  Returns the packet sequence number. 
   *  @return the sequence number as an unsigned short (16 bits)
   */
   public UnsignedShort getSequenceNumber()
   { 
     return (UnsignedShort)sequenceNumber.clone();
   }


   /**
   *  Returns the packet timestamp. 
   *  @return the timestamp as an unsigned int (32 bits)
   */
   public UnsignedInt getTimestamp()
   { 
     return (UnsignedInt)timestamp.clone();
   }


   /**
   *  Returns the packet Syncronization Source Identifier. 
   *  @return the SSRC as an unsigned int (32 bits)
   */
   public UnsignedInt getSSRC()
   { 
      return (UnsignedInt)SSRC.clone();
   }


   /**
   *  Sets the packet sequence number. 
   *  @param pSequenceNumber the sequence number as an unsigned short (16 bits)
   */
   public void setSequenceNumber(UnsignedShort pSequenceNumber)
   { 
      sequenceNumber = pSequenceNumber;
   }


   /**
   *  Sets the packet timestamp. 
   *  @param pTimestamp the timestamp as an unsigned int (32 bits)
   */
    public void setTimestamp(UnsignedInt pTimestamp)
   { 
      timestamp = pTimestamp;
     
   }


   /**
   *  Sets the Syncronization Source Identifier. 
   *  @param pSSRC the SSRC as a unsigned int (32 bits)
   */
    public void setSSRC(UnsignedInt pSSRC)
   { 
      SSRC = pSSRC;
   }


   /**
   *  Increments the sequence number. The RtpHeader class mantains a static variable 
   *  with the next sequence number to be assigned to a packet. This function increments
   *  this variable. If the sequence number will exceed the 16-bit boundary 
   *  it is reset to zero. 
   */
   private void incrementSequenceNumber()
   {
      // if after the increment the sequence number gets longer than 16 bits
      // than it should be set to zero
      ++nextSequenceNumber;
      if( nextSequenceNumber > UnsignedShort.MAX_SHORT_VALUE ){
            nextSequenceNumber = 0;
      }

      return;
   }


   /**
   *  Prepares the header for sending. Assigns the sequential number from a static variable,
   *  takes the timestamp from the DIS pdu and sets the SSRC. 
   *  @param pdu the DIS pdu that will be transmitted
   */
    public void prepareToSend( ProtocolDataUnit pdu )
   {
      // assigns a sequence number (the next sequence number kept by a static variable)
      sequenceNumber = new UnsignedShort( nextSequenceNumber );

      // increments the next sequence number variable
      incrementSequenceNumber();

      // assigns as a timestamp the Dis-Java-Vrml timestamp
      timestamp = pdu.getTimestamp();

      // assigns the common SSRC
      SSRC = new UnsignedInt( mySSRC );

      return;
   }


   /**
   *  Returns the size of the header.  
   *  @return the header size
   */
    public int length()
   { 
      return RtpHeader.sizeOf;          
   }


   /**
   *  Serializes the header into a DataOutputStream. 
   *  @param outputStream the stream that will receive the serialized header.
   */
    public void  serialize(DataOutputStream outputStream)
   {
      UnsignedByte  firstByte = new UnsignedByte( (RTP_VERSION * 64) + 
             (RTP_PADDING * 32) + (RTP_EXTENSION * 16) + RTP_CSRC_COUNT );      
      UnsignedByte  secondByte = new UnsignedByte( (RTP_MARKER * 128 ) +
                   RTP_PAYLOAD_TYPE_FOR_DIS );      
     
      firstByte.serialize(outputStream);
      secondByte.serialize(outputStream);
      sequenceNumber.serialize(outputStream);
      timestamp.serialize(outputStream);
      SSRC.serialize(outputStream);

      return;
   }


   /**
   *  Fills the header contents with data from a DataInputStream 
   *  @param inputStream the stream which contains the header.
   */
    public void  deSerialize(DataInputStream inputStream)
   {
      UnsignedByte firstByte = new UnsignedByte(0); 
      UnsignedByte secondByte = new UnsignedByte(0);

      firstByte.deSerialize(inputStream);
      secondByte.deSerialize(inputStream);
      sequenceNumber.deSerialize(inputStream);
      timestamp.deSerialize(inputStream);
      SSRC.deSerialize(inputStream);
    
      return;
   }


   /**
   *  Makes deep copies of all the instance variables. 
   *  
   */
    public Object clone()
   {
      RtpHeader newHeader = (RtpHeader)super.clone();
 
      newHeader.setSequenceNumber(this.getSequenceNumber());
      newHeader.setTimestamp(this.getTimestamp());
      newHeader.setSSRC(this.getSSRC());
      
      return newHeader;
   }


   /**
   *  Prints internal values for debugging. 
   *  
   */
   public void printValues(int indentLevel, PrintStream printStream)
   {
      StringBuffer  buf = ProtocolDataUnit.getPaddingOfLength(indentLevel);

      printStream.println(buf + "sequenceNumber: " + sequenceNumber.intValue());
      printStream.println(buf + "timestamp: " + timestamp.longValue());
      printStream.println(buf + "SSRC: " + SSRC.longValue());

      return;
   }


} // end of class RtpHeader



