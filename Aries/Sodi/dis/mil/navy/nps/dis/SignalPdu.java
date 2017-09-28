/*
 File:		SignalPdu.java
 CVS Info:	$Id: SignalPdu.java,v 1.0 2000/06/07 18:00:00 laflam Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;                // package for Naval Postgraduate School DIS Libaray
import mil.navy.nps.util.*;              // General-purpose utilities
import mil.navy.nps.disEnumerations.*;   // Enumerations for DIS
import java.lang.*;                      //
import java.util.*;                      // utility stuff we need
import java.io.*;                        // input/output for serialization


/**
 * Signal PDU for DIS.
 *
 *@version 1.0
 *@author <a href="mailto:dave@laflam.net">David W. Laflam</a> (<a href="http://wwww.laflam.net/Dave">http://wwww.laflam.net/dave</a>)
 *<br>
 *@author <a href="mailto:brutzman@nps.navy.mil">Don Brutzman</a> (<a href="http://web.nps.navy.mil/~brutzman">http://web.nps.navy.mil/~brutzman</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/SignalPdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/SignalPdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/SignalPdu.java">
 *  ~/mil/navy/nps/dis/SignalPdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>The actual transmission of voice, audio or other data shall be communicated by issuing a Signal PDU.
 *
 *<dt><b>Explanation:</b>
 *<dd>The Signal pdu denotes the reciving of a transmission from a radio.
 *  It inherits the header information from ProtocolDataUnit,
 *  an abstract class that contains assorted protocol information.
 *  It implements the IDs of what's transmitting a signalPDU.
 *  <P>
 *
 *  As with other PDUs, it knows how to serialize and deserialize itself
 *  from the wire. It also knows how to clone itself, and knows how to
 *  calculate its size when sent to the wire.<P>
 *
 *<dt><b>History:</b>
 *<dd>		15 May  2000
 *<dd>		17DAug00 	/Dave Laflam		/Added toString method	
 *<dd>		1Sep00 	/Don Brutzman		/Added extra data elements
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary:
 *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/f4.htm">Signal PDU (local) and
 *    		<A href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/f4.htm">Signal PDU (SISO)</a>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">
 *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1-1995, Section 5.3.8.2
 *
 *
 *@see ProtocolDataUnit
 *@see PduElement
 *@see SerializationInterface
 *@see RadioCommunicationsFamily
 *@see ReceiverPdu
 *@see TransmitterPdu
 *@see RadioCommunicationsPduScriptNode
 *@see mil.navy.nps.disEnumerations.TDLTypefield
 *
 */


public class SignalPdu extends RadioCommunicationsFamily
{
    

/**
  * 
  *entity ID: This field shall identify the entity that is the source of the radio transmission.
  *The source entity may either represent the radio itself or represent an entity (such as a vehicle)
  *that contains the radio.
  *This field shall be represented by an Entity Identifier record (see 5.2.14).
  * 
  * <dl>
  * <dt><b>Value:</b>
  * <dd>If the intended Entity ID is unknown, this field shall contain Entity ID_UNKNOWN.
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  *  	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/2c.htm">
  *          Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/2c.htm">Event Identifier Record</a>
  * </dl>
  */
protected EntityID    entityID;     // (Site , Applications , Entity) are all 16 Bit Unsigned Int
                                    // ID of entity that's doing the Transmission of the Signal




/**
  * radioID. This field shall identify a particular radio within a given entity. 
  * This field shall be represented by a 16-bit unsigned integer. The Entity ID, 
  * Radio ID pair associates each Signal PDU with the preceding Transmitter PDU 
  * that contains the same Entity ID, Radio ID pair. The combination of Entity ID 
  * and Radio ID uniquely identiﬁes a particular radio within a simulation exercise. 
  * Pg 115 (5.3.8.2)
  *<dl>
  *<dt><b>Reference:</b>
  *<dd>  DIS Data Dictionary: 
  *	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm">Event Identifier Record (local)</A> and
  *	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  *</dl>
  */
protected UnsignedShort          radioID;   //16-bit unsigned integer 
  

/**
  * encodingScheme: This field shall specify the encoding used in the Data ﬁeld of this PDU. 
  * The encoding scheme shall be composed of a 2-bit ﬁeld specifying the encoding class and 
  * a 14-bit field specifying either the encoding type, or the number of TDL messages contained 
  * in this Signal PDU(see table 57 pg 115).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */
	
protected UnsignedShort          encodingScheme;    //16 Bit Enumeration
 

/**
  * tdlType:  This field shall specify the TDL Type as a 16-bit enumeration ﬁeld when the encoding
  * class is the raw binary, audio, application-speciﬁc, or database index representation of a TDL
  * message. When the Data ﬁeld is not representing a TDL Message, this ﬁeld shall be set to zero (see
  * Section 9 of EBV-DOC for enumeration of the TDL Type field).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */
protected UnsignedShort tdlType ; // 16 bit enumeration 

  
 /**
  * sampleRate:  This field shall specify either the sample rate in samples per second if the encoding
  * class is encoded audio or, the data rate in bits per second for data transmissions. If the encoding class
  * is database index, this ﬁeld shall be zero. This ﬁeld shall be represented by a 32-bit unsigned integer.
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedInt  sampleRate;  // 32 bit integer     //look in UnsignedInt.java
   
  
 /**
  * dataLength:  This field shall specify the number of bits of digital voice audio or digital data being
  * sent in this Signal PDU, and shall be represented by a 16-bit unsigned integer. If the encoding class
  * is database index, the Data Length field shall contain the value 96.
  *
  * Currently hardwired to support 11 data elements.
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedShort dataLength;  // 16 bit integer    // look in UnsignedShort.java
     
    
/**
  * samples:  This field shall specify the number of samples in this PDU, and shall be represented by a
  * 16-bit unsigned integer. If the encoding class is not encoded audio, this ﬁeld shall be zero.
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedShort samples;  // 16 bit integer       // look in UnsignedShort.java
         
  
/**
  * data00:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data00;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data01:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data01;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data02:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data02;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data03:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data03;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data04:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data04;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data05:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data05;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data06:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data06;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data07:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data07;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data08:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data08;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data09:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data09;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  * data10:  This field shall specify the audio or digital data conveyed by the radio transmission. The
  * interpretation of each Data field depends on the value of the encoding scheme [see 5.3.8.2 item d)]
  * and TDL Type [see 5.3.8.2 item e)] fields (page 116).
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm"> Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */  
protected UnsignedByte data10;  // 8 bit unsigned integer  // look in UnsignedByte.java
   
   
   
   
/**
  *Constant value--size of Fire PDU with header. Here:
  *<code>sizeOf = 256 bytes</code>
  * 
  * Total Signal Size = 256 + Data Length + 0 to 31, padding bits to increase the total 
  * Signal Size to a multiple of 32 Bits.
  * Current Size 264 bits
  */
  
public final static int     sizeOf = 264 + 9*8;       // is this the PDU Size Total or is this number in the header   
                                                // size of object as written to wire




/**
 *Default constructor
 * - creates  entityID, radioID, encodingScheme,
 *   tdlType, sampleRate, dataLength, samples, data00
 *
 * - fills with zeros for all values of the following parameters:
 * 
 */
	 
public SignalPdu()
{
   super.setPduType(PduTypeField.SIGNAL);      // inherited from the super class
   entityID  = new EntityID();                 // 3 field (site,app,enity) 16-bit unsigned integer
   radioID = new UnsignedShort(0);             // 16-bit unsigned integer
   encodingScheme = new UnsignedShort(0);      // 16-bit enumeration
   tdlType = new UnsignedShort(0);             // 16-bit enumeration
   sampleRate = new UnsignedInt(0);            // 32-bit integer
   dataLength = new UnsignedShort(11);         // 16-bit integer
   samples = new UnsignedShort(0);            // 16-bit integer
   data00 = new UnsignedByte(0);                // 8-bit unsigned integer
   data01 = new UnsignedByte(0);                // 8-bit unsigned integer
   data02 = new UnsignedByte(0);                // 8-bit unsigned integer
   data03 = new UnsignedByte(0);                // 8-bit unsigned integer
   data04 = new UnsignedByte(0);                // 8-bit unsigned integer
   data05 = new UnsignedByte(0);                // 8-bit unsigned integer
   data06 = new UnsignedByte(0);                // 8-bit unsigned integer
   data07 = new UnsignedByte(0);                // 8-bit unsigned integer
   data08 = new UnsignedByte(0);                // 8-bit unsigned integer
   data09 = new UnsignedByte(0);                // 8-bit unsigned integer
   data10 = new UnsignedByte(0);                // 8-bit unsigned integer
   
   return;
} // end public SignalPdu()


/**
 * Make a copy of the object. This requires a deep copy, so we don't have two
 * objects sharing pointers to the same data.
 * @return a new Signal PDU entity
 */


public Object clone()
{

 SignalPdu    newSignalPdu = (SignalPdu)super.clone(); // this will inherit from the super class //dwl

 newSignalPdu.setEntityID(this.getEntityID());
 newSignalPdu.setRadioID(this.getRadioID());
 newSignalPdu.setEncodingScheme(this.getEncodingScheme());
 newSignalPdu.setTdlType(this.getTdlType());
 newSignalPdu.setSampleRate(this.getSampleRate());
 newSignalPdu.setDataLength(this.getDataLength());        
 newSignalPdu.setSamples(this.getSamples()); 
 newSignalPdu.setData00(this.getData00());
 newSignalPdu.setData01(this.getData01());
 newSignalPdu.setData02(this.getData02());
 newSignalPdu.setData03(this.getData03());
 newSignalPdu.setData04(this.getData04());
 newSignalPdu.setData05(this.getData05());
 newSignalPdu.setData06(this.getData06());
 newSignalPdu.setData07(this.getData07());
 newSignalPdu.setData08(this.getData08());
 newSignalPdu.setData09(this.getData09());
 newSignalPdu.setData10(this.getData10());

 return newSignalPdu;
} // end public Object clone()



/**
 * Serialize and write out the output stream, order is important here since
 * it needs to conform to the DIS standard
 * @exception RuntimeException when IO error occurs.
 */
 
public void serialize(DataOutputStream outputStream)
{
    super.serialize(outputStream);      // write out header info

//Note: you do not need a try and catch in this method, these are in the entityId.java 
//which has it in site.java go back up the tree

 //   try
 //   {
        entityID.serialize(outputStream);
        radioID.serialize(outputStream);
        encodingScheme.serialize(outputStream);
        tdlType.serialize(outputStream);
        sampleRate.serialize(outputStream);
	dataLength.serialize(outputStream);
	samples.serialize(outputStream);
	data00.serialize(outputStream);
	data01.serialize(outputStream);
	data02.serialize(outputStream);
	data03.serialize(outputStream);
	data04.serialize(outputStream);
	data05.serialize(outputStream);
	data06.serialize(outputStream);
	data07.serialize(outputStream);
	data08.serialize(outputStream);
	data09.serialize(outputStream);
	data10.serialize(outputStream);

       // padding.serialize(outputStream);
       // outputStream.writeFloat(receiverPower);   	// since this is a primitive value
      
//    }
//    catch (IOException ioError)
//    {
//        throw new
//            RuntimeException("Exception in SignalPdu.serialize, error writing to wire.");
//    }
    return;
}// end public void serialize()



/**
 * Deserialize the input stream, and order is important here, since we need to
 * read in the same order as specified by the DIS standard
 * @exception RuntimeException when IO error occurs.
 */

public void deSerialize(DataInputStream inputStream)
{
    super.deSerialize(inputStream);     // read in all the header info

//    try
//    {
        entityID.deSerialize(inputStream);
        radioID.deSerialize(inputStream);
        encodingScheme.deSerialize(inputStream);
        tdlType.deSerialize(inputStream);
        sampleRate.deSerialize(inputStream);
	dataLength.deSerialize(inputStream);
	samples.deSerialize(inputStream);
	data00.deSerialize(inputStream);
	data01.deSerialize(inputStream);
	data02.deSerialize(inputStream);
	data03.deSerialize(inputStream);
	data04.deSerialize(inputStream);
	data05.deSerialize(inputStream);
	data06.deSerialize(inputStream);
	data07.deSerialize(inputStream);
	data08.deSerialize(inputStream);
	data09.deSerialize(inputStream);
	data10.deSerialize(inputStream);

     //   receiverPower = inputStream.readFloat();   // since this is a primitive value
     //   tranmitterEntityID.deSerialize(inputStream);
       
//    }
//    catch (IOException ioError)
//    {
//        throw new
//            RuntimeException("Exception in SignalPdu.deSerialize, error reading from wire.");
//    }
}// end public void deSerialize()


/**
 * Returns the length of the entity
 * @return an integer length of the entity
 */

public int length()
{
    return sizeOf;          // EntityTypes are this long, always.  This is the 288
}// end public int length()


/**
 * Returns the PDU name - Signal PDU
 * @return a string "Signal PDU"
 */


public String pduName()
{
  return new String("Signal PDU");
} // end public String pduName()


/**
 * Print the values of the following object out, with correct level of
 * indentation on the page.
 * EntityID, RadioID, Encoding Scheme, TDL Type, Sample Rate, Data Length, Samples, data00
 * 
 */

public void printValues(int indentLevel, PrintStream printStream)
{

  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println();
    printStream.println("Signal PDU-");

    // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
    // so the header info will be indented a bit less.

    if(superclassIndent > 0)
      superclassIndent -= 1;

     super.printValues(superclassIndent, printStream);
     entityID.printValues(indentLevel, printStream);
     printStream.println(indent + "radioID: " +  radioID);  // print the primitive type   
     printStream.println(indent + "encodingScheme: " +  encodingScheme);  // print the primitive type  
     printStream.println(indent + "tdlType: " + tdlType);     
     printStream.println(indent + "sampleRate: " + sampleRate); 
     printStream.println(indent + "dataLength: " + dataLength);     
     printStream.println(indent + "samples: " + samples);   
     printStream.println(indent + "data00: " + data00);   
    
   // tranmitterEntityID.printValues(indentLevel, printStream);
   // printStream.println(indent + "transmitterRadioID: " + transmitterRadioID);  // print the primitive type

    return;
} // end public void printValues()



//Accessor methods ( the Set and Get Methods)

/**
 * Gets entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the firing entity ID
 */
 
public EntityID getEntityID()
{
	return (EntityID)entityID.clone();
} 

/**
 * Sets entity ID
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pFiringEntityID the firing entity ID
 */

public void setEntityID(EntityID pEntityID)
{
	entityID = pEntityID;
}

/**
 *Sets setEntityID(short pSiteID, short pApplicationID, short pEntityID),accessor method.
 *will create an new EntityID = entityID
 *This field shall identify the entity issuing the PDU,
 * and shall be represented by the PDU Header Record (see 5.2.24)
 */
public void setEntityID(short pSiteID, short pApplicationID, short pEntityID)
{ entityID = new EntityID(pSiteID, pApplicationID, pEntityID);
}


/**
 * Gets getRadio ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the target entity ID
 */

public UnsignedShort getRadioID()
{
	return (UnsignedShort)radioID.clone();
}

/**
 * Sets setRadio ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pRadioID target entity ID value
 */

public void setRadioID(UnsignedShort pRadioID)
{
	radioID = pRadioID;
}

/**
 * Gets the EncodingScheme.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of a EncodingScheme
 */

public UnsignedShort getEncodingScheme()
{
		return (UnsignedShort)encodingScheme.clone();
}

/**
 * Sets the EncodingScheme
 * @param pEncodingScheme a EncodingScheme
 */

public void setEncodingScheme(UnsignedShort pEncodingScheme)
{
	encodingScheme = pEncodingScheme;
}


// no need for a get and set for the padding //DWL



/**
 * Sets TdlType.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pTdlType target entity ID value
 */
public void setTdlType (UnsignedShort pTdlType)
{
	tdlType = pTdlType;
}


/**
 * Gets the TdlType.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of a TdlType
 */
public UnsignedShort getTdlType()
{
    return (UnsignedShort)tdlType.clone();
}


/**
 * Sets SampleRate
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pSampleRate target entity ID value
 */
public void setSampleRate(UnsignedInt pSampleRate)
{
		sampleRate = pSampleRate;
}


/**
 * Gets the SampleRate.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of a SampleRate
 */
public UnsignedInt getSampleRate()
{
	return (UnsignedInt)sampleRate.clone();
}


/**
 * Sets DataLength
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pDataLength target entity ID value
 */
public void setDataLength(UnsignedShort pDataLength)
{
	dataLength = pDataLength;
}


/**
 * Gets the DataLength.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of a DataLength
 */
public UnsignedShort getDataLength()
{
	return (UnsignedShort)dataLength.clone();
}


/**
 * Sets Samples
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pSamples target entity ID value
 */
public void setSamples(UnsignedShort pSamples)
{
	samples = pSamples;
}


/**
 * Gets the Samples.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of a Samples
 */
public UnsignedShort getSamples()
{
	return (UnsignedShort)samples.clone();
}


/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData00(UnsignedByte pdata00)
{
	data00 = pdata00;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData01(UnsignedByte pdata01)
{
	data01 = pdata01;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData02(UnsignedByte pdata02)
{
	data02 = pdata02;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData03(UnsignedByte pdata03)
{
	data03 = pdata03;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData04(UnsignedByte pdata04)
{
	data04 = pdata04;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData05(UnsignedByte pdata05)
{
	data05 = pdata05;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData06(UnsignedByte pdata06)
{
	data06 = pdata06;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData07(UnsignedByte pdata07)
{
	data07 = pdata07;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData08(UnsignedByte pdata08)
{
	data08 = pdata08;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData09(UnsignedByte pdata09)
{
	data09 = pdata09;	
}
        

/**
 * accessor method
 * @param raw 8-bit data
 */
public void setData10(UnsignedByte pdata10)
{
	data10 = pdata10;	
}
        

/**
 * accessor method
 * @return a clone of a data00
 */
public UnsignedByte getData00()
{
	return (UnsignedByte)data00.clone();
}

/**
 * accessor method
 * @return a clone of a data01
 */
public UnsignedByte getData01()
{
	return (UnsignedByte)data01.clone();
}

/**
 * accessor method
 * @return a clone of a data02
 */
public UnsignedByte getData02()
{
	return (UnsignedByte)data02.clone();
}

/**
 * accessor method
 * @return a clone of a data03
 */
public UnsignedByte getData03()
{
	return (UnsignedByte)data03.clone();
}

/**
 * accessor method
 * @return a clone of a data04
 */
public UnsignedByte getData04()
{
	return (UnsignedByte)data04.clone();
}

/**
 * accessor method
 * @return a clone of a data05
 */
public UnsignedByte getData05()
{
	return (UnsignedByte)data05.clone();
}

/**
 * accessor method
 * @return a clone of a data06
 */
public UnsignedByte getData06()
{
	return (UnsignedByte)data06.clone();
}

/**
 * accessor method
 * @return a clone of a data07
 */
public UnsignedByte getData07()
{
	return (UnsignedByte)data07.clone();
}

/**
 * accessor method
 * @return a clone of a data08
 */
public UnsignedByte getData08()
{
	return (UnsignedByte)data08.clone();
}

/**
 * accessor method
 * @return a clone of a data09
 */
public UnsignedByte getData09()
{
	return (UnsignedByte)data09.clone();
}

/**
 * accessor method
 * @return a clone of a data10
 */
public UnsignedByte getData10()
{
	return (UnsignedByte)data10.clone();
}

/**
  * String toString
  * Used for debuging  
  * System.out.println("Signal Object.  = " + signal); 
  * This print out all values for the fields for the NEW Signal object
  */ 
 
 public String toString ()
 {
 	String result;
 	result = "\nEntityID = " + entityID + " \nRadioID = " + radioID 
 	          + "\nEncodingScheme = " + encodingScheme 
 	          + "\nTdlType = " + tdlType + "\nSampleRate = " + sampleRate  
 	          + "\nDataLength = " + dataLength 
 	          + "\nSamples = " + samples
 	          + "\ndata = " + data00 + " " + data01 + " " + data02 + " " +
 	          	          data03 + " " + data04 + " " + data05 + " " +
 	          	          data06 + " " + data07 + " " + data08 + " " +
 	          	          data09 + " " + data10; 
 	
 return result ; 
 }



} // end of class signalPdu.java

