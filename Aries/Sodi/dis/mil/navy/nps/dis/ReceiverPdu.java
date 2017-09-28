/*
 File:		ReceiverPdu.java
 CVS Info:	$Id: ReceiverPdu.java,v 1.0 2000/06/07 18:00:00 laflam Exp $
 Compiler:	jdk 1.3
 */




package mil.navy.nps.dis;                // package for Naval Postgraduate School DIS Libaray

import mil.navy.nps.util.*;              // General-purpose utilities
import mil.navy.nps.disEnumerations.*;   // Enumerations for DIS
import java.lang.*;
import java.util.*;                      // utility stuff we need
import java.io.*;                        // input/output for serialization

/**
  * Receiver PDU for DIS .
  *
  *@version 1.0
  *@author <a href="mailto:dave@laflam.net">David W. Laflam</a> (<a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>)
  *<br>
  *@author <a href="mailto:brutzman@nps.navy.mil">Don Brutzman</a> (<a href="http://web.nps.navy.mil/~brutzman">http://web.nps.navy.mil/~brutzman</a>)
  *
  *<dt><b>Location:</b>
  *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/ReceiverPdu.java">
  *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/ReceiverPdu.java</a>
  *
  *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/ReceiverPdu.java">
  *  ~/mil/navy/nps/dis/ReceiverPdu.java</a>
  *
  *<dt><b>Hierarchy Diagram:</b>
  *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
  *
  *<dt><b>Summary:</b>
  *<dd>The receiving radio this is the the incomming traffic .
  *
  *<dt><b>Explanation:</b>
  *<dd>The ReceiverPdu denotes the reciving of a transmission from a radio.
  *  It inherits the header information from ProtocolDataUnit,
  *  an abstract class that contains assorted protocol information.
  *  It implements the IDs of what's transmitting a signal.
  *  <P>
  *
  *  As with other PDUs, it knows how to serialize and deserialize itself
  *  from the wire. It also knows how to clone itself, and knows how to
  *  calculate its size when sent to the wire.<P>
  *
  *<dt><b>History:</b>
  *<dd>		15May2000	/Dave Laflam		/New
  *<dd>		17DAug00 	/Dave Laflam		/Added toString method
  *
  *<dt><b>References:</b>
  *<dd>		DIS Data Dictionary:
  *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/fd.htm">Receiver PDU (local) and
  *    		<A href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/fd.htm">Receiver PDU (SISO)</a>
  *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">
  *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
  *<dd>		DIS specification : IEEE 1278.1-1995, Section 5.3.8.3
  *
  *<dt><b>Note:</b>
  *<dd>   No accessor methods to nested records is provided.
  *
  * (NOTE: the ProtocolDataUnit will keep all the header information)
  *@see EntityID
  *@see PduElement
  *@see ProtocolDataUnit 
  *@see RadioCommunicationsFamily
  *@see RadioCommunicationsPduScriptNode
  *@see SerializationInterface
  *@see SignalPdu
  *@see TransmitterPdu
  */
public class ReceiverPdu extends RadioCommunicationsFamily
{

/**
  * entityID: Entity Identification - This field shall identify the entity that is controlling the radio transmission. 
  *The source entity may either represent the radio itself or represent an entity (such as a vehicle) 
  *that contains the radio. This field shall be represented by an Entity Identifier record (see 5.2.14).
  * 
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/2c.htm">Event Identifier (local)</A> and
  *  	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/2c.htm">Event Identifier Record</a>
  * </dl>
  */ 
protected EntityID          entityID;             // ID of entity that's doing the shooting

/**
  * Radio Identification - This field shall identify a particular radio within a given entity. 
  * Radio IDs shall be assigned sequentially to the radios within an entity, starting with 
  * Radio ID 1. The combination of Entity ID and Radio ID uniquely identifies a radio within 
  * a simulation exercise. The Radio ID field shall be represented by a 16-bit unsigned integer.
  * 
  * <dl>
  * <dt><b>Value:</b>
  * <dd>If the intended target is unknown, this field shall contain TARGET_ID_ UNKNOWN.
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  *  	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/2c.htm">Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/2c.htm">Event Identifier Record</a>
  * </dl>
  */
protected UnsignedShort          radioID;   // 16-bit unsigned integer
                                            // short 16 bit and ints are 32 bit
	
	
/**
  * Receiver State. This field shall indicate the state of the receiver, 
  * which shall either be idle or active, and shall be represented by 
  * a 16-bit enumeration (see Section 9 of EBV-DOC).
  * 
  * <dl>
  * <dt><b>Value:</b>
  * <dd>
  *  
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/2c.htm">Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/2c.htm">Event Identifier Record</a>
  * </dl>
  */
protected UnsignedShort          receiverState;     // 16-bit enumeration         
                                               


/**
  * Padding: is never set or read but they are serialized and de-serialized. 
  * The values are initially set to zero and then they are passed back and forth 
  * to insure the proper placement of the bits and in the fields. 
  * 
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm">Event Identifier Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
  * </dl>
  */
protected UnsignedShort           padding;    //16 bits unused             



/**
  * Received Power. This field shall indicate the radio frequency power received, 
  * after applying any propagation loss and antenna gain, and shall be represented 
  * by a 32-bit foating point number in units of decibel milliwatts.
  * 
  * <dl>
  * <dt><b>Value:</b>
  * <dd>
  * This field shall be represented by a 32-bit unsigned integer
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/7d.htm">????? Index Field (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/7d.htm">   Index Field</a>
  * </dl>
  */
protected float        receiverPower;           // 32-bit ﬂoating point



/**
  * Transmitter Entity ID. This ﬁeld shall identify the entity that is the source of the 
  * transmission that is currently being received. The selection of the received transmitter 
  * depends on the characteristics and state of the simulated receiver. This ﬁeld shall be 
  * represented by an Entity Identiﬁer record (see5.2.14).
  <dt><b>Value:</b>
  * <dd>This field shall contain the following:
  *     a. Site - 16 bit unsigned integer
  *     b. Application - 16 bit unsigned integer
  *     c. Entity - 16 bit unsigned integer
  * 
  *(note short 16 bit and ints are 32 bit) 
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/19.htm">receiverPower Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/19.htm">World Coordinate Record</a>
  * </dl>
  */    
protected EntityID     transmitterEntityID;        



/**
  * Transmitter Radio ID. This field shall identify the particular radio within 
  * the entity cited in item f)that is the source of the radio transmission(Who the message is from). 
  * The Transmitter Radio ID shall be represented by a 16-bit unsigned integer.
  * 
  * (note short 16 bit and ints are 32 bit) 
  * <dl>
  * <dt><b>Reference:</b>
  * <dd>  DIS Data Dictionary:
  * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/7e.htm">Burst Descriptor Record (local)</A> and
  * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/7e.htm">Burst Descriptor Record</a>
  * </dl>
  */
protected UnsignedShort   transmitterRadioID;  // 16-bit unsigned integer, (short 16 bit and ints are 32 bit)
                                                   


/**
  *Constant value--size of ReceiverPDU with header. Here:
  *<code>sizeOf = 288 bytes</code>
  */
public final static int     sizeOf = 288;       // is this the PDU Size Total   
                                                // size of object as written to wire

//=============================================================================

/**
 *Default constructor
 * - creates entityID, radioID, receiverState, padding, reciverPower, transmitterEntityID,
 * and transmitterRadioID
 *
 * - fills with zeros for all values of the following parameters:
 *  padding
 */

public ReceiverPdu()
{
   super.setPduType(PduTypeField.RECEIVER);      // PDU Header
   entityID  = new EntityID();                   // Entity ID
   radioID = new UnsignedShort(0);               //16-bit unsigned integer
   receiverState = new UnsignedShort(0);         //16-bit enumeration
   padding = new UnsignedShort(0);               //16 bits unused
   receiverPower = 0.0f;                         //32-bit floating point
   transmitterEntityID = new EntityID();         // Transmitter Entity ID
   transmitterRadioID = new UnsignedShort(0);    //16-bit unsigned integer
  
   return;
}// end public ReceiverPdu()


/**
 * Make a copy of the object. This requires a deep copy, so we don't have two
 * objects sharing pointers to the same data.
 * @return a new Receiver PDU entity
 */
public Object clone()
{
 ReceiverPdu    newReceiverPdu = (ReceiverPdu)super.clone(); // this will inherit from the super class 

        // Header  
 	newReceiverPdu.setEntityID(this.getEntityID());
 	newReceiverPdu.setRadioID(this.getRadioID());
 	newReceiverPdu.setReceiverState(this.getReceiverState());
        // Padding
 	newReceiverPdu.setReceiverPower(this.getReceiverPower());
  	newReceiverPdu.setTransmitterEntityID(this.getTransmitterEntityID());
	newReceiverPdu.setTransmitterRadioID(this.getTransmitterRadioID());

 
 return newReceiverPdu;  // Return a new receiver pdu 
}// end public Object clone()


/**
* Serialize and write out the output stream, order is important here since
* it needs to conform to the DIS standard
* @exception RuntimeException when IO error occurs.
*/
public void serialize(DataOutputStream outputStream)
{
    super.serialize(outputStream);      // write out header info

    try
    {

        //header
        entityID.serialize(outputStream);
        radioID.serialize(outputStream);
        receiverState.serialize(outputStream);
        padding.serialize(outputStream);
        outputStream.writeFloat(receiverPower);   // since this is a primitive value
        transmitterEntityID.serialize(outputStream);
        transmitterRadioID.serialize(outputStream);
    }
    catch (IOException ioError)
    {
        throw new
            RuntimeException("Exception in ReceiverPdu.serialize, error writing to wire.");
    }

    return;
}//public void serialize()


/**
* Deserialize the input stream, and order is important here, since we need to
* read in the same order as specified by the DIS standard
* @exception RuntimeException when IO error occurs.
*/
public void deSerialize(DataInputStream inputStream)
{
    super.deSerialize(inputStream);     // read in all the header info

    try
    {
        // header     
        entityID.deSerialize(inputStream);
        radioID.deSerialize(inputStream);
        receiverState.deSerialize(inputStream);
        padding.deSerialize(inputStream);
        receiverPower = inputStream.readFloat();   // since this is a primitive value no methods
        transmitterEntityID.deSerialize(inputStream);
        transmitterRadioID.deSerialize(inputStream);
    }
    catch (IOException ioError)
    {
            throw new
                RuntimeException("Exception in ReceiverPdu.deSerialize, error reading from wire.");
    }
}//public void deSerialize()


/**
 * Returns the length of the entity
 * @return an integer length of the entity
 */
public int length()
{
    return sizeOf;          // EntityTypes are this long, always.  This is the 288
}//end public int length()


/**
 * Returns the PDU name - Receiver PDU
 * @return a string "Receiver PDU"
 */
public String pduName()
{
  return new String("Receiver PDU");
}// end public String pduName()


/**
 * Print the values of the following object out, with correct level of
 * indentation on the page.
 * radioID,receiverState , receiverPower, entityID, transmitterRadioID.
 * 
 */
public void printValues(int indentLevel, PrintStream printStream)
{

  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println();
    printStream.println("Receiver PDU-");

    // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
    // so the header info will be indented a bit less.

    // this does not print out PDU Header and entity ID ?  	

    if(superclassIndent > 0)
      superclassIndent -= 1;

    super.printValues(superclassIndent, printStream); // header 
    entityID.printValues(indentLevel, printStream);   // entity
    printStream.println(indent + "radioID: " + radioID);  // print the primitive type
    printStream.println(indent + "receiverState: " + receiverState);  // print the primitive type

    // padding is just zeros    

    printStream.println(indent + "receiverPower: " + receiverPower);  // print the primitive type
    transmitterEntityID.printValues(indentLevel, printStream);
    printStream.println(indent + "transmitterRadioID: " + transmitterRadioID);  // print the primitive type   
    
    return;
}//end public void printValues



//Accessor methods(set and get methods) 

/**
 * Gets reciver entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the reciver entity ID
 */
public EntityID getEntityID()
{
	return (EntityID)entityID.clone();
} 


/**
 * Sets reciver entity ID
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pEntityID the reciver entity ID
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
 * Gets Radio entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the Radio entity ID
 */
public UnsignedShort getRadioID()
{
	return (UnsignedShort)radioID.clone();
}

/**
 * Sets Radio entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pRadioID target entity ID value
 */
public void setRadioID(UnsignedShort pRadioID)
{
	radioID = pRadioID;
}

/**
 * Gets the ReceiverStateID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of a ReceiverStateID
 */
public UnsignedShort getReceiverState()
{
	return (UnsignedShort)receiverState.clone();
}

/**
 * Sets the ReceiverState ID
 * @param pReceiverState a ReceiverState ID
 */
public void setReceiverState(UnsignedShort pReceiverState)
{
	receiverState = pReceiverState;
}


// NO need for a get and set for the padding //DWL


/**
 * Gets the getReceiverPower
 * @param receiverPower a getReceiverPower
 */

public float getReceiverPower()
{
	return (float) receiverPower;
}

/**
 * Sets the ReceiverPower
 * @param pReceiverPower a ReceiverPower
 */
public void setReceiverPower(float pReceiverPower)
{
	receiverPower = pReceiverPower;
}

/**
 * Gets the Transmitter EntityID.
 * The EntityID shall identify the entity that is the source of the transmission that is currently being received.
 * The selection of the received transmitter depends on the characteristics and state of the simulated receiver.
 * This field shall be identified by an Entity Identifier record (EntityID) (see paragraph 5.2.14).
 *
 * @return a clone of an EntityID
 */
public EntityID getTransmitterEntityID()
{
	return (EntityID)transmitterEntityID.clone();
}


/**
 * Sets the Transmitter EntityID.
 * The EntityID shall identify the entity that is the source of the transmission that is currently being received.
 * The selection of the received transmitter depends on the characteristics and state of the simulated receiver.
 * This field shall be identified by an Entity Identifier record (EntityID) (see paragraph 5.2.14).
 * @param pEntityID entityID value
 */	
public void setTransmitterEntityID(EntityID pEntityID)
{
	transmitterEntityID = pEntityID;
}

/**
 * Gets the TransmitterRadioID
 * @return a clone an unsigned short value of TransmitterRadioID
 */
public UnsignedShort getTransmitterRadioID()
{
	return (UnsignedShort)transmitterRadioID.clone();
}

/**
 * Sets the TransmitterRadioID
 * @param pTransmitterRadioID an unsigned short value for TransmitterRadioID
 */
public void setTransmitterRadioID(UnsignedShort pTransmitterRadioID)
{
	transmitterRadioID = pTransmitterRadioID;
}
/**
  * String toString
  * Used for debuging  
  * System.out.println("Receiver Object.  = " + receiver); 
  * This print out all values for the fields for the NEW Receiver object
  */ 
 
 public String toString ()
 {
 	String result;
 	result = "\nEntityID = " + entityID + " \nRadioID = " + radioID 
 	          + "\nReceiverState = " + receiverState 
 	          + "\nPadding = " + padding + "\nReceiverPower = " + receiverPower  
 	          + "\nTransmitterEntityID = " + transmitterEntityID 
 	          + "\nTransmitterRadioID = " + transmitterRadioID; 
 	
 return result ; 
 }



} // end of class ReceiverPdu.java

