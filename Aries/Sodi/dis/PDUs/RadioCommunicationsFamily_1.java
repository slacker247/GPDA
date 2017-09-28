/*
 File:		RadioCommunicationsFamily.java
 CVS Info:	$Id: RadioCommunicationsFamily.java,v 1.0 2000/06/07 18:00:00 laflam Exp $
 Compiler:	jdk 1.3
 */


package PDUs;                // package for Naval Postgraduate School DIS Library
import mil.navy.nps.util.*;              // General-purpose utilities
import disEnumerations.*;   // Enumerations for DIS
import java.lang.*;                      // Native Java Language Stuff
import java.util.*;                      // utility stuff 
import java.io.*;                        // input/output for serialization
/**
 * Abstract (uninstantiated) parent class for RadioCommunicationsFamily.
 *
 *@version 1.0
 *@author <a> David W. Laflam</a> (<a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>)
 *@author <a href="mailto:Brutzman @nps.navy.mil"> Don Brutzman</a> (<a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>)
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>)
  *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/RadioCommunicationsFamily.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/RadioCommunicationsFamily.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/RadioCommunicationsFamily.java">
 *  ~/mil/navy/nps/dis/RadioCommunicationsFamily.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>The RadioCommunicationsFamily is an abstract class, for all Radio Communications 
 *  Protocol Family PDUs ( TransmitterPDU.java, ReceiverPDU.Java, and SignaPDU.java). 
 *
 *<dt><b>Explanation:</b>
 *<dd>It encapsulates the specific header for the RadioCommunicationsFamily PDUs 
 *  which includes the PDU header, the originating entity identity, and
 *  The advantage is that, we define methods to deal with this header, 
 *  and all the classes inherit from it, avoiding
 *  rewriting them a lot of times.
 *
 *<dt><b>History:</b>
 *<dd>		6 AUG 2000 David W. Laflam	/New
 *
 *
 *<dt><b>References:</b>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 5.3.8 Radio Communications protocol (RCP) family (page 112)
 *
 * IEEE 1278.1-1995 paragraph 5.2.2.4 page  69, PDU Header Record
 * IEEE 1278.1-1995 paragraph 5.3.8.1 page 112, Transmitter PDU
 * IEEE 1278.1-1995 paragraph 5.3.8.2 page 115, Signal PDU   
 * IEEE 1278.1-1995 paragraph 5.3.8.3 page 117, Receiver PDU 
 *
 *@see ProtocolDataUnit
 *@see PduElement 
 *@see SerializationInterface
 *@see TransmitterPdu
 *@see ReceiverPdu
 *@see SignalPdu
 */

public class RadioCommunicationsFamily_1 extends ProtocolDataUnit
{
				  
    protected EntityID  entityID;     // ID of entity that's doing the communicating
    protected UnsignedShort radioID;  // ID of entity that is what radio they are using 

    /**
     *Constant value--size of RadioCommunicationsFamily with header. Here:
     *<code>sizeOf = 288 ????????????????? bytes</code>
     */
    public final static int     sizeOf = 288;       // is this the PDU Size Total DWL         
                                                    // size of object as written to wire



/**
 *Default constructor
 * - creates entityID, radioID
 * - fills with zeros for all values of the following parameters:
 *   entityID, radioID.
 */
    public RadioCommunicationsFamily_1()
{
	
   super.setPduType(PduTypeField.RECEIVER);

   entityID  = new EntityID();
   radioID = new UnsignedShort(0);

   return;
}

// Methods 

/**
 * Make a copy of the object. This requires a deep copy, so we don't have two
 * objects sharing pointers to the same data.
 * @return a new RadioCommunicationsFamily PDU entity
 */
public Object clone()
{

 RadioCommunicationsFamily   newRadioCommunicationsFamily = (RadioCommunicationsFamily)super.clone(); 
 // So this will inherit from the super class 

 newRadioCommunicationsFamily.setEntityID(this.getEntityID());
 newRadioCommunicationsFamily.setRadioID(this.getRadioID());

 return newRadioCommunicationsFamily;
}


/**
 * Serialize and write out the output stream, order is important here since
 * it needs to conform to the DIS standard
 * @exception RuntimeException when IO error occurs.
 */
public void serialize(DataOutputStream outputStream)
{

    super.serialize(outputStream);      // To write out header info

    try
    {
        entityID.serialize(outputStream);
        radioID.serialize(outputStream);
    }
    catch (Exception someError)
    {
        throw new
            RuntimeException("Exception in RadioCommunicationsFamily. Error writing to wire ." + someError);
    }

    return;
}


/**
 * Deserialize the input stream, and order is important here, since we need to
 * read in the same order as specified by the DIS standard
 * @exception RuntimeException when IO error occurs.
 */
public void deSerialize(DataInputStream inputStream)
{
    super.deSerialize(inputStream);     //To read in all the header info

    try
    {
        entityID.deSerialize(inputStream);
        radioID.deSerialize(inputStream);
     }
    catch (Exception someError)
        {
            throw new
                RuntimeException("Exception in RadioCommunicationsFamily. Error reading from wire." + someError );
        }
}


/**
 * Returns the length of the entity
 * @return an integer length of the entity
 */
public int length()
{
    return sizeOf;          // EntityTypes are this long, always.  This is the 288
}

/**
 * Returns the PDU name - RadioCommunicationsFamily
 * @return a string "RadioCommunicationsFamily"
 */
public String pduName()
{
  return new String("RadioCommunicationsFamily");
}


/**
 * Print the values of the following object out, with correct level of
 * indentation on the page for entityID and  radioID.
 * 
 */
public void printValues(int indentLevel, PrintStream printStream)
{

  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println();
    printStream.println("RadioCommunicationsFamily PDU-");

    // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
    // so the header info will be indented a bit less.

    if(superclassIndent > 0)
      superclassIndent -= 1;

    super.printValues(superclassIndent, printStream);
    entityID.printValues(indentLevel, printStream);
    
    return;
}


//Accessor (Set and Get methods) 

/**
 * Gets RadioCommunicationProtocolFamily entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the firing entity ID
 */
public EntityID getEntityID()
{
	return (EntityID)entityID.clone();
}

/**
 * Sets RadioCommunicationProtocolFamily entity ID
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pEntityID the firing entity ID
 */
public void setEntityID(EntityID pEntityID)
{
	entityID = pEntityID;
}

/**
 * Gets RadioCommunicationProtocolFamily entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the radioID entity ID
 */
 
 
public UnsignedShort getRadioID()
{
	return (UnsignedShort)radioID.clone();
}

/**
 * Sets RadioID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pTargetEntityID target entity ID value
 */
public void setRadioID(UnsignedShort pRadioID)
{
	radioID = pRadioID;
}


} // end of class RadioCommunicationsFamily

