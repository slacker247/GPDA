/*
 File:		EntityStatePdu.java
 CVS Info:	$Id: EntityStatePdu.java,v 1.6 1998/02/03 00:09:39 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;                           // package to which we belong

import mil.navy.nps.util.*;                    			// General-purpose utilities
import mil.navy.nps.disEnumerations.*;         			// Enumerations for DIS

import java.util.*;                                 // utility stuff we need
import java.io.*;                                   // input/output for serialization

/**
 * PDU which passes all physics information for an entity.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EntityStatePdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EntityStatePdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/EntityStatePdu.java">
 *  ~/mil/navy/nps/dis/EntityStatePdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>This Class implements an Entity State PDU which in turn stores information about an entity
 * in a DIS format, such as velocity, orientation, dead reckoning parameter etc.
 *
 *<dt><b>Explanation:</b>
 *<dd>The entity state PDU communicates information about an entity, such as
 *  its location, its appearance, and how it appears to others.
 *
 *  This is a subclass of the abstract ProtocolDataUnit class, and
 *  overrides the serialization, deSerialization, and clone methods.
 *  ProtocolDataUnit encompasses the PDU Header information, such as
 *  the protocol version, PDU Type, etc.<P>
 *
 *  This is pretty simple stuff. You can really go to sleep writing it.
 *  It screams out for automation and an algorithm to do all this automatically.
 *  Can you say dial-a-protocol?<P>
 *
 *<dt><b>Note:</b>
 *<dd>We have flattened the Dead Reckoning record.
 * There are some additionnal easy to use  accessor methods to modify in a simple way,
 * fields that are nested in records and that are frequently accessed.
 * Note that we do not define an Entity Marking Record. It is replaced by CharacterSet and by Marking.
 * At last, we have an articulationParameter list whose accessor methods are addArticulationParameter() and
 * articulationParameterCount().
 *
 *<dt><b>History:</b>
 *<dd>		17Oct96	/Don McGregor		/New
 *<dd>		25Oct96	/Don McGregor		/changed full-fledged Float objects to mere float instance variables
 *<dd>		06Mar97	/Don McGregor		/javadoc changes, swapped in entityType and altEntityType in place of
 *						individual fields
 *<dd>		16Apr97	/Don McGregor		/PrintStream passed to printValues
 *<dd>		12Aug97	/Don McGregor		/elaborated printValues
 *			19Aug97	/Don McGregor		/Made "multicast" terminology consistent
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changed access methods: thisVariable() --> getThisVariable()
 *<dd>		27Nov99	/David W. Laflam		/changed access methods: Updated the Java Docs
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/29.htm">Entity State PDU</a>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">
 *	 	http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 5.4.3.1, 4.4.2, 4.4.5
 *
 *@see ProtocolDataUnit
 *@see PduElement
 *@see SerializationInterface
 *@see CollisionPdu
 */
public class EntityStatePdu extends ProtocolDataUnit
{
    /**
     *Debugging flag;
     *when set true, turns on verbose diagnostic, statements that prints in the java console.
     *Default is false.
     */
    public static final boolean DEBUG = false;                  // debugging/trace output

/**
  Debugging output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected static void debug (String pDiagnostic)
{
  if (DEBUG) System.out.println("EntityStatePdu: " + pDiagnostic);
}

/**
  Guaranteed debugging output. Pass in a string, and it gets printed out on the console.
  You can pass in strings such as "foo " + bar.getName().
*/

protected static void trace (String pDiagnostic)
{
  System.out.println("EntityStatePdu: " + pDiagnostic);
}

    /**
     *Entity Identification - This field shall identify the entity issuing the PDU.
     */
    protected EntityID          entityID;                       // triplet that identifies what entity we are

    /**
     *Force Identification - This field shall identify the force to which the issuing entity belongs.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>Enumeration, see references below for further information.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/21.htm">Force ID Field</a>
     *<dd>	See Section 4 in Document IST-CR-93-19
     *</dl>
     */
    protected UnsignedByte      forceID;                        // force to which issuing entity belongs

    // We don't maintain a separate variable for the parameter count; that's handled by simply
    // querying the vector object that holds the articulation parameters. That way the two numbers
    // can't get out of sync.
    //protected UnsignedByte        articulationParameterCount;     // number of articulation parameters

    /**
     *Entity Type - This field shall identify the entity type to be displayed
     *by members of the same force as the issuing entity.
     *
     *<dl>
     *<dt><b>References:</b>
     *<dd>	See Section 6 in Document IST-CR-93-19
     *</dl>
     */
    protected EntityType        entityType;                     // Entity type, (type, domain, country, category, etc.)

    /**
     *Alternate Entity Type - This field shall identify the entity type to be displayed by
     *members of forces other than that of the issuing entity.
     *
     *<dl>
     *<dt><b>References:</b>
     *<dd>	See Section 4 in Document IST-CR-93-19.
     *</dl>
     */
    protected EntityType        alternativeEntityType;                  // entity type displayed by other forces


    /**
     *Entity Linear Velocity - This field shall specify an entity's linear velocity.
     *The coordinate system for an entities' linear velocity depends on the
     *dead reckoning algorithm used.
     */
    protected LinearVelocity          entityLinearVelocity;                       // velocity (x, y, and z together, all floats)


    /**
     *Entity Location - This field shall specify an entity's physical location in the simulated world.
     */
    protected WorldCoordinate     entityLocation;                       // location in world coordinates (doubles, 64 bits each)


    /**
     *Entity Orientation - This field shall specify an entity's orientation.
     */
    protected EulerAngle       entityOrientation;                    // orientation of entity (psi, theta, phi floats)



    // entity appearance

    /**
     *Entity Appearance - This field shall specify the dynamic changes to the entity's
     *appearance attributes.
     *
     *<dl>
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/36.htm">Entity Appearance Record</a>
     *<dd>	See Section 3 in Document IST-CR-93-19.
     *<dd>	DIS specification : IEEE 1278.1, Section 5.3.12
     *</dl>
     */
    protected UnsignedInt       entityAppearance;               // how do we look--bitchin' or thrashed?


    // Dead reckoning parameters, I reckon.

    /**
     *Dead Reckoning Algorithm - This field shall specify the dead reckoning algorithm in use by the issuing
     *entity.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>Enumeration; see references below.
     *<dt><b>References:</b>
     *<dd>	See Section 7 in Document IST-CR-93-19.
     *<dd>	DIS Data Dictionary:
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/37.htm">Dead Reckoning Parameter Record</a>,
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/38.htm">Dead Reckoning Algorithm Field</a>.
     *</dl>
     */
    protected UnsignedByte      deadReckoningAlgorithm;         // How do we figure dead reckoning between updates?


    /**
     *Other Parameters - This field shall specify other required dead reckoning parameters to be determined.
     *This field shall consist of 120 bits.
     *
     *<dl>
     *<dt><b>Note:</b>
     *<dd>we don't respect our syntax
     *<dt><b>References:</b>
     *<dd>	See Section 7 in Document IST-CR-93-19.
     *<dd>	DIS Data Dictionary:
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/37.htm">Dead Reckoning Parameter Record</a>,
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/39.htm">Dead Reckoning Other Parameters Field</a>.
     *</dl>
     */
    protected byte[]            deadReckoningParameters;        // 120 bits of space for DR parameters


    /**
     *Entity Linear Acceleration - This field shall specify an entity's linear acceleration.
     */
    protected LinearAcceleration      entityLinearAcceleration;                   // Acceleration of object (x, y, z in floats)


    /**
     *Entity Angular Velocity - This field shall specify an entity's angular velocity.
     */
    protected AngularVelocity   entityAngularVelocity;                // angular velocity about x, y, z (all floats)


     /**
     *This field represents the Character set used in representing the Entity Marking Record.
     *
     *<dl>
     *<dt><b>References:</b>
     *<dd>	DIS specification : IEEE 1278.1, 5.3.15
     *<dd>	DIS Data Dictionary:
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/63.htm">Entity Marking record</a>,
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/7b.htm">Entity Marking String Record</a>.
     *</dl>
     *
     *@see #marking
     */
    protected UnsignedByte      characterSet;                   // what character set to use when drawing markings


    /**
     *This is a String  used to designate the Entity Marking.
     *It is not more than 11 characters.
     *Besides it can't be more than 11 bytes when serializing.
     *
     *<dl>
     *<dt><b>Explanation</b>
     *  The Entity Marking record has been flatened here into the marking and the characterSet fields.
     *  In general, this record is used to specify the character set used in the marking
     *  and the string of characters to be interpreted for display.
     *
     *<dt><b>Note:</b><br>
     *<dd>In Java, Strings are NOT the same as byte arrays. Strings use
     *use unicode characters of two bytes each, unlike in C,
     *where there's a direct 1-byte=1 char mapping. We get around
     *this by taking only the low-order byte of the two-byte unicode
     *char when serializing. One of the side effects of this is that
     *we can SET unicode strings, but can't transmit them. This is a
     *wart, but it makes the code a bit easier.
     *
     *<dt><b>References:</b>
     *<dd>	DIS specification : IEEE 1278.1, 5.3.15
     *<dd>	DIS Data Dictionary:
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/63.htm">Entity Marking record</a>,
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/7b.htm">Entity Marking String Record</a>.
     *</dl>
     */
    protected String            marking = new String ("");


    // Capabilities

    /**
     *A collection of boolean fields which describe the capabilities of the Entity.
     *<b>Note:</b> The record is being flatened here in one field.
     *

     *<dl>
     *<dt><b>References:</b>
     *<dd>	See Section 4 in Document IST-CR-93-19.
     *<dd>	DIS Data Dictionary:
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/67.htm">Entity Capabilities Record</a>,
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/38.htm">Dead Reckoning Algorithm Field</a>.
     *<dd>	DIS specification : IEEE 1278.1, 5.3.13
     *</dl>
     */
    protected UnsignedInt       capabilities;                   // 32 boolean flags.


    // Articulation parameters. This is simply a list of articulation parameters.
    // The number of articulation parameters, which is designated as a field in
    // the external representation of the PDU, is actually maintained here, so the
    // length of the vector and the count can't get out of synch.

    /**
     * The number of articulation parameters and the Articulation Paramater record are being wrapped
     * in Vectors. It makes the interface look better.
     *
     *<dl>
     *<dt><b>References:</b>
     *<dd>	See Section 4 in Document IST-CR-93-19.
     *<dd>	DIS Data Dictionary:
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/30.htm">Number of Articulation Parameters Field</a>,
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/30.htm">Articulation Parameter Record</a>
     *<dd>	DIS specification : IEEE 1278.1, Annex A, 5.3.5
     *</dl>
     */
    protected Vector            articulationParameters;         // list of articulation parameters


    /**
     *Constant value--size of an Entity State PDU without header.
     *<code>sizeOf = 132 bytes</code>
     */
    public static final int sizeOf = 132;                   // size, in bytes, of a stripped ESPDU, AS WRITTEN, less header info


    /**
     *An "examplar" object, which is filled out to the state that is needed most of the time.
     *
     *<dl>
     *<dt><b>Explanation</b>
     *<dd>A brand new object has to have most of its values set,
     *  such as the forceID, protocol version, and so on. This lets the user fill
     *  out most of the values, save it in the class, then retrieve a copy of it
     *  as needed.
     *</dl>
     */
    static protected EntityStatePdu exemplar;



/**
 *Default constructor--fills with zeros for all values.
 */
public EntityStatePdu()     // default constructor
{
    pduType = new UnsignedByte(PduTypeField.ENTITYSTATE);   // set superclass ivar to correct type

    entityID = new EntityID();
    forceID = new UnsignedByte();

    entityType = new EntityType();
    alternativeEntityType = new EntityType();
    entityLinearVelocity = new LinearVelocity();
    entityLocation = new WorldCoordinate();
    entityOrientation = new EulerAngle();
    entityAppearance = new UnsignedInt();

    deadReckoningAlgorithm  = new UnsignedByte();
    deadReckoningParameters = new byte[15];

    // assorted quantities used for dead reckoning

    entityLinearAcceleration = new LinearAcceleration(0,0,0);
    entityAngularVelocity = new AngularVelocity(0,0,0);

    characterSet    = new UnsignedByte();                   // character set. No unicode.
    capabilities    = new UnsignedInt(0);
    articulationParameters = new Vector(0);

    return;
};




/**
 *@exception RuntimeException when io error occurs.
 *fileEncoding ensures we convert string to local system encoding properly.
 */
public void  serialize(DataOutputStream outputStream)
{
    int             idx = 0;
    byte[]          entityMarkingBytes = new byte[11];
    byte            nullByte = 0;                       // null byte
    UnsignedByte    articulationParameterCount;
    String          fileEncoding;

    debug("Entering ESPDU serialization");

    super.serialize(outputStream);

    debug("Serialized superclass in ESPDU");

    // Do our ivars

    try     // catch-all for any exceptions in writing to stream
     {
        debug("EntityStatePdu serialized " + outputStream.size());

        entityID.serialize(outputStream);
        forceID.serialize(outputStream);

        // the number of articulation parameters

        articulationParameterCount = new UnsignedByte(articulationParameters.size());
        articulationParameterCount.serialize(outputStream);

        // the entity as friendly forces see it
        entityType.serialize(outputStream);

        // entity as other forces see it
        alternativeEntityType.serialize(outputStream);

        // entity entityLinearVelocity.
        entityLinearVelocity.serialize(outputStream);

        // entity location
        entityLocation.serialize(outputStream);

        debug("EntityStatePdu serialized " + outputStream.size());

        // entity orientation
        entityOrientation.serialize(outputStream);

        debug("EntityStatePdu serialized " + outputStream.size());

        entityAppearance.serialize(outputStream);

        // dead reckoning

        debug("EntityStatePdu serialized " + outputStream.size());

        deadReckoningAlgorithm.serialize(outputStream);
        for(idx = 0; idx < 15; idx++)
        {
            outputStream.writeByte(deadReckoningParameters[idx]);
        }

        entityLinearAcceleration.serialize(outputStream);
        entityAngularVelocity.serialize(outputStream);

        debug("EntityStatePdu serialized " + outputStream.size());

        // entity marking. We know the character length is < 11, since that's
        // enforced by the accessor methods. CharacterSet needs to be mapped
        // intelligently to something real.

        characterSet.serialize(outputStream);

        // get the bytes. Write them out; if we run out of bytes, fill the remaining
        // space with spaces, up to the specified length of 11.

        debug("just serialized the character set in ESPDU..");

        // This is deprecated, but characters.getBytes(), a recommended way to
        // do things in Java 1.1, goes off into la-la land and never returns.
        // file:///C|/jdk1.1.3/docs/api/java.lang.String.html#getBytes(int, int, byte[], int)
        // marking.getBytes(0, marking.length(), entityMarkingBytes, 0);
        // entityMarkingBytes = marking.getBytes();

	      fileEncoding = System.getProperty("file.encoding");
        debug("fileEncoding = " + fileEncoding);
        entityMarkingBytes = marking.getBytes(fileEncoding);  // untested..

        debug("going into entity marking loop, number of bytes=" + entityMarkingBytes.length);

        for(idx = 0; idx < 11; idx++)
         {
            if(idx < (entityMarkingBytes.length))
             outputStream.writeByte(entityMarkingBytes[idx]);  // write out the string
            else
              outputStream.writeByte(nullByte);                // not enough chars; fill with nulls

            debug("serialized byte " + idx + " of entity marking bytes");
        }

        debug("EntityStatePdu serialized " + outputStream.size());

        capabilities.serialize(outputStream);

        for(idx = 0; idx < articulationParameters.size(); idx++)
         {
            ArticulationParameter  aParameter;

            aParameter = (ArticulationParameter)articulationParameters.elementAt(idx);
            aParameter.serialize(outputStream);
         }

        debug("EntityStatePdu serialized " + outputStream.size());

    }
    catch(UnsupportedEncodingException unsupportedEncodingException)      // catch-all for all IO exceptions in the above code
     {
        trace("Exception in EntityStatePdu.  UnsupportedEncodingException");
        throw new
            RuntimeException("Error serializing unit.");
     }
    catch(IOException ioException)      // catch-all for all IO exceptions in the above code
     {
        throw new
            RuntimeException("Exception in EntityStatePdu. Error serializing unit.");
     }


    debug("Exiting serialize in ESPDU");

 }  // end of serialize




/**
 *@exception RuntimeException when io error occurs.
 *fileEncoding ensures we convert string to local system encoding properly.
 */
public void  deSerialize(DataInputStream dataInputStream)
{
    int             idx = 0;
    byte[]          entityMarkingBytes = new byte[11];
    UnsignedByte    articulationParameterCount = new UnsignedByte();
    String          fileEncoding;

    super.deSerialize(dataInputStream);

    // Do our ivars (Initialization variables)

    try     // catch-all for any exceptions in reading from stream
     {
        entityID.deSerialize(dataInputStream);
        forceID.deSerialize(dataInputStream);
        articulationParameterCount.deSerialize(dataInputStream);    // used later, but thrown away eventually

        // the entity as friendly forces see it
        entityType.deSerialize(dataInputStream);

        // entity as other forces see it
        alternativeEntityType.deSerialize(dataInputStream);

        // entity entityLinearVelocity.
        entityLinearVelocity.deSerialize(dataInputStream);

        // entity location
        entityLocation.deSerialize(dataInputStream);

        // entity orientation
        entityOrientation.deSerialize(dataInputStream);
        entityAppearance.deSerialize(dataInputStream);

        // dead reckoning
        deadReckoningAlgorithm.deSerialize(dataInputStream);
        for(idx = 0; idx < 15; idx++)
        {
            deadReckoningParameters[idx] = dataInputStream.readByte();
        }

        entityLinearAcceleration.deSerialize(dataInputStream);
        entityAngularVelocity.deSerialize(dataInputStream);

        // entity marking. According to the spec, there are guaranteed to
        // be 11 bytes there for us to read.

        characterSet.deSerialize(dataInputStream);

        for(idx = 0; idx < 11; idx++)
         {
            entityMarkingBytes[idx] = dataInputStream.readByte();
         }
//  deprecated:  file:///C|/jdk1.1.3/docs/api/java.lang.String.html#String(byte[], int)
//      marking = new String(entityMarkingBytes,0);  // instantiate with default char set
//      marking = new String(entityMarkingBytes  );  // instantiate with default char set

	fileEncoding = System.getProperty("file.encoding");
        debug ("fileEncoding = " + fileEncoding);

	marking = new String(entityMarkingBytes, fileEncoding);

        capabilities.deSerialize(dataInputStream);

        // articulation parameters.There's no way I know of to do a sanity check on the
        // actual number of articulation parameters vs. the number claimed earlier
        // in the pdu. If it's an overestimate in the pdu, we'll probably throw an
        // exception when we run out of buffer space to read. If it's an underestimate,
        // we'll probably throw away articulation parameters and silently fail.
        // Hmmmm.... maybe do a check of the stream pointer at the end and confirm
        // that it is the same as the buffer size?

        for(idx = 0; idx < articulationParameterCount.intValue(); idx++)
         {
            ArticulationParameter  aParameter = new ArticulationParameter();

            aParameter.deSerialize(dataInputStream);
            articulationParameters.addElement(aParameter);

         }

    }
    catch(UnsupportedEncodingException unsupportedEncodingException)      // catch-all for all IO exceptions in the above code
     {
    	debug ("Exception in EntityStatePdu.  UnsupportedEncodingException ");
        throw new
            RuntimeException("Error deSerializing unit.");
     }

    catch(IOException ioException)      // catch-all for all IO exceptions in the above code
     {
        throw new
            RuntimeException("Exception in EntityStatePdu. Error deSerializing unit.");
     }

 }  // end of deSerialize



/**
 *Returns a clone of the Entity State PDU .
 *
 *@return newPdu
 */

public Object clone()
{
  EntityStatePdu     newPdu = (EntityStatePdu)super.clone(); // new entity state pdu
  UnsignedByte       parameterCount;                         // number of articulation parameters
  int                idx;                                    // general purpose index

  newPdu.setEntityID(this.entityID);
  newPdu.setForceID(this.getForceID());

  // we do NOT set the articulation parameter count here. Below we add the articulation parameters
  // individually, and the process of adding them will increment the parameter count within
  // the object. We do keep the value around, however, to use in the loop below.

  parameterCount = this.articulationParameterCount();

  newPdu.setEntityType(this.getEntityType());
  newPdu.setAlternativeEntityType(this.getAlternativeEntityType());

  newPdu.setEntityLinearVelocity(this.getEntityLinearVelocity());

  newPdu.setEntityLocation(this.getEntityLocation());

  newPdu.setEntityOrientation(this.getEntityOrientation());

  newPdu.setDeadReckoningAlgorithm(this.getDeadReckoningAlgorithm());
  newPdu.setDeadReckoningParameters(this.getDeadReckoningParameters());

  newPdu.setEntityLinearAcceleration(this.getEntityLinearAcceleration());
  newPdu.setEntityAngularVelocity(this.getEntityAngularVelocity());

  newPdu.setCharacterSet(this.getCharacterSet());
  newPdu.setMarking(this.getMarking());

  newPdu.setCapabilities(this.getCapabilities());

  for(idx = 0; idx < parameterCount.intValue(); idx++)
  {
    newPdu.addArticulationParameter(this.getArticulationParameterAt(idx));
  }

  return newPdu;

}   // end of clone()



/**
 *
 * Calculates the length of the Entity State PDU on the fly. This should reflect the current length
 * of the PDU; if an articulation parameter is added, you have to call length() again
 * to find the current length. Note that this is the length of the PDU AS WRITTEN TO
 * THE WIRE.
 *
 *@return currentLength
 */

public int length()
{
  int   currentLength = 0;

  currentLength = super.length() +
                  EntityStatePdu.sizeOf +
                 (articulationParameters.size() * ArticulationParameter.sizeOf);

  return currentLength;
}



/**
 *This is the string value of Entity State Pdu
 *
 *@return new String of ESPDU
 */
 public String pduName()
  {
    return new String("ESPDU");
  }


/**
 *This will print the values of Entity State Pdu to a stream
 *
 */

public void printValues (int indentLevel, PrintStream printStream)
{
    StringBuffer buf = ProtocolDataUnit.getPaddingOfLength(indentLevel);
    int idx, superclassIndent = indentLevel;

    // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
    // so the header info will be indented a bit less.

    if(superclassIndent > 0)
      superclassIndent -= 1;

    printStream.println();
    printStream.println("Entity State PDU printValues()");

    super.printValues(superclassIndent, printStream);

    // Yech. This fairly screams for a better way to handle ivars. p-list?

    entityID.printValues(indentLevel, printStream);
    printStream.println(buf + "forceID: " + forceID.intValue());
    printStream.println(buf + "articulationParameterCount: " + this.articulationParameterCount());

    printStream.println("entityType");
    entityType.printValues(indentLevel, printStream);

    printStream.println("alternativeEntityType");
    alternativeEntityType.printValues(indentLevel, printStream);

    entityLinearVelocity.printValues(indentLevel, printStream);
    entityLocation.printValues(indentLevel, printStream);
    entityOrientation.printValues(indentLevel, printStream);

    printStream.println(buf + "entityAppearance: " + entityAppearance.longValue());

    printStream.println(buf + "deadReckoningAlgorithm: " + deadReckoningAlgorithm.intValue());
    printStream.print(buf);
    for(idx = 0; idx < 15; idx++)
      {
        printStream.print(deadReckoningParameters[idx]);
        if(idx < 14)
            printStream.print(", ");
      }

    printStream.println();

    entityLinearAcceleration.printValues(indentLevel, printStream);
    entityAngularVelocity.printValues(indentLevel, printStream);

    printStream.println(buf + "characterSet: " + characterSet.intValue());
    printStream.println(buf + "marking: " + marking);

    printStream.println(buf + "capabilities: " + capabilities.longValue());

    for(idx = 0; idx < articulationParameters.size(); idx ++)
     {
        ArticulationParameter   aParam;

        aParam = (ArticulationParameter)articulationParameters.elementAt(idx);
        aParam.printValues(indentLevel + 4, printStream, idx);                // indent the parts of us a bit
     }

    return;
}

/**
 *This gets the exemplar,accessor method
 *
 *@return exemplar.clone
 */

public static EntityStatePdu  getExemplar()
{
    return (EntityStatePdu)exemplar.clone();
}

/**
 *This sets the exemplar,accessor method
 *
 *
 */

public static void setExemplar(EntityStatePdu newExemplar)
{
    exemplar = newExemplar;
}


/**
 *This is the string value of Entity State Pdu,accessor method
 *This field shall identify the entity issuing the PDU,
 * and shall be represented by the PDU Header Record (see 5.2.24)
 *
 *@return entityID.clone
 */
public EntityID getEntityID()
{ return (EntityID)entityID.clone();
}



/**
 *Sets EntityID of an EntityID,accessor method.
 *This field shall identify the entity issuing the PDU,
 * and shall be represented by the PDU Header Record (see 5.2.24)
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
{
	entityID = new EntityID(pSiteID, pApplicationID, pEntityID);
}


/**
 *Get ForceID of UnsignedByte ,accessor method.
 * This field shall idenitify the enity issuing the PDU,
 *  and shall be represented by an Entity Identifier record (sec 5.2.14)
 *
 *@return forceID.clone
 */
public UnsignedByte getForceID()
{
	return (UnsignedByte)forceID.clone();
}


/**
 *Set the ForceID with UnsignedByte,accessor method.
 * This field shall idenitify the enity issuing the PDU,
 *  and shall be represented by an Entity Identifier record (sec 5.2.14)
 *
 */
public void setForceID(UnsignedByte pForceID)
{
	forceID = pForceID;}


/**
 *Set the ForceID with new UnsignedByte,accessor method.
 * This field shall idenitify the enity issuing the PDU,
 *  and shall be represented by an Entity Identifier record (sec 5.2.14)
 *
 */
public void setForceID(int pForceID)
{
	forceID = new UnsignedByte(pForceID);
}



/**
 *Returns the number of articulation parameter in this Entity State PDU,accessor method.
 *Shall be represented by an 8 bit unsigned interger (see appendix A)
 *
 *@return articulationParameters.size
 */
public UnsignedByte articulationParameterCount()
{
	return new UnsignedByte(articulationParameters.size());
}




// both directly set the entity type and provide covers in this object for
// setting values within entity type.

/**
 *Gets the Entity State PDU type,accessor method.
 *
 *@return enititType.clone
 */
public EntityType getEntityType()
{ return (EntityType)entityType.clone();
}



/**
 *Sets the Entity State PDU type,accessor method.
 */
public void setEntityType(EntityType pEntityType)
{ entityType = pEntityType;
}

/**
 *Gets the Entity State PDU type,accessor method.
 *
 *@return entityType.getKind
 */
public UnsignedByte getEntityTypeKind()
{ return entityType.getKind();
}

/**
 *Sets the Entity State PDU type of a UnsignedByte ,accessor method.
 *
 */
public void setEntityTypeKind(UnsignedByte pEntityKind)
{ entityType.setKind(pEntityKind);
}


/**
 *Sets the Entity State PDU type of a Int accessor method.
 *
 */
public void setEntityTypeKind(int pEntityKind)
{ entityType.setKind(pEntityKind);
}


/**
 *Gets the Entity State PDU type of Domain ,accessor method.
 *
 *@return entityType.getDomain
 */
public UnsignedByte getEntityTypeDomain()
{ return entityType.getDomain();
}


/**
 *Sets the Entity State PDU type of Domain ,accessor method.
 *
 */
public void setEntityTypeDomain(UnsignedByte pEntityDomain)
{ entityType.setDomain(pEntityDomain);}


/**
 *Sets the Entity State PDU type of Domain ,accessor method.
 *
 */
public void setEntityDomain(int pEntityDomain)
{  entityType.setDomain(pEntityDomain);
}


/**
 *Gets the Entity Type's Country of origin ,accessor method.
 *
 *@return entityType.getCountry
 */
public UnsignedShort getEntityTypeCountry()
{ return entityType.getCountry();
}

/**
 *Sets the Entity Type's Country of origin ,accessor method.
 *
 */
public void setEntityTypeCountry(UnsignedShort pEntityCountry)
{ entityType.setCountry(pEntityCountry);}


/**
 *Sets the Entity Type's Country of origin ,accessor method.
 *
 */
public void setEntityTypeCountry(int pEntityCountry)
{ entityType.setCountry(pEntityCountry);
}


/**
 *Gets the Entity Type's Category ,accessor method.
 *
 *@return entityType.getCategory
 */
public UnsignedByte getEntityTypeCategory()
{ return entityType.getCategory();
}


/**
 *Sets the Entity Type's Category of origin ,accessor method.
 *
 */
public void setEntityTypeCategory(UnsignedByte pEntityCategory)
{ entityType.setCategory(pEntityCategory);}


/**
 *Sets the Entity Type's Category of origin ,accessor method.
 *
 */
public void setEntityTypeCategory(int pEntityCategory)
{ entityType.setCategory(pEntityCategory);
}


/**
 *Sets the Entity Type's Sub Category of origin ,accessor method.
 *
 *@return entityType.getSubCategory
 */
public UnsignedByte getEntityTypeSubcategory()
{ return entityType.getSubCategory();
}


/**
 *Sets the Entity Type's Sub Category of origin ,accessor method.
 *
 */
public void setEntityTypeSubcategory(UnsignedByte pEntitySubcategory)
{ entityType.setSubCategory(pEntitySubcategory);}


/**
 *Sets the Entity Type's Sub Category of origin ,accessor method.
 *
 */
public void setEntityTypeSubcategory(int pEntitySubcategory)
{entityType.setSubCategory(pEntitySubcategory);
}


/**
 *Gets the Entity Type's Specific Category of origin ,accessor method.
 *
 * @return entityType.getSpecific
 */
public UnsignedByte getEntityTypeSpecific()
{ return entityType.getSpecific();
}


/**
 *Gets the Entity Type's Specific  ,accessor method.
 *
 */
public void setEntityTypeSpecific(UnsignedByte pEntitySpecific)
{ entityType.setSpecific(pEntitySpecific);}


/**
 *Sets the Entity Type's Specific  ,accessor method.
 *
 */
public void setEntityTypeSpecific(int pEntitySpecific)
{ entityType.setSpecific(pEntitySpecific);
}


/**
 *Gets the Entity Type's Extra data field  ,accessor method.
 *
 * @return entityType.getExtra
 */
public UnsignedByte getEntityTypeExtra()
{ return entityType.getExtra();
}


/**
 *Sets the Entity Type's Extra data field  ,accessor method.
 *
 */
public void setEntityTypeExtra(UnsignedByte pEntityExtra)
{ entityType.setExtra(pEntityExtra);}


/**
 *Sets the Entity Type's Extra data field  ,accessor method.
 *
 */
public void setEntityTypeExtra(int pEntityExtra)
{ entityType.setExtra(pEntityExtra);
}


/**
 *Gets the Entity Type's Alternative Type data field, whcih is a clone of the Enity  ,accessor method.
 *
 * @alternativeEntityType.clone
 */
public EntityType getAlternativeEntityType()
{ return (EntityType)alternativeEntityType.clone();
}

/**
 *Sets the Entity Type's Alternative Type data field, whcih is a clone of the Enity  ,accessor method.
 *
 */
public void setAlternativeEntityType(EntityType pNewAltEntity)
{ alternativeEntityType = pNewAltEntity;
}


/**
 *Gets the Entity Type's Alternative Type data field, whcih is a clone of the Enity  ,accessor method.
 *
 * @return alternativeEntityType.getKind
 */
public UnsignedByte getAlternativeEntityTypeKind()
{ return alternativeEntityType.getKind();
}

/**
 *Sets the Entity Type's Alternative Kind, whcih is the kind of Enity  ,accessor method.
 *
 */
public void setAlternativeEntityTypeKind(UnsignedByte pEntityKind)
{ alternativeEntityType.setKind(pEntityKind);
}


/**
 *Sets the Entity Type's Alternative Kind, which is the kind of Enity  ,accessor method.
 *
 */
public void setAlternativeEntityTypeKind(int pEntityKind)
{ alternativeEntityType.setKind(pEntityKind);
}


/**
 *Gets the Entity Type's Alternative Type Domain, which is a clone of the Enity  ,accessor method.
 *
 * @return alternativeEntityType.getDomain
 */
public UnsignedByte getAlternativeEntityTypeDomain()
{ return alternativeEntityType.getDomain();
}

/**
 *Sets the Entity Type's Alternative Type Domain, which is a clone of the Enity  ,accessor method.
 *
 */
public void setAlternativeEntityTypeDomain(UnsignedByte pEntityDomain)
{
  alternativeEntityType.setDomain(pEntityDomain);
}


/**
 *Sets the Entity Type's Alternative Type Domain, which is a clone of the Enity  ,accessor method.
 *
 */
public void setAlternativeEntityDomain(int pEntityDomain)
{  alternativeEntityType.setDomain(pEntityDomain);
}


/**
 *Gets the Entity Type's Alternative Type Country, which is a clone of the Enity  ,accessor method.
 *
 * @return alternativeEntityType.getCountry
 */
 public UnsignedShort getAlternativeEntityTypeCountry()
{ return alternativeEntityType.getCountry();
}


/**
 *Sets the Entity Type's Alternative Type Country, which is a clone of the Enity  ,accessor method.
 *
 */
 public void setAlternativeEntityTypeCountry(UnsignedShort pEntityCountry)
  { alternativeEntityType.setCountry(pEntityCountry);}


/**
 *Sets the Entity Type's Alternative Type Country, which is a clone of the Enity  ,accessor method.
 *
 */
public void setAlternativeEntityCountry(int pEntityCountry)
{ alternativeEntityType.setCountry(pEntityCountry);
}


/**
 *Gets the Entity Type's Alternative Type Category, which is a clone of the Enity  ,accessor method.
 *
 *@return alternativeEntityType.getCategory
 */
public UnsignedByte getAlternativeEntityTypeCategory()
{ return alternativeEntityType.getCategory();
}


/**
 *Sets the Entity Type's Alternative Type Category, which is a clone of the Enity  ,accessor method.
 *
 */
public void setAlternativeEntityTypeCategory(UnsignedByte pEntityCategory)
{ alternativeEntityType.setCategory(pEntityCategory);}


/**
 *Sets the Entity Type's Alternative Type Category, which is a clone of the Enity  ,accessor method.
 *
 */
public void setAlternativeEntityTypeCategory(int pEntityCategory)
{ alternativeEntityType.setCategory(pEntityCategory);
}


/**
 *Gets the Entity Type's Alternative Type SubCategory, which is a clone of the Enity  ,accessor method.
 *
 *@return alternativeEntityType.getSubCategory
 */
public UnsignedByte getAlternativeEntityTypeSubcategory()
{ return alternativeEntityType.getSubCategory();
}


/**
 *Sets the Entity Type's Alternative Type SubCategory ,accessor method.
 *
 */
public void setAlternativeEntityTypeSubcategory(UnsignedByte pEntitySubcategory)
{ alternativeEntityType.setSubCategory(pEntitySubcategory);}


/**
 *Sets the Entity Type's Alternative Type SubCategory  ,accessor method.
 *
 */
public void setAlternativeEntityTypeSubcategory(int pEntitySubcategory)
{alternativeEntityType.setSubCategory(pEntitySubcategory);
}


/**
 *Gets the Entity Type's Alternative Type Specific,accessor method.
 *
 *@return alternativeEntityType.getSpecific
 */
public UnsignedByte getAlternativeEntityTypeSpecific()
{ return alternativeEntityType.getSpecific();
}


/**
 *Sets the Entity Type's Alternative Type Specific,accessor method.
 *
 */
public void setAlternativeEntityTypeSpecific(UnsignedByte pEntitySpecific)
{ alternativeEntityType.setSpecific(pEntitySpecific);}


/**
 *Sets the Entity Type's Alternative Type Specific,accessor method.
 *
 */
public void setAlternativeEntityTypeSpecific(int pEntitySpecific)
{ alternativeEntityType.setSpecific(pEntitySpecific);
}


/**
 *Sets the Entity Type's Alternative Type Extra,accessor method.
 *
 */
public UnsignedByte getAlternativeEntityTypeExtra()
{ return alternativeEntityType.getExtra();
}


/**
 *Sets the Entity Type's Alternative Type Extra,accessor method.
 *
 */
public void setAlternativeEntityTypeExtra(UnsignedByte pEntityExtra)
{ alternativeEntityType.setExtra(pEntityExtra);}


/**
 *Sets the Entity Type's Alternative Type Extra,accessor method.
 *
 */
public void setAlternativeEntityTypeExtra(int pEntityExtra)
{ alternativeEntityType.setExtra(pEntityExtra);
}


/**
 *Gets the Entity Type's Linear Velocity in meters per sec,accessor method.
 *
 *@return entityLinearVelocity.clone
 */
public LinearVelocity getEntityLinearVelocity()
{ return (LinearVelocity)entityLinearVelocity.clone();
}


/**
 *Sets the Entity Type's Linear Velocity in meters per sec,accessor method.
 *
 */
public void setEntityLinearVelocity(LinearVelocity pVelocity)
{ entityLinearVelocity = pVelocity;}



/**
 *Gets the Entity Type's Linear Velocity in meters per sec in the X Direction ,accessor method.
 *
 *@return entityLinearVelocity.getX
 */
public float getEntityLinearVelocityX()
{ return entityLinearVelocity.getX();
}


/**
 *Sets the Entity Type's Linear Velocity in meters per sec in the X Direction ,accessor method.
 *
 */
public void setEntityLinearVelocityX(float pVelocityX)
{ entityLinearVelocity.setX(pVelocityX);}


/**
 *Gets the Entity Type's Linear Velocity in meters per sec in the Y Direction ,accessor method.
 *
 *@return entityLinearVelocity.getY
 */
public float getEntityLinearVelocityY()
{ return entityLinearVelocity.getY();
}


/**
 *Sets the Entity Type's Linear Velocity in meters per sec in the Y Direction ,accessor method.
 *
 */
public void setEntityLinearVelocityY(float pVelocityY)
{ entityLinearVelocity.setY(pVelocityY);}


/**
 *Gets the Entity Type's Linear Velocity in meters per sec in the Z Direction ,accessor method.
 *
 *@return entityLinearVelocity.getZ
 */
public float getEntityLinearVelocityZ()
{ return entityLinearVelocity.getZ();
}


 /**
 *Sets the Entity Type's Linear Velocity in meters per sec in the Y Direction ,accessor method.
 *
 */
public void setEntityLinearVelocityZ(float pVelocityZ)
{ entityLinearVelocity.setZ(pVelocityZ);}


 /**
 *Gets the Entity Type's location in World Cordinates ,accessor method.
 *
 *@return entityLocation.clone
 */
public WorldCoordinate getEntityLocation()
{ return (WorldCoordinate)entityLocation.clone();
}


 /**
 *Sets the Entity Type's location in World Cordinates  ,accessor method.
 *
 */
public void setEntityLocation(WorldCoordinate pLocation)
{ entityLocation = pLocation;
}

/**
 *Gets the Entity Type'slocation in World Cordinates in the X direction ,accessor method.
 *
 *@return entityLocation.getX
 */
public double getEntityLocationX()
{ return entityLocation.getX();
}


/**
 *Sets the Entity Type'slocation in World Cordinates in the X direction ,accessor method.
 *
 */
public void setEntityLocationX(double pLocationX)
{ entityLocation.setX(pLocationX);
}


 /**
 *Gets the Entity Type'slocation in World Cordinates in the Y direction ,accessor method.
 *
 *@return entityLocation.getY
 */
public double getEntityLocationY()
{ return entityLocation.getY();
}

 /**
 *Sets the Entity Type'slocation in World Cordinates in the Y direction ,accessor method.
 *
 */
public void setEntityLocationY(double pLocationY)
{ entityLocation.setY(pLocationY);
}


 /**
 *Gets the Entity Type'slocation in World Cordinates in the Z direction ,accessor method.
 *
 *@return entityLocation.getZ
 */
public double getEntityLocationZ()
{ return entityLocation.getZ();
}


 /**
 *Sets the Entity Type'slocation in World Cordinates in the Z direction ,accessor method.
 *
 */
public void setEntityLocationZ(double pLocationZ)
{ entityLocation.setZ(pLocationZ);
}

/**
 *Gets the Entity Type's Orientation as  a EulerAngle ,accessor method.
 *
 *@return entityOrientation.clone
 */
public EulerAngle getEntityOrientation()
{ return (EulerAngle)entityOrientation.clone();
}


/**
 *Sets the Entity Type'slocation in World Cordinates in the Z direction ,accessor method.
 *
 */
public void setEntityOrientation(EulerAngle pNewOrientation)
{ entityOrientation = pNewOrientation;
}


/**
 *Gets the Entity Type's Orientation as  a EulerAngle (Psi  ,accessor method.                        //dwl      RotationMatrix3D[psi,theta,phi].
 *                                                                                                                  //psi
 *@return entityOrientation.getPsi                                                                                  //theta
 */                                                                                                                 //phi
public float getEntityOrientationPsi()
{ return entityOrientation.getPsi();
}


/**
 *Sets the Entity Type'slocation in World Cordinates in the Z direction ,accessor method.
 *
 */
public void setEntityOrientationPsi(float pPsi)
{ entityOrientation.setPsi(pPsi);
}


/**
 *Gets the Entity Type's Orientation as  a EulerAngle (Theta ,accessor method.
 *
 *@return entityOrientation.getTheta
 */
public float getEntityOrientationTheta()
{ return entityOrientation.getTheta();
}



public void setEntityOrientationTheta(float pTheta)
{ entityOrientation.setTheta(pTheta);
}



public float getEntityOrientationPhi()
{ return entityOrientation.getPhi();
}



public void setEntityOrientationPhi(float pPhi)
{ entityOrientation.setPhi(pPhi);
}



public UnsignedInt getEntityAppearance()
{ return (UnsignedInt)entityAppearance.clone();
}



public void setEntityAppearance(UnsignedInt pEntityAppearance)
{ entityAppearance = pEntityAppearance;
}



public void setEntityAppearance(int pEntityAppearance)
{ entityAppearance = new UnsignedInt(pEntityAppearance);
}



public UnsignedByte getDeadReckoningAlgorithm()
{ return (UnsignedByte)deadReckoningAlgorithm.clone();
}



public void setDeadReckoningAlgorithm(UnsignedByte pDeadReckoningAlgorithm)
{ deadReckoningAlgorithm = pDeadReckoningAlgorithm;
}



public void setDeadReckoningAlgorithm(int pDeadReckoningAlgorithm)
{ deadReckoningAlgorithm = new UnsignedByte(pDeadReckoningAlgorithm);
}



public byte[] getDeadReckoningParameters()
{
    byte[]  newDRParams = new byte[11];
    int     idx = 0;

    for(idx = 0; idx < 11; idx++)
     newDRParams[idx] = deadReckoningParameters[idx];

    return newDRParams;
}



/**
 *@exception RuntimeException if Dead Reckoning parameters are too long
 */
public void setDeadReckoningParameters(byte[] pDeadReckoningParameters)
{
    int     idx = 0;

  // sanity check the incoming array to make sure it's a reasonable length

  if(pDeadReckoningParameters.length > 11)
   {
    throw new
            RuntimeException("Exception in EntityStatePdu. Length of dead reckoning parameters too long.");
   }

   // If it's less than or equal to 11, we can cram it into the existing length. First zero out
   // everything that exists, then put the new data in.

    for(idx = 0; idx < 11; idx++)
      deadReckoningParameters[idx] = 0;

    for(idx = 0; idx < pDeadReckoningParameters.length; idx++)
        deadReckoningParameters[idx] = pDeadReckoningParameters[idx];

    return;
}



public LinearAcceleration getEntityLinearAcceleration()
{ return (LinearAcceleration)entityLinearAcceleration.clone();
}



public void setEntityLinearAcceleration(LinearAcceleration pAcceleration)
{ entityLinearAcceleration = pAcceleration;
}



public void setEntityLinearAcceleration(float pX, float pY, float pZ)
{ entityLinearAcceleration.setValues(pX, pY, pZ);
}



public float getEntityLinearAccelerationX()
{ return entityLinearAcceleration.getX();
}



public void setEntityLinearAccelerationX(float pEntityAcceleration)
{entityLinearAcceleration.setX(pEntityAcceleration);
}



public float getEntityLinearAccelerationY()
{ return entityLinearAcceleration.getY();
}



public void setEntityLinearAccelerationY(float pEntityAcceleration)
{entityLinearAcceleration.setY(pEntityAcceleration);
}



public float getEntityLinearAccelerationZ()
{ return entityLinearAcceleration.getZ();
}



public void setEntityLinearAccelerationZ(float pEntityAcceleration)
{entityLinearAcceleration.setZ(pEntityAcceleration);
}



public AngularVelocity getEntityAngularVelocity()
{ return (AngularVelocity)entityAngularVelocity.clone();
}



public void setEntityAngularVelocity(AngularVelocity pAngV)
{ entityAngularVelocity = pAngV;
}



public void setEntityAngularVelocity(float pX, float pY, float pZ)
{ entityAngularVelocity.setValues(pX, pY, pZ);
}



public float getEntityAngularVelocityX()
{ return entityAngularVelocity.getX();
}



public void setEntityAngularVelocityX(float pEntityAngularVelocity)
{entityAngularVelocity.setX(pEntityAngularVelocity);
}



public float getEntityAngularVelocityY()
{ return entityAngularVelocity.getY();
}



public void setEntityAngularVelocityY(float pEntityAngularVelocity)
{entityAngularVelocity.setY(pEntityAngularVelocity);
}



public float getEntityAngularVelocityZ()
{ return entityAngularVelocity.getZ();
}



public void setEntityAngularVelocityZ(float pEntityAngularVelocity)
{entityAngularVelocity.setZ(pEntityAngularVelocity);
}



public UnsignedByte getCharacterSet()
{ return (UnsignedByte)characterSet.clone();
}



public void setCharacterSet(UnsignedByte pCharacterSet)
{characterSet = pCharacterSet;
}



public void setCharacterSet(int pCharacterSet)
{ characterSet = new UnsignedByte(pCharacterSet);
}



public String getMarking()
{
   if (marking == null)
   {
   	debug ("getMarking():  marking == null!  reset to blank string");
   	marking = new String ("           ");;
   	return marking;
   }

   StringBuffer readableMarking = new StringBuffer (marking);
   for (int i=0; i<marking.length(); i++)
   {
   	if ((readableMarking.charAt (i) == (char) 0) ||
   	    (readableMarking.charAt (i) == '\n')     ||
   	    (readableMarking.charAt (i) == '\r')     ||
   	    (readableMarking.charAt (i) == '\t'))
   	     readableMarking.setCharAt (i, ' ');
   }

   return readableMarking.toString();
}

public String toString()
{
  String result = entityID.toString();
  result = result + " " + timestamp;
  return result;
}

/*
 *@exception RuntimeException if entity Marking is too long.
 */
public void setMarking(String pMarking)
{
  /**
  We can have no more than 11 characters in a string. We check that and crash if
  it's more, which is pretty extreme. Probably better to truncate, but this works
  for early testing, when we want to detect problems early on.

  Note that the DIS spec expects 11-byte strings, with one byte per char, while
  java uses two-byte unicode chars. We don't check the incoming string for its
  use of unicode; we just assume it doesn't use it. When serializing, we take only
  the low-order byte of each unicode char.

  This makes a COPY of the string passed in and uses that for internal storage.
  */
   if (pMarking == null)
   {
   	debug ("setMarking():  pMarking == null!");
   	marking = new String("           ");
   }

 if(pMarking.length() > 11)
 {
	marking = new String(pMarking.substring(0,10));
	debug ("marking too long, truncating to fit 11 characters:  " + marking);
 }
 else
 {
 	marking = new String(pMarking);
 	for (int i = 1; i <= 11 - pMarking.length(); i++)
 	    marking += " ";
	debug ("marking = [" + marking + "]"); // "], length = " + marking.length()
 }
 return;

}   // end of setMarking




public UnsignedInt getCapabilities()
{ return (UnsignedInt)capabilities.clone();
}



public void setCapabilities(UnsignedInt pCapabilities)
{capabilities = pCapabilities;
}



public void setCapabilities(int pCapabilities)
{ capabilities = new UnsignedInt(pCapabilities);
}

/**
 *Returns the articulation parameter pointed at the given index.
 *
 *@param pIdx the index of the articulation parameter we want to get.
 *@return the articulation paramter at the given index.
 */
public ArticulationParameter getArticulationParameterAt(int pIdx)
{
 ArticulationParameter  aParameter;

  // return a copy of the pIdx-th articulation parameter.

 aParameter = (ArticulationParameter)articulationParameters.elementAt(pIdx);

 return (ArticulationParameter)aParameter.clone();
}


/**
 *Inserts a new articulation parameter in the list.
 *
 *@param pParameter the articulation parameter to insert in the list.
 */
public void addArticulationParameter(ArticulationParameter pParameter)
{
 // Add one articulation parameter to the end, and increment the parameter
 // count accordingly.

 articulationParameters.addElement(pParameter);
}


} // end of class EntityStatePdu

// END OF FILE