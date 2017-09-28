/*
 File:		FirePdu.java
 CVS Info:	$Id: FirePdu.java,v 1.2 1998/01/27 18:44:15 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package PDUs;                                   // package to which we belong

import mil.navy.nps.util.*;         // General-purpose utilities
import disEnumerations.*;         // Enumerations for DIS

import java.lang.*;
import java.util.*;                                         // utility stuff we need
import java.io.*;                                           // input/output for serialization

/**
 * Weapon firing PDU (bang!).
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/FirePdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/FirePdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/FirePdu.java">
 *  ~/mil/navy/nps/dis/FirePdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>The firing of a weapon shall be communicated by issuing a Fire PDU.
 *
 *<dt><b>Explanation:</b>
 *<dd>The fire pdu denotes the firing of a weapon. It inherits the header
 *  information from ProtocolDataUnit, an abstract class that contains
 *  assorted protocol information. It implements the IDs of what's
 *  firing, what's being shot at, the munition, the event, and where
 *  the munition is headed.<P>
 *
 *  As with other PDUs, it knows how to serialize and deserialize itself
 *  from the wire. It also knows how to clone itself, and knows how to
 *  calculate its size when sent to the wire.<P>
 *
 *<dt><b>History:</b>
 *<dd>		11Dec96	/Don McGregor		/New
 *<dd>		10Mar97	/Don McGregor		/changes for javadoc compliance
 *<dd>		16Apr97	/Don McGregor		/PrintStream passed to printValues
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changed access methods: thisVariable() --> getThisVariable()
 *<dd>		14Jan97	/Ronan Fauglas		/changed position to locationInWorldCoordinate
 *<dd>		19Nov99	/Ivan Chang			/changes for javadoc compliance
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary:
 *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/7c.htm">Fire PDU (local) and
 *    		<A href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/7c.htm">Fire PDU</a>	
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 5.4.4.1, 4.4.3.2
 *
 *<dt><b>Note:</b>
 *<dd>   No accessor methods to nested records is provided.
 *
 *@see ProtocolDataUnit
 *@see PduElement 
 *@see SerializationInterface
 *@see DetonationPdu
 */
public class FirePdu extends ProtocolDataUnit 
{

    /**
     *Firing Entity Identification - This field shall identify the firing entity, and shall be 
     *represented by an Entity Identifier record
     *
     *<dl>
     *<dt><b>Reference:</b>
     *<dd>  DIS Data Dictionary: 
     *	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/2c.htm">Event Identifier (local)</A> and
     * 	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/2c.htm">Event Identifier Record</a>
     *</dl>
     */
    protected EntityID          firingEntityID;             // ID of entity that's doing the shooting

    /**
     *Target Entity Identification - This field shall identify the intended target, and shall be
     *represented by an Entity Identifier record
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>If the intended target is unknown, this field shall contain TARGET_ID_ UNKNOWN.
     *<dt><b>Reference:</b>
     *<dd>  DIS Data Dictionary: 
     * 	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/2c.htm">Event Identifier Record (local)</A> and
     *	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/2c.htm">Event Identifier Record</a>
     *</dl>
     */
    protected EntityID          targetEntityID;             // ID of entity that is undergoing darwinian selection

    /**
     *Munition Identification - This field shall specify the entity ID of the fired munition 
     *if tracking data is required, and shall be represented by an Entity Identifier record
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>A munition ID shall have a value of MUNITION _NOT_TRACKED if tracking data for the munition
     * is not required.
     *<dt><b>Reference:</b>
     *<dd>  DIS Data Dictionary: 
     *	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/2c.htm">Event Identifier Record (local)</A> and
     *	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/2c.htm">Event Identifier Record</a>
     *</dl>
     */
    protected EntityID          munitionID;                 // ID of munition being shot

    /**
     *Event Identification - This field shall contain an identification generated by the firing entity
     *to associate related firing and detonation events, and shall be represented by an Event Identifier
     *record
     *
     *<dl>
     *<dt><b>Reference:</b>
     *<dd>  DIS Data Dictionary: 
     *	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/79.htm">Event Identifier Record (local)</A> and
     *	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/79.htm">Event Identifier Record</a>
     *</dl>
     */
    protected EventID           eventID;                    // ID of event

    /**
     *Fire Mission Index - This field shall identify the fire mission
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>If the fire mission is unknown this field shall contain NO_FIRE_MISSION.
     *This field shall be represented by a 32-bit unsigned integer
     *<dt><b>Reference:</b>
     *<dd>  DIS Data Dictionary: 
     *	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/7d.htm">Fire Missile Index Field (local)</A> and
     *	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/7d.htm">Fire Missile Index Field</a>
     *</dl>
     */
    protected UnsignedInt       fireMissionIndex;           // ID of fire mission

    /**
     *Location in World Coordinate - This field shall specify the location, in world coordinates, 
     *from which the munition was launched, and shall be represented by a World Coordinates record
     *
     *<dl>
     *<dt><b>Reference:</b>
     *<dd>  DIS Data Dictionary: 
     *	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/19.htm">World Coordinate Record (local)</A> and
     *	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/19.htm">World Coordinate Record</a>
     *</dl>
     */
    protected WorldCoordinate     locationInWorldCoordinate;                   // x,y,z location, in 64 bit coords
    
    /**
     *Burst Descriptor - This field shall describe the type of munition fired, the warhead, the fuse,
     *the quantity, and the rate of fire, and shall be represented by a Burst Descriptor record
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>This field shall contain the following:
     *    Type of munuition fired
     *    Warhead of the munition (if applicable, otherwise, it has a value of zero)
     *    Fuse employed by the munition (if applicable, otherwise, it has a value of zero)
     *    Quantity and rate at which munition was fired
     *<dt><b>Reference:</b>
     *<dd>  DIS Data Dictionary: 
     *	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/7e.htm">Burst Descriptor Record (local)</A> and
     *	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/7e.htm">Burst Descriptor Record</a>
     *</dl>
     */
    protected BurstDescriptor   burstDescriptor;           // info about burst

    /**
     *Velocity - This field shall specify the velocity of the fired munition at the point 
     *when the issuing simulation application intends the externally visible effects 
     *of the launch (e.g. exhaust plume or muzzle blast) to first become apparent.
     *The velocity shall be represented in world coordinates, and shall be represented by
     *a Linear Velocity Vector record
     *
     *<dl>
     *<dt><b>Reference:</b>
     *<dd>  DIS Data Dictionary: 
     *	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/34.htm">Linaer Velocity Vector Record (local)</A> and
     *	<a href="http://siso.sc.ist.ucf.edu/dis-dd/pdu/34.htm">
     *Linear Velocity Vector Record</a>
     *</dl>
     */
    protected LinearVelocity          velocity;                   // x, y, z velocity (3 floats)

    /**
     *This field shall specify the range that an entity's fire control system
     *has assumed in computing the fire control solution.
     *This range is a 3-dimension, straight-line distance.
     *     
     *<dl>
     *<dt><b>Value:</b>
     *<dd> This field shall be represented by a 32-bit floating point number in meters. 
     *For systems where range is unknown or unavailable, this field shall contain a value of zero.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: 
     *	<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/83.htm">Range Field (local)</A> and
     *	<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/83.htm">Range Field</a>
     *</dl>
     */
    protected float             range;                      // Assumed range (from shooter point of view)

    /**
     *Constant value--size of Fire PDU with header. Here:
     *<code>sizeOf = 96 bytes</code>
     */
    public final static int     sizeOf = 96;                // size of object as written to wire

/**
 *Default constructor
 * - creates firingEntityID, targetEntityID, munitionID, eventID, fireMissionIndex, locationInWorldCoordinate,
 * and burstDescriptor
 * - fills with zeros for all values of the following parameters:
 * fireMissionIndex, locationInWorldCoordinate, velocity, range
 */
public FirePdu()
{
    super.setPduType(PduTypeField.FIRE);

    firingEntityID  = new EntityID();
    targetEntityID  = new EntityID();
    munitionID      = new EntityID();
    eventID         = new EventID();

    fireMissionIndex = new UnsignedInt(0);

    locationInWorldCoordinate        = new WorldCoordinate(0, 0, 0);

    burstDescriptor = new BurstDescriptor();

    velocity        = new LinearVelocity(0, 0, 0);
    range           = 0.0f;

    return;
}

/**
 * Make a copy of the object. This requires a deep copy, so we don't have two
 * objects sharing pointers to the same data.
 * @return a new Fire PDU entity
 */
public Object clone()
{
    
 FirePdu    newFirePdu = new FirePdu();

 newFirePdu.setFiringEntityID(this.getFiringEntityID());
 newFirePdu.setTargetEntityID(this.getTargetEntityID());
 newFirePdu.setMunitionID(this.getMunitionID());
 newFirePdu.setEventID(this.getEventID());
 newFirePdu.setFireMissionIndex(this.getFireMissionIndex());

 newFirePdu.setLocationInWorldCoordinate(this.getLocationInWorldCoordinate());
 
 newFirePdu.setBurstDescriptor(this.getBurstDescriptor());

 newFirePdu.setVelocity(this.getVelocity());
 newFirePdu.setRange(this.getRange());

 return newFirePdu;
}

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
        firingEntityID.serialize(outputStream);
        targetEntityID.serialize(outputStream);
        munitionID.serialize(outputStream);
        eventID.serialize(outputStream);
        fireMissionIndex.serialize(outputStream);

        locationInWorldCoordinate.serialize(outputStream);

        burstDescriptor.serialize(outputStream);
        velocity.serialize(outputStream);

        outputStream.writeFloat(range);
     }
    catch (IOException ioError)
    {
        throw new 
            RuntimeException("Exception in FirePdu. Error writing to wire.");
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
   
    super.deSerialize(inputStream);     // read in all the header info

    try
    {
        firingEntityID.deSerialize(inputStream);
        targetEntityID.deSerialize(inputStream);
        munitionID.deSerialize(inputStream);
        eventID.deSerialize(inputStream);
        fireMissionIndex.deSerialize(inputStream);

        locationInWorldCoordinate.deSerialize(inputStream);

        burstDescriptor.deSerialize(inputStream);
        velocity.deSerialize(inputStream);

        range = inputStream.readFloat();
     }
    catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in FirePdu. Error reading from wire.");
        }
}


/**
 * Returns the length of the entity
 * @return an integer length of the entity
 */
public int length()
{
    return sizeOf;          // EntityTypes are this long, always.
}

/**
 * Returns the PDU name - Fire PDU
 * @return a string "Fire PDU"
 */ 
public String pduName()
{
  return new String("Fire PDU");
}


/**
 * Print the values of the following object out, with correct level of
 * indentation on the page.
 * firingEntityID, targetEntityID, munitionID, eventID, fireMissionIndex, locationInWorldCoordinate,
 * burstDescriptor, velocity, and range.
 */
public void printValues(int indentLevel, PrintStream printStream)
{
    
  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println();
    printStream.println("Fire PDU-");

    // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
    // so the header info will be indented a bit less.

    if(superclassIndent > 0)
      superclassIndent -= 1;

    super.printValues(superclassIndent, printStream);

    firingEntityID.printValues(indentLevel, printStream);
    targetEntityID.printValues(indentLevel, printStream);
    munitionID.printValues(indentLevel, printStream);
    eventID.printValues(indentLevel, printStream);

    printStream.println(indent + "fireMissionIndex: "    + fireMissionIndex.longValue());

    locationInWorldCoordinate.printValues(indentLevel, printStream);

    burstDescriptor.printValues(indentLevel, printStream);
    velocity.printValues(indentLevel, printStream);

    printStream.println(indent + "range: "   + range);

    return;
}

    //Accessor methods

/**
 * Gets firing entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record 
 * Unique to the exercise.
 * @return a clone of the firing entity ID
 */
public EntityID getFiringEntityID()
{   
	return (EntityID)firingEntityID.clone();
}

/**
 * Sets firing entity ID
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record 
 * Unique to the exercise.
 * @param pFiringEntityID the firing entity ID
 */
public void setFiringEntityID(EntityID pFiringEntityID)
{
	firingEntityID = pFiringEntityID;
}

/**
 * Gets target entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record 
 * Unique to the exercise.
 * @return a clone of the target entity ID
 */
public EntityID getTargetEntityID()
{   
	return (EntityID)targetEntityID.clone();
}

/**
 * Sets target entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record 
 * Unique to the exercise.
 * @param pTargetEntityID target entity ID value
 */
public void setTargetEntityID(EntityID pTargetEntityID)
{
	targetEntityID = pTargetEntityID;
}
/**
 * Gets the munition ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record 
 * Unique to the exercise.
 * @return a clone of a munition ID 
 */
public EntityID getMunitionID()
{   
	return (EntityID)munitionID.clone();
}

/**
 * Sets the munition ID
 * @param pMunitionID a munition ID 
 */
public void setMunitionID(EntityID pMunitionID)
{
	munitionID = pMunitionID;
}

/**
 * Gets the event ID
 * The event identification shall be specified by the Event Identifier Record. 
 * The record shall consist of a Simulation Address Record and an Event Number. 
 * The latter is uniquely assigned within the host by the simulation application that initiates the sequence of events. 
 * The Event Identifier Record shall be set to one for each exercise and incremented by one for each fire 
 * event or collision event. 
 * In the case where all possible values are exhausted, the numbers may be reused, beginning at one.
 * @return a clone of an event ID 
 */
public EventID getEventID()
{   
	return (EventID)eventID.clone();
}

/**
 * Sets the event ID
 * The event identification shall be specified by the Event Identifier Record. 
 * The record shall consist of a Simulation Address Record and an Event Number. 
 * The latter is uniquely assigned within the host by the simulation application that initiates the sequence of events. 
 * The Event Identifier Record shall be set to one for each exercise and incremented by one for each fire 
 * event or collision event. 
 * In the case where all possible values are exhausted, the numbers may be reused, beginning at one.
 * @param pEventID eventID value
 */
public void setEventID(EventID pEventID)
{
	eventID = pEventID;
}

/**
 * Gets the fire mission index
 * @return a clone an unsigned integer value of fire mission index
 */
public UnsignedInt getFireMissionIndex()
{   
	return (UnsignedInt)fireMissionIndex.clone();
}

/**
 * Sets the fire mission index
 * @param pFireMissionIndex an unsigned integer value for fire mission index
 */
public void setFireMissionIndex(UnsignedInt pFireMissionIndex)
{
	fireMissionIndex = pFireMissionIndex;
}

/**
 * Sets the fire mission index
 * @param pFireMissionIndex an integer value for fire mission index
 */
public void setFireMissionIndex(int pFireMissionIndex)
{ 
	fireMissionIndex = new UnsignedInt(pFireMissionIndex);
}

/**
 * Gets the location in world coordinate vector
 * Location of the origin of the entity's coordinate system shall be specified by a set of three coordinates:
 * X, Y, and Z. The shape of the earth shall be specified using WGS 84. The origin of the world coordinate system 
 * shall be the centroid of the earth, with the X-axis passing through the Prime Meridian at the Equator, the
 * Y-axis passing through 90 degrees East longitude at the Equator, and the Z-axis passing through the North pole. 
 * These coordinates shall represent meters from the centroid of the earth. A 64-bit double precision floating point 
 * number shall represent the location for each coordinate.
 * @return the location in world coordinates
 */
public WorldCoordinate getLocationInWorldCoordinate()
{   
	return (WorldCoordinate)locationInWorldCoordinate.clone();
}

/**
 * Sets the location in world coordinate vector
 * Location of the origin of the entity's coordinate system shall be specified by a set of three coordinates:
 * X, Y, and Z. The shape of the earth shall be specified using WGS 84. The origin of the world coordinate system 
 * shall be the centroid of the earth, with the X-axis passing through the Prime Meridian at the Equator, the
 * Y-axis passing through 90 degrees East longitude at the Equator, and the Z-axis passing through the North pole. 
 * These coordinates shall represent meters from the centroid of the earth. A 64-bit double precision floating point 
 * number shall represent the location for each coordinate.
 * @param pWorldCoordinate the world coordinates 
 */
public void setLocationInWorldCoordinate(WorldCoordinate pWorldCoordinate)
{
	locationInWorldCoordinate = pWorldCoordinate;
}

/**
 * Sets the location in world coordinate vector (x, y, z).
 * Location of the origin of the entity's coordinate system shall be specified by a set of three coordinates:
 * X, Y, and Z. The shape of the earth shall be specified using WGS 84. The origin of the world coordinate system 
 * shall be the centroid of the earth, with the X-axis passing through the Prime Meridian at the Equator, the
 * Y-axis passing through 90 degrees East longitude at the Equator, and the Z-axis passing through the North pole. 
 * These coordinates shall represent meters from the centroid of the earth. A 64-bit double precision floating point 
 * number shall represent the location for each coordinate.
 * @param pX the X coordinate in world coordinates
 * @param pY the Y coordinate in world coordinates
 * @param pZ the Z coordinate in world coordinates
 */
public void setLocationInWorldCoordinate(double pX, double pY, double pZ)
{ 
	locationInWorldCoordinate = new WorldCoordinate(pX, pY, pZ);
}

/**
 * Gets the burst descriptor.
 * The firing of a round or a burst of ammunition shall be represented by a Burst Descriptor Record.  
 * This record shall specify the type of ammunition fired, the type of warhead, the type of fuse,
 * the number of rounds fired, and the rate at which the rounds are fired in rounds per minute.
 * @return a clone of the burst descriptor record
 */
public BurstDescriptor getBurstDescriptor()
{  
	return (BurstDescriptor)burstDescriptor.clone();
}

/**
 * Sets the burst descriptor.
 * The firing of a round or a burst of ammunition shall be represented by a Burst Descriptor Record.  
 * This record shall specify the type of ammunition fired, the type of warhead, the type of fuse,
 * the number of rounds fired, and the rate at which the rounds are fired in rounds per minute.
 * @param pBurstDescriptor a burst descriptor record
 */
public void setBurstDescriptor(BurstDescriptor pBurstDescriptor)
{
	burstDescriptor = pBurstDescriptor;
}


/**
 * Gets the linear velocity vector.
 * Linear velocity shall be represented as a vector with three component.  
 * Each vector component shall represent velocity in meters per second   
 * @return a clone of the linear velocity vector
 */
public LinearVelocity getVelocity()
{   return (LinearVelocity)velocity.clone();
}

/**
 * Sets the linear velocity vector 
 * Linear velocity shall be represented as a vector with three component.  
 * Each vector component shall represent velocity in meters per second   
 * @param pVelocity a linear velocity vector
 */
public void setVelocity(LinearVelocity pVelocity)
{
	velocity = pVelocity;
}

/**
 * Sets the linear velocity vector (x, y, z)
 * Each field represents velocity in meters per second in x, y, z vector respectively
 * @param pX the X velocity vector
 * @param pY the Y velocity vector
 * @param pZ the Z velocity vector
 */
public void setVelocity(float pX, float pY, float pZ)
{ 
	velocity = new LinearVelocity(pX, pY, pZ);
}

/**
 * Gets the range value.
 * This field shall specify the range that an entity's fire control system has assumed in computing
 * the fire control solution.  
 * This field shall be represented by a 32bit floating point number in meters. 
 * For system where range is unknown or unavailable, this field shall contain a value of zero.  
 * @return a range value
 */
public float getRange()
{   
	return range;
}

/**
 * Sets the range value.
 * This field shall specify the range that an entity's fire control system has assumed in computing
 * the fire control solution.  
 * This field shall be represented by a 32bit floating point number in meters. 
 * For system where range is unknown or unavailable, this field shall contain a value of zero. 
 * @param pRange a range value 
 */
public void setRange(float pRange)
{
	range = pRange;
}

} // end of class FirePdu
