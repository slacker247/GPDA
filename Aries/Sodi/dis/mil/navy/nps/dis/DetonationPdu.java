/*
 File:		DetonationPdu.java
 CVS Info:	$Id: DetonationPdu.java,v 1.2 1998/01/27 18:44:05 mcgredo Exp $
 Compiler:	jdk 1.3
 */


package mil.navy.nps.dis;                      // package to which we belong

import mil.navy.nps.util.*;                    // General-purpose utilities
import mil.navy.nps.disEnumerations.*;         // Enumerations for DIS

import java.lang.*;
import java.util.*;                            // utility stuff we need
import java.io.*;                              // input/output for serialization

/**
 * Detonation or impact.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href=
 * "http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo
 * </a>)
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/
 * DetonationPdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/
 *  DetonationPdu.<java/a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/
 * DetonationPdu.java">
 *  ~/mil/navy/nps/dis/DetonationPdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg">
 * <IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg"
 * ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd> The detonation or impact of munitions shall be communicated by issuing
 * a Detonation PDU.
 *
 *<dt><b>Explanation:</b>
 *<dd>The detonation Pdu denotes the detonation of a weapon. It inherits the
 *  header information from ProtocolDataUnit, an abstract class that contains
 *  assorted protocol information. It implements the IDs of what's
 *  firing, what's being shot at, the munition, the event, and where
 *  the munition is headed.
 *
 *  As with other PDUs, it knows how to serialize and deserialize itself
 *  from the wire. It also knows how to clone itself, and knows how to
 *  calculate its size when sent to the wire.
 *
 *<dt><b>History:</b>
 *<dd>		16Dec96	/Don McGregor	    /New
 *<dd>		10Mar97	/Don McGregor	    /Cleaned up for javadoc
 *<dd>		16Apr97	/Don McGregor	    /PrintStream passed to printValues
 *<dd>		8Dec97	/Ronan Fauglas	    /changes for documentation
 *                                           templates + complements in
 *                                           documentation
 *<dd>		11Dec97	/Ronan Fauglas	    /changed access methods:
 *                                           thisVariable() -->
 *                                           getThisVariable()
 *<dd>          23Nov99 /Thomas Miller      /General cleanup and javadocs
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis/
 *              dis-dd/pdu/84.htm">Detonation PDU</a>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/
 *              WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/
 *              WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 4.4.3.3, 5.4.4.2
 *
 *@see ProtocolDataUnit
 *@see PduElement 
 *@see SerializationInterface
 *@see FirePdu
 */


public class DetonationPdu extends ProtocolDataUnit 
{
    /**
     *Firing Entity Identification - This field shall identify the firing entity
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>If the detonation is not preceded by a Fire PDU then the Firing Entity
     * Identification shall be NO SPECIFIC ENTITY.
     *</dl>
     */

    protected EntityID          firingEntityID;     // ID of entity
                                                    // that's doing the shooting

    /**
     *Target Entity Identification - This field shall identify the target entity
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd> If the target ID is unknown, this field shall contain the value
     * TARGET ID UNKNOWN.
     *</dl>
     */

    protected EntityID          targetEntityID;   // ID of entity that is
                                                  // undergoing darwinian
                                                  // selection

    /**
     *Munition Identification - This field shall specify the entity ID of the
     *fired munition if tracking data is required.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd> A munition ID shall have a value of MUNITION NOT TRACKED if tracking
     *  data for the munition is not required.
     *</dl>
     */

    protected EntityID          munitionID;        // ID of munition being shot

    /**
     *Event Identification - This field shall contain the same data as in the
     *Event Identification field of the Fire PDU that communicated the launch of
     *the munition.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>If the detonation is not preceded by a corresponding fire event, then
     *the Event Number field of the Event Identifier record shall
     *be zero (e.g., land mines detonation).
     *</dl>
     */

    protected EventID           eventID;                    // ID of event

    /**
     *Velocity - This field shall specify the velocity of the munition
     *immediately before detonation/impact.
     */

    protected LinearVelocity          velocity;      // Velocity of muntion just
                                                     // before detonation/impact

    /**
     *Location in World Coordinate - This field shall specify the location of
     *the detonation in world coordinates (x, y, z coordinates ).
     */

    // x,y,z location, in 64 bit coords
    protected WorldCoordinate     locationInWorldCoordinate;
    
    /**
     *Burst Descriptor - This field shall describe the type of munition
     *impacting or detonating, the warhead, the fuse, the quantity, and the rate
     */

    protected BurstDescriptor   burstDescription;           // info about burst

    /**
     *Location in Entity's coordinates - This field shall specify the location
     * of the detonation or impact in the target entity's coordinate system.
     *This information should be used for damage assessment.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>  If the ID of the target is unknown, this field shall contain
     *NO LOCATION.
     *</dl>
     */

    // location of impact, in entity local coords
    protected EntityCoordinate    locationInEntityCoordinates;

    /**
     *Detonation Result - This field shall specify the result of the detonation. 
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>  Enumeration. See information below for more information
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis/
     *dis-dd/pdu/21.htm">Force ID Field</a>
     *<dd>	See Section 5 in EBV-DOC
     *</dl>
     */

    protected UnsignedByte      detonationResult;       // Blood & guts, usually

    // not used; we query the list for this instead
    //protected UnsignedByte        articulationParamterCount;
     // unused; we write out padding directly in serialize
    //protected UnsignedShort       padding;

    /**
     *List of articulation parameters.
     *
     *<dl>
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis/
     *dis-dd/pdu/6d.htm">Articulation Parameter Record</a>
     *<dd>	See Section 5 in EBV-DOC
     *</dl>
     */

    // Vector of articulation parameters
    protected Vector            articulationParameters;

    /**
     *Constant value--size of an Detonation PDU with header, without the
     *articulation parameters.
     *<code>sizeOf = 104 bytes</code>
     */

    // size of "stripped" DetonationPDU, AS WRITTEN TO WIRE
    public static final int   sizeOf = 104;


/**
 *Default constructor--fills with zeros for all values.
 */

public DetonationPdu()
{

    super.setPduType(PduTypeField.DETONATION);

    firingEntityID  = new EntityID();
    targetEntityID  = new EntityID();
    munitionID      = new EntityID();
    eventID         = new EventID();
    // special nerf ammo by default--hits at zero speed.  let's play nice!
    velocity        = new LinearVelocity(0,0,0);

    locationInWorldCoordinate        = new WorldCoordinate(0, 0, 0);

    burstDescription = new BurstDescriptor();

    locationInEntityCoordinates = new EntityCoordinate(0,0,0);
    detonationResult = new UnsignedByte(0);
    articulationParameters = new Vector();

    return;

}  //  end default constructor


/** clone - make a copy of the object. This requires a deep copy, so we don't
 *  have two objects sharing pointers to the same data.
 *  @return the cloned object
 */

public Object clone()
{

 DetonationPdu  newDetonationPdu = (DetonationPdu)super.clone();
 int            idx = 0;

 newDetonationPdu.setFiringEntityID(this.getFiringEntityID());
 newDetonationPdu.setTargetEntityID(this.getTargetEntityID());
 newDetonationPdu.setMunitionID(this.getMunitionID());
 newDetonationPdu.setEventID(this.getEventID());
 newDetonationPdu.setVelocity(this.getVelocity());

 newDetonationPdu.setLocationInWorldCoordinate
                 (this.getLocationInWorldCoordinate());

 newDetonationPdu.setBurstDescription(this.getBurstDescription());
 newDetonationPdu.setDetonationResult(this.getDetonationResult());
 
 // Each of the articulation parameters needs to be cloned as well--it's not
 // good enough to just clone the Vector alone.

 for(idx = 0; idx < articulationParameters.size(); idx++)
 {
    newDetonationPdu.addArticulationParameter(
                     this.getArticulationParameterAt(idx));
 }  //  end for

 return newDetonationPdu;

}  //  end clone


/** serialize - writes the detonation PDU to the wire in DIS format
 *  Order is important here, since it needs to conform to the DIS standard.
 *  @param DataOutputStream
 *  @return void
 */

public void serialize(DataOutputStream outputStream)
{
    UnsignedShort   padding = new UnsignedShort(0);
    UnsignedByte    parameterCount;
    int             idx;

    // write out the data to an output stream.

    super.serialize(outputStream);      // write out header info
    

    firingEntityID.serialize(outputStream);
    targetEntityID.serialize(outputStream);
    munitionID.serialize(outputStream);
    eventID.serialize(outputStream);
    velocity.serialize(outputStream);
    locationInWorldCoordinate.serialize(outputStream);
    burstDescription.serialize(outputStream);
    locationInEntityCoordinates.serialize(outputStream);
    detonationResult.serialize(outputStream);

    // We synthisize these from the list data or out of thin air
    parameterCount = new UnsignedByte(articulationParameters.size());

    parameterCount.serialize(outputStream);
    padding.serialize(outputStream);              // aligns us w/ byte boundary

    for(idx = 0; idx < articulationParameters.size(); idx++)
    {
        ArticulationParameter  aParameter;

        aParameter = (ArticulationParameter)
                        articulationParameters.elementAt(idx);
        aParameter.serialize(outputStream);

    }  //  end for

    return;

}  // end serialize


/** deserialize - reads the detonation PDU to the wire in DIS format
 *
 *  @param DataOutputStream
 *  @return void
 */

public void  deSerialize(DataInputStream inputStream)
{

 // deserialize our data from the stream. We first call the superclass,
 // so that all the data there is read into the buffer first.
 // Then we deserialize each of our ivars to the stream, being careful
 //  about the order.

    int             idx = 0;
    UnsignedByte    articulationParameterCount = new UnsignedByte();
    UnsignedShort   padding = new UnsignedShort(0);

    super.deSerialize(inputStream);

    // Do our ivars

    firingEntityID.deSerialize(inputStream);
    targetEntityID.deSerialize(inputStream);
    munitionID.deSerialize(inputStream);
    eventID.deSerialize(inputStream);
    velocity.deSerialize(inputStream);
    locationInWorldCoordinate.deSerialize(inputStream);
    burstDescription.deSerialize(inputStream);
    locationInEntityCoordinates.deSerialize(inputStream);
    detonationResult.deSerialize(inputStream);

    articulationParameterCount.deSerialize(inputStream);
    padding.deSerialize(inputStream);

    for(idx = 0; idx < articulationParameterCount.intValue(); idx++)
    {
        ArticulationParameter  aParameter = new ArticulationParameter();

        aParameter.deSerialize(inputStream);
        articulationParameters.addElement(aParameter);
            
    }  //  end for

    return;

}   //  end deserialize


/** 
 *  length returns basic size + size of list of parameters.
 *  @return int representing the length
 */
 
public int length()
{
    
    return sizeOf + (articulationParameters.size() *
                     ArticulationParameter.sizeOf);
}  //  end length


/** 
 *  pduName returns the type of PDU
 *  @return String 
 */
 
public String pduName()
{
  return new String("Detonation PDU");
}  //  end pduName


/** 
 *  printValues - print the values of the object out, with correct level of
 *  indentation on the page..
 *  @param int indentLevel
 *  @param PrintStream printStream
 *  @return void
 */
 
public void printValues(int indentLevel, PrintStream printStream)
{
       
  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println();
    printStream.println("Detonation PDU-");

    if(superclassIndent > 0)
      superclassIndent -= 1;

    super.printValues(superclassIndent, printStream);

    firingEntityID.printValues(indentLevel, printStream);
    targetEntityID.printValues(indentLevel, printStream);
    munitionID.printValues(indentLevel, printStream);
    eventID.printValues(indentLevel, printStream);
    velocity.printValues(indentLevel, printStream);
    locationInWorldCoordinate.printValues(indentLevel, printStream);
    burstDescription.printValues(indentLevel, printStream);
    locationInEntityCoordinates.printValues(indentLevel, printStream);
    printStream.println(indent + "detonationResult: " 
                        + detonationResult.intValue());
    printStream.println(indent + "Parameter count: " 
                        + articulationParameters.size());

    for(idx = 0; idx < articulationParameters.size(); idx ++)
     {
        ArticulationParameter   aParam;

        aParam = (ArticulationParameter)articulationParameters.elementAt(idx);
        
        // indent the parts of us a bit
        aParam.printValues(indentLevel + 4, printStream);                
     }  //  end for

    return;
}  //  end printValues


/**
 *Returns the Articulation Parameter at the index given in parameter.
 *
 *@param int pIdx the index of the Articulation Parameter we want to obtain
 *@return  the Articulation Parameter pointed by the index in parameter
 */
 
public ArticulationParameter getArticulationParameterAt(int pIdx)
{
 ArticulationParameter  aParameter;

  // return a copy of the pIdx-th articulation parameter.

 aParameter = (ArticulationParameter)articulationParameters.elementAt(pIdx);

 return (ArticulationParameter)aParameter.clone();
 
}  // end getArticulationParameter

 
/**
 *Inserts a Articulation Parameter at the end of the list and increment the 
 * parameter count accordingly.
 *
 *@param pParameter the Articulation Parameter to be inserted in the list
 */
 
public void addArticulationParameter(ArticulationParameter pParameter)
{
  articulationParameters.addElement(pParameter);
}  // end addArticulationParameter


    //Accessor methods

public EntityID getFiringEntityID()
{   return (EntityID)firingEntityID.clone();
}


public void setFiringEntityID(EntityID pFiringEntityID)
{firingEntityID = pFiringEntityID;
}


public EntityID getTargetEntityID()
{   return (EntityID)targetEntityID.clone();
}


public void setTargetEntityID(EntityID pTargetEntityID)
{targetEntityID = pTargetEntityID;
}


public EntityID getMunitionID()
{   return (EntityID)munitionID.clone();
}


public void setMunitionID(EntityID pMunitionID)
{munitionID = pMunitionID;
}


public EventID getEventID()
{   return (EventID)eventID.clone();
}


public void setEventID(EventID pEventID)
{eventID = pEventID;
}


public LinearVelocity getVelocity()
{   return (LinearVelocity)velocity.clone();
}


public void setVelocity(LinearVelocity pVelocity)
{velocity = pVelocity;
}
public void setVelocity(float x, float y, float z)
{
    velocity = new LinearVelocity(x, y, z);
}

public WorldCoordinate getLocationInWorldCoordinate()
{   return (WorldCoordinate)locationInWorldCoordinate.clone();
}


public void setLocationInWorldCoordinate(WorldCoordinate pLocation)
{locationInWorldCoordinate = pLocation;
}


public void setLocationInWorldCoordinate(double x, double y, double z)
{
    locationInWorldCoordinate = new WorldCoordinate(x, y, z);
}


public BurstDescriptor getBurstDescription()
{   return (BurstDescriptor)burstDescription.clone();
}


public void setBurstDescription(BurstDescriptor pBurstDescription)
{burstDescription = pBurstDescription;
}


public EntityCoordinate getLocationInEntityCoordinates()
{   return (EntityCoordinate)locationInEntityCoordinates.clone();
}


public void setLocationInEntityCoordinates(EntityCoordinate pLocationInEntityCoordinates)
{locationInEntityCoordinates = pLocationInEntityCoordinates;
}


public void setLocationInEntityCoordinates(float x, float y, float z)
{
    locationInEntityCoordinates = new EntityCoordinate(x, y, z);
}


public UnsignedByte getDetonationResult()
{   return (UnsignedByte)detonationResult.clone();
}


public void setDetonationResult(UnsignedByte pDetonationResult)
{detonationResult = pDetonationResult;
}


public void setDetonationResult(int pDetonationResult)
{ detonationResult = new UnsignedByte(pDetonationResult);
}


} // end of class DetonationPdu

