/*
 File:		EntityID.java
 CVS Info:	$Id: EntityID.java,v 1.5 1998/02/11 21:04:29 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package PDUs;       // our package

import mil.navy.nps.util.*;         // General-purpose utilities
import disEnumerations.*;         // Enumerations for DIS

import java.io.*;               // input/output library
import java.util.*;             // utilities

/**
 * Record uniquely identifying an entity.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EntityID.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EntityID.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/EntityID.java">
 *  ~/mil/navy/nps/dis/EntityID.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>Each Entity in a given exercise executing on a DIS application shall be assigned
 *  an Entity Identifier Record Unique to the exercise.
 *
 *<dt><b>Explanation:</b>
 *<dd>EntityID is a composite of several values that are commonly found together,
 *  and that,when combined, uniquely identify an entity in a simulation.
 *  It's used in a variety of PDUs, typically to identify a receving and a
 *  sending entity. Since it uniquely identifies an entity in a virtual world, 
 *  it's a good candidate to act as a hash table key.<P>
 *
 *<dt><b>Note:</b>
 *<dd>Note that I've "flattened" the Simulation Address Record, so that you instead 
 *  of the simulation address record , you will find the Site ID and the Application ID.
 *  This is a questionnable move.
 *
 *<dt><b>History:</b>
 *<dd>		15Nov96	/Don McGregor		/New
 *<dd>		10Mar97	/Don McGregor		/cleaned up for javadoc
 *<dd>		16Apr97	/Don McGregor		/PrintStream passed to printValues
 *<dd>		12Aug97 /DonB    		/elaborated printValues
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changes access methods names from "variable()" to "getVariable()"
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary :<A HREF="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/2c.htm">Entity Identifier Record</A>
 *<dd>		DIS specification : IEEE 1278.1, 5.3.14
 *  
 *@see ProtocolDataUnit
 *@see PduElement 
 *@see SerializationInterface
 */

public class EntityID extends PduElement
{

  public static final int SHORT_HIGH_BYTE_MASK = 0xff00;
  public static final int SHORT_LOW_BYTE_MASK  = 0x00ff;
/**
 *Site the entity came from.
 *Each DIS Site has a unique Identifier. 0, 2^16-1 and 2^16-2 are reserved.
 *
 *<dl>
 *<dt><b>Value:</b>
 *<dd> No site shall be assigned an ID containin all zero's, (2e16-1) or (2e16-2). The mechanism by which Site IDs
 *  are assigned is outside the scope of this standard.
 *  The simulation manager shall use the reserved site IDs to identify receivers 
 *  of Simulation Management PDUs. A site ID equal to zero shall mean no site; 
 *  this may be used to annotate a PDU log. A site ID equal to all ones (2e16-1) shall mean all sites;
 *  this may be used to start all sites. An application ID equal to (2e16-2) shall have no meaning 
 *  but is reserved for consistency.
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/2e.htm">Site Identifier Field</a>
 *<dd>		DIS specification : IEEE 1278.1, 5.3.14.1.1
 *</dl>
 */
protected    UnsignedShort   siteID;               // site entity came from

/**
 *Application at that site this came from.
 *Each DIS application has a unique Identifier on a given site. 0, 2^16-1 and 2^16-2 are reserved.
 *
 *<dl>
 *<dt><b>Value:</b>
 *<dd> No simulation application shall be assigned an id containin
 *  all zeros, (2e16-1), or (2e16-2). One or more simulation applications may reside 
 *  in a single host computer.
 *  The mechanism by which application IDs are assigned is outside the scope of this standard. 
 *  The simulation manager shall use the reserved application IDs to identify receivers 
 *  of Simulation Management PDUs. An application ID equal to zero shall mean no application (NO_APPLIC). 
 *  An application ID equal to all ones (2e16-1) shall mean all applications (ALL_APPLIC); this may be used 
 *  to start all applications within a site. An application ID equal to (2e16-2) shall have no
 *  meaning but is reserved for consistency.
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/2f.htm">Application Identifier Field</a>			
 *<dd>		DIS specification : IEEE 1278.1, 5.3.14.1.2
 *</dl>
 */
protected    UnsignedShort   applicationID;        // application at that site this came from

/**
 *Entity within the current application.
 *Each Entity has a unique Identifier on a given site. 0, 2^16-1 and 2^16-2 are reserved.
 *
 *<dl>
 *<dt><b>Value:</b>
 *<dd> This identifier is valid for the duration of the exercise; however, entity IDs shall be reused 
 *  when all possible entity IDs have been exhausted. No entity shall have an ID of zero, (2e16 -1) 
 *  or (2e16-2) (which is NO_ENTITY, ALL_ENTITIES or RQSRT_ASSIGN_ID). This number need not be registered or retained for future exercises. 
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/31.htm"> Entity Identity Field</a>			
 *<dd>		DIS specification : IEEE 1278.1, 5.3.14.2
 *</dl>
 */
protected    UnsignedShort   entityID;             // entity ID within that application

/**
 *Constant value--size of an EntityID as written out to the wire.
 *<code>sizeOf = 6 bytes</code>
 */
private static final int sizeOf = 6;// size of an EntityID, as written to wire.

/**
 *Default constructor--fills with zeros for all values.
 */
public EntityID()                       // constructor
{
    siteID        = new UnsignedShort(0);
    applicationID = new UnsignedShort(0);
    entityID      = new UnsignedShort(0);

    return;
}

/**
 *Constructs a new Entity Identifier, with variables values passed in parameters.
 *
 *@param pSiteID the Site Identifier
 *@param pApplicationID the Application Identifier for that site
 *@param pEntityID the Entity identifier for that application
 */
public EntityID(short pSiteID, short pApplicationID, short pEntityID)
{
    siteID        = new UnsignedShort(pSiteID);
    applicationID = new UnsignedShort(pApplicationID);
    entityID      = new UnsignedShort(pEntityID);

    return;
}

/**
 *Constructs a new Entity Identifier, with variables values passed in parameters.
 *This is a convienence method for when you've got integers and don't want to
 * convert to shorts, or if you have unsigned shorts that are greater than
 * a signed short.
 *
 *@param pSiteID the Site Identifier
 *@param pApplicationID the Application Identifier for that site
 *@param pEntityID the Entity identifier for that application
 */
public EntityID(int pSiteID, int pApplicationID, int pEntityID)
{
    siteID        = new UnsignedShort(pSiteID);
    applicationID = new UnsignedShort(pApplicationID);
    entityID      = new UnsignedShort(pEntityID);

    return;
}


/*
 *Makes a deep copy of the object.
 *This requires a deep copy, so we don't have two objects sharing pointers to the same data.
 *
 *@return a copy of this object
 */ 
public Object clone()
{
    // make a copy of the object. This requires a deep copy, so we don't have two
    // objects sharing pointers to the same data.

 EntityID   newEntityID = new EntityID();

 newEntityID.setSiteID(this.getSiteID());
 newEntityID.setApplicationID(this.getApplicationID());
 newEntityID.setEntityID(this.getEntityID());

 return newEntityID;
}

public void serialize(DataOutputStream outputStream)
{
    //super.serialize(outputStream);        // No need to call super, since no ivars are in our direct superclass
    
    siteID.serialize(outputStream);
    applicationID.serialize(outputStream);
    entityID.serialize(outputStream);
    
    return;
}   
    
public void deSerialize(DataInputStream inputStream)
{
  // no need to call super, since there are no ivars in the superclass.

    siteID.deSerialize(inputStream);
    applicationID.deSerialize(inputStream);
    entityID.deSerialize(inputStream);

    return;
}

public int length()
{
    return sizeOf;          // entity IDs are this long, always.
}

public void printValues(int indentLevel, PrintStream printStream)
{
    // print the values of the object out, with correct level of
    // indentation on the page.
 
  StringBuffer  indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int           idx, superclassIndent = indentLevel;
  int           siteIDValue, appIDValue;
  int           dottedDecimal[] = new int[4];


    printStream.println(indent + "EntityID siteID: " + siteID.intValue());
    printStream.println(indent + "EntityID applicationID: " + applicationID.intValue());

    siteIDValue = siteID.intValue();
    appIDValue  = applicationID.intValue();

    // Since we often use the four bytes in site & application ID to hold the IP number of the
    // sending machine, we also print out the four bytes in dotted decimal format, eg
    // 131.120.7.205.  We just get these values by masking out the appropriate bytes.

    // mask all but high byte, then shift right 8 bits, with zero-fill on the left.
    dottedDecimal[0] = siteIDValue & SHORT_HIGH_BYTE_MASK;
    dottedDecimal[0] = dottedDecimal[0] >>> 8;

    dottedDecimal[1] = siteIDValue & SHORT_LOW_BYTE_MASK;

    dottedDecimal[2] = appIDValue & SHORT_HIGH_BYTE_MASK;
    dottedDecimal[2] = dottedDecimal[2] >>> 8;

    dottedDecimal[3] = appIDValue & SHORT_LOW_BYTE_MASK;


    printStream.println(indent + "Dotted decimal format: " + dottedDecimal[0] + "." + dottedDecimal[1] + "." +
                        dottedDecimal[2] + "." + dottedDecimal[3]);

    printStream.println(indent + "EntityID entityID: " + entityID.intValue());

    return;
}

public String toString ()
{
    return (	"(site, application, entity ID) = (" + siteID.toString() +
    		", " + applicationID.toString() +
		", " + entityID.toString() + ")");
}

/** 
 *This provides a hash code for the object.
 *A Hash code is a handy way to uniquely identify entities.
 *
 *<ul><li>The problem:<br>
 *There's a problem with the core API, since there are 48 bits of information in the object, 
 *but the hash code has only 32 bits to put things in. So we have to hash on some subset
 *of the data available.
 *<li>The solution:<br>
 *The solution right now is to ignore the application ID when generating the hash code. 
 *Furthermore, the entity ID is in the upper 16 bits, while the site ID is in the lower 16 bits. 
 *The idea is that this will help spread out the hash code a bit more, since there are probably
 *many more entities than sites, and putting the part with more information
 *in the MSB helps spread things out. This helps prevent collisions, which improves
 *performance.
 *</ul>
 *
 *@return a hash code value for this object
 */
public int hashCode()
{

    int hashCode = 0;
    int site = siteID.intValue();
    int entity = entityID.intValue();

    hashCode = entity << 16;      // shift 16 bits to the left, zero-fill on right.
    hashCode = hashCode | site;   // stick the site ID bits in the lower half of the int
                                  // (bit mask is faster)

    return hashCode;
}

/** 
 *Makes a "numeric equality" test. 
 *
 *@param obj the object want to be compared with this object.
 *@return yes if the object in parameter is of the same type and 
 * if the numerical values of both objects are equal.
 */
public boolean equals(Object obj) 
{
    int         site, application, entity;            // my internal values
    int         objSiteID, objApplicationID, objEntityID;   // internal values of the other guy
    EntityID    compareObj;

    if(obj == null)
        return false;

    // if the object we're being compared to isn't an EntityID, we bloody well
    // better not be equal.

    if(this.getClass() != obj.getClass())
        return false;

    // now that we're sure, cast to an object of this type
    compareObj = (EntityID)obj;
    
    // grumble...oughta be a more elegant way to do this.

    site        = siteID.intValue();
    application = applicationID.intValue();
    entity      = entityID.intValue();

    objSiteID        = compareObj.getSiteID().intValue();
    objApplicationID = compareObj.getApplicationID().intValue();
    objEntityID      = compareObj.getEntityID().intValue();

    if((site == objSiteID) && (application == objApplicationID) && (entity == objEntityID))
        return true;

    return false;
}

    // accessor methods. The get() methods return a copy of the instance variable, rather than
    // the instance variable itself.

public UnsignedShort getSiteID()
{   return (UnsignedShort)siteID.clone();
}
public void setSiteID(UnsignedShort pSiteID)
{siteID = pSiteID;
}
public void setSiteID(int pSiteID)
{ siteID = new UnsignedShort(pSiteID);
}

public UnsignedShort getApplicationID()
{   return (UnsignedShort)applicationID.clone();
}
public void setApplicationID(UnsignedShort pApplicationID)
{applicationID = pApplicationID;
}
public void setApplicationID(int pApplicationID)
{ applicationID = new UnsignedShort(pApplicationID);
}


public UnsignedShort getEntityID()
{   return (UnsignedShort)entityID.clone();
}
public void setEntityID(UnsignedShort pEntityID)
{entityID = pEntityID;
}
public void setEntityID(int pEntityID)
{ entityID = new UnsignedShort(pEntityID);
}

} // End of class EntityID
