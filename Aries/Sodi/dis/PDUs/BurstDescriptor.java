/*
 File:		BurstDescriptor.java
 CVS Info:	$Id: BurstDescriptor.java,v 1.2 1998/01/27 18:43:55 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package PDUs;               // our package

import mil.navy.nps.util.*;         // General-purpose utilities
import disEnumerations.*;         // Enumerations for DIS

import java.io.*;
import java.util.*;

/**
 * Weapons burst (firing) description record.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/BurstDescriptor.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/BurstDescriptor.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/BurstDescriptor.java">
 *  ~/mil/navy/nps/dis/BurstDescriptor.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>The firing of a round or a burst of ammunition shall be represented by a Burst Descriptor Record.
 *<dt><b>Explanation</b>
 *<dd>Describes information about a weapons burst fired by some entity, such as the
 *  warhead type, the fuze, and how much was fired.<p>
 * Includes the EntityType record, which describes the country and assorted
   other geeky information.<p>
 *<dt><b>History:</b>
 *<dd>		13Dec96	/Don McGregor    	/New
 *<dd>		05Mar97	/Don McGregor    	/Cleaned up for javadoc
 *<dd>		16Apr97	/Don McGregor    	/PrintStream passed to printValues
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changes access methods names from "variable()" to "getVariable()"
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/7e.htm">Burst Descriptor Record</a>
 *<dd>		DIS specification : IEEE 1278.1, 5.3.7
 *
 *@see PduElement 
 *@see SerializationInterface
 *@see FirePdu
 *@see DetonationPdu
 */
public class BurstDescriptor extends PduElement
{
    /**
     *Munition Record.
     */
    protected EntityType        munition;                       // describes country & type of munition

    /**
     *This filed shall describe the kind of explosive.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>Enumeration, see references below for values.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/7f.htm">Warhead Field</a>
     *<dd>	see Section 5 in EBV-DOC.	
     *</dl>
     */
    protected UnsignedShort     warhead;                        // type of warhead

    /**
     *Type of fuse.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>Enumeration, see references below for values.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/80.htm">Fuse Field</a>	
     *<dd>	see Section 5 in EBV-DOC.
     *</dl>
     */
    protected UnsignedShort     fuse;                           // type of fuse

    /**
     *Quantity shall represent the number of rounds fired in the burst for the munition specified.
     *
     *<dl>
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/81.htm">Quantity Field</a>	
     *</dl>
     */
    protected UnsignedShort     quantity;                       // how much was fired in the burst
 
    /**
     *The Rate shall represent the rounds per minute for the munition specified.
     *
     *<dl>
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/82.htm">Rate Field</a>	
     *</dl>
     */
    protected UnsignedShort     rate;                           // rate of fire for the burst

    /**
     *Constant value--size of a Burst Desciptor Record; here :<code>sizeOf = 16 bytes</code>.
     */
    public static final int     sizeOf = 16;                    // 16 bytes long when written to wire

/**
 *Default constructor. Initializes everything to zero, basically.
 */
public BurstDescriptor()
{
    munition = new EntityType();
    warhead  = new UnsignedShort();
    fuse     = new UnsignedShort();
    quantity = new UnsignedShort();
    rate     = new UnsignedShort();

    return;
}

public Object clone()
{
    // make a copy of the object. This requires a deep copy, so we don't have two
    // objects sharing pointers to the same data.

    BurstDescriptor newBurstDescriptor = new BurstDescriptor();

    newBurstDescriptor.setMunition(this.getMunition());
    newBurstDescriptor.setWarhead(this.getWarhead());
    newBurstDescriptor.setFuse(this.getFuse());
    newBurstDescriptor.setQuantity(this.getQuantity());
    newBurstDescriptor.setRate(this.getRate());

    return newBurstDescriptor;
}

/* serialize the object in DIS format to a stream.
@param outputStream stream the object is being serialized to
*/

public void serialize(DataOutputStream outputStream)
{
    // write out the data to an output stream. Order is important here, since
    // it needs to conform to the DIS standard.

    //super.serialize(outputStream);        // No need to call super, since no ivars are in our direct superclass
    
    munition.serialize(outputStream);
    warhead.serialize(outputStream);
    fuse.serialize(outputStream);
    quantity.serialize(outputStream);
    rate.serialize(outputStream);

    return;
}


public void deSerialize(DataInputStream inputStream)
{
    // order is important here, since we need to read in the same order as
    // specified by the DIS standard.
    // no need to call super, since there are no ivars in the superclass.

    munition.deSerialize(inputStream);
    warhead.deSerialize(inputStream);
    fuse.deSerialize(inputStream);
    quantity.deSerialize(inputStream);
    rate.deSerialize(inputStream);

    return;
}

public int length()
{
    return sizeOf;          // BurstDescriptors are this long, always.
}

public void printValues(int indentLevel, PrintStream printStream)
{
    // print the values of the object out, with correct level of
    // indentation on the page.

    StringBuffer buf = ProtocolDataUnit.getPaddingOfLength(indentLevel);
    int idx, superclassIndent = indentLevel;

    munition.printValues(indentLevel, printStream);
    printStream.println(buf + "warhead: " + warhead.intValue());
    printStream.println(buf + "fuse: "    + fuse.intValue());
    printStream.println(buf + "quantity: "+ quantity.intValue());
    printStream.println(buf + "rate: "    + rate.intValue());

    return;
}

// Accessor methods

public EntityType getMunition()
{ return (EntityType)munition.clone();
}
public void setMunition(EntityType pMunition)
{ munition = pMunition;
}

public UnsignedShort getWarhead()
{   return (UnsignedShort)warhead.clone();
}
public void setWarhead(UnsignedShort pWarhead)
{warhead = pWarhead;
}
public void setWarhead(int pWarhead)
{ warhead = new UnsignedShort(pWarhead);
}


public UnsignedShort getFuse()
{   return (UnsignedShort)fuse.clone();
}
public void setFuse(UnsignedShort pFuse)
{fuse = pFuse;
}
public void setFuse(int pFuse)
{fuse = new UnsignedShort(pFuse);
}

public UnsignedShort getQuantity()
{   return (UnsignedShort)quantity.clone();
}
public void setQuantity(UnsignedShort pQuantity)
{quantity = pQuantity;
}
public void setQuantity(int pQuantity)
{ quantity = new UnsignedShort(pQuantity);
}

public UnsignedShort getRate()
{   return (UnsignedShort)rate.clone();
}
public void setRate(UnsignedShort pRate)
{rate = pRate;
}
public void setRate(int pRate)
{ rate = new UnsignedShort(pRate);
}

} // end of class BurstDescriptor
