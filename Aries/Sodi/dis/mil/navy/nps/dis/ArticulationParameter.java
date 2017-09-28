/*
 File:		ArticulationParameter.java
 CVS Info:	$Id: ArticulationParameter.java,v 1.2 1998/01/27 18:43:54 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;                       // package we belong to

import mil.navy.nps.util.*;         // General-purpose utilities
import mil.navy.nps.disEnumerations.*;         // Enumerations for DIS

import java.io.*;                               // input/output library
import java.util.*;                             // utilities

/**
 * Articulation parameters are components of an entity that can move - this data is appended
 * to the EntityState PDU.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/ArticulationParameter.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/ArticulationParameter.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/ArticulationParameter.java">
 *  ~/mil/navy/nps/dis/ArticulationParameter.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>Location of the origin of the entity's coordinate system shall be specified 
 *  by a set of three coordinates: X, Y, and Z. 
 *
 *<dt><b>Explanation</b>
 *<dd>The articulation parameter describes how a part of an entity, such as a turret or fin,
 *  is oriented wrt the rest of the entity. <p>
 *
 *  This is a subclass of the PduElement class, the abstract class that all parts of a
 *  PDU inherit from. It knows how to serialize, deserialize, and clone itself. It
 *  is kept as a part of the EntityStatePdu.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21Oct96 /Don McGregor    	/New
 *<dd>		04Mar97 /Don McGregor    	/changes for javadoc
 *<dd>		16Apr97 /Don McGregor    	/PrintStream passed to printValues<BR>
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changes access methods names from "variable()" to "getVariable()"
 *<dd>		04Jan98	/Ronan Fauglas		/suppressed redundant comments for clone, printValues, etc...
 *<dd>		12Jan98	/Ronan Fauglas		/changed changeIndicator to parameterChangeIndicator.
 *<dd>		12Jan98	/Ronan Fauglas		/changed partID to articulationAttachmentID.
 *<dd>		31Mar99	/Don Brutzman, Don McGregor	/changed articulationParameter from long to double
 *<dd>		4Mar99	/Don Brutzman		/updated disEnumeration javadoc links
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary:
 *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/6d.htm">Articulation Parameter Record</a> (local) and
 *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/6d.htm">Articulation Parameter Record</a> (SISO)
 *<dd>		DIS specification : IEEE 1278.1, Section 5.3.5, Annex A
 *
 *@see PduElement 
 *@see DetonationPdu
 *@see SerializationInterface
 *@see EntityStatePdu
 */
public class ArticulationParameter extends PduElement
{
    /**
     *The identification of whether the Parameter Type Record is for an articulated 
     *  or attached part shall be designated by this field.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>Enumeration, see references below for values.
     *<dt><b>References:</b>
     *<dd>	disEnumeration class:
     *		<A href="../disEnumerations/ParameterTypeDesignatorField.html">Parameter Type Designator Field</a> (local)
     *<dd>	DIS Data Dictionary:
     *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/73.htm">Parameter Type Designator Field</a> (local) and
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/73.htm">Parameter Type Designator Field</a> (SISO)
     *</dl>
     */
    protected   UnsignedByte    parameterTypeDesignator;        // Type of parameter (enumeration)

    /**
     *The change of any parameter for any articulated part shall be indicated by a change indicator field.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>This field shall be set to o for each exercise and sequencially incremented by one
     *  for each change in articulation parameter. In the case all possible values are exhausted, the numbers 
     *  shall be reused beginning at zero.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary:
     *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/74.htm">Parameter Change Indicator Field</a> (local) and
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/74.htm">Parameter Change Indicator Field</a> (SISO)
     *<dd>	See section 4 in EBV-DOC
     *</dl>
     */
    protected   UnsignedByte    changeIndicator;                // change indicator    
    
    /**
     *The identification of the articulated part to which this articulation parameter is attached.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>This field shall contain the value 0 if the articulated part is attached directly to the entity.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary:
     *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/75.htm">Articulation Attachment ID Field</a> (local) and
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/75.htm">Articulation Attachment ID Field</a> (SISO)
     *</dl>
     */
    protected   UnsignedShort   articulationAttachmentID;                         // What part we're attached to 

    /**
     *The type of parameter represented shall be specified by an enumeration.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>Enumeration, see references below for values.
     *
     *<dt><b>References:</b>
     *<dd>	disEnumeration classes:
     *		<A href="../disEnumerations/ParameterTypeAttachedPartsField.html">Parameter Type Attached Parts Field</a> (local), or
     *		<A href="../disEnumerations/ParameterTypeArticulatedPartsLowBitsField.html">Parameter Type Articulated Parts Low Bits Field</a> (local) and
     *		<A href="../disEnumerations/ParameterTypeArticulatedPartsHighBitsField.html">Parameter Type Articulated Parts High Bits Field</a> (local) 
     *<dd>      DIS Data Dictionary:
     *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/72.htm">Parameter Type Variant</a> (local) and
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/72.htm">Parameter Type Variant</a> (SISO)
     *<dd>      DIS specification:  IEEE 1278.1, Annex A
     *<dd>      See Section 4 in EBV-DOC
     *</dl>
     */
    protected   UnsignedInt     parameterType;                  // parameter type

   /**
     *This field contains the value of the Articulation Parameter. 
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>The definition of the 64 bits shall be determined based on the type of parameter specified
     * in the ParameterType field. See References below for further information.
     *
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary:
     *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/76.htm">Articulation Parameter Record</a> (local) and
     *		<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/76.htm">Articulation Parameter Record</a> (SISO)
     *<dd>	DIS specification:  IEEE 1278.1, 5.3.5.4
     *<dd>	See Section 4 in EBV-DOC
     *</dl>
     */
//    protected   long            parameterValue;                 // 64 bits of parameter data 
      protected   double          parameterValue;                 // 64 bits of parameter data 

    /**
     *Constant value--size of an Articulation Parameter WHEN WRITTEN TO THE WIRE; here :<code>sizeOf = 16 bytes</code>.
     */
    public static final int     sizeOf = 16;                    // size, in bytes, of a serialized articulation parameter */


/**
 *Default constructor--fills with zeros for all values.
 */
public ArticulationParameter()                                  // default constructor
 {
    parameterTypeDesignator = new UnsignedByte(0);
    changeIndicator         = new UnsignedByte(0);
    articulationAttachmentID                  = new UnsignedShort(0);
    parameterType           = new UnsignedInt(0);
    parameterValue          = 0.0;

    return;
 }

public Object clone()
{

 ArticulationParameter  newArticulationParam = new ArticulationParameter();

 newArticulationParam.setParameterTypeDesignator(this.getParameterTypeDesignator());
 newArticulationParam.setChangeIndicator(this.getChangeIndicator());
 newArticulationParam.setArticulationAttachmentID(this.getArticulationAttachmentID());
 newArticulationParam.setParameterType(this.getParameterType());
 newArticulationParam.setParameterValue(this.getParameterValue());
 newArticulationParam.decrementChangeIndicator();

 return newArticulationParam;
}

/* 
 *@exception RuntimeException when IO Error occurs.
 */
public void serialize(DataOutputStream outputStream)
{
    //super.serialize(outputStream);        // No need to call super, since no ivars are in our direct superclass
    
    try
     {
        parameterTypeDesignator.serialize(outputStream);
        changeIndicator.serialize(outputStream);
        articulationAttachmentID.serialize(outputStream);
        parameterType.serialize(outputStream);
        outputStream.writeDouble(parameterValue);
     }
    catch(IOException ioException)      // catch-all for all IO exceptions in the above code
     {
        throw new 
            RuntimeException("Exception in ArticulationParameter. Error serializing unit.");
     }
    
    return;
}   

/* 
 *@exception RuntimeException when IO Error occurs.
 */
public void deSerialize(DataInputStream inputStream)
{
  // no need to call super, since there are no ivars in the superclass.

  try
    {
        parameterTypeDesignator.deSerialize(inputStream);
        changeIndicator.deSerialize(inputStream);
        articulationAttachmentID.deSerialize(inputStream);
        parameterType.deSerialize(inputStream);
        parameterValue = inputStream.readDouble();
     }
    catch(IOException ioException)      // catch-all for all IO exceptions in the above code
     {
        throw new 
            RuntimeException("Exception in ArticulationParameter. Error deSerializing unit.");
     }
    
    return;
}   

public int length()
{
    return sizeOf;          // articulation parameters are sizeOf bytes long, defined above
}

public void printValues(int indentLevel, PrintStream printStream)
{
    StringBuffer buf = ProtocolDataUnit.getPaddingOfLength(indentLevel);
    int idx, superclassIndent = indentLevel;

    printStream.println("ArticulationParameter ");
    printStream.println(buf + "parameterTypeDesignator: " + parameterTypeDesignator.intValue());
    printStream.println(buf + "changeIndicator: " + changeIndicator.intValue());
    printStream.println(buf + "articulationAttachmentID: " + articulationAttachmentID.intValue());
    printStream.println(buf + "parameterType: " + parameterType.longValue());
    printStream.println(buf + "parameterValue: " + parameterValue);

    return;
}

/**
  Include index of which ArticulationPararameter is being printed.
*/

public void printValues(int indentLevel, PrintStream printStream, int idx)
{
    StringBuffer buf = ProtocolDataUnit.getPaddingOfLength(indentLevel);
    int superclassIndent = indentLevel;

    printStream.println("ArticulationParameter " + idx);
    printStream.println(buf + "parameterTypeDesignator: " + parameterTypeDesignator.intValue());
    printStream.println(buf + "changeIndicator: " + changeIndicator.intValue());
    printStream.println(buf + "articulationAttachmentID: " + articulationAttachmentID.intValue());
    printStream.println(buf + "parameterType: " + parameterType.longValue());
    printStream.println(buf + "parameterValue: " + parameterValue);

    return;
}

public String toString ()
{
    return	  "ArticulationParameter\n"
    		+ "parameterTypeDesignator: " + parameterTypeDesignator + "\n"
    		+ "changeIndicator: " + changeIndicator + "\n"
    		+ "articulationAttachmentID: " + articulationAttachmentID + "\n"
    		+ "parameterType: " + parameterType + "\n" 
    		+ "parameterValue: " + parameterValue + "\n";
}

    // accessor methods. The get() methods return a copy of the instance variable, rather than
    // the instance variable itself.

public UnsignedByte getParameterTypeDesignator()
{   return (UnsignedByte)parameterTypeDesignator.clone();
}
public void setParameterTypeDesignator(UnsignedByte pParameterTypeDesignator)
{parameterTypeDesignator = pParameterTypeDesignator;
}
public void setParameterTypeDesignator(int pParameterTypeDesignator)
{ parameterTypeDesignator = new UnsignedByte(pParameterTypeDesignator);
}

public UnsignedByte getChangeIndicator()
{   return (UnsignedByte)changeIndicator.clone();
}
/**
 * incremented automatically when parameterValue is changed, also available to users.
 */
public void incrementChangeIndicator()
{
	if (changeIndicator.intValue() == UnsignedByte.MAX_UNSIGNED_BYTE_VALUE)
	{
		changeIndicator = new UnsignedByte (0);
	}
	else
	{
		changeIndicator = new UnsignedByte (changeIndicator.intValue() + 1);
	}
	return;
}
public void decrementChangeIndicator()
{
	if (changeIndicator.intValue() == 0)
	{
		changeIndicator =
			new UnsignedByte (UnsignedByte.MAX_UNSIGNED_BYTE_VALUE);
	}
	else
	{
		changeIndicator = new UnsignedByte (changeIndicator.intValue() - 1);
	}
	return;
}
public void setChangeIndicator(UnsignedByte pChangeIndicator)
{changeIndicator = pChangeIndicator;
}
public void setChangeIndicator(int pChangeIndicator)
{ changeIndicator = new UnsignedByte(pChangeIndicator);
}

public UnsignedShort getArticulationAttachmentID()
{   return (UnsignedShort)articulationAttachmentID.clone();
}
public void setArticulationAttachmentID(UnsignedShort pArticulationAttachmentID)
{articulationAttachmentID = pArticulationAttachmentID;
}
public void setArticulationAttachmentID(int pArticulationAttachmentID)
{ articulationAttachmentID = new UnsignedShort(pArticulationAttachmentID);
}

public UnsignedInt getParameterType()
{   return (UnsignedInt)parameterType.clone();
}
public void setParameterType(UnsignedInt pParameterType)
{parameterType = pParameterType;
}
public void setParameterType(int pParameterType)
{ parameterType = new UnsignedInt(pParameterType);
}


/**
 * changed type of parameterValue from long to double (but note that value gets downcasted to float when sent to VRML)
 */

public double getParameterValue()
{   return parameterValue;                              // Don't have to copy, since this is a basic type, not an object
}

/**
 *@deprecated 31 MAR 99, use setParameterValue(double) -- changed type of parameterValue from long to double
 * to match DIS specification (nevertheless note that value gets further downcasted to float if sent to VRML)
 */

public void setParameterValue(long pParameterValue)
{
	parameterValue = (double) pParameterValue;
	incrementChangeIndicator();
}

public void setParameterValue(double pParameterValue)
{
	parameterValue = pParameterValue;
	incrementChangeIndicator();
}


}       // end of class ArticulationParameter
