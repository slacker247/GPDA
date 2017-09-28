/*
 File:		ModulationType.java
 CVS Info:	$Id: ModulationType.java,v 1.2 2000/07/03 18:44:35 laflam Exp $
 Compiler:	jdk 1.3 
 */

package PDUs;

import mil.navy.nps.util.*;                     // General-purpose utilities
import disEnumerations.*;         // Enumerations for DIS

import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * Entity location in world coordinates.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/ModulationType.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/ModulationType.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/ModulationType.java">
 *  ~/mil/navy/nps/dis/ModulationType.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd> Information about the type of modulation used for radio transmission shall be represented by a 
 * Modulation Type record.
 *  
 *
 *<dt><b>Explanation</b>
 *<dd> This record uniquely identifies the various sets of signal parameters 
 * (i.e., the modulation type) that are used to determine whether two radios may interoperate. 
 * The modulation is characterized in a generic fashion by the Spread Spectrum, Major Modulation 
 * Type, and Detail ﬁelds. The classes of interoperable modulation types are enumerated by the 
 * System field. This record shall specify the spread-spectrum usage,major modulation type,
 * detailed information, and system compatibility.
 *  <P>
 *
 *<dt><b>History:</b>
 *<dd>		3AUG00 /Dave Laflam    		/New
 *<dd>		17DAug00 /Dave Laflam		/Added toString method, changed system varible to system
 *
 *<dt><b>References:</b>
 *<dd>	DIS Data Dictionary:
 *	<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/19.htm">Modulation Type Record</a>
 *<dd>	DIS specification : IEEE 1278.1, 5.2.23 Modulation Type record pg 68
 *
 *@see PduElement 
 *@see SerializationInterface
 *@see EntityCoordinate
 */
public class ModulationType extends PduElement implements SerializationInterface, Cloneable
{

    /**
     *Spread spectrum :This field shall indicate the spread spectrum technique or combination of spread
     * spectrum techniques in use. The Spread Spectrum ﬁeld shall consist of a 16 element Boolean array.
     * Each independent type of spread spectrum technique shall be represented by a single element of this
     * array. If a particular spread spectrum technique is in use, the corresponding array element shall be
     * set to one, otherwise it shall be set to zero. All unused array elements shall be set to zero. The 
     * supported spread spectrum techniques and their assignment to elements of the 16 element array are
     * defined in Section 9 of EBV-DOC and illustrated in table 23.
     */
    protected short spreadSpectrum ;        // 16-bit Boolean array 
    //protected boolean[] spreadSpectrum ;  // 16-bit Boolean array 
                                            // a boolean is 8 bits
    /**
     *Major :Major Modulation Type. This field shall specify the major classiﬁcation of 
     * the modulation type. This field shall be represented by a 
     * 16-bit enumeration (see Section 9 of EBV-DOC).
     */
    protected UnsignedShort  major;        // 16-bit enumeration

    /**
     *Detail : This field shall provide certain detailed information depending upon the 
     * major modulation type. This field shall be represented by a 
     * 16-bit enumeration (see Section 9 of EBV-DOC).
     */
    protected UnsignedShort  detail;       // 16-bit enumeration

     /**
     *System : This field shall specify the interpretation of the Modulation Parameter 
     * field(s) in the Transmitter PDU. This field shall be represented by a 
     * 16-bit enumeration (see Section 9 of EBV-DOC).
     */
    protected UnsignedShort  system;       // 16-bit enumeration  
                                           // system is a key word in java 

    /**
     *Constant value--size of a World Coordinate record when written out; here :<code>sizeOf = 64 bytes</code>.
     */
    public final int sizeOf = 64;   // object is 64 bytes long 




/**
 *Constructs an new ModulationType Object, centered.
 */
public ModulationType()
{
    // default constructor
    //for ( int i=0; i<16; i++)  {
    //   spreadspectrum [ i ] = false;
    //}  //  end for
    
    spreadSpectrum = 0 ;            // 16-bit Boolean array 
    major = new UnsignedShort(0);   // 16-bit enumeration
    detail = new UnsignedShort(0);  // 16-bit enumeration
    system = new UnsignedShort(0);  // 16-bit enumeration 
    
    
    return;
} // end constructor

/**
 *Constructs a new ModulationType Object whose field values are passed in parameters.
 *
 *@param pSpreadSpectrum the first field in the ModulationType Object.
 *@param pMajor the second field in the ModulationType Object.
 *@param pDetail the third field in the ModulationType Object. 
 *@param pSystem the third field in the ModulationType Object. 
 */
public ModulationType(short pSpreadSpectrum, UnsignedShort pMajor,UnsignedShort pDetail,UnsignedShort pSystem)
{
     spreadSpectrum = pSpreadSpectrum;
     major = pMajor;
     detail = pDetail;
     system = pSystem;

    return;
}// end copy constructor


/**
 * Make a copy of the object. This requires a deep copy, so we don't have two
 * objects sharing pointers to the same data.
 * @return a new ModulationType
 */
public Object clone()
{

 ModulationType  newModulationType = new ModulationType(spreadSpectrum, major, detail, system );

 return newModulationType;
} //end Object clone() method



/**
 *@exception RuntimeException when IO error occurs.
 */
public void serialize(DataOutputStream outputStream)
{
    // write out the data to an output stream. Order is important here, since
    // it needs to conform to the DIS standard.

    //super.serialize(outputStream);        // No need to call super, since no ivars are in our direct superclass
    
    try
     {
        outputStream.writeShort(spreadSpectrum); // don should this be an UnsignedShort ?
        major.serialize(outputStream); 
        detail.serialize(outputStream); 
        system.serialize(outputStream); 
        

     }
    catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in ModulationType. Error writing to wire.");
        }
}// end  void serialize method



/**
 *@exception RuntimeException when IO error occurs.
 */
public void deSerialize(DataInputStream pInputStream)
{
    try
     {
        spreadSpectrum = pInputStream.readShort();
        major = new UnsignedShort(pInputStream.readShort());         
        detail = new UnsignedShort(pInputStream.readUnsignedShort());
        system = new UnsignedShort(pInputStream.readUnsignedShort());
        
        
     }
     catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in WorldPosition. Error reading from wire.");
        }

}// end void deSerialize method



public int length()
{
    return sizeOf;          // EntityTypes are this long, always.
} //end int length() method




public void printValues(int indentLevel, PrintStream printStream)
{
    // print the values of the object out, with correct level of
    // indentation on the page.

    
  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println(indent + "ModulationType spreadSpectrum: "   + spreadSpectrum);
    printStream.println(indent + "ModulationType major: "   + major);
    printStream.println(indent + "ModulationType detail: "   + detail);
    printStream.println(indent + "ModulationType System: "   + system);

    return;
}

// Accessor methods (get and set methods)

/**
 * Gets SpreadSpectrum
 * @return spreadSpectrum
 */
public short getSpreadSpectrum()
{   
	return spreadSpectrum;
}

/**
 * Sets SpreadSpectrum
 * @param pSpreadSpectrum a SpreadSpectrum
 */
public void setSpreadSpectrum(short pSpreadSpectrum)
{
	spreadSpectrum = pSpreadSpectrum;
}


/**
 * Gets Major
 * @return major
 */
public UnsignedShort getMajor()
{   
	return major;
}
/**
 * Sets Major
 * @param pMajor a Major
 */
public void setMajor(UnsignedShort pMajor)     
{
	major = pMajor;
}

/**
 * Gets Detail
 * @return detail
 */
public UnsignedShort getDetail()
{   
	return detail;
}
/**
 * Sets Detail
 * @param pDetail a Detail
 */
public void setDetail(UnsignedShort pDetail)
{
	detail = pDetail;
}

/**
 * Gets System
 * @return System
 */
public UnsignedShort getSystem()
{   
	return system;
}

/**
 * Sets System
 * @param pSystem a System
 */
public void setSystem(UnsignedShort pSystem)
{	
	system = pSystem; 
}

/**
  * String toString
  * Used for debuging  
  */
public String toString()
{
	String result ;
	result = " SpreadSpectrum = " + spreadSpectrum + " Major = " + major + " Detail = " + detail + " system = " +  system;
	
return result;

}





} // end of class ModulationType

