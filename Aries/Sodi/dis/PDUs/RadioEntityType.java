/*
 File:		RadioEntityType.java
 CVS Info:	$Id: RadioEntityType.java,v 1.2 2000/07/03 18:44:35 laflam Exp $
 Compiler:	jdk 1.3 
 */

package PDUs;

import mil.navy.nps.util.*;                    // General-purpose utilities
import disEnumerations.*;         // Enumerations for DIS
import java.lang.*;                            // Native Java Language Stuff
import java.util.*;                            // utility stuff 
import java.io.*;                              // input/output for serialization

/**
 * Entity Radio Type
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/RadioEntityType.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/RadioEntityType.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/RadioEntityType.java">
 *  ~/mil/navy/nps/dis/RadioEntityType.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>The type of radio in a DIS exercise shall be speciﬁed by a Radio Entity Type record. This record shall 
 * specify the kind of entity, the domain, the country of design, and speciﬁc information about the radio. 
 * The fields of this record are as follows: 
 * 			Entity Kind 8-bit enumeration
 * 			Domain 8-bit enumeration
 * 			Country 16-bit enumeration
 * 			Category 8-bit enumeration
 * 			Nomenclature Version 8-bit enumeration
 * 			Nomenclature 16-bit enumeration
 *
 *<dt><b>Explanation</b>
 *<dd>The RadioEntityType class describes the location of an entity in 
 *  64-bit format. This crops up often enough to warrant
 *  its own class. (This is also known as "World Coordinates", 
 *  as contrasted to "entity coordinates", which are 32-bit.<P>
 *
 *<dt><b>History:</b>
 *<dd>		10Aug00 /Dave Laflam    	/New
 *<dd>		17DAug00 /Dave Laflam		/Added toString method
 *
 *
 *<dt><b>References:</b>
 *<dd>	DIS Data Dictionary:
 *	<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/19.htm">Radio Entity Type record</a>
 *<dd>	DIS specification : IEEE 1278.1, 5.2.25 Radio Entity Type record
 *
 *@see PduElement 
 *@see SerializationInterface
 *@see EntityCoordinate
 */
public class RadioEntityType extends PduElement implements SerializationInterface, Cloneable
{

    /**
     *First field of RadioEntityType 
     * This field shall identify the kind of entity described by the Radio Entity Type record, and
     * shall be represented by an 8-bit enumeration. Values for this field are deﬁned in 
     * Section 4 of EBV-DOC.
     */
    protected UnsignedByte   entityKind ;            // 8 bit enumeration    
    
    /**
     *Second field of RadioEntityType
     * This field shall specify the domain in which the radio entity operates, and shall be 
     * represented by an 8-bit enumeration. Values for this field are defined in 
     * Section 4 of EBV-DOC. 
     */
    protected UnsignedByte   domain  ;               // 8 bit enumeration
    
    /**
     *Thrid field of RadioEntityType 
     * This field shall specify the country to which the design of the radio entity is attributed, 
     * and shall be represented by a 16-bit enumeration. 
     * Values for this field are deﬁned in Section 4 of EBV-DOC.
     */
    protected UnsignedShort  country  ;              // 16 bit enumeration
    
    /**
     *Fourth field of RadioEntityType 
     * This field shall specify the main category that describes the radio entity, and shall be 
     * represented by an 8-bit enumeration. Values for this field are in Section 4 of EBV-DOC.
     */
    protected UnsignedByte   category  ;             // 8 bit enumeration
    
    /**
     *Fifth field of RadioEntityType 
     * This field shall specify the speciﬁc modification or individual unit type for a
     * series and/or family of equipment. This field shall be represented by an 8-bit enumeration.
     */
    protected UnsignedByte   nomenclatureVersion ;   //8 bit enumeration    
    
    /**
     *Sixth field of RadioEntityType 
     * This field shall specify the nomenclature for a particular communications device.
     * Nomenclatures are a combination of letters and/or numbers arranged in a speciﬁc sequence to
     * provide a short significant method of identifying specific equipment, series and/or families 
     * of equipment. This field shall be represented by a 16-bit enumeration.
     */
    protected UnsignedShort  nomenclature   ;        //16 bit enumeration
       
    

    /**
     *Constant value--size of a RadioEntityType record when written out; here :<code>sizeOf = 64 bytes</code>.
     */
    public final int sizeOf = 64;   // object is 64 bytes long 


/**
 *Constructs an new RadioEntityType Object, centered.
 */
public RadioEntityType()
{
    // default constructor

    entityKind = new UnsignedByte(0);               // 8 bit enumeration    
                
    domain =  new UnsignedByte(0);                  // 8 bit enumeration    
   
    country = new UnsignedShort(0);                 //16 bit enumeration
   
    category =  new UnsignedByte(0);                // 8 bit enumeration    

    nomenclatureVersion =  new UnsignedByte(0);     // 8 bit enumeration        

    nomenclature = new UnsignedShort(0);            //16 bit enumeration
             

    return;
} // end public RadioEntityType()

/**
 *Constructs a new RadioEntityType Object whose coordinate values are passed in parameters.
 *
 *@param pEntityKind the first  
 *@param pDomain the second 
 *@param pCountry the third  
 *@param pCategory the fourth
 *@param pNomenclatureVersion the fifth 
 *@param pNomenclature the sixth  
 */
 
// public RadioEntityType(double pX, double pY, double pZ)

public RadioEntityType(UnsignedByte pEntityKind, UnsignedByte pDomain, UnsignedShort pCountry, UnsignedByte pCategory, UnsignedByte pNomenclatureVersion, UnsignedShort pNomenclature)
{
    
    entityKind = pEntityKind ;    
    domain = pDomain  ;
    country =  pCountry ;
    category = pCategory ;
    nomenclatureVersion =  pNomenclatureVersion ;
    nomenclature = pNomenclature ;
        
    return;
} // end public RadioEntityType()

public Object clone()
{

 RadioEntityType  newRadioEntityType = new RadioEntityType(entityKind, domain, country, category, nomenclatureVersion, nomenclature);

 return newRadioEntityType;
}// end public Object clone()


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
//**********// We must ask Don McGregor to see if this does conform to the DIS Standard (HOT HOT HOT) 
     	
     	
     	//entityKind, domain, country, category, nomenclatureVersion, nomenclature
        outputStream.writeByte((byte)entityKind.intValue());             //8 bit enumeration 
        outputStream.writeByte((byte)domain.intValue());                 //8 bit enumeration
        outputStream.writeShort((short)country.intValue());                //16 bit enumeration
        outputStream.writeByte((byte)category.intValue());               //8 bit enumeration
        outputStream.writeByte((byte)nomenclatureVersion.intValue());    //8 bit enumeration
        outputStream.writeShort((short)nomenclature.intValue());           //16 bit enumeration 

        
     }
    catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in RadioEntityType.serialize, error writing to wire.");
        }
} // end public void serialize

/**
 *@exception RuntimeException when IO error occurs.
 */
public void deSerialize(DataInputStream pInputStream)
{
    try
     {
        
        entityKind = new UnsignedByte (pInputStream.readByte());           //8 bit enumeration 
        domain = new UnsignedByte (pInputStream.readByte());               //8 bit enumeration 
        country  = new UnsignedShort (pInputStream.readShort());           //16 bit enumeration   
        category = new UnsignedByte (pInputStream.readByte());             //8 bit enumeration 
        nomenclatureVersion = new UnsignedByte (pInputStream.readByte());  //8 bit enumeration 
        nomenclature = new UnsignedShort (pInputStream.readShort());       //16 bit enumeration 
        
     }
     catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in  RadioEntityType.deSerialize,. Error reading from wire.");
        }

}// end public void deSerialize

public int length()
{
    return sizeOf;          // EntityTypes are this long, always.
} // end public int length()





public void printValues(int indentLevel, PrintStream printStream)
{
    // print the values of the object out, with correct level of
    // indentation on the page.
    //entityKind, domain, country, category, nomenclatureVersion, nomenclature

    
  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println(indent + "RadioEntityType entityKind: "   + entityKind);
    printStream.println(indent + "RadioEntityType domain: "   + domain);
    printStream.println(indent + "RadioEntityType country: "   + country);
    printStream.println(indent + "RadioEntityType category: "   + category);
    printStream.println(indent + "RadioEntityType nomenclatureVersion: "   + nomenclatureVersion);
    printStream.println(indent + "RadioEntityType nomenclature: "   + nomenclature);

    return;
} // end public void printValues()



// Accessor methods (set and get) 

/**
 * Gets the EntityKind
 * @return entityKind 
 */
public UnsignedByte getEntityKind()
{
	   return entityKind;
}
/**
 * Sets the EntityKind
 * @param pEntityKind a EntityKind
 */
public void setEntityKind(UnsignedByte pEntityKind)
{
	entityKind = pEntityKind;
}


/**
 * Gets the Domain
 * @return domain 
 */
public UnsignedByte getDomain()
{   
	return domain;
}
/**
 * Sets the Domain
 * @param pDomain a Domain
 */
public void setDomain(UnsignedByte pDomain)
{
	domain = pDomain;
}

/**
 * Gets the Country
 * @return country 
 */
public UnsignedShort getCountry()
{   
	return country;
}
/**
 * Sets the Country
 * @param pCountry a Country
 */
public void setCountry(UnsignedShort pCountry)
{
	country = pCountry;
}

/**
 * Gets the Category
 * @return category 
 */
public UnsignedByte  getCategory()
{   
	return category;
}
/**
 * Sets the Category
 * @param pCategory a Category
 */
public void setCategory(UnsignedByte  pCategory)
{
	category = pCategory;
}


/**
 * Gets the NomenclatureVersion
 * @return nomenclatureVersion 
 */
public UnsignedByte getNomenclatureVersion()
{   
	return nomenclatureVersion;
}
/**
 * Sets the NomenclatureVersion
 * @param pNomenclatureVersion a NomenclatureVersion
 */
public void setNomenclatureVersion(UnsignedByte  pNomenclatureVersion)
{
	nomenclatureVersion = pNomenclatureVersion;
}



/**
 * Gets the Nomenclature
 * @return nomenclature 
 */
public UnsignedShort getNomenclature()
{   
	return nomenclature;
}
/**
 * Sets the Nomenclature
 * @param pNomenclature a Nomenclature
 */
public void setNomenclature(UnsignedShort pNomenclature)
{
	nomenclature = pNomenclature;
}

/**
  * String toString
  * Used for debuging  
  */
  
 public String toString ()
 {
 	String result;
 	result = " entityKind = " +entityKind  + " domain = " + domain + " country= " + country +
 	" category =" + category + " nomenclatureVersion = " +nomenclatureVersion + " padding1 = " +  nomenclature; 
 	
 	
 return result ; 
 }





} // end of class RadioEntityType
