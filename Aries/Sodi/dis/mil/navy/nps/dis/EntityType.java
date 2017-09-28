/*
 File:		EntityType.java
 CVS Info:	$Id: EntityType.java,v 1.2 1998/01/27 18:44:11 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;				// package to which we belong

import mil.navy.nps.util.*;				// General-purpose utilities
import mil.navy.nps.disEnumerations.*;	// Enumerations for DIS

import java.util.*;						// utility stuff we need
import java.io.*;						// input/output for serialization

/**
 * Record providing full identification of entity type.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd><a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EntityType.java">
 *                   http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EntityType.java</a>
 *
 *<dd><a href="../../../../../../mil/navy/nps/dis/EntityType.java">
 *                                       ~/mil/navy/nps/dis/EntityType.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>This record shall specify the kind of entity, the country of design, the domain, 
 *  the specific identification of the entity, and any extra information necessary 
 *  for describing the entity. <p>
 *<dt><b>Explanation:</b>
 *<dd>The type of entity in a DIS exercise shall be specified by an Entity Type record.
 *  Fields not used shall contain the value zero.<p>
 *
 *<dt><b>History:</b>
 *<dd>		2Dec96	/Don McGregor		/New
 *<dd>  	10Mar97	/Don McGregor		/Cleaned up for javadoc
 *<dd>  	16Apr97	/Don McGregor		/PrintStream passed to printValues
 *<dd>		8Dec97  /Ronan Fauglas		/changes for documentation templates + complements in documentation<br>
 *<dd>		8Dec97  /Ronan Fauglas		/changed entityKind to kind
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary :<A HREF="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/28.htm">Entity Type Record</A><P>
 *<dd>		DIS specification : IEEE 1278.1, 5.3.16
 *
 *@see PduElement 
 */
public class EntityType extends PduElement
{
    /**
     *Entity kind: munition, life form, environmental...
     *For now, entity kind is : 0-9
     *
     *<dl>
     *<dt><b>Value:</b>
     *Enumeration; see references below for information.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/22.htm">Entity Kind Field</a>			
     *<dd>	see Section 4 in EBV-DOC
     *</dl>
     */
    protected UnsignedByte      kind;                     // kind of entity


    /**
     *This field shall specify the domain in which the equipment operates 
     *(for example, subsurface, surface, land, etc.) except for munition entities. 
     *For Munition entities this field shall specify the domain of the target 
     *(for example the munition might be surface-to-air, so the domain would be anti-air).
     *<dl>
     *<dt><b>Value:</b>
     *  Enumeration; see references below for information.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/23.htm">Entity Domain Field</a>			
     *<dd>	see Section 4 in EBV-DOC
     *</dl>
     */
    protected UnsignedByte      domain;                         // subsurface, surface, etc.

    /**
     *This field shall specify the country to which the design of the entity is
     *attributed. a 16 bits enumeration.
     *
     *<dl>
     *<dt><b>Value:</b>
     *  Enumeration; see references below for information.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/d.htm">Country Field</a>			
     *<dd>	See Section 4 in EBV-DOC
     *</dl>
     */
    protected UnsignedShort     country;                        // country to which design is attributed

    /**
     *This field shall specify the main category that describes the entity. 
     *The enumerations of category depend upon both the Kind and Domain. 
     *
     *<dl>
     *<dt><b>Value:</b>
     *  Enumeration; see references below for information.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/24.htm">Entity Category Field</a>			
     *<dd>	See Section 4 in EBV-DOC
     *</dl>
     */
    protected UnsignedByte      category;                       // main category that describes entityt

    /**
     *This field shall specify a particular subcategory to which the entity
     *belongs based on the category and the country. 
     *
     *<dl>
     *<dt><b>Value:</b>
     *  Enumeration; see references below for information.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/25.htm">Entity Subcategory Field</a>			
     *<dd>	See Section 4 in EBV-DOC
     *</dl>
     */
    protected UnsignedByte      subCategory;                    // subcategory, based on category field

    /**
     *This field shall specify specific information about an entity based upon
     *the subcategory field to which it belongs. 
     *
     *<dl>
     *<dt><b>Value:</b>
     *  Enumeration; see references below for information.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/26.htm">Entity Specific Field</a>			
     *<dd>	See Section 4 in EBV-DOC
     *</dl>
     */
    protected UnsignedByte      specific;                       // specific info, based on subCategory field

    /**
     *This field shall specify extra information required to describe a particular entity. 
     *The contents of this field shall depend on the type of entity represented. 
     *
     *<dl>
     *<dt><b>Value:</b>
     *  Enumeration.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/27.htm">Entity Extra Field</a>			
     *</dl>
     */
    protected UnsignedByte      extra;                          // extra info, depending on type of entity

    /**
     *Constant value--size of an EntityType as written out to the wire. Here:
     *<code>sizeOf = 8 bytes</code>
     */
    public final static int     sizeOf = 8;                     // size of default object AS WRITTEN TO WIRE


/**
 *Default constructor--fills with zeros for all values.
 */
public EntityType()                                             // default constructor
{
    kind  = new UnsignedByte(0);
    domain      = new UnsignedByte(0);
    country     = new UnsignedShort(0);
    category    = new UnsignedByte(0);
    subCategory = new UnsignedByte(0);
    specific    = new UnsignedByte(0);
    extra       = new UnsignedByte(0);

    return;
}

public Object clone()
{
 EntityType newEntityType = new EntityType();

 newEntityType.setKind(this.getKind());
 newEntityType.setDomain(this.getDomain());
 newEntityType.setCountry(this.getCountry());
 newEntityType.setCategory(this.getCategory());
 newEntityType.setSubCategory(this.getSubCategory());
 newEntityType.setSpecific(this.getSpecific());
 newEntityType.setExtra(this.getExtra());

 return newEntityType;
}

public void serialize(DataOutputStream outputStream)
{
    // write out the data to an output stream. Order is important here, since
    // it needs to conform to the DIS standard.

    //super.serialize(outputStream);        // No need to call super, since no ivars are in our direct superclass
    
    kind.serialize(outputStream);
    domain.serialize(outputStream);
    country.serialize(outputStream);
    category.serialize(outputStream);
    subCategory.serialize(outputStream);
    specific.serialize(outputStream);
    extra.serialize(outputStream);

    return;
}


public void deSerialize(DataInputStream inputStream)
{
    // order is important here, since we need to read in the same order as
    // specified by the DIS standard.
    // no need to call super, since there are no ivars in the superclass.

    kind.deSerialize(inputStream);
    domain.deSerialize(inputStream);
    country.deSerialize(inputStream);
    category.deSerialize(inputStream);
    subCategory.deSerialize(inputStream);
    specific.deSerialize(inputStream);
    extra.deSerialize(inputStream);

    return;
}

public int length()
{
    return sizeOf;          // EntityTypes are this long, always.
}

public void printValues(int indentLevel, PrintStream printStream)
{
    // print the values of the object out, with correct level of
    // indentation on the page.

    
  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println(indent + "kind: "  + kind.intValue());
    printStream.println(indent + "domain: "      + domain.intValue());
    printStream.println(indent + "country: "     + country.intValue());
    printStream.println(indent + "category: "    + category.intValue());
    printStream.println(indent + "subCategory: " + subCategory.intValue());
    printStream.println(indent + "specific: "    + specific.intValue());
    printStream.println(indent + "extra: "       + extra.intValue());

    return;
}

    //Accessor methods

public UnsignedByte getKind()
{   return (UnsignedByte)kind.clone();
}
public void setKind(UnsignedByte pKind)
{kind = pKind;
}
public void setKind(int pKind)
{ kind = new UnsignedByte(pKind);
}

public UnsignedByte getDomain()
{   return (UnsignedByte)domain.clone();
}
public void setDomain(UnsignedByte pDomain)
{domain = pDomain;
}
public void setDomain(int pDomain)
{ domain = new UnsignedByte(pDomain);
}

public UnsignedShort getCountry()
{   return (UnsignedShort)country.clone();
}
public void setCountry(UnsignedShort pCountry)
{country = pCountry;
}
public void setCountry(int pCountry)
{ country = new UnsignedShort(pCountry);
}

public UnsignedByte getCategory()
{   return (UnsignedByte)category.clone();
}
public void setCategory(UnsignedByte pCategory)
{category = pCategory;
}
public void setCategory(int pCategory)
{ category = new UnsignedByte(pCategory);
}

public UnsignedByte getSubCategory()
{   return (UnsignedByte)subCategory.clone();
}
public void setSubCategory(UnsignedByte pSubCategory)
{subCategory = pSubCategory;
}
public void setSubCategory(int pSubCategory)
{ subCategory = new UnsignedByte(pSubCategory);
}

public UnsignedByte getSpecific()
{   return (UnsignedByte)specific.clone();
}
public void setSpecific(UnsignedByte pSpecific)
{specific = pSpecific;
}
public void setSpecific(int pSpecific)
{ specific = new UnsignedByte(pSpecific);
}

public UnsignedByte getExtra()
{   return (UnsignedByte)extra.clone();
}
public void setExtra(UnsignedByte pExtra)
{extra = pExtra;
}
public void setExtra(int pExtra)
{ extra = new UnsignedByte(pExtra);
}

} // end of class EntityType
