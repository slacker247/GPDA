/*
 File:		CommentPdu.java
 CVS Info:	$Id: CommentPdu.java,v 1.3 1998/01/28 08:28:15 mcgredo Exp $
 Compiler:	jdk 1.3
 */

 package mil.navy.nps.dis;
 
 import java.util.*;                                           
 import java.io.*;       
 
 import mil.navy.nps.util.*;
 import mil.navy.nps.disEnumerations.*;
 

/**
 * Comment message PDU.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/CommentPdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/CommentPdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/CommentPdu.java">
 *  ~/mil/navy/nps/dis/CommentPdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>The CommentPdu is used to send arbitrary messages, such
 *  as character strings.
 *
 *<dt><b>Explanation:</b>
 *<dd>The specificatrion says: "Arbitrary messages (character strings, for example) shall be entered
 *  into data stream by using a Comment PDU".<p>
 *  This has a header section, which is
 *  inherited from the Simulation management PDU abstract class 
 *  which contains the header information.  After that, the 
 *  PDU consists of counts of the number of fixed and variable
 *  datum records, and the records themselves.<p>
 *  The commentPDU will be implemented using the Datum Specification
 *  record despite it does not have one. The thing is it has an amputated
 *  part of it. It has a Datum Specification with no Fixed Datum data fields.
 *  So we are  going to use the Datum Specification with proper care to
 *  ensure that will not ever exist in this PDU Fixed Datum values.
 *  IF modification are done, pay atention to setDatumSpecification method,
 *  because it  is reponsible for maintaining this integrity, and is a little
 *  bit different from other classes accessor methods.
 *
 *<dt><b>Note:</b>
 *<dd> Here we implement a datumSpecification instead of the Number of Variable Datum fields
 *  and the variable Datum Record as in the specification.
 *
 *<dt><b>History:</b>
 *<dd>		 9 December 96	/Don McGregor		/New
 *<dd>		10 March 97	/Don McGregor		/Cleaned up for javadoc
 *<dd>		16 April 97	/Don McGregor		/PrintStream passed to printValues
 *<dd>		 6 May 97	/Don McGregor		/Bug fixed in deSerialize; attempting to get int value of unsigned
 *<dd>	                 				int, also a stray semicolon at end of for statement.
 *<dd>		16 September 97	/Antonio Alexandre Rua	/header inherited from Simulation Management+use setDatumSpecification 
 *<dd>		 8 December 97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11 December 97	/Ronan Fauglas		/changed access methods: thisVariable() --> getThisVariable()
 *<dd>		 5 January 98	/Ronan Fauglas		/changed DatumSpecification to protected.
 *<dd>		29 November 99	/Don Brutzman		/changed PduTypeField.MESSAGE->PduTypeField.COMMENT
 *
 *<dt><b>References:</b>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS Data Dictionary:<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/b5.htm">Comment PDU</a>	
 *<dd>		DIS specification: IEEE 1278.1, 5.4.6.12, 4.4.5.4.12
 *
 *@see ProtocolDataUnit
 *@see PduElement 
 *@see SerializationInterface
 *@see DatumSpecification
 *@see CreateEntityPdu
 *@see RemoveEntityPdu
 *@see StartResumePdu
 *@see StopFreezePdu
 *@see AcknowledgePdu
 *@see ActionRequestPdu
 *@see ActionResponsePdu
 *@see DataQueryPdu
 *@see SetDataPdu
 *@see DataPdu
 *@see EventReportPdu
 *@see CommentPdu
 */
public class CommentPdu extends SimulationManagementFamily 
{
  /**
   *This represents represents the number of fixed datum, 
   *the number of variable datum, and the variable datum values.
   *<b>Note:</b> This is actually a truncated datum specification record 
   *where we don't make use of the fixed datum record.
   */
  protected DatumSpecification datumSpecification;

  /**
   *An "exemplar" object, which is filled out to the state that is needed most of the time.
   *
   *<dl>
   *<dt><b>Explanation</b>
   *<dd>A brand new object has to have most of its values set,
   *  such as the forceID, protocol version, and so on. This lets the user fill
   *  out most of the values, save it in the class, then retrieve a copy of it
   *  as needed. 
   *</dl>
   */
  static protected CommentPdu exemplar;

/**
 *Default constructor--fills with zeros for all values.
 */
public CommentPdu()     // default constructor
{
  super.setPduType(PduTypeField.COMMENT);
  datumSpecification = new DatumSpecification();
  
  return;
}
 
 public String pduName()
{
  return new String("Comment PDU");
}

 public void  serialize(DataOutputStream outputStream)
 {
  /**
     Serialize our data out to the stream. We first call the superclass,
     so that all the data there is written out to the buffer first.
     Then we serialize each of our ivars to the stream, being careful
     about the order.
 */
 
  //first serialize headers in superclasses
   super.serialize(outputStream);         
  //ivars
  datumSpecification.serialize(outputStream);
 
  return;
}
 
public void  deSerialize(DataInputStream inputStream)
{
  /**
     deserialize our data from the stream. We first call the superclass,
     so that all the data there is read into the buffer first.
     Then we deserialize each of our ivars to the stream, being careful
     about the order.
  */
 
  super.deSerialize(inputStream);
  // Do our ivars
  datumSpecification.deSerialize(inputStream);

  return;
} 
 
public Object clone()
{
  /**
     Clone the CommentPdu, being careful to not share any pointers between the
     new object and the old object.
   */
 
  CommentPdu      newPdu = (CommentPdu)super.clone();     // new entity state pdu
  newPdu.setDatumSpecification(this.getDatumSpecification());
 
  return newPdu;
}
 
public int length()
{
   /** calculate the length of the PDU on the fly. This should reflect the current length
   of the PDU; if a variable datum is added, you have to call length() again
   to find the current length. Note that this is the length of the PDU AS WRITTEN TO 
   THE WIRE.
    */
 
  int   currentLength = 0;

  currentLength = super.length() + datumSpecification.length();
 
  return currentLength;
}
 
public void printValues(int indentLevel, PrintStream printStream)
{
  // print the values of the object out, with correct level of
  // indentation on the page.
  int         superclassIndent = indentLevel;     // level of indentation for our superclass
  
  printStream.println();
  printStream.println("Comment PDU");

  // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
  // so the header info will be indented a bit less.
  if(superclassIndent > 0)
    superclassIndent -= 1;
  super.printValues(superclassIndent, printStream);
  datumSpecification.printValues(indentLevel,printStream);

  return;
}
 
public CommentPdu   getExemplar()
{
  return (CommentPdu)exemplar.clone();
}
 
public void setExemplar(CommentPdu newExemplar)
{
  exemplar = newExemplar;
}
 
 
// Accessor methods
 
public DatumSpecification getDatumSpecification()
{ return (DatumSpecification)datumSpecification.clone();
}

/**
 *<b>Note:</b> the fixed List is automatically suppressed by the method.
 */
public void setDatumSpecification(DatumSpecification pDatumSpecification)
{ 
  // this method is the only way users can set Datum Specification
  // records to this class. In this case we duplicate the sent
  // object to not let user cheat around keeping a reference to the passed object
  // and manipulating it.
  // To ensure no Fixed datum list will ever exist in the context of
  // this object we drop the fixed list.
  datumSpecification = (DatumSpecification)pDatumSpecification.clone();
  datumSpecification.dropFixedDatum();
}

} 
 
