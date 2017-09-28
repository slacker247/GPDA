/*
 File:		EventReportPdu.java
 CVS Info:	$Id: EventReportPdu.java,v 1.3 1998/01/28 08:28:20 mcgredo Exp $
 Compiler:	jdk 1.3
 */

 package PDUs;
 
 import java.util.*;                                           
 import java.io.*;                                             
 
 import mil.navy.nps.util.*;
 import disEnumerations.*;

/**
 * Simulation management PDU to report special events.
 *
 *@version 1.0
 *@author Antonio Alexandre Rua (<A HREF="http://www.garfield.fe.up.pt/~alexrua">http://www.garfield.fe.up.pt/~alexrua</A>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EventReportPdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/EventReportPdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/EventReportPdu.java">
 *  ~/mil/navy/nps/dis/EventReportPdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd> A managed entity shall report the occurrence of a significant event to the simulation
 *  manager using an Event Report PDU.
 *
 *<dt><b>History:</b>
 *<dd>		16Sep97	/Antonio Alexandre Rua	/New
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changed access methods: thisVariable() --> getThisVariable()
 *<dd>		14jan98	/Ronan Fauglas		/changed datumspecification to datumInformation
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/b3.htm">Event Report PDU</a>	
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 5.4.6.11, 4.4.5.4
 *
 *@see ProtocolDataUnit
 *@see PduElement 
 *@see SerializationInterface
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
public class EventReportPdu extends SimulationManagementFamily 
{
  /**
   *Event Type - This field shall specify the type of event that caused the issue of an Event PDU.
   *
   *<dl>
   *<dt><b>Value:</b>
   *<dd>Enumeration. See references below for further information.
   *<dt><b>References:</b>
   *<dd>	see Section 7 in EBV-DOC
   *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/b4.htm">Event Type Field</a>
   *</dl>
   */
  protected UnsignedInt eventType;

  /**
   *Datum Information - This field shall specify the types of datum and their value be communicated. 
   */
  protected DatumSpecification datumInformation;
 
  /**
   *Constant value--size of a Event Report PDU without header nor Datum Specification Record.
   *<code>sizeOf = 8 bytes</code>
   */
  public static final int sizeOf = 8;  // 8 bytes long (stands for RequestId and padding)
 
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
  static protected EventReportPdu exemplar;
 
/**
 *Default constructor--fills with zeros for all values.
 */
 public EventReportPdu()                                             // default constructor
{
  super.setPduType(PduTypeField.EVENTREPORT);
  eventType          = new UnsignedInt();
  datumInformation = new DatumSpecification();
  
  return;
}
 
 public String pduName()
{
  return new String("Event Report PDU");
}

 /**
  *@exception RuntimeException when IO Error occurs.
  */
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
  eventType.serialize(outputStream);
  // padding
  try
  {
    outputStream.writeInt(0);
  }
  catch(Exception e) {trace(e.getMessage());}
 
  datumInformation.serialize(outputStream);
 
  return;
}
 
/**
 *@exception RuntimeException when IO Error occurs.
 */
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
  eventType.deSerialize(inputStream);
  // padding
  try
  {
    int i = inputStream.readInt();
  }
  catch(Exception e) {trace(e.getMessage());}

  datumInformation.deSerialize(inputStream);

  return;
} 
 
public Object clone()
{
  /**
     Clone the CommentPdu, being careful to not share any pointers between the
     new object and the old object.
   */
 
  EventReportPdu      newPdu = (EventReportPdu)super.clone();     // new entity state pdu
 
  newPdu.setEventType(eventType.longValue());
  newPdu.setDatumInformation(this.getDatumInformation());
 
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
 
  currentLength = super.length() + EventReportPdu.sizeOf + 
                 datumInformation.length();
 
  return currentLength;
}
 
public void printValues(int indentLevel, PrintStream printStream)
{
  // print the values of the object out, with correct level of
  // indentation on the page.
 
  
  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;
  
  printStream.println();
  printStream.println("Event Report PDU");

  // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
  // so the header info will be indented a bit less.
  if(superclassIndent > 0)
    superclassIndent -= 1;
  super.printValues(superclassIndent, printStream);

  // Yech. This fairly screams for a better way to handle ivars. p-list?
  printStream.println(indent + "eventType: " + eventType.longValue() );
 
  datumInformation.printValues(indentLevel,printStream);

  return;
}
 
public EventReportPdu   getExemplar()
{
  return (EventReportPdu)exemplar.clone();
}
 
public void setExemplar(EventReportPdu newExemplar)
{
  exemplar = newExemplar;
}
 
 
// Accessor methods
 
 
public UnsignedInt getEventType()
{ return (UnsignedInt)eventType.clone();
}
public void setEventType(long pEventType )
{ eventType = new UnsignedInt(pEventType);
}

public DatumSpecification getDatumInformation()
{ return (DatumSpecification)datumInformation.clone();
}
public void setDatumInformation(DatumSpecification pDatumSpecification)
{ datumInformation = pDatumSpecification;
}

} 
 
