/*
 File:		ActionResponsePdu.java
 CVS Info:	$Id: ActionResponsePdu.java,v 1.3 1998/01/28 08:28:14 mcgredo Exp $
 Compiler:	jdk 1.3
 */

 package PDUs;
 
 import java.util.*;                                           
 import java.io.*;                                             
 
 import mil.navy.nps.util.*;
 import disEnumerations.*;


/**
 * Simulation management PDU to respond to an Action Request PDU.
 *
 *@version 1.0
 *@author Antonio Alexandre Rua (<A HREF="http://www.garfield.fe.up.pt/~alexrua">http://www.garfield.fe.up.pt/~alexrua</A>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/ActionResponsePdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/ActionResponsePdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/ActionResponsePdu.java">
 *  ~/mil/navy/nps/dis/ActionResponsePdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd> When an entity receives an Action Request PDU, that entity shall acknowledge 
 *  the receipt of the Action Request PDU with an Action Response PDU. 
 *
 *<dt><b>History:</b>
 *<dd>		16Sep97	/Antonio Alexandre Rua	/New
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changed access methods: thisVariable() --> getThisVariable()
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/ad.htm">Action Response PDU</a>	
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 5.4.6.7
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
public class ActionResponsePdu extends SimulationManagementFamily 
{
  /**
   *Request ID - This field shall identify the matching response to a request made by the simulation manager. 
   *
   *<dl>
   *<dt><b>Value:</b>
   *<dd>A 32-bit monotonically increasing integer identifier inserted by the Simulation Manager
   *  into all Simulation Manager PDUs. 
   *<dt><b>References:</b>
   *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/99.htm">Request ID Field</a>
   *</dl>
   */
  protected UnsignedInt requestID;

  /**
   *Request Status - This field shall identify the status of the requested action.
   *
   *<dl>
   *<dt><b>Value:</b>
   *<dd>Enumeration, see references below for values.
   *<dt><b>References:</b>
   *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/ae.htm">Request Status Field</a>
   *</dl>
   */
  protected UnsignedInt requestStatus;

  /**
   *Datum Information - This field shall specify the types of datum 
   *and the value of the datum to be communicated. 
   */
  protected DatumSpecification datumInformation;
 
  /**
   *Constant value--size of an Action Response PDU without headder nor DatumSpecification.
   * Here:<code>sizeOf = 8 bytes</code>
   */
  public static final int sizeOf = 8;  // 8 bytes long (stands for requestId and requestStatus)
 
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
  static protected ActionResponsePdu exemplar;
 
/**
 *Default constructor--fills with zeros for all values.
 */
public ActionResponsePdu()                                             // default constructor
{
  super.setPduType(PduTypeField.ACTIONRESPONSE);
  requestID          = new UnsignedInt();
  requestStatus      = new UnsignedInt();
  datumInformation = new DatumSpecification();
  
  return;
}
 
 public String pduName()
{
  return new String("Action Response PDU");
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
  requestID.serialize(outputStream);
  requestStatus.serialize(outputStream);
  
  datumInformation.serialize(outputStream);
 
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
  requestID.deSerialize(inputStream);
  requestStatus.deSerialize(inputStream);
  
  datumInformation.deSerialize(inputStream);

  return;
} 
 
public Object clone()
{
  /**
     Clone the CommentPdu, being careful to not share any pointers between the
     new object and the old object.
   */
 
  ActionResponsePdu      newPdu = (ActionResponsePdu)super.clone();     // new entity state pdu
 
  newPdu.setRequestID(requestID.longValue());
  newPdu.setRequestStatus(requestStatus.longValue());
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
 
  currentLength = super.length() + ActionResponsePdu.sizeOf + 
                 datumInformation.length();
 
  return currentLength;
}
 
public void printValues(int indentLevel, PrintStream printStream)
{
  // print the values of the object out, with correct level of
  // indentation on the page.
 
  StringBuffer buf = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int idx, superclassIndent = indentLevel;

  printStream.println();
  printStream.println("Action Response PDU");

  // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
  // so the header info will be indented a bit less.
  if(superclassIndent > 0)
    superclassIndent -= 1;
  super.printValues(superclassIndent, printStream);

  // Yech. This fairly screams for a better way to handle ivars. p-list?
  printStream.println(buf + "requestID     : " + requestID.longValue() );
  printStream.println(buf + "requestStatus : " + requestStatus.longValue() );
 
  datumInformation.printValues(indentLevel,printStream);

  return;
}
 
public ActionResponsePdu   getExemplar()
{
  return (ActionResponsePdu)exemplar.clone();
}
 
public void setExemplar(ActionResponsePdu newExemplar)
{
  exemplar = newExemplar;
}
 
 
// Accessor methods
 
public UnsignedInt getRequestID()
{ return (UnsignedInt)requestID.clone();
}
public void setRequestID(long pRequestID )
{ requestID = new UnsignedInt(pRequestID);
}

public UnsignedInt getRequestStatus()
{ return (UnsignedInt)requestStatus.clone();
}
public void setRequestStatus(long pRequestStatus )
{ requestStatus = new UnsignedInt(pRequestStatus);
}

public DatumSpecification getDatumInformation()
{ return (DatumSpecification)datumInformation.clone();
}
public void setDatumInformation(DatumSpecification pDatumSpecification)
{ datumInformation = pDatumSpecification;
}

} 
 

