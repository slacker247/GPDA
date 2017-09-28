/*
 File:		AcknowledgePdu.java
 CVS Info:	$Id: AcknowledgePdu.java,v 1.2 1998/01/27 18:43:49 mcgredo Exp $
 Compiler:	jdk1.3
 */

package PDUs;                                       // package to which we belong

import mil.navy.nps.util.*;					// General-purpose utilities
import disEnumerations.*;				// Enumerations for DIS

import java.util.*;                                             // utility stuff we need
import java.io.*;                                               // input/output for serialization

/**
 * Simulation management PDU to acknowledge other simulation management PDUs.
 *
 *@version 1.0
 *@author Antonio Alexandre Rua (<A HREF="http://www.garfield.fe.up.pt/~alexrua">http://www.garfield.fe.up.pt/~alexrua</A>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/AcknowledgePdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/AcknowledgePdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/AcknowledgePdu.java">
 *  ~/mil/navy/nps/dis/AcknowledgePdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>The acknowledgment of the receipt of a Start/Resume PDU, Stop/Freeze PDU, Create Entity PDU,
 *  or a Remove Entity PDU shall be communicated by issuing an Acknowledge PDU.
 *
 *<dt><b>Explanation:</b>
 *<dd>As the other Simulation Manager Pdu, this PDU has two header section, 
 *  the first one is inherited from the ProtocolDataUnit
 *  abstract class, the second from the SimulationManagementFamily abstract class.
 *  After That the PDU constains Acknowledge Flag, Response Flag and requestID fields.
 *  As with other things, the AcknowledgePdu has to know how to serialize and
 *  deserialize itself, clone itself, and print out its values.<P>
 *
 *<dt><b>History:</b>
 *<dd>		16Sep97	/Antonio Alexandre Rua	/New
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changed access methods: thisVariable() --> getThisVariable()
 *<dd>		17Dec97	/Ronan Fauglas		/bug fix changed sizeOf 40->32
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/a3.htm">
 *		Acknowledge PDU</a>	
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 5.4.6.5, 4.4.5.4.5
 *<dd>		See Section 7 in EBV-DOC
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
public class AcknowledgePdu extends SimulationManagementFamily
{
    /**
     *Acknowledge Flag - This field shall indicate what type of message has been acknowledged.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>Enumeration, see references below for values.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/a4.htm">Acknowledge Flag Field</a>
     *<dd>	See Section 7 in EBV-DOC
     *</dl>
     */
    protected UnsignedShort acknowledgeFlag;

    /**
     *Reponse flag - This field shall indicate whether or not the receiving entity was able 
     *to comply with the request, and for what reason.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>Enumeration, see references below for values.
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/a5.htm">Response Flag Field</a>
     *<dd>	See Section 7 in EBV-DOC
     *</dl>
     */
    protected UnsignedShort responseFlag;

    /**
     *Request ID - This field shall identify the matching response to the specific a Start/Resume,
     *Stop/Freeze, Create Entity, or Remove Entity PDU sent by the simulation manager.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>A 32-bit monotonically increasing integer identifier inserted by the Simulation Manager
     *  into all Simulation Manager PDUs. 
     *<dt><b>References:</b>
     *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/99.htm">Request ID Field</a>
     *</dl>
     */
    protected UnsignedInt requestID;        //identifies entity creation request by SM
      
    /**
     *Constant value--size of a full Acknowledge PDU with header.
     *<code>sizeOf = 32 bytes</code>
     */
    public static final int sizeOf = 32; // The total size of PDU is known

    /**
     *An "exemplar" object, which is filled out to the state that is needed most of the time.
     *
     *<dl>
     *<dd>
     *<dt><b>Explanation</b>
     *<dd>A brand new object has to have most of its values set,
     *  such as the forceID, protocol version, and so on. This lets the user fill
     *  out most of the values, save it in the class, then retrieve a copy of it
     *  as needed. 
     *</dd>
     *</dl>
     */
    static protected AcknowledgePdu exemplar;

/**
 *Default constructor--fills with zeros for all values.
 */
public AcknowledgePdu()                                             // default constructor
{
  setPduType(PduTypeField.ACKNOWLEDGE);
  acknowledgeFlag = new UnsignedShort();
  responseFlag    = new UnsignedShort();
  requestID       = new UnsignedInt();

  return;
}

public void  serialize(DataOutputStream outputStream)
{
 /**
    Serialize our data out to the stream. We first call the superclass,
    so that all the data there is written out to the buffer first.
    Then we serialize each of our ivars to the stream, being careful
    about the order.
*/

  // takes care of all the header variables in the two consecutive superclasses
  super.serialize(outputStream);          
  // write out ivars
  acknowledgeFlag.serialize(outputStream);
  responseFlag.serialize(outputStream);
  requestID.serialize(outputStream);

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
  acknowledgeFlag.deSerialize(inputStream);
  responseFlag.deSerialize(inputStream);
  requestID.deSerialize(inputStream);

  return;
}

public Object clone()
{
 /**
    Clone the CommentPdu, being careful to not share any pointers between the
    new object and the old object.
  */

  AcknowledgePdu newPdu = (AcknowledgePdu)super.clone();     // new Create Entity Pdu pdu

  newPdu.setAcknowledgeFlag(this.getAcknowledgeFlag());
  newPdu.setResponseFlag(this.getResponseFlag());
  newPdu.setRequestID(this.getRequestID());

  return newPdu;
}

public int length()
{
  return sizeOf;
}

public String pduName()
{
  return new String("Acknowledge PDU");
}

public void printValues(int indentLevel, PrintStream printStream)
{
   // print the values of the object out, with correct level of
  // indentation on the page.
  StringBuffer buf = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

  printStream.println();
  printStream.println("Acknowledge PDU");

  // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
  // so the header info will be indented a bit less.
  if(superclassIndent > 0)
    superclassIndent -= 1;
  super.printValues(superclassIndent, printStream);
  
  printStream.println(buf + "Acknowledge Flag: "+acknowledgeFlag.intValue());
  printStream.println(buf + "Response Flag   : "+responseFlag.intValue());
  printStream.println(buf + "Request ID      : "+requestID.longValue());

  return;
}

public AcknowledgePdu getExemplar()
{
    return (AcknowledgePdu)exemplar.clone();
}
public void setExemplar(AcknowledgePdu newExemplar)
{
    exemplar = newExemplar;
}


// Accessor methods
public UnsignedShort getAcknowledgeFlag()
{  return (UnsignedShort)acknowledgeFlag.clone();
}
public void setAcknowledgeFlag(UnsignedShort pAcknowledgeFlag)
{  acknowledgeFlag = pAcknowledgeFlag;
}
public void setAcknowledgeFlag(int pAcknowledgeFlag)
{  acknowledgeFlag = new UnsignedShort(pAcknowledgeFlag);
}

public UnsignedShort getResponseFlag()
{  return (UnsignedShort)responseFlag.clone();
}
public void setResponseFlag(UnsignedShort pResponseFlag)
{  responseFlag = pResponseFlag;
}
public void setResponseFlag(int pResponseFlag)
{  responseFlag = new UnsignedShort(pResponseFlag);
}

public UnsignedInt getRequestID()
{  return (UnsignedInt)requestID.clone();
}
public void setRequestID(UnsignedInt pRequestID)
{  requestID = pRequestID;
}
public void setRequestID(int pRequestID)
{  requestID = new UnsignedInt(pRequestID);
}

}   // end of class AcknowledgePdu
