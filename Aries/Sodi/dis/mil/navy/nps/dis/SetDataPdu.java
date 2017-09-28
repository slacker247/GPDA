/*
 File:		SetDataPdu.java
 CVS Info:	$Id: SetDataPdu.java,v 1.3 1998/01/28 08:28:23 mcgredo Exp $
 Compiler:	jdk 1.3
 */

 package mil.navy.nps.dis;
 
 import java.util.*;                                           
 import java.io.*;                                             
 
 import mil.navy.nps.util.*;
 import mil.navy.nps.disEnumerations.*;

/**
 * Simulation management PDU to set distributed state data.
 *
 *@version 1.0
 *@author Antonio Alexandre Rua (<A HREF="http://www.garfield.fe.up.pt/~alexrua">http://www.garfield.fe.up.pt/~alexrua</A>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/SetDataPdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/SetDataPdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/SetDataPdu.java">
 *  ~/mil/navy/nps/dis/SetDataPdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>Initializing or changing internal state information shall be communicated
 *  using a Set Data PDU.
 *
 *<dt><b>History:</b>
 *<dd>		16Sep97	/Antonio Alexandre Rua	/New
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changed access methods: thisVariable() --> getThisVariable()
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/b0.htm">
 *		Set Data PDU</a>	
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, 5.4.6.9
 *
 *@see ProtocolDataUnit
 *@see PduElement 
 *@see SerializationInterface
 */
public class SetDataPdu extends SimulationManagementFamily 
{
  /**
   *Request ID - This field shall identify the set data request being made by the simulation manager. 
   *
   *<dt><b>References:</b>
   *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/99.htm">Request ID Field</a>
   */
  protected UnsignedInt requestID;

    /**
     *Datum Information - This field shall specify the types of datum and their value to be communicated.
     */
  protected DatumSpecification datumInformation;
 
  /**
   *Constant value--size of a Set Data PDU without header (stands for RequestId and padding which is not represented).
   *<code>sizeOf = 8 bytes</code>
   */
  public static final int sizeOf = 8;  // 8 bytes long (stands for RequestId and padding)
 
  /**
   *An "exemplar" object, which is filled out to the state that is needed most of the time.
   *
   *<dt><b>Explanation</b>
   *<dd>A brand new object has to have most of its values set,
   *  such as the forceID, protocol version, and so on. This lets the user fill
   *  out most of the values, save it in the class, then retrieve a copy of it
   *  as needed. 
   */
  static protected SetDataPdu exemplar;
 
/**
 *Default constructor. Initializes everything to zero, basically.
 */
public SetDataPdu()                                             // default constructor
{
  super.setPduType(PduTypeField.SETDATA);
  requestID          = new UnsignedInt();
  datumInformation = new DatumSpecification();
  
  return;
}
 
 public String pduName()
{
  return new String("Set Data PDU");
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
  // padding
  try
  {
    outputStream.writeInt(0);
  }
  catch(Exception e) {trace(e.getMessage());}
 
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
 
  SetDataPdu      newPdu = (SetDataPdu)super.clone();     // new entity state pdu
 
  newPdu.setRequestID(requestID.longValue());
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
 
  currentLength = super.length() + SetDataPdu.sizeOf + 
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
  printStream.println("Set Data PDU");

  // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
  // so the header info will be indented a bit less.
  if(superclassIndent > 0)
    superclassIndent -= 1;
  super.printValues(superclassIndent, printStream);

  // Yech. This fairly screams for a better way to handle ivars. p-list?
  printStream.println(indent + "requestID: " + requestID.longValue() );
 
  datumInformation.printValues(indentLevel,printStream);

  return;
}
 
public SetDataPdu   getExemplar()
{
  return (SetDataPdu)exemplar.clone();
}
 
public void setExemplar(SetDataPdu newExemplar)
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

public DatumSpecification getDatumInformation()
{ return (DatumSpecification)datumInformation.clone();
}
public void setDatumInformation(DatumSpecification pDatumSpecification)
{ datumInformation = pDatumSpecification;
}

} 
 
