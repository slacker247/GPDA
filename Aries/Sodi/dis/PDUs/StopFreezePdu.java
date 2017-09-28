/*
 File:		StopFreezePdu.java
 CVS Info:	$Id: StopFreezePdu.java,v 1.3 1998/01/28 08:28:25 mcgredo Exp $
 Compiler:	jdk 1.3 
 */

package PDUs;                                       // package to which we belong

import mil.navy.nps.util.*;         // General-purpose utilities
import disEnumerations.*;         // Enumerations for DIS

import java.util.*;                                             // utility stuff we need
import java.io.*;                                               // input/output for serialization

/**
 * Simulation management PDU to stop or freeze an exercise.
 *
 *@version 1.0
 *@author Antonio Alexandre Rua (<A HREF="http://www.garfield.fe.up.pt/~alexrua">http://www.garfield.fe.up.pt/~alexrua</A>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/StopFreezePdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/StopFreezePdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/StopFreezePdu.java">
 *  ~/mil/navy/nps/dis/StopFreezePdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>  The stopping of freezing of an entity/exercise shall be communicated using a Stop/Freeze PDU.
 *
 *<dt><b>History:</b>
 *<dd>		16Sep97	/Antonio Alexandre Rua	/New
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changed access methods: thisVariable() --> getThisVariable(), changed sizeof to "relative size"
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary :<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/a2.htm">
 *		Stop/Freeze PDU</a>	
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 4.4.5.4, 5.4.6.4
 *
 *@see ProtocolDataUnit
 *@see PduElement 
 *@see SerializationInterface
 */
public class StopFreezePdu extends SimulationManagementFamily
{
  /**
   *Real-World Time - This field shall specify the real-world time (UTC) 
   *at which the entity is to stop/freeze in the exercise.
   */
  protected ClockTime realWorldTime;

  /**
   *Reason - This field shall specify the reason that an entity or exercise was stopped/frozen.  
   *
   *<dl>
   *<dt><b>Value:</b>
   *<dd>Enumeration. See references below for value.
   *<dt><b>References:</b>
   *<dd>	see Section 7 in EBV-DOC
   *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/a0.htm">
   *		Reason Field</a>
   *</dl>
   */
  protected UnsignedByte reason;

  /**
   *Frozen Behavior - This field shall specify the internal behavior of the simulation 
   *and its appearance while frozen to the other participants of the exercise. 
   *
   *<dl>
   *<dt><b>Value:</b>
   *<dd>Enumeration. See references below for value.
   *<dt><b>References:</b>
   *<dd>	see Section 7 in EBV-DOC
   *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/a1.htm">
   *		Frozen Behavior Field</a>
   *</dl>
   */
  protected UnsignedByte frozenBehavior;

  /**
   *A 16 bit Padding Field. It is unused.
   *
   *<dl>
   *<dt><b>References:</b>
   *<dd>	
   *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/2a.htm">
   *		Padding - 16bit Field</a>
   *</dl>
   */
  protected UnsignedShort padding;

  /**
   *Request ID - This field shall identify the specific and unique stop/freeze request 
   *being made by the simulation manager.
   *
   *<dt><b>References:</b>
   *<dd>	
   *<dd>	DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/99.htm">Request ID Field</a>
   */
  protected UnsignedInt requestID;        //identifies entity creation request by SM
      
  /**
   *Constant value--size of a Stop/Freeze PDU including header.
   *<code>sizeOf = 40 bytes</code>
   */
  public static final int sizeOf = 40; // The total size of PDU is known

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
  static protected StopFreezePdu exemplar;

/**
 *Default constructor. Initializes everything to zero, basically.
 */
public StopFreezePdu()                       // default constructor
{
  setPduType(PduTypeField.STOPFREEZE);
  realWorldTime =  new ClockTime();
  reason =         new UnsignedByte();
  frozenBehavior = new UnsignedByte();
  padding =        new UnsignedShort();
  requestID =      new UnsignedInt();

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
  realWorldTime.serialize(outputStream);
  reason.serialize(outputStream);
  frozenBehavior.serialize(outputStream);
  padding.serialize(outputStream);
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
  realWorldTime.deSerialize(inputStream);
  reason.deSerialize(inputStream);
  frozenBehavior.deSerialize(inputStream);
  padding.deSerialize(inputStream);
  requestID.deSerialize(inputStream);

  return;
}

public Object clone()
{
 /**
    Clone the CommentPdu, being careful to not share any pointers between the
    new object and the old object.
  */

  StopFreezePdu newPdu = (StopFreezePdu)super.clone();     // new Create Entity Pdu pdu

  newPdu.setRealWorldTime(this.getRealWorldTime());
  newPdu.setReason(this.getReason());
  newPdu.setFrozenBehavior(this.getFrozenBehavior());
  // dont need to copy padding it is unused
  newPdu.setRequestID(this.getRequestID());

  return newPdu;
}

public int length()
{
  return sizeOf;
}

public String pduName()
{
  return new String("Stop Freeze PDU");
}

public void printValues(int indentLevel, PrintStream printStream)
{
   // print the values of the object out, with correct level of
  // indentation on the page.
     
  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

  printStream.println();
  printStream.println("Stop Freeze PDU");

  // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
  // so the header info will be indented a bit less.
  if(superclassIndent > 0)
    superclassIndent -= 1;
  super.printValues(superclassIndent, printStream);
  
  printStream.println(indent + "Real-World Time");
  realWorldTime.printValues(indentLevel+1, printStream);
  printStream.println(indent + "Reason         : "+reason.intValue());
  printStream.println(indent + "Frozen Behavior: "+frozenBehavior.intValue());
  printStream.println(indent + "Padding        : ");  //padding is unused dont need to print value
  printStream.println(indent + "Request ID     : "+requestID.longValue());

  return;
}

public StopFreezePdu getExemplar()
{
    return (StopFreezePdu)exemplar.clone();
}
public void setExemplar(StopFreezePdu newExemplar)
{
    exemplar = newExemplar;
}


// Accessor methods
public ClockTime getRealWorldTime()
{  return (ClockTime)realWorldTime.clone();
}
public void setRealWorldTime(ClockTime pRealWorldTime)
{  realWorldTime=pRealWorldTime;
}
public void setRealWorldTime(long pHour, long pTimePastHour)
{  realWorldTime=new ClockTime(pHour,pTimePastHour);
}

public UnsignedByte getReason()
{  return (UnsignedByte)reason.clone();
}
public void setReason(UnsignedByte pReason)
{  reason=pReason;
}
public void setReason(int pReason)
{  reason=new UnsignedByte(pReason);
}

public UnsignedByte getFrozenBehavior()
{  return (UnsignedByte)frozenBehavior.clone();
}
public void setFrozenBehavior(UnsignedByte pFrozenBehavior)
{  frozenBehavior=pFrozenBehavior;
}
public void setFrozenBehavior(int pFrozenBehavior)
{  frozenBehavior=new UnsignedByte(pFrozenBehavior);
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

}   // end of class StopFreezePdu
