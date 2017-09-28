package mil.navy.nps.dis;

/**
 *
 *  PduSubscriber interface for receivePDU () methods.
 *
 *  This is an interface, a list of methods that a class can implement.
 *  If the class does in fact implement all of the methods declared here,
 *  it is said to implement the interface.
 *
 *  The interface here consists of a single method, receivePDU(). Presumably
 *  this object has registered itself with a PDUPublisher object via 
 *  an addListener() method. That object will in turn send PDUs to us.
 *  The intention here is to closely match the technique and methods
 *  used in the Java 1.1 event model.
 *
 *  This is an example of the Observer pattern. See _Design Patterns_,
 *  Gamma, Helm, Johnson, & Vlissides, pp. 293-303.
 *
 * @version 1.0
 * @author <a href="mailto:mcgredo@nps.navy.mil">Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/PduPublisher.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/PduPublisher.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/PduPublisher.java">
 *  ~/mil/navy/nps/dis/PduPublisher.java</a>
 *
 *  HISTORY
 *  11/9/98 DMcG new
 *
 * @see PduPublisher
 */

public interface PduSubscriber
{
  /**
  Receive a PDU that has been forwarded to us by a PduPublisher.
  */
  public void receivePDU(ProtocolDataUnit pPDU);  
}
