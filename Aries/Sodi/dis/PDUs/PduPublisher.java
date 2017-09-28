package PDUs;           // our package name

/**
 *
 *  PduPublisher interface for addListener () and removeListener () methods.
 *
 *  This is an interface, a list of methods that a class can implement.
 *  If the class does in fact implement all of the methods declared here,
 *  it is said to implement the interface.
 *
 *  This interface specifies two methods, addListener() and removeListener().
 *  The first adds a passed-in object to the list of objects that will
 *  receive PDUs that we publish. The second removes it from the list.
 *  This is intended to be similar to the Java 1.1 event model.
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
 *
 * @see PduSubscriber
 */

public interface PduPublisher
{
  /**
  addListener adds newListener to the list of objects that will recieve
  PDUs identified by the triplet entityID. 
  */
  public void addListener(PduSubscriber newListener, EntityID entityID);

  /**
  removeListener removes the object passed in from the list of objects
  that will recieve PDUs identified by the triplet entityID. If the
  object is not on the list, this does nothing.
  */
  public void removeListener(PduSubscriber listenerToRemove, EntityID entityID);
}
