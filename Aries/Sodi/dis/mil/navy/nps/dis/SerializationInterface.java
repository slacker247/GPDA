/*
 File:		SerializationInterface.java
 CVS Info:	$Id: SerializationInterface.java,v 1.2 1998/08/03 20:00:59 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;           // our package name

import java.io.*;

/**
 * Defines interface methods governing object serialization.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/SerializationInterface.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/SerializationInterface.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/SerializationInterface.java">
 *  ~/mil/navy/nps/dis/SerializationInterface.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>This interface defines the API for serialization of objects in a DIS format, 
 *  which is not the format of the serialization interface of the core API.
 *
 *<dt><b>Explanation:</b>
 *<dd>The SerializationInterface defines the API that all objects
 *  that are "pieces" of a PDU must implement to be written out on the
 *  wire--serialization and deserialization. Note that the Java 1.1 API
 *  defines its own interface for serialization, and in an ideal world
 *  we'd be using that, rather than defining our own. But the Java
 *  serialization code wants to write things out in its own format, more
 *  or less untouched by human hands. That doesn't work very well in our
 *  situation, where we are trying to make things _exactly_ correspond to
 *  the defined format for the DIS protocol, which almost certainly are
 *  not java-based. Perhaps a better, but more wordy, term for this
 *  would be "SerializeObjectToDISFormat".<p>
 *
 *WHAT'S AN INTERFACE?<p>
 *
 *  An interface defines a set of methods. Usually the set of methods
 *  is related to some operation, such as drag and drop--the class must
 *  implement several methods to fully implement the behavior, and the
 *  method definitions need to be consistent across every class that 
 *  implements the behavior. <p>
 *
 *  So what's the advantage to Interfaces over inheritence? After all, we
 *  might create an abstract class, and have all the classes that inherit
 *  from that class implement the set of methods defined in the interface.
 *  The advantage is that interfaces don't have to be applied to classes
 *  in the same class hierarchy. In our case, the serialization interface
 *  is defined, and both the PduElement class tree and the Unsigned number
 *  class tree implement the interface. These classes are related only in
 *  that they have a common ancestor, which we can't modify, and probably
 *  wouldn't want to anyway, since we would then have to implement the methods
 *  for every intermediate class. Java doesn't do multiple inheritence 
 *  (thank the elder gods), so we can't do mix-ins.<p>
 *
 *  By declaring that a class implements an interface, we can make compile-time
 *  checks to confirm that the class does indeed have the correct method
 *  prototypes. Interfaces are also useful for determining at runtime whether
 *  a class can perform the operations required to implement a behavior.<p>
 *
 *<dt><b>History:</b>
 *<dd>		06Oct96   /Don McGregor    /New
 *<dd>		09Oct96   /Don McGregor    /Changed name to SerializationInterface, added to mil.navy.nps.dis package
 *<dd>		17Oct96   /Don McGregor    /Modified explanatory comments
 *<dd>		10Mar97   /Don McGregor    /Changes for javadoc
 *<dd>		08Dec97   /Ronan Fauglas   /Changes for documentation templates + complements in documentation
 *
 *<dt><b>References</b>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis/">http://SISO.sc.ist.ucf.edu/dis/</a>
 *<dd>		Latest DIS specification : IEEE 1278.1
 */
public interface SerializationInterface
{
    /**
     *Writes an object out in DIS format.
     *
     *@param outputstream the targetted output stream for the object
     */
    public void serialize(DataOutputStream outputStream);   // write an object out in DIS format


    /**
     *Reads an object in from DIS format.
     *
     *@param inputstream the input stream that builds the object
     */
    public void deSerialize(DataInputStream outputStream);  // read an object in from DIS format
};

